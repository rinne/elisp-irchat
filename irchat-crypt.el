;;;  -*- emacs-lisp -*-
;;;
;;; see file irchat-copyright.el for change log and copyright info

(eval-when-compile (require 'irchat-inlines))

(eval-and-compile  
  (require 'irchat-vars)
  (require 'irchat-misc)
  (require 'idea))

(defvar irchat-crypt-version-default 3
  "Which encryption version to use as default (1-3).  1 is proved insecure.")

(defvar irchat-default-idea-key-list '() 
  "List to save ADDRESS KEY pairs")

(defvar irchat-known-idea-key-list '()
  "List of known IDEA decryption keys.")

(defconst irchat-idea-encrypt-msg-format-1 "|*E*|IDEA|1.0|%s|%s|")
(defconst irchat-idea-encrypt-msg-format-2 "|*E*|IDEA|2.0|%s|%s|")
(defconst irchat-idea-encrypt-msg-format-3 "|*E*|IDEA|3.0|%s|%s|")

(defun irchat-idea-encrypt-msg-format (&optional version)
  (if (not (numberp version)) (setq version irchat-crypt-version-default))
  (cond ((= version 1) irchat-idea-encrypt-msg-format-1)
	((= version 2) irchat-idea-encrypt-msg-format-2)
	((= version 3) irchat-idea-encrypt-msg-format-3)
	(t nil)))

(defun irchat-encrypted-message-p (message)
  (if (string-match "^|\\*E\\*|[^|]*|[0-9][0-9]*\\.[0-9][0-9]*|[^|]*|[^|]*|$"
		    message)
      t
    nil))

(defun irchat-crypt-address-has-default-key-p (address)
  "Is there a default key for ADDRESS."
  (if (assoc-ci-regexp-rev address irchat-default-idea-key-list)
      t
    nil))

(defun irchat-init-crypt ()
  "Initialize crypt variables"
  (while irchat-crypt-known-keys
    (irchat-Command-add-new-key (car irchat-crypt-known-keys))
    (setq irchat-crypt-known-keys (cdr irchat-crypt-known-keys)))
  (while irchat-crypt-default-keys
    (let ((c (car irchat-crypt-default-keys)))
      (if (and (consp c)
	       (stringp (car c))
	       (stringp (cdr c)))
	  (irchat-Command-set-default-key (car c) (cdr c))
	(if (and (listp c)
		 (stringp (nth 0 c))
		 (stringp (nth 1 c))
		 (or (null (nth 2 c)) (numberp (nth 2 c))))
	    (irchat-Command-set-default-key (nth 0 c) (nth 1 c) (nth 2 c)))))
    (setq irchat-crypt-default-keys (cdr irchat-crypt-default-keys)))
  t)

(defun irchat-read-passphrase (&optional prompt)
  "PROMPT for passphrase.  Use secure keyboard if possible."
  (if (null prompt)
      (setq prompt ""))
  (if (and (fboundp 'read-passwd)
	   irchat-crypt-secure-passphrase-read)
      (read-passwd prompt)
    (read-from-minibuffer prompt)))

(defun irchat-Command-add-new-key (key-var &optional interactive-p)
  "Add new KEY to known decryption keys list"
  (interactive (list (irchat-read-passphrase "Add passphrase: ") t))
  (let* ((my-key-type (idea-legal-key key-var))
	 (my-key (cond ((equal 'key-complete-encryption my-key-type)
			(error "KEY is not an decryption key."))
		       ((equal 'key-complete-decryption my-key-type)
			key-var)
		       ((or (equal 'key-string my-key-type)
			    (equal 'key-intlist my-key-type))
			(idea-build-decryption-key key-var 1))
		       (t (error "Invalid key."))))
	 (my-key-2 (if (or (equal 'key-string my-key-type)
			   (equal 'key-intlist my-key-type))
		       (idea-build-decryption-key key-var 2)
		     nil))
	 (my-key-3 (if (or (equal 'key-string my-key-type)
			   (equal 'key-intlist my-key-type))
		       (idea-build-decryption-key key-var 3)
		     nil))
	 (fingerprint (idea-key-fingerprint my-key))
	 (fingerprint-2 (if my-key-2 (idea-key-fingerprint my-key-2) nil))
	 (fingerprint-3 (if my-key-3 (idea-key-fingerprint my-key-3) nil)))
    (setq irchat-known-idea-key-list 
	  (cons (cons fingerprint my-key)
		(remassoc fingerprint
			  irchat-known-idea-key-list)))
    (if (and my-key-2 fingerprint-2)
	(setq irchat-known-idea-key-list 
	      (cons (cons fingerprint-2 my-key-2)
		    (remassoc fingerprint-2
			      irchat-known-idea-key-list))))
    (if (and my-key-3 fingerprint-3)
	(setq irchat-known-idea-key-list 
	      (cons (cons fingerprint-3 my-key-3)
		    (remassoc fingerprint-3
			      irchat-known-idea-key-list))))
    (if interactive-p
	(message (format "Added new decryption key (%s%s%s)."
			 fingerprint
			 (if fingerprint-2 
			     (concat " & " fingerprint-2)
			   "")
			 (if fingerprint-3
			     (concat " & " fingerprint-3)
			   ""))))))

(defun irchat-Command-delete-key (key-var &optional interactive-p)
  "Delete a KEY from known decryption keys list"
  (interactive (list (irchat-read-passphrase "Delete passphrase: ") t))
  (let ((fingerprint-1 (idea-key-fingerprint key-var 1))
	(fingerprint-2 (idea-key-fingerprint key-var 2))
	(fingerprint-3 (idea-key-fingerprint key-var 3)))
    (setq irchat-known-idea-key-list (remassoc fingerprint-1
					       irchat-known-idea-key-list))
    (setq irchat-known-idea-key-list (remassoc fingerprint-2
					       irchat-known-idea-key-list))
    (setq irchat-known-idea-key-list (remassoc fingerprint-3
					       irchat-known-idea-key-list))
    (if interactive-p
	(message (format "Removed decryption keys (%s and %s and %s)." 
			 fingerprint-1 
			 fingerprint-2
			 fingerprint-3)))))

(defun irchat-get-idea-decryption-key (fingerprint)
  "Find decryption key associated with FINGERPRINT"
  (let ((k (assoc fingerprint irchat-known-idea-key-list)))
    (if k
	(cdr k)
      nil)))

(defun irchat-get-idea-encryption-key (address &optional version)
  (let ((r (assoc-ci-regexp-rev address 
				irchat-default-idea-key-list)))
    (if r
	(let* ((ve (cond ((numberp (nth 7 r)) (nth 7 r)) ; In default key
			 ((numberp version) version)
			 (t irchat-crypt-version-default))))
	  (cond ((= ve 1) (nth 2 r))
		((= ve 2) (nth 4 r))
		((= ve 3) (nth 6 r))
		(t nil)))
      nil)))

(defun irchat-Command-set-default-key (addr-var 
				       pass-var
				       &optional version-var
				       interactive-p)
  "Set a default key for ADDRESS (channel/nick) to be KEY"
  (interactive (let (addr-var pass-var)
		 (setq addr-var (irchat-completing-default-read
				 "Default key for channel/user: "
				 (append irchat-nick-alist
					 irchat-channel-alist)
				 '(lambda (s) t) nil irchat-privmsg-partner))
		 (setq pass-var (irchat-read-passphrase "Passphrase: "))
		 (if (string= pass-var "")
		     (setq pass-var nil))	 
		 (list addr-var pass-var nil t)))
  (if (null pass-var)
      (let ((addr-var (upcase addr-var)))
	(setq irchat-default-idea-key-list
	      (remassoc addr-var irchat-default-idea-key-list))
	(if interactive-p
	    (message (format "Removed a default key from \"%s\"." addr-var))))
    (let* ((addr-var (upcase addr-var))
	   (e-key-1 (if (or (null version-var)
			    (= 1 version-var))
			(idea-build-encryption-key pass-var 1)
		      nil))
	   (fingerprint-1 (if e-key-1 
			      (idea-key-fingerprint e-key-1)
			    nil))
	   (e-key-2 (if (or (null version-var)
			    (= 2 version-var))
			(idea-build-encryption-key pass-var 2)
		      nil))
	   (fingerprint-2 (if e-key-2
			      (idea-key-fingerprint e-key-2)
			    nil))
	   (e-key-3 (if (or (null version-var)
			    (= 3 version-var))
			(idea-build-encryption-key pass-var 3)
		      nil))
	   (fingerprint-3 (if e-key-3
			      (idea-key-fingerprint e-key-3)
			    nil)))
      (irchat-Command-add-new-key pass-var)
      (setq irchat-default-idea-key-list 
	    (cons (list addr-var
			fingerprint-1 e-key-1
			fingerprint-2 e-key-2
			fingerprint-3 e-key-3
			version-var
			pass-var)
		  (remassoc addr-var 
			    irchat-default-idea-key-list)))
      (if interactive-p
	  (message (format "Added a default key for \"%s\"." addr-var)))
      (irchat-set-crypt-indicator))))

(defun irchat-make-encrypted-message (message key)
  "Build an encrypted message from MESSAGE with KEY"
  (let ((version (idea-key-version key)))
    (format (irchat-idea-encrypt-msg-format (if (numberp version) version))
	    (idea-key-fingerprint key)
	    (idea-cbc-encrypt-string message key))))

(defun irchat-crypt-valid-version-p (method major minor)
  "Is METHOD, MAJOR, MINOR a valid encryption method?"
  (and (string= method "IDEA")
       (or (= major 1)
	   (= major 2)
	   (= major 3))
       (>= minor 0)))

(defun irchat-encrypt-message (message address &optional no-clear-text)
  "Encrypt MESSAGE to ADDRESS.  NO-CLEAR-TEXT prohibits cleartext output"
  (let ((key (irchat-get-idea-encryption-key address)))
    (cond ((and no-clear-text
		(null key))
	   (error (format "No default key associated with \"%s\"." address)))
	  ((null key)
	   message)
	  (t (irchat-make-encrypted-message 
	      (concat irchat-real-nickname 
		      "" 
		      (irchat-generate-hex-timestamp)
		      ""
		      message)
	      key)))))

(defun irchat-decrypt-message (message)
  "Decrypt MESSAGE"
  (if (string-match "^|\\*E\\*|\\([^|]*\\)|\\([0-9][0-9]*\\)\\.\\([0-9][0-9]*\\)|\\([^|]*\\)|\\([^|]*\\)|$"
		    message)
      (let ((method (substring message (match-beginning 1) (match-end 1)))
	    (version-major (string-to-number (substring message
							(match-beginning 2)
							(match-end 2))))
	    (version-minor (string-to-number (substring message
							(match-beginning 3)
							(match-end 3))))
	    (fingerprint (substring message (match-beginning 4) (match-end 4)))
	    (msg (substring message (match-beginning 5) (match-end 5))))
	; (list method version-major version-minor fingerprint msg)
	(if (irchat-crypt-valid-version-p method version-major version-minor)
	    (let ((key (irchat-get-idea-decryption-key fingerprint)))
	      (if key
		  (let ((r (idea-cbc-decrypt-string msg key)))
		    (if r
			(if (string-match 
			     "^\\([^][^]*\\)\\([^][^]*\\)\\(.*\\)$"
			     r)
			    (let ((nick (substring r 
						   (match-beginning 1)
						   (match-end 1)))
				  (time (substring r 
						   (match-beginning 2)
						   (match-end 2)))
				  (msg (substring r 
						   (match-beginning 3)
						   (match-end 3))))

			      (list 'success nick time msg fingerprint))
			  (list
			   'error nil nil
			   "** Unable to decrypt: Invalid cleartext format!"
			   fingerprint))
		      (list
		       'error nil nil
		       "** Unable to decrypt: Decryption failed!"
		       fingerprint)))
		(list
		 'error nil nil
		 "** Unable to decrypt: No key!"
		 fingerprint)))
	  (list
	   'error nil nil
	   "** Unable to decrypt: Unknown version!"
	   fingerprint)))
    (list
     'error nil nil
     "** Unable to decrypt: Invalid message!"
     nil)))


(eval-and-compile
  (provide 'irchat-crypt))

;;; eof (irchat-crypt.el)
