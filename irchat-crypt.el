;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-crypt.el,v 3.7 1997/03/05 13:41:39 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

(eval-when-compile (require 'irchat-inlines))
(eval-and-compile  
  (require 'irchat-vars)
  (require 'irchat-misc)
  (require 'idea))

(defvar irchat-default-idea-key-list '() "List to save ADDRESS KEY pairs")
(defvar irchat-known-idea-key-list '())

(defconst irchat-idea-encrypt-msg-format "|*E*|IDEA|1.0|%s|%s|")

(defun irchat-encrypted-message-p (message)
  (if (string-match "^|\\*E\\*|[^|]*|[0-9][0-9]*\\.[0-9][0-9]*|[^|]*|[^|]*|$"
		    message)
      t
    nil))

(defun irchat-init-crypt ()
  (if (null irchat-known-idea-key-list)
      (let ((lst irchat-crypt-known-keys))
	(while lst
	  (irchat-Command-add-new-key (car lst))
	  (setq lst (cdr lst)))))
  (if (null irchat-default-idea-key-list)
      (let ((lst irchat-crypt-default-keys))
	(while lst
	  (irchat-Command-set-default-key (car (car lst)) (cdr (car lst)))
	  (setq lst (cdr lst))))))

(defun irchat-read-passphrase (&optional prompt)
  "PROMPT for passphrase.  Use comint if possible."
  (if (null prompt)
      (setq prompt ""))
  (if (and (fboundp 'comint-read-noecho)
	   irchat-crypt-use-comint-on-passphrase)
      (comint-read-noecho prompt)
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
			(idea-build-decryption-key key-var))
		       (t (error "Invalid key."))))
	 (fingerprint (idea-key-fingerprint my-key)))
    (setq irchat-known-idea-key-list 
	  (cons (cons fingerprint my-key)
		(remassoc fingerprint
			  irchat-known-idea-key-list)))
    (if interactive-p
	(message (format "Added new decryption key (%s)." fingerprint)))))

(defun irchat-Command-delete-key (key-var &optional interactive-p)
  "Delete a KEY from known decryption keys list"
  (interactive (list (irchat-read-passphrase "Delete passphrase: ") t))
  (let ((fingerprint (idea-key-fingerprint key-var)))
    (setq irchat-known-idea-key-list (remassoc fingerprint
					       irchat-known-idea-key-list))
    (if interactive-p
	(message (format "Removed decryption key (%s)." fingerprint)))))

(defun irchat-get-idea-decryption-key (fingerprint)
  "Find decryption key associated with FINGERPRINT"
  (let ((k (assoc fingerprint irchat-known-idea-key-list)))
    (if k
	(cdr k)
      nil)))


(defun irchat-Command-set-default-key (addr-var pass-var)
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
		 (list addr-var pass-var)))
  (if (null pass-var)
      (let ((addr-var (upcase addr-var)))
	(setq irchat-default-idea-key-list
	      (remassoc addr-var irchat-default-idea-key-list))
	(message (format "Removed a default key from \"%s\"." addr-var)))
    (let* ((addr-var (upcase addr-var))
	   (e-key (idea-build-encryption-key pass-var))
	   (d-key (idea-build-decryption-key pass-var))
	   (print (idea-key-fingerprint d-key)))
      (irchat-Command-add-new-key d-key)
      (setq irchat-default-idea-key-list 
	    (cons (list addr-var print e-key d-key)
		  (remassoc addr-var 
			    irchat-default-idea-key-list)))
      (message (format "Added a default key for \"%s\"." addr-var)))))

(defun irchat-make-encrypted-message (message key)
  "Build an encrypted message from MESSAGE with KEY"
  (format irchat-idea-encrypt-msg-format
	  (idea-key-fingerprint key)
	  (idea-cbc-encrypt-string message key)))

(defun irchat-crypt-valid-version-p (method major minor)
  "Is METHOD, MAJOR, MINOR a valid encryption method?"
  (and (string= method "IDEA")
       (= major 1)
       (>= minor 0)))

(defun irchat-encrypt-message (message address &optional no-clear-text)
  "Encrypt MESSAGE to ADDRESS.  NO-CLEAR-TEXT prohibits cleartext output"
  (let ((key (car 
	      (cdr
	       (cdr
		(assoc-ci-regexp-rev address 
				     irchat-default-idea-key-list))))))
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


(eval-and-compile (provide 'irchat-crypt))

;;; eof (irchat-crypt.el)
