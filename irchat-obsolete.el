;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-obsolete.el,v 1.4 1997/10/20 05:57:17 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

(eval-when-compile (require 'irchat-inlines))

;;;
;;;  Ignores
;;;
(defvar irchat-kill-nickname nil
  "*Obsolete.  See irchat-ignore-nickname")

(defvar irchat-kill-message-regexp nil
  "*Obsolete.  See irchat-ignore-message-regexp.")

;(defvar irchat-kill-realname nil
;  "*Obsolete.  See irchat-ignore-realname.")

;(defvar irchat-kill-logon nil
;  "*Obsolete.  See irchat-ignore-logon.")

(defun irchat-append-obsolete-vars ()
  "Append obsolete variable values to the current ones."
  (if irchat-kill-nickname
      (progn
	(message "Obsolete variable irchat-kill-nickname converted.")
	(setq irchat-ignore-nickname (append irchat-ignore-nickname
					     irchat-kill-nickname))
	(setq irchat-kill-nickname nil)
	(setq irchat-save-vars-is-dirty t)))
  (if irchat-kill-message-regexp
      (progn
	(message "Obsolete variable irchat-kill-message-regexp converted.")
	(setq irchat-ignore-message-regexp (append irchat-ignore-message-regexp
						   irchat-kill-message-regexp))
	(setq irchat-kill-message-regexp nil)
	(setq irchat-save-vars-is-dirty t))))

(defun irchat-Command-kill (kill-nickname-var &optional timeout silent)
  "Obsolete front end to irchat-Command-ignore."
  (interactive (let ((kill-nickname-var nil)
		     (timeout nil)
		     (completion-ignore-case t))
		 (setq kill-nickname-var 
		       (completing-read "Ignore nickname or regexp: " 
					(append irchat-nick-alist
						irchat-kill-nickname)
					'(lambda (s) t) nil nil))
		 (if (and (not (string= "" kill-nickname-var))
			  (not (assoc-ci-string kill-nickname-var irchat-kill-nickname)))
		     (setq timeout
			   (string-to-int
			    (read-from-minibuffer "Timeout [RET for none]: "))))
		 (list kill-nickname-var timeout)))

  (let ((msg (concat "Obsolete interface irchat-Command-kill."
		     "  "
		     "Use irchat-Command-ignore.")))
    (message msg)
    (irchat-Command-ignore kill-nickname-var timeout silent)
    (message msg)))
  
  

(defun irchat-Command-kill-by-regexp (kill-regexp-var &optional timeout silent)
  "Obsolete front end to irchat-Command-ignore-by-regexp."
  (interactive (let ((kill-regexp-var nil)
		     (timeout nil)
		     (completion-ignore-case t))
		 (setq kill-regexp-var 
		       (completing-read "Ignore messages matching: " 
					irchat-kill-message-regexp
					'(lambda (s) t) nil nil))
		 (if (and (not (string= "" kill-regexp-var))
			  (not (assoc-ci-string kill-regexp-var
						irchat-kill-message-regexp)))
		     (setq timeout
			   (string-to-int
			    (read-from-minibuffer
			     "Timeout [RET for none]: "))))
		 (list kill-regexp-var timeout)))

  (let ((msg (concat "Obsolete interface irchat-Command-kill-by-regexp."
		     "  "
		     "Use irchat-Command-ignore-by-regexp.")))
    (message msg)
    (irchat-Command-ignore-by-regexp kill-regexp-var timeout silent)
    (message msg)))

(defun irchat-Command-global-kill (nickname timeout reason)
  "Obsolete front end to irchat-Command-global-ignore."
  (interactive (let ((nickname-var nil)
		     (timeout 0)
		     (reason ""))
		 (setq nickname
		       (completing-read "Global ignore nickname: "
					irchat-nick-alist
					'(lambda (s) t) nil nil))
		 (if (not (string= "" nickname))
		     (progn
		       (while (not (and (> timeout 0)
					(< timeout 1000)))
			 (setq timeout
			       (string-to-int
				(read-from-minibuffer "Timeout [1-99]: "))))
		       (setq reason (read-from-minibuffer "Reason: "))))
		 (list nickname timeout reason)))

  (let ((msg (concat "Obsolete interface irchat-Command-global-kill."
		     "  "
		     "Use irchat-Command-global-ignore.")))
    (message msg)
    (irchat-Command-global-ignore nickname timeout reason)
    (message msg)))

(eval-and-compile (provide 'irchat-obsolete))
;;;
;;; eof
;;;
