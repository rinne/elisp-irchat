;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-obsolete.el,v 1.2 1997/10/19 19:46:34 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

(eval-when-compile (require 'irchat-inlines))

(defun irchat-Command-kill (kill-nickname-var &optional timeout silent)
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
