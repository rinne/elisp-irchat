;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-cta.el,v 1.3 1996/12/19 19:49:48 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

(eval-when-compile
  (require 'irchat-globals)  
  (require 'irchat-vars)
  (require 'irchat-inlines))

;;;
;;; decode and encode of binary data
;;;
(defun irchat-quote-decode (string-to-decode)
  (interactive)
  (save-excursion
    (set-buffer (get-buffer-create "*IRC DECODE*"))
    (delete-region (point-min) (point-max))
    (insert string-to-decode)
    (goto-char (point-min))
    (replace-string "\\\\" "\\")
    (goto-char (point-min))
    (replace-string "\\a" "")
    (goto-char (point-min))
    (replace-string "\\n" "\n")
    (goto-char (point-min))
    (replace-string "\\r" "\r")
    (setq string-to-decode (buffer-substring (point-min) (point-max)))
    string-to-decode))

(defun irchat-quote-encode (string)
  (interactive)
  (save-excursion
    (set-buffer (get-buffer-create "*IRC ENCODE*"))
    (delete-region (point-min) (point-max))
    (insert string)
    (goto-char (point-min))
    (while (search-forward "\\" nil 1)
      (insert "\\"))
    (goto-char (point-min))
    (while (search-forward "" nil 1)
      (delete-char -1)
      (insert "\\a"))
    (goto-char (point-min))
    (while (search-forward "" nil 1)
      (delete-char -1)
      (insert "\\0"))
    (goto-char (point-min))
    (while (search-forward "\n" nil 1)
      (delete-char -1)
      (insert "\\n"))
    (goto-char (point-min))
    (while (search-forward "\r" nil 1)
      (delete-char -1)
      (insert "\\r"))
    (setq string (buffer-substring (point-min) (point-max)))
    string))


(defun irchat-ctl-a-action-msg (from rest)
  (irchat-w-insert irchat-D-buffer (format "*** Action: %s %s\n" from rest)))


(defun irchat-ctl-a-msg (from rest)
  "It's ctl-a request, act on it."
  (let* (left now right message rest-of-line hook)
    (if (string-match "^\\([^]*\\)\\([^]*\\)\\(.*\\)" rest)
	(progn
	  (setq left (matching-substring rest 1))
	  (setq now (matching-substring rest 2))
	  (setq right (matching-substring rest 3))
	  (setq rest (concat left right))
	  (if (string-match "^\\([^ ]*\\) \\(.*\\)" now)
	      (progn
		(setq message (downcase (matching-substring now 1)))
		(setq rest-of-line (matching-substring now 2)))
	    (if (string-match "^\\([^ ]*\\)" now)
		(progn
		  (setq message (downcase (matching-substring now 1)))
		  (setq rest-of-line nil))
	      (progn
		(setq message "errmsg")
		(setq rest-of-line "Couldn't figure out what was said."))))
	  (if (and (boundp 
		    (setq hook
			  (intern 
			   (concat "irchat-ctl-a-" message "-msg-hook"))))
		   (eval hook)
		   (eq (eval (list hook from rest-of-line)) t))
	      ;; If we have a hook, and it returns T, do nothing more
	      nil
	    ;; else call the handler
	    (if (fboundp (setq fun (intern
				    (concat "irchat-ctl-a-" message "-msg"))))
		(progn
		  (eval (list fun from rest-of-line)))
	      (progn
		(irchat-send "NOTICE %s :ERRMSG %s :%s"
			     from
			     (upcase message)
			     (format irchat-client-error-msg (upcase message)))
		(message (format "CLIENT %s query from %s."
				 (upcase message) from)))))))
    rest))


(defun irchat-ctl-a-client-msg (from rest)
  (let* (message rest-of-line hook)
    (if rest
	(progn
	  (if (string-match "^\\([^ ]*\\) \\(.*\\)" rest)
	      (progn
		(setq message (downcase (matching-substring rest 1)))
		(setq rest-of-line (matching-substring rest 2)))
	    (if (string-match "^\\([^ ]*\\)" rest)
		(progn
		  (setq message (downcase (matching-substring rest 1)))
		  (setq rest-of-line nil))
	      (progn
		(setq message "errmsg")
		(setq rest-of-line "Couldn't figure out what was said."))))
	  (if (and (boundp 
		    (setq hook
			  (intern 
			   (concat "irchat-ctl-a-" message "-msg-hook"))))
		   (eval hook)
		   (eq (eval (list hook from rest-of-line)) t))
	      ;; If we have a hook, and it returns T, do nothing more
	      nil
	    ;; else call the handler
	    (if (fboundp (setq fun (intern
				    (concat "irchat-ctl-a-" message "-msg"))))
		(progn
		  (eval (list fun from rest-of-line)))
	      (message (format 
			"IRCHAT: Unknown CLIENT message from %s \"%s\""
			from (upcase message)))))))))	      


(defun irchat-ctl-a-version-msg (from rest)
  (let ((emacs-type (emacs-version))
	(m1)
	(emacs-subtype "GNU-emacs"))
    (if (string-match "\\(.*\\) of .*" emacs-type) 
	(progn
	  (setq m1 (format "%s" (matching-substring emacs-type 1)))
	  (if (string-match "\\(.*\\) (.*)" m1)
	      (setq emacs-subtype (format "%s" (matching-substring m1 1)))
	    (setq emacs-subtype m1))))
    (irchat-send 
     (format "NOTICE %s :VERSION %s %s :%s for %s" 
	     from irchat-version emacs-subtype irchat-version emacs-subtype))
    (message (format "CLIENT VERSION query from %s." from))))


(defun irchat-ctl-a-userinfo-msg (from rest)
  (irchat-send "NOTICE %s :USERINFO %s"
	       from irchat-client-userinfo)
  (message (format "CLIENT USERINFO query from %s." from)))

(defun irchat-ctl-a-clientinfo-msg (from rest)
  (irchat-send 
   (format 
    "NOTICE %s :CLIENTINFO :VERSION USERINFO CLIENTINFO X-FACE HELP ERRMSG" from))
  (message (format "CLIENT CLIENTINFO query from %s." from)))


(defun irchat-ctl-a-help-msg (from rest)
  (irchat-send 
   (format 
    "NOTICE %s :HELP :VERSION gives version of this client" 
    from))
  (irchat-send
   (format 
    "NOTICE %s :HELP :USERINFO gives user supplied info (if any)" 
    from))
  (irchat-send 
   (format 
    "NOTICE %s :HELP :CLIENTINFO gives commands this client knows" 
    from))
  (irchat-send 
   (format 
    "NOTICE %s :HELP :X-FACE gives you user supplied X-Face (if exists)" 
    from))
  (irchat-send 
   (format 
    "NOTICE %s :HELP :HELP gives this help message" 
    from))
  (irchat-send 
   (format 
    "NOTICE %s :HELP :ERRMSG tells you your command was not valid" 
    from))
  (message (format "CLIENT HELP query from %s." from)))


(defun irchat-ctl-a-comment-msg (from rest)  
  (message (format "CLIENT COMMENT query from %s." from)))


(defun irchat-ctl-a-xyzzy-msg (from rest)
  (irchat-send "NOTICE %s :Nothing happens.(xyzzy inactive)" from)
  (message (format "CLIENT XYZZY query from %s." from)))


(defun irchat-ctl-a-ping-msg (from rest)
  (if (not rest)
      (setq rest ""))
  (irchat-send "NOTICE %s :PING %s" from rest)
  (message (format "CLIENT PING query from %s." from)))

(defun irchat-ctl-a-x-face-msg (from rest)
  (irchat-send "NOTICE %s :X-FACE %s"
	       from irchat-client-x-face)
  (message (format "CLIENT X-FACE query from %s." from)))

;;;
;;; read CLIENT messages from notice, no postprocessing done
;;;

(defun irchat-ctl-a-notice (prefix rest)
  "Ctl-a notice."
  (let* (message rest-of-line hook) 
    (if (string-match "\\([^ ]*\\) \\(.*\\)" rest)
	  (setq message (downcase (matching-substring rest 1))
		rest-of-line (matching-substring rest 2))
      (if (string-match "\\([^ ]*\\)" rest)
	  (setq message (downcase (matching-substring rest 1))
		rest-of-line nil)
	(setq message "errmsg"
	      rest-of-line "Couldn't figure out what was said.")))

    (if (and (boundp 
	      (setq hook
		    (intern (concat "irchat-ctl-a-" message "-notice-hook"))))
	     (eval hook)
	     (eq (eval (list hook prefix rest-of-line)) t))
	;; If we have a hook, and it returns T, do nothing more
	nil
      ;; else call the handler
      (if (fboundp (setq fun (intern
			      (concat "irchat-ctl-a-" message "-notice"))))
	  (progn
	    (eval (list fun prefix rest-of-line)))
	(message (format 
		  "IRCHAT: Unknown ctl-a notice \":%s %s %s\"" 
		  prefix (upcase message) rest-of-line))))))


(defun irchat-ctl-a-file-notice (prefix rest)
  (if irchat-file-accept
      (let* (message file-name rest-of-line hook)
	(string-match "^\\([^ ]*\\) \\([^ ]*\\) :\\(.*\\)" rest)
	(setq message (downcase (matching-substring rest 1)))
	(setq file-name (matching-substring rest 2))
	(setq rest-of-line (matching-substring rest 3))
	(if (and (boundp 
		  (setq hook
			(intern (concat "irchat-file-" message "-hook"))))
		 (eval hook)
		 (eq (eval (list hook prefix file-name rest-of-line)) t))
	    ;; If we have a hook, and it returns T, do nothing more
	    nil
	  ;; else call the handler
	  (if (fboundp (setq fun (intern
				  (concat "irchat-file-" message))))
	      (progn
		(eval (list fun prefix file-name rest-of-line)))
	    (message (format 
		      "IRCHAT: Unknown FILE message \":%s %s %s %s\""
		      prefix (upcase message) file-name rest-of-line)))))
    (message (format
	      "FILE: %s sending, set irchat-file-accept and ask to resend"
	      prefix))))

(defun irchat-file-start (prefix name data)
  (save-excursion
    (set-buffer (get-buffer-create (format "*IRC R_FILE_%s*" name)))
    (delete-region (point-min) (point-max))
    (insert data)))

(defun irchat-file-cont (prefix name data)
  (save-excursion
    (set-buffer (get-buffer-create (format "*IRC R_FILE_%s*" name)))
    (goto-char (point-max))
    (insert data)))


(defun irchat-file-end (prefix name data)
  (save-excursion
    (set-buffer (get-buffer-create (format "*IRC R_FILE_%s*" name)))
    (goto-char (point-max))
    (insert data)
    (let (str)
      (setq str (buffer-string))
      (delete-region (point-min) (point-max))
      (insert (irchat-quote-decode str)))
    (goto-char (point-min))
    (if (and irchat-file-confirm-save
	     (not (y-or-n-p "Save file?")))
	nil
      (progn
	(if (not (file-exists-p "~/.irchat"))
	    (shell-command (format "mkdir %s"
				   (expand-file-name ".irchat" "$HOME"))))
	(write-region (point-min) (point-max) (expand-file-name 
					       (file-name-nondirectory
						(format "%s(%s)" name prefix))
					       "~/.irchat"))
	(kill-buffer (get-buffer-create (format "*IRC R_FILE_%s*" name)))))))


(defun irchat-ctl-a-client-notice (prefix rest)
  (let* (message rest-of-line hook)
    (if (string-match "^\\([^ ]*\\) \\(.*\\)" rest)
	(setq message (downcase (matching-substring rest 1))
	      rest-of-line (matching-substring rest 2))
      (if (string-match "^\\([^ ]*\\)" rest)
	  (setq message (downcase (matching-substring rest 1))
		rest-of-line nil)
	(setq message "errmsg"
	      rest-of-line "Couldn't figure out what was said.")))
    (if (and (boundp (setq hook
			   (intern (concat 
				    "irchat-client-" message "-notice-hook"))))
	     (eval hook)
	     (eq (eval (list hook prefix rest-of-line)) t))
	;; If we have a hook, and it returns T, do nothing more
	nil
      ;; else call the handler
      (if (fboundp (setq fun (intern
			      (concat "irchat-client-" message "-notice"))))
	  (progn
	    (eval (list fun prefix rest-of-line)))
	(message (format 
		  "IRCHAT: Unknown CLIENT notice \":%s %s %s %s\""
		  prefix (upcase message) rest-of-line))))))


(defvar irchat-client-message "CLIENT@%s: %s\n" 
  "*Message in which info of other clients is displayed.")


(defun irchat-client-version-notice (prefix rest)  
  (if rest
      (if (string-match "^\\([^:]*\\):\\(.*\\)" rest)
	  (irchat-w-insert irchat-D-buffer (format irchat-client-message prefix 
			  (matching-substring rest 1)))
	(irchat-w-insert irchat-D-buffer (format irchat-client-message prefix rest)))
    (message (format "Empty CLIENT version notice from \"%s\"." prefix))))


(defun irchat-client-clientinfo-notice (prefix rest)  
  (irchat-w-insert irchat-D-buffer (format irchat-client-message prefix rest)))


(defun irchat-client-userinfo-notice (prefix rest)  
  (irchat-w-insert irchat-D-buffer (format irchat-client-message prefix rest)))


(defun irchat-client-help-notice (prefix rest)  
  (irchat-w-insert irchat-D-buffer (format irchat-client-message prefix rest)))


(defun irchat-client-x-face-notice (prefix rest)  
  (irchat-w-insert irchat-D-buffer (format irchat-client-message prefix rest)))


(defun irchat-client-errmsg-notice (prefix rest)  
  (irchat-w-insert irchat-D-buffer (format irchat-client-message prefix rest)))

(defun irchat-client-comment-notice (from rest)  
  (message (format "CLIENT COMMENT query from %s." from)))

(defun irchat-client-ping-notice (from rest)  
  (let ((timenow (current-time)))
    (irchat-w-insert 
     irchat-Dialogue-buffer 
     (format 
      "PING time from %s: %ds\n" 
      from
      (+ (* 65536 (- (car timenow) (car irchat-ctcp-ping-time)))
	 (- (car (cdr timenow)) (car (cdr irchat-ctcp-ping-time))))))))

;;;
(defun irchat-ctl-a-version-notice (prefix rest)  
  (if rest
      (if (string-match "^\\([^:]*\\):\\(.*\\)" rest)
	  (irchat-w-insert irchat-D-buffer (format 
					    irchat-client-message prefix 
					    (matching-substring rest 1)))
	(if (string-match "^\\([^ ]*\\) \\([^ ]*\\) \\([^ ]*\\) \\(.*\\)" rest)
	    (irchat-w-insert irchat-D-buffer 
			     (format irchat-client-message 
				     prefix
				     (format "%s %s for %s: %s" 
					     (matching-substring rest 1)
					     (matching-substring rest 2)
					     (matching-substring rest 3)
					     (matching-substring rest 4))))
	  (irchat-w-insert irchat-D-buffer 
			   (format irchat-client-message prefix rest))))
    (message (format "Empty CLIENT version notice from \"%s\"." prefix))))


(defun irchat-ctl-a-clientinfo-notice (prefix rest)  
  (irchat-w-insert irchat-D-buffer (format irchat-client-message prefix rest)))


(defun irchat-ctl-a-userinfo-notice (prefix rest)  
  (irchat-w-insert irchat-D-buffer (format irchat-client-message prefix rest)))


(defun irchat-ctl-a-help-notice (prefix rest)  
  (irchat-w-insert irchat-D-buffer (format irchat-client-message prefix rest)))


(defun irchat-ctl-a-errmsg-notice (prefix rest)  
  (irchat-w-insert irchat-D-buffer (format irchat-client-message prefix rest)))


(defun irchat-ctl-a-x-face-notice (prefix rest)  
  (irchat-w-insert irchat-D-buffer (format irchat-client-message prefix rest)))


(defun irchat-ctl-a-comment-notice (from rest)  
  (message (format "CLIENT COMMENT query from %s." from)))

(defun irchat-ctl-a-ping-notice (from rest)  
  (let ((timenow (current-time)))
    (irchat-w-insert 
     irchat-Dialogue-buffer 
     (format 
      "PING time from %s: %ds\n" 
      from
      (+ (* 65536 (- (car timenow) (car irchat-ctcp-ping-time)))
	 (- (car (cdr timenow)) (car (cdr irchat-ctcp-ping-time))))))))
;;;
;;; eof
;;;
