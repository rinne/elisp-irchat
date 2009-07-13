;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-000.el,v 3.4 2009/07/13 18:06:49 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

(eval-when-compile (require 'irchat-inlines))
(eval-and-compile  
  (require 'irchat-filter)
  (require 'irchat-vars))

;;;
;;;  000 replies -- what is the author of ircd thinking
;;;
(defun irchat-handle-000-msgs (number parsed-sender parsed-msg prefix rest)
  (setq irchat-nick-accepted 'ok)
  (if (string-match "[^ ]* \\([^ :]*\\) *\\([^ :]*\\) *:\\(.*\\)" rest)
      (let ((target1 (matching-substring rest 1))
	    (target2 (matching-substring rest 2))
	    (msg (matching-substring rest 3)))
	(cond ((string-equal target1 "")
	       (irchat-w-insert irchat-000-buffer 
				(format "%s%s\n" irchat-info-prefix msg)))
	      ((string-equal target2 "")
	       (irchat-w-insert irchat-000-buffer 
				(format "%s%s (%s)\n"
					irchat-info-prefix msg target1)))
	      (t
	       (irchat-w-insert irchat-000-buffer 
				(format "%s%s %s (%s)\n" irchat-info-prefix
					target1 msg target2)))))
    (message "IRCHAT: Strange %s reply" number)))


(defun irchat-handle-004-msg (parsed-sender parsed-msg prefix rest)
  (if (string-match "[^ ]* \\(.*\\)" rest)
      (let ((msg (matching-substring rest 1)))
	(irchat-w-insert irchat-000-buffer
			 (format "%s%s\n" irchat-info-prefix msg)))
    (message "IRCHAT: Strange 004 reply")))

;;;
;;;  eof
;;;
