;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-500.el,v 3.5 2009/07/13 20:29:32 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

(eval-when-compile
  (require 'irchat-inlines))

(eval-and-compile  
  (require 'irchat-filter)
  (require 'irchat-vars))

;;;
;;;  500 replies -- ERRORS
;;;
(defun irchat-handle-500-msgs (number parsed-sender parsed-msg prefix rest)
  (if (string-match "[^ ]* \\([^ :]*\\) *\\([^ :]*\\) *:\\(.*\\)" rest)
      (let ((target1 (matching-substring rest 1))
	    (target2 (matching-substring rest 2))
	    (msg (matching-substring rest 3)))
	(cond ((string-equal target1 "")
	       (irchat-w-insert 
		irchat-500-buffer
		(format "%s%s\n" irchat-error-prefix msg)))
	      ((string-equal target2 "")
	       (irchat-w-insert 
		irchat-500-buffer 
		(format "%s%s (%s)\n" irchat-error-prefix msg target1)))
	      (t
	       (irchat-w-insert 
		irchat-500-buffer 
		(format "%s%s %s (%s)\n" 
			irchat-error-prefix
			target1 
			msg
			target2))))
	)
    (message "IRCHAT: Strange %s reply" number)))
;;;
;;; eof
;;;


