;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-500.el,v 3.1 1997/02/24 16:00:02 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

(eval-when-compile (require 'irchat-inlines))

;;;
;;;  500 replies -- ERRORS
;;;
(defun irchat-handle-500-msgs (number prefix rest)
  (if (string-match "[^ ]* \\([^ :]*\\) *\\([^ :]*\\) *:\\(.*\\)" rest)
      (let ((target1 (matching-substring rest 1))
	    (target2 (matching-substring rest 2))
	    (msg (matching-substring rest 3)))
	(cond ((string-equal target1 "")
	       (irchat-w-insert 
		irchat-500-buffer
		(format "*** Error: %s\n" msg)))
	      ((string-equal target2 "")
	       (irchat-w-insert 
		irchat-500-buffer 
		(format "*** Error: %s (%s)\n" msg target1)))
	      (t
	       (irchat-w-insert 
		irchat-500-buffer 
		(format "*** Error: %s %s (%s)\n" target1 msg target2))))
	)
    (message "IRCHAT: Strange %s reply" number)))
;;;
;;; eof
;;;


