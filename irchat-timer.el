;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-timer.el,v 3.2 1998/05/26 10:35:00 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

(defvar irchat-emacs-major-version 
  (string-to-number (substring emacs-version 0 2)))

(if (= irchat-emacs-major-version 18)
    (progn 
      (defun irchat-start-timer (function interval)
	"Timers are not supported under emacs 18."
	nil)
      (defun  irchat-cancel-timer (timer)
	"Timers are not supported under emacs 18."
	nil))
  (if (or (string-match ".*Lucid.*" (emacs-version))
	  (string-match ".*XEmacs.*" (emacs-version)))
      (progn
	(defun irchat-cancel-timer (timer)
	  "Cancel timer."
	  (delete-itimer timer))
	(defun irchat-start-timer (function interval)
	  "Add timer function."
	  (interactive)
	  (if interval
	      (start-itimer "IRCHAT" function interval interval))))
    (progn
;;      (eval-and-compile
;;	(require 'timer))
      (defun irchat-cancel-timer (timer)
	"Cancel timer."
	(cancel-timer timer))
      (defun irchat-start-timer (func interval)
	"Add timer function."
	(interactive)
	(setq timer-dont-exit t)
	(if interval
	    ;; messy implementation as RMS emacs keeps on killing the 
	    ;; timers signalling errors. (actually it may lose timers 
	    ;; without reason as well...)
	    (run-at-time (format "%d sec" interval) 
			 interval
			 '(lambda (f)
			    (run-at-time "1 sec" nil f)) func))))))
;;;
;;; eof
;;;
