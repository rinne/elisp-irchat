;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-random.el,v 1.1 1998/06/23 14:46:28 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info
;;;
(eval-and-compile  
  (require 'rc4))

(defvar irchat-random-state nil
  "State of the random generator in irchat.")

(defun irchat-random-8 ()
  (if (null irchat-random-state)
      (setq irchat-random-state (rc4-make-state 
				 (concat (if (fboundp 'current-time)
					     (prin1-to-string 
					      (current-time))
					   "*")
					 (if (fboundp 'emacs-pid)
					     (prin1-to-string 
					      (emacs-pid))
					   "*")
					 (if (fboundp 'user-login-name)
					     (prin1-to-string 
					      (user-login-name))
					   "*")
					 (if (fboundp 'emacs-version)
					     (prin1-to-string 
					      (emacs-version))
					   "*")))))
  (rc4-random irchat-random-state))

(defun irchat-random-16 ()
  (let ((x1 (irchat-random-8))
	(x2 (irchat-random-8)))
    (+ x1 (* x2 256))))

(defun irchat-random-24 ()
  (let ((x1 (irchat-random-8))
	(x2 (irchat-random-8))
	(x3 (irchat-random-8)))
    (+ x1 (* x2 256) (* x3 65536))))

(eval-and-compile (provide 'irchat-random))
;;;
;;; eof (irchat-random.el)
;;;