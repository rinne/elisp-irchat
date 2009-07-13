;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-random.el,v 1.3 2009/07/13 20:29:32 tri Exp $
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
					 (format 
					  "%c%c%c%c%c%c%c%c%c%c"
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255))
					 (if (fboundp 'emacs-pid)
					     (prin1-to-string 
					      (emacs-pid))
					   "*")
					 (format 
					  "%c%c%c%c%c%c%c%c%c%c"
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255))
					 (if (fboundp 'user-login-name)
					     (prin1-to-string 
					      (user-login-name))
					   "*")
					 (format 
					  "%c%c%c%c%c%c%c%c%c%c"
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255))
					 (if (fboundp 'emacs-version)
					     (prin1-to-string 
					      (emacs-version))
					   "*")
					 (format 
					  "%c%c%c%c%c%c%c%c%c%c"
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255)
					  (logand (random) 255))))))
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

(eval-and-compile
  (provide 'irchat-random))
;;;
;;; eof (irchat-random.el)
;;;
