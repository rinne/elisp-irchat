;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-test.el,v 1.1 1996/12/19 14:54:52 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

(if (not (featurep 'irchat))
    (progn
;;;      (setq load-path (append (list ".") load-path))
      (load "irchat-commands")
      (load "irchat-handle")
      (load "irchat-000")
      (load "irchat-200")
      (load "irchat-300")
      (load "irchat-400")
      (load "irchat-500")
      (load "irchat-timer")
      (load "irchat-misc")
      (load "irchat-main")))

