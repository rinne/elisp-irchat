;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-test.el,v 3.1 1997/02/24 16:00:02 tri Exp $
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

