;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat.el,v 3.1 1997/02/24 16:00:02 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;;
;;; This file is inteded to ease testing/developing irchat before compilation.
;;; CD to this directory before evaluating the sexp below.
;;;

(if (not (featurep 'irchat))
    (let ((load-path  (append (list ".") load-path)))
      (load-file "irchat-commands.el")
      (load-file "irchat-handle.el")
      (load-file "irchat-000.el")
      (load-file "irchat-200.el")
      (load-file "irchat-300.el")
      (load-file "irchat-400.el")
      (load-file "irchat-500.el")
      (load-file "irchat-timer.el")
      (load-file "irchat-cta.el")
      (load-file "irchat-misc.el")
      (load-file "irchat-main.el")))

