;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat.el,v 3.3 2002/06/04 15:47:27 tri Exp $
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
      (load-file "irchat-time.el")
      (load-file "irchat-cta.el")
      (load-file "irchat-misc.el")
      (load-file "irchat-main.el")
      (load-file "irchat-msn-vars.el")
      (load-file "irchat-msn-handle.el")
      (load-file "irchat-msn-proc.el")
      (load-file "irchat-msn-cmd.el")
      (load-file "irchat-msn-sub.el")
      (load-file "irchat-msn-msg.el")
      (load-file "irchat-msn.el")))

