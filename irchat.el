;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat.el,v 3.4 2009/07/13 19:53:42 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;;
;;; This file is inteded to ease testing/developing irchat before compilation.
;;; CD to this directory before evaluating the sexp below.
;;;

(if (not (featurep 'irchat))
    (let ((load-path  (append (list ".") load-path)))
      (load-file "irchat-globals.el")
      (load-file "irchat-vars.el")
      (load-file "irchat-inlines.el")
      (load-file "irchat-filter.el")
      (load-file "irchat-utf8.el")
      (load-file "irchat-dcc.el")
      (load-file "irchat-caesar.el")
      (load-file "irchat-000.el")
      (load-file "irchat-200.el")
      (load-file "irchat-300.el")
      (load-file "irchat-400.el")
      (load-file "irchat-500.el")
      (load-file "irchat-commands.el")
      (load-file "irchat-copyright.el")
      (load-file "irchat-cta.el")
      (load-file "irchat-handle.el")
      (load-file "irchat-misc.el")
      (load-file "irchat-timer.el")
      (load-file "irchat-time.el")
      (load-file "irchat-main.el")
      (load-file "irchat-uah-cache.el")
      (load-file "irchat-global-kill.el")
      (load-file "irchat-obsolete.el")
      (load-file "irchat-crypt-vars.el")
      (load-file "b64.el")
      (load-file "crc32.el")
      (load-file "rc4.el")
      (load-file "idea.el")
      (load-file "irchat-crypt.el")
      (load-file "irchat-random.el")))
