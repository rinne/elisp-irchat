;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-test.el,v 3.2 2009/07/13 20:29:32 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

(if (not (featurep 'irchat))
    (progn
;;;      (setq load-path (append (list ".") load-path))
      (load-file "irchat-globals")
      (load-file "irchat-vars")
      (load-file "irchat-inlines")
      (load-file "irchat-filter")
      (load-file "irchat-utf8")
      (load-file "irchat-dcc")
      (load-file "irchat-caesar")
      (load-file "irchat-000")
      (load-file "irchat-200")
      (load-file "irchat-300")
      (load-file "irchat-400")
      (load-file "irchat-500")
      (load-file "irchat-commands")
      (load-file "irchat-copyright")
      (load-file "irchat-cta")
      (load-file "irchat-handle")
      (load-file "irchat-misc")
      (load-file "irchat-timer")
      (load-file "irchat-time")
      (load-file "irchat-main")
      (load-file "irchat-uah-cache")
      (load-file "irchat-global-kill")
      (load-file "irchat-obsolete")
      (load-file "irchat-crypt-vars")
      (load-file "b64")
      (load-file "crc32")
      (load-file "rc4")
      (load-file "idea")
      (load-file "irchat-crypt")
      (load-file "irchat-random")))
