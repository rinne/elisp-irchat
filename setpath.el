;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: setpath.el,v 1.2 1997/02/04 00:03:31 tri Exp $
;;;
;;;  set path for compiling irchat

(setq load-path (append (list ".") load-path))

;;; Emacs 19 has a builtin defsubst.
(if (> 19 (string-to-int (substring emacs-version 0 2)))
    (load "defsubst"))
