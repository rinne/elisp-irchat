;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: setpath.el,v 1.1 1996/12/19 14:54:53 tri Exp $
;;;
;;;  set path for compiling irchat

(setq load-path (append (list ".") load-path))
(load "defsubst")
