;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-no-crypt.el,v 1.2 2009/07/13 20:29:32 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;;
;;; Non functional supplements for interactive crypt functions.
;;;

(eval-when-compile
  (require 'irchat-inlines))

(eval-and-compile  
  (require 'irchat-vars))

(defun irchat-Command-add-new-key (key-var &optional interactive-p)
  (interactive (list nil t))
  (error "Crypto not supported in this version of Irchat."))

(defun irchat-Command-delete-key (key-var &optional interactive-p)
  (interactive (list nil t))
  (error "Crypto not supported in this version of Irchat."))

(defun irchat-Command-set-default-key (addr-var 
				       pass-var
				       &optional version-var
				       interactive-p)
  (interactive (list nil nil nil t))
  (error "Crypto not supported in this version of Irchat."))

(eval-and-compile
  (provide 'irchat-crypt))
;;; eof
