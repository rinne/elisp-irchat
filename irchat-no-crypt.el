;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-no-crypt.el,v 1.1 1998/11/04 10:54:33 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;;
;;; Non functional supplements for interactive crypt functions.
;;;

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

;;; eof
