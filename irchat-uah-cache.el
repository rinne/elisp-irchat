;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-uah-cache.el,v 1.2 1997/10/19 16:03:53 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info
;;;
;;; Nick to user-at-host cache by jsl.
;;; Modifications by tri.
;;;
;;; Created              : Mon Mar 10 17:07:39 1997 tri
;;; Integrated to Irchat : Sun Oct 19          1997 tri
;;;

(eval-when-compile (require 'irchat-inlines))

(defconst irchat-nick-to-uah-vector-length 128
  "*Vector length")

(defvar irchat-nick-to-uah-pos 0
  "Current update position")

(defvar irchat-nick-to-uah-vector nil
  "*Vector to store nick to user-at-host mappings")


(defun irchat-nick-to-uah-init ()
  (setq irchat-nick-to-uah-vector
	(make-vector irchat-nick-to-uah-vector-length (cons "" "")))
  (setq irchat-nick-to-uah-pos 0))


(defun irchat-nick-to-uah-append (nick uah)
  "Append NICK UAH tuple into the vector"
  (if (null irchat-nick-to-uah-vector)
      (irchat-nick-to-uah-init))
  (if (and (stringp nick)
	   (stringp uah)
	   (> (length nick) 0)
	   (> (length uah) 0))
      (progn
	(aset irchat-nick-to-uah-vector irchat-nick-to-uah-pos (cons nick uah))
	(setq irchat-nick-to-uah-pos (mod (+ irchat-nick-to-uah-pos 1)
					  irchat-nick-to-uah-vector-length)))))


(defun irchat-nick-to-uah (nick)
  "Find uah associated with NICK from cache if possible."
  (if (null irchat-nick-to-uah-vector)
      (irchat-nick-to-uah-init))
  (let ((pos (if (= irchat-nick-to-uah-pos 0)
		 (- irchat-nick-to-uah-vector-length 1)
	       (- irchat-nick-to-uah-pos 1))))
    (while (and (not (string-ci-equal nick (car (elt irchat-nick-to-uah-vector
						     pos))))
		(not (= pos irchat-nick-to-uah-pos)))
      (setq pos (if (= pos 0) 
		    (- irchat-nick-to-uah-vector-length 1)
		  (- pos 1))))
    (if (string-ci-equal nick (car (elt irchat-nick-to-uah-vector pos)))
	(cdr (elt irchat-nick-to-uah-vector pos))
      nil)))


(defun irchat-convert-uah-to-ignore-list (uah)
  "Convert UAH-string to list of regexps to be ignored."
  (let ((usr nil)
	(dom nil))
    (cond ((string-match "^\\([^ \t@][^ \t@]*\\)@[^ \t@][^ \t@]*\\.\\([^ \t@.][^ \t@.]*\\.[^ \t@.][^ \t@.]*\\)$"
			 uah)
	   (setq usr (substring uah (match-beginning 1) (match-end 1)))
	   (setq dom (substring uah (match-beginning 2) (match-end 2))))
	  ((string-match "^\\([^ \t@][^ \t@]*\\)@\\([^ \t@.][^ \t@.]*\\.[^ \t@.][^ \t@.]*\\)$"
			 uah)
	   (setq usr (substring uah (match-beginning 1) (match-end 1)))
	   (setq dom (substring uah (match-beginning 2) (match-end 2)))))
    (if (and usr dom)
	(progn
	  (setq usr (regexp-quote usr))
	  (setq dom (regexp-quote dom))
	  (list (format "%s@%s" usr dom) (format "%s@.*\\.%s" usr dom)))
      (list (regexp-quote uah)))))

(eval-and-compile (provide 'irchat-uah-cache))
;;;
;;; eof
;;;
