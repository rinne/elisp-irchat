;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-utf8.el,v 3.3 2009/07/13 22:32:13 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;;
;;; UTF-8 kludge implementation by tri@iki.fi.
;;;

(eval-and-compile
  (require 'irchat-utf8-table))

(defun irchat-utf8-kludge-nth-char (str n)
  "Get Nth character from STR as integer or nil if not in range."
  (if (and (stringp str)
	   (>= n 0)
	   (> (length str) n))
      (+ 0 (elt str n))
    nil))

(defun irchat-utf8-kludge-decode (str)
  "Decode the first character from STR as utf-8 if possible and return
as (code . rest) pair containing the character code of the first
character and the rest of the string"
  (let ((b1 (irchat-utf8-kludge-nth-char str 0))
	(b2 (irchat-utf8-kludge-nth-char str 1))
	(b3 (irchat-utf8-kludge-nth-char str 2))
	(b4 (irchat-utf8-kludge-nth-char str 3)))
    (cond ((or (null b1)
	       (= b1 0))
	   ; This is empty string or string that contains NIL characters
	   ; and it simply means that caller is out of luck
	   nil)
	  ((< b1 128)
	   ; This is ASCII
	   (cons b1 (substring str 1)))
	  ((and (>= b1 192)
		(< b1 224)
		(not (null b2))
		(>= b2 128)
		(< b2 192))
	   ; This is 2 byte encoded UTF-8 character
	   (cons (+ (* 64 (- b1 192))
		    (- b2 128))
		 (substring str 2)))
	  ((and (>= b1 224)
		(< b1 240)
		(not (null b2))
		(>= b2 128)
		(< b2 192)
		(not (null b3))
		(>= b3 128)
		(< b3 192))
	   ; This is 3 byte encoded UTF-8 character
	   (cons (+ (* 64 64 (- b1 224))
		    (* 64 (- b2 128))
		    (- b3 128))
		 (substring str 3)))
	  ((and (>= b1 240)
		(< b1 248)
		(not (null b2))
		(>= b2 128)
		(< b2 192)
		(not (null b3))
		(>= b3 128)
		(< b3 192)
		(not (null b4))
		(>= b4 128)
		(< b4 192))
	   ; This is 4 byte encoded UTF-8 character
	   (cons (+ (* 64 64 64 (- b1 224))
		    (* 64 64 (- b2 128))
		    (* 64 (- b3 128))
		    (- b4 128))
		 (substring str 4)))
	  (t
	   ; This is NOT UTF-8 encoding at all and is returned as is
	   (cons b1 (substring str 1))))))

(defun irchat-utf8-kludge-visible-char (ch)
  "Convert a character code into a visible string."
  (let ((x))
    (cond ((< ch 128)
	   (char-to-string ch))
	  ((setq x (assq ch irchat-utf8-kludge-assoc-list))
	   (cdr x))
	  ((< ch 256)
	   (char-to-string ch))
	  (t
	   (format "[U+%04x]" ch)))))


(defun irchat-utf8-kludge-decode-string (str)
  "Decode STR as utf-8 opportunisticly."
  (let ((r "")
	(e nil))
    (while (setq e (irchat-utf8-kludge-decode str))
	(setq r (concat r (irchat-utf8-kludge-visible-char (car e))))
	(setq str (cdr e)))
    r))

(eval-and-compile
  (provide 'irchat-utf8))
;;;
;;; eof
;;;
