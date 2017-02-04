;;;   -*- lisp -*-
;;; 
;;;  ----------------------------------------------------------------------
;;;  b64 - almost base64
;;;  ----------------------------------------------------------------------
;;;  Created      : Thu Feb 20 16:21:28 1997 tri
;;;  Last modified: Sat Feb  4 20:21:48 2017 tri
;;;  ----------------------------------------------------------------------
;;;  Copyright © 1997, 2017
;;;  Timo J. Rinne <tri@iki.fi>
;;;  ----------------------------------------------------------------------
;;;  Any express or implied warranties are disclaimed.  In no event
;;;  shall the author be liable for any damages caused (directly or
;;;  otherwise) by the use of this software.
;;;  
;;;  irchat-copyright.el applies only if used with irchat IRC client.
;;;  Contact the author for additional copyright info.
;;;

(eval-and-compile  
  (provide 'b64))

(defconst b64-alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/"
  "Alphabet used in b64 encoding")
(defvar b64-decode-vector 
  nil
  "Vector used in b64 decoding.  Generated in run-time.")

(defun b64-build-decode-vector ()
  "Generate b64-decode-vector from b64-alphabet"
  (let ((v (make-vector 256 -1))
	(l (length b64-alphabet))
	(i 0))
    (while (< i l)
      (aset v (elt b64-alphabet i) i)
      (setq i (+ 1 i)))
    (aset v ?= -2)
    v))

(defun b64-encode-string (str)
  "B64 encode STR"
  (let* ((l (length str))
	 (n (/ l 3))
	 (i 0)
	 (r ""))
    (while (< i n)
      (let* ((c0 (elt str (* i 3)))
	     (c1 (elt str (+ 1 (* i 3))))
	     (c2 (elt str (+ 2 (* i 3))))
	     (e0 (>> c0 2))
	     (e1 (logior (logand 63 (<< c0 4)) (>> c1 4)))
	     (e2 (logior (logand 63 (<< c1 2)) (>> c2 6)))
	     (e3 (logand 63 c2)))
	(setq r (concat r 
			(char-to-string (elt b64-alphabet e0))
			(char-to-string (elt b64-alphabet e1))
			(char-to-string (elt b64-alphabet e2))
			(char-to-string (elt b64-alphabet e3))))
	(setq i (+ 1 i))))
    (cond ((= (% l 3) 2)
	   (let* ((c0 (elt str (* i 3)))
		  (c1 (elt str (+ 1 (* i 3))))
		  (e0 (>> c0 2))
		  (e1 (logior (logand 63 (<< c0 4)) (>> c1 4)))
		  (e2 (logand 63 (<< c1 2))))
	     (setq r (concat r 
			     (char-to-string (elt b64-alphabet e0))
			     (char-to-string (elt b64-alphabet e1))
			     (char-to-string (elt b64-alphabet e2))
			     "="))))
	  ((= (% l 3) 1)
	   (let* ((c0 (elt str (* i 3)))
		  (e0 (>> c0 2))
		  (e1 (logand 63 (<< c0 4))))
	     (setq r (concat r 
			     (char-to-string (elt b64-alphabet e0))
			     (char-to-string (elt b64-alphabet e1))
			     "==")))))
    r))

(defun b64-decode-string (str)
  "B64 decode STR"
  (if (not b64-decode-vector)
      (setq b64-decode-vector (b64-build-decode-vector)))
  (let* ((l (length str))
	 (n (/ l 4))
	 (i 0)
	 (r ""))
    (if (> (% l 4) 0)
	(message "b64-decode: Trailing garbage ignored"))
    (while (< i n)
      (let ((e0 (elt b64-decode-vector (elt str (* i 4))))
	    (e1 (elt b64-decode-vector (elt str (+ 1 (* i 4)))))
	    (e2 (elt b64-decode-vector (elt str (+ 2 (* i 4)))))
	    (e3 (elt b64-decode-vector (elt str (+ 3 (* i 4))))))
	(cond ((and (>= e0 0) (>= e1 0) (>= e2 0) (>= e3 0))
	       (let ((c0 (logior (logand 255 (<< e0 2)) (>> e1 4)))
		     (c1 (logior (logand 255 (<< e1 4)) (>> e2 2)))
		     (c2 (logior (logand 255 (<< e2 6)) e3)))
		 (setq r (concat r 
				 (char-to-string c0)
				 (char-to-string c1)
				 (char-to-string c2)))))
	      ((and (>= e0 0) (>= e1 0) (>= e2 0) (= e3 -2))
	       (let ((c0 (logior (logand 255 (<< e0 2)) (>> e1 4)))
		     (c1 (logior (logand 255 (<< e1 4)) (>> e2 2))))
		 (setq r (concat r 
				 (char-to-string c0)
				 (char-to-string c1)))))
	      ((and (>= e0 0) (>= e1 0) (= e2 -2) (= e3 -2))
	       (let ((c0 (logior (logand 255 (<< e0 2)) (>> e1 4))))
		 (setq r (concat r (char-to-string c0)))))
	      (t (progn 
		   (setq r nil)
		   (setq i n))))
	(setq i (+ 1 i))))
    r))

;;; eof (b64.el)
