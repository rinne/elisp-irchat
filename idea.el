;;;   -*- lisp -*-
;;; 
;;;  ----------------------------------------------------------------------
;;;  IDEA encryption in elisp.  Cool, ha?
;;;  ----------------------------------------------------------------------
;;;  Created      : Thu Jun 29 08:11:25 1995 tri
;;;  Last modified: Thu Jun 25 02:29:12 1998 tri
;;;  ----------------------------------------------------------------------
;;;  Copyright © 1995-1998
;;;  Timo J. Rinne <tri@iki.fi>
;;; 
;;;  Address: Cirion oy, PO-BOX 250, 00121 Helsinki, Finland
;;;  ----------------------------------------------------------------------
;;;  Any express or implied warranties are disclaimed.  In no event
;;;  shall the author be liable for any damages caused (directly or
;;;  otherwise) by the use of this software.
;;;  
;;;  irchat-copyright.el applies only if used with irchat IRC client.
;;;  Contact the author for additional copyright info.
;;;
;;;  $Id: idea.el,v 3.18 1998/06/24 23:29:29 tri Exp $
;;;

(eval-and-compile  
  (require 'b64)
  (require 'crc32))

(eval-and-compile  
  (provide 'idea))

(defvar idea-default-key-expand-version 3
  "Which version of key expand is used as default (1 or 2 or 3).")

;;;; 16bit basic arithmetic operations for IDEA.
(defun idea-mask-16bit (x)
  "Mask X with 0xffff"
  (logand 65535 x))

(defun idea-+ (a b)
  "16 bit add A and B.  Ignore overflow."
  (idea-mask-16bit (+ a b)))

(defun idea-- (a b)
  "16 bit add A and B.  Ignore overflow."
  (idea-mask-16bit (- a b)))

(defun idea-/ (a b)
  "16 bit add A and B.  Ignore overflow."
  (idea-mask-16bit (/ a b)))

(defun idea-% (a b)
  "16 bit add A and B.  Ignore overflow."
  (idea-mask-16bit (%  a b)))

(defun idea-* (a b)
  "16 bit multiply A and B.  Ignore overflow."
  (let* ((al (logand 255 a))
	 (ah (logand 255 (/ a 256)))
	 (bl (logand 255 b))
	 (bh (logand 255 (/ b 256)))
	 (r1 (* al bl))
	 (r2 (+ (* al bh) (* ah bl)))
	 (l (idea-mask-16bit (+ r1 (* r2 256)))))
    l))

(defun idea-xor-blocks (a b)
  "Perform logical xor operation between encryption blocks A and B"
  (list (idea-^ (nth 0 a) (nth 0 b))
	(idea-^ (nth 1 a) (nth 1 b))
	(idea-^ (nth 2 a) (nth 2 b))
	(idea-^ (nth 3 a) (nth 3 b))))

(defun idea-& (a b)
  "16 bit AND A and B.  Ignore overflow."
  (logand (idea-mask-16bit a) (idea-mask-16bit b)))

(defun idea-| (a b)
  "16 bit OR A and B.  Ignore overflow."
  (logior (idea-mask-16bit a) (idea-mask-16bit b)))

(defun idea-^ (a b)
  "16 bit XOR A and B.  Ignore overflow."
  (logxor (idea-mask-16bit a) (idea-mask-16bit b)))

(defun idea-mul (a b)
  "A * B in idea style."
  (let* ((al (logand 255 a))
	 (ah (logand 255 (/ a 256)))
	 (bl (logand 255 b))
	 (bh (logand 255 (/ b 256)))
	 (r1 (* al bl))
	 (r2 (+ (* al bh) (* ah bl)))
	 (r3 (* ah bh))
	 (l (+ r1 (* (logand 255 r2) 256)))
	 (h (+ (/ r2 256) (/ l 65536) r3))
	 (l (idea-mask-16bit l)))
    (if (not (and (= 0 l) (= 0 h)))
	(idea-mask-16bit (idea-+ (idea-- l h) (if (< l h) 1 0)))
      (if (= 0 a)
	  (idea-mask-16bit (idea-- 1 b))
	(idea-mask-16bit (idea-- 1 a))))))

(defun idea-random ()
  "Generate 16 bit random value"
  (if (fboundp 'irchat-random-16)
      (irchat-random-16)
    (idea-mask-16bit (abs (random)))))

(defun idea-random-char ()
  "Generate 8 bit random value"
  (if (fboundp 'irchat-random-8)
      (irchat-random-8)
    (idea-& 255 (idea-random))))

(defun idea-<< (x n)
  "Shift integer X left N bits"
  (if (> n 16)
      (setq n 16))
  (setq x (idea-mask-16bit x))
  (if (or (< n 0)
	  (< x 0))
      0
    (while (> n 0)
      (setq x (idea-mask-16bit (* x 2)))
      (setq n (- n 1))))
  x)

(defun idea->> (x n)
  "Shift integer X right N bits"
  (if (> n 16)
      (setq n 16))
  (setq x (idea-mask-16bit x))
  (if (or (< n 0)
	  (< x 0))
      0
    (while (> n 0)
      (setq x (/ x 2))
      (setq n (- n 1))))
  x)

(defun idea-additive-inverse (x)
  (idea-mask-16bit (- 0 x)))

(defun idea-multiplicative-inverse (n)
  "16 bit multiplicative inverse of N"
  (let ((x (idea-mask-16bit n)))
    (if (< x 2)
	x
      (let ((hlp2 (idea-/ 65537 x))
	    (y  (idea-% 65537 x))
	    (do-brk '()))
	(if (= 1 y)
	    (idea-mask-16bit (idea-- 1 hlp2))
	  (let ((hlp1 1)
		(q)
		(ret))
	    (while (not do-brk)
	      (setq q (idea-/ x y))
	      (setq x (idea-% x y))
	      (setq hlp1 (idea-+ hlp1 (idea-* q hlp2)))
	      (if (= 1 x)
		  (progn
		    (setq ret (idea-mask-16bit hlp1))
		    (setq do-brk t))
		(progn
		  (setq q (idea-/ y x))
		  (setq y (idea-% y x))
		  (setq hlp2 (idea-+ hlp2 (idea-* q hlp1)))
		  (if (= 1 y)
		      (progn
			(setq ret (idea-mask-16bit (idea-- 1 hlp2)))
			(setq do-brk t))))))
	    ret))))))

(defun idea-shift-key-25bits-left (key)
  "Shift expanded (128bit) idea key left 25 bits"
  (list (idea-| (idea-<< (nth 1 key) 9) (idea->> (nth 2 key) 7))
	(idea-| (idea-<< (nth 2 key) 9) (idea->> (nth 3 key) 7))
	(idea-| (idea-<< (nth 3 key) 9) (idea->> (nth 4 key) 7))
	(idea-| (idea-<< (nth 4 key) 9) (idea->> (nth 5 key) 7))
	(idea-| (idea-<< (nth 5 key) 9) (idea->> (nth 6 key) 7))
	(idea-| (idea-<< (nth 6 key) 9) (idea->> (nth 7 key) 7))
	(idea-| (idea-<< (nth 7 key) 9) (idea->> (nth 0 key) 7))
	(idea-| (idea-<< (nth 0 key) 9) (idea->> (nth 1 key) 7))))

;;; [(1 2 3 4) (5 6 7 8) (9 10 11 12)]
;;; transforms to
;;; [(1 5 9 2) (6 10 3 7) (11 4 8 12)]
;;; and
;;; [(1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16)]
;;; transforms to
;;; [(1 5 9 13) (2 6 10 14) (3 7 11 15) (4 8 12 16)]
;;; transforms to
;;; [(1 2 3 4) (5 6 7 8) (9 10 11 12) (13 14 15 16)]
(defun idea-interlace-blocklist (bl)
  "Make interlaced version of BLOCKLIST."
  (let* ((l (length bl))
	 (r (make-vector l 0))
	 (i 0))
    (while (< i l)
      (aset r i
	    (let ((j (* 4 i)))
	      (list (nth (/ j l) (elt bl (% j l)))
		    (nth (/ (+ j 1) l) (elt bl (% (+ j 1) l)))
		    (nth (/ (+ j 2) l) (elt bl (% (+ j 2) l)))
		    (nth (/ (+ j 3) l) (elt bl (% (+ j 3) l))))))
      (setq i (+ i 1)))
    r))

(defun idea-expand-string-to-key (string &optional version)
  "Expand string to full 128bit key (list of 8 16bit ints)"
  (if (null version) (setq version idea-default-key-expand-version))
  (cond ((= version 1) (idea-expand-string-to-key-version-1 string))
	((= version 2) (idea-expand-string-to-key-version-2 string))
	((= version 3) (idea-expand-string-to-key-version-3 string))
	(t (error "Unknown key expansion version"))))

(defun idea-expand-string-to-key-version-1 (string)
  "Expand string to full 128bit key (list of 8 16bit ints) (version 1)"
  (if (= (length string) 0)
      '(0 0 0 0 0 0 0 0)
    (let* ((s (if (< (length string) 64)
		  (let* ((ss string)
			 (ss (concat ss (crc32-string ss)))
			 (ss (concat ss (crc32-string ss)))
			 (ss (concat ss (crc32-string ss)))
			 (ss (concat ss (crc32-string ss)))
			 (ss (concat ss (crc32-string ss)))
			 (ss (concat ss (crc32-string ss)))
			 (ss (concat ss (crc32-string ss)))
			 (ss (concat ss (crc32-string ss))))
		    ss)
		string))
	   (l (length s))
	   (s (concat s l))
	   (l (length s))
	   (x1 0)
	   (x2 (/ l 4))
	   (x3 (* 2 (/ l 4)))
	   (x4 (* 3 (/ l 4)))
	   (v1 (crc32-string (substring s x1 l)))
	   (v2 (crc32-string (substring s x2 l)))
	   (v3 (crc32-string (substring s x3 l)))
	   (v4 (crc32-string (substring s x4 l)))
	   (k1 (idea-4hex-to-int (substring v1 0 4)))
	   (k2 (idea-4hex-to-int (substring v1 4 8)))
	   (k3 (idea-4hex-to-int (substring v2 0 4)))
	   (k4 (idea-4hex-to-int (substring v2 4 8)))
	   (k5 (idea-4hex-to-int (substring v3 0 4)))
	   (k6 (idea-4hex-to-int (substring v3 4 8)))
	   (k7 (idea-4hex-to-int (substring v4 0 4)))
	   (k8 (idea-4hex-to-int (substring v4 4 8))))
      (list k1 k2 k3 k4 k5 k6 k7 k8))))

(defun idea-expand-string-to-key-version-2 (string)
  "Expand string to full 128bit key (list of 8 16bit ints) (version 2)"
  (if (= (length string) 0)
      '(0 0 0 0 0 0 0 0)
    (let* ((s (if (> (length string) 3) 
		  string 
		(concat string (crc32-string string 'raw-string))))
	   (l (length s))
	   (s1 (idea-expand-substring (concat (format "%c%c" 0 (logand l 255)) 
					      (substring s 0 (/ l 4)))))
	   (s2 (idea-expand-substring (concat (format "%c%c" 85 (logand l 255))
					      (substring s (/ l 4) (/ l 2)))))
	   (s3 (idea-expand-substring (concat (format "%c%c" 
						      170 
						      (logand l 255))
					      (substring s  
							 (/ l 2) 
							 (+ (/ l 2)
							    (/ l 4))))))
	   (s4 (idea-expand-substring (concat (format "%c%c" 
						      255 
						      (logand l 255))
					      (substring s
							 (+ (/ l 2) (/ l 4)) 
							 l))))
	   (v1 (crc32-string s1))
	   (v2 (crc32-string s2))
	   (v3 (crc32-string s3))
	   (v4 (crc32-string s4))
	   (k1 (idea-4hex-to-int (substring v1 0 4)))
	   (k2 (idea-4hex-to-int (substring v1 4 8)))
	   (k3 (idea-4hex-to-int (substring v2 0 4)))
	   (k4 (idea-4hex-to-int (substring v2 4 8)))
	   (k5 (idea-4hex-to-int (substring v3 0 4)))
	   (k6 (idea-4hex-to-int (substring v3 4 8)))
	   (k7 (idea-4hex-to-int (substring v4 0 4)))
	   (k8 (idea-4hex-to-int (substring v4 4 8))))
      (list k1 k2 k3 k4 k5 k6 k7 k8))))

;;;
;;; Mystical constants in following function are just digits from pi
;;; taken with following scheme.
;;;   - Digits are split into sequences of five 31415 58979 32384 ...
;;;   - Any sequence forming a number > 65535 is dropped
;;;
(defun idea-expand-string-to-key-version-3 (str)
  "Generate idea key from STRING."
  (if (= (length str) 0)
      '(0 0 0 0 0 0 0 0)
    (let* ((kk '(31415 58979 32384 62643 38327 16939 5820 45923))
	   (ek (idea-build-encryption-key kk))
	   (bl (idea-cleartext-string-to-block-list str nil))
	   (bl (idea-interlace-blocklist bl))
	   (r1 '(7816 40628 62089 3482))
	   (r2 '(53421 17067 13282 30664))
	   (r3 '(44609 55058 22317 25359))
	   (r4 '(40812 17450 28410 27019))
	   (i 0)
	   (l (length bl)))
      (while (< i l)
	(setq r1 (idea-xor-blocks 
		  (idea-crypt-transform-block (idea-xor-blocks 
					       (elt bl i)
					       r2)
					      ek) 
		  r1))
	(if (< (+ i 1) l)
	    (setq r2 (idea-xor-blocks
		      (idea-crypt-transform-block (idea-xor-blocks 
						   (elt bl (+ i 1))
						   r1)
						  ek)
		      r2)))
	(setq i (+ i 1)))
      (setq ek (idea-build-encryption-key (list (nth 0 r1) (nth 0 r2)
						(nth 1 r1) (nth 1 r2)
						(nth 2 r1) (nth 2 r2)
						(nth 3 r1) (nth 3 r2))))
      (setq bl (idea-interlace-blocklist bl))
      (setq i 0)
      (while (< i l)
	(setq r3 (idea-xor-blocks 
		  (idea-crypt-transform-block (idea-xor-blocks 
					       (elt bl i)
					       r4)
					      ek) 
		  r3))
	(if (< (+ i 1) l)
	    (setq r4 (idea-xor-blocks
		      (idea-crypt-transform-block (idea-xor-blocks 
						   (elt bl (+ i 1))
						   r3)
						  ek)
		      r4)))
	(setq i (+ i 1)))
      (list (nth 0 r3) (nth 0 r4) (nth 1 r3)
	    (nth 1 r4) (nth 2 r3) (nth 2 r4)
	    (nth 3 r3) (nth 3 r4)))))

(defun idea-expand-substring (string)
  (if (= (length string) 0)
      string
    (progn
      (setq string (concat (crc32-string string 'raw-string) string))
      (let ((i (+ 3 (logand 3 (elt string 0)))))
	(while (> i 0)
	  (setq i (- i 1))
	  (setq string (concat (crc32-string string 'raw-string) string))))
      string)))

(defun idea-random-key ()
  "Generate random IDEA key (list of 8 16bit values)"
  (list (idea-random) (idea-random) (idea-random) (idea-random)
	(idea-random) (idea-random) (idea-random) (idea-random)))

(defun idea-build-encryption-key (passphrase &optional version)
  "Build idea encryption context from string or keylist (list of 8 16bit ints)"
  (let* ((s1 (if (stringp passphrase) 
		 (idea-expand-string-to-key passphrase version)
	       (if (listp passphrase)
		   passphrase
		 (error "IDEA key can be built from string or keylist."))))
	 (annotation (idea-build-key-annotation s1 
						"e"
						version))
	 (s2 (idea-shift-key-25bits-left s1))
	 (s3 (idea-shift-key-25bits-left s2))
	 (s4 (idea-shift-key-25bits-left s3))
	 (s5 (idea-shift-key-25bits-left s4))
	 (s6 (idea-shift-key-25bits-left s5))
	 (s7 (idea-shift-key-25bits-left s6))
	 ;;; (s8 (idea-shift-key-25bits-left s7))
	 (r1 (list (nth 0 s1) (nth 1 s1) (nth 2 s1) 
		   (nth 3 s1) (nth 4 s1) (nth 5 s1)))
	 (r2 (list (nth 6 s1) (nth 7 s1) (nth 0 s2) 
		   (nth 1 s2) (nth 2 s2) (nth 3 s2)))
	 (r3 (list (nth 4 s2) (nth 5 s2) (nth 6 s2) 
		   (nth 7 s2) (nth 0 s3) (nth 1 s3)))
	 (r4 (list (nth 2 s3) (nth 3 s3) (nth 4 s3) 
		   (nth 5 s3) (nth 6 s3) (nth 7 s3)))
	 (r5 (list (nth 0 s4) (nth 1 s4) (nth 2 s4) 
		   (nth 3 s4) (nth 4 s4) (nth 5 s4)))
	 (r6 (list (nth 6 s4) (nth 7 s4) (nth 0 s5) 
		   (nth 1 s5) (nth 2 s5) (nth 3 s5)))
	 (r7 (list (nth 4 s5) (nth 5 s5) (nth 6 s5) 
		   (nth 7 s5) (nth 0 s6) (nth 1 s6)))
	 (r8 (list (nth 2 s6) (nth 3 s6) (nth 4 s6) 
		   (nth 5 s6) (nth 6 s6) (nth 7 s6)))
	 (r9 (list (nth 0 s7) (nth 1 s7) (nth 2 s7) 
		   (nth 3 s7))))
    (list r1 r2 r3 r4 r5 r6 r7 r8 r9 annotation)))

(defun idea-build-decryption-key (passphrase &optional version)
  "Build idea decryption context from string or keylist (list of 8 16bit ints)"
  (let* ((k (idea-build-encryption-key passphrase version))
	 (annotation (idea-build-key-annotation (list (nth 0 (nth 0 k))
						      (nth 1 (nth 0 k))
						      (nth 2 (nth 0 k))
						      (nth 3 (nth 0 k))
						      (nth 4 (nth 0 k))
						      (nth 5 (nth 0 k))
						      (nth 0 (nth 1 k))
						      (nth 1 (nth 1 k))) 
						"d"
						version)))
    (list
     (list (idea-multiplicative-inverse (nth 0 (nth 8 k)))
	   (idea-additive-inverse (nth 1 (nth 8 k)))
	   (idea-additive-inverse (nth 2 (nth 8 k)))
	   (idea-multiplicative-inverse (nth 3 (nth 8 k)))
	   (nth 4 (nth 7 k))
	   (nth 5 (nth 7 k)))
     (list (idea-multiplicative-inverse (nth 0 (nth 7 k)))
	   (idea-additive-inverse (nth 2 (nth 7 k)))
	   (idea-additive-inverse (nth 1 (nth 7 k)))
	   (idea-multiplicative-inverse (nth 3 (nth 7 k)))
	   (nth 4 (nth 6 k))
	   (nth 5 (nth 6 k)))
     (list (idea-multiplicative-inverse (nth 0 (nth 6 k)))
	   (idea-additive-inverse (nth 2 (nth 6 k)))
	   (idea-additive-inverse (nth 1 (nth 6 k)))
	   (idea-multiplicative-inverse (nth 3 (nth 6 k)))
	   (nth 4 (nth 5 k))
	   (nth 5 (nth 5 k)))
     (list (idea-multiplicative-inverse (nth 0 (nth 5 k)))
	   (idea-additive-inverse (nth 2 (nth 5 k)))
	   (idea-additive-inverse (nth 1 (nth 5 k)))
	   (idea-multiplicative-inverse (nth 3 (nth 5 k)))
	   (nth 4 (nth 4 k))
	   (nth 5 (nth 4 k)))
     (list (idea-multiplicative-inverse (nth 0 (nth 4 k)))
	   (idea-additive-inverse (nth 2 (nth 4 k)))
	   (idea-additive-inverse (nth 1 (nth 4 k)))
	   (idea-multiplicative-inverse (nth 3 (nth 4 k)))
	   (nth 4 (nth 3 k))
	   (nth 5 (nth 3 k)))
     (list (idea-multiplicative-inverse (nth 0 (nth 3 k)))
	   (idea-additive-inverse (nth 2 (nth 3 k)))
	   (idea-additive-inverse (nth 1 (nth 3 k)))
	   (idea-multiplicative-inverse (nth 3 (nth 3 k)))
	   (nth 4 (nth 2 k))
	   (nth 5 (nth 2 k)))
     (list (idea-multiplicative-inverse (nth 0 (nth 2 k)))
	   (idea-additive-inverse (nth 2 (nth 2 k)))
	   (idea-additive-inverse (nth 1 (nth 2 k)))
	   (idea-multiplicative-inverse (nth 3 (nth 2 k)))
	   (nth 4 (nth 1 k))
	   (nth 5 (nth 1 k)))
     (list (idea-multiplicative-inverse (nth 0 (nth 1 k)))
	   (idea-additive-inverse (nth 2 (nth 1 k)))
	   (idea-additive-inverse (nth 1 (nth 1 k)))
	   (idea-multiplicative-inverse (nth 3 (nth 1 k)))
	   (nth 4 (nth 0 k))
	   (nth 5 (nth 0 k)))
     (list (idea-multiplicative-inverse (nth 0 (nth 0 k)))
	   (idea-additive-inverse (nth 1 (nth 0 k)))
	   (idea-additive-inverse (nth 2 (nth 0 k)))
	   (idea-multiplicative-inverse (nth 3 (nth 0 k))))
     annotation)))

(defun idea-encryption-round (data subkey)
  "Basic round of IDEA encryption.  Method 1."
  (let* ((v1  (idea-mul (nth 0 data) (nth 0 subkey)))
	 (v2  (idea-+ (nth 1 data) (nth 1 subkey)))
	 (v3  (idea-+ (nth 2 data) (nth 2 subkey)))
	 (v4  (idea-mul (nth 3 data) (nth 3 subkey)))
	 (v5  (idea-^ v1 v3))
	 (v6  (idea-^ v2 v4))
	 (v7  (idea-mul v5 (nth 4 subkey)))
	 (v8  (idea-+ v6 v7))
	 (v9  (idea-mul v8 (nth 5 subkey)))
	 (v10 (idea-+ v7 v9))
	 (v11 (idea-^ v1 v9))
	 (v12 (idea-^ v3 v9))
	 (v13 (idea-^ v2 v10))
	 (v14 (idea-^ v4 v10)))
    (list v11 v12 v13 v14)))

(defun idea-final-transform (data subkey)
  "Final transformation round of IDEA encryption."
  (let* ((v1 (idea-mul (nth 0 data) (nth 0 subkey)))
	 (v2 (idea-+ (nth 2 data) (nth 1 subkey)))
	 (v3 (idea-+ (nth 1 data) (nth 2 subkey)))
	 (v4 (idea-mul (nth 3 data) (nth 3 subkey))))
    (list v1 v2 v3 v4)))

(defun idea-hex-to-key (h)
  "Expand 32 character HEXSTRING to idea key."
  (let ((h1 (idea-4hex-to-int (substring h 0 4)))
	(h2 (idea-4hex-to-int (substring h 4 8)))
	(h3 (idea-4hex-to-int (substring h 8 12)))
	(h4 (idea-4hex-to-int (substring h 12 16)))
	(h5 (idea-4hex-to-int (substring h 16 20)))
	(h6 (idea-4hex-to-int (substring h 20 24)))
	(h7 (idea-4hex-to-int (substring h 24 28)))
	(h8 (idea-4hex-to-int (substring h 28 32))))
    (if (and (> h1 -1) (> h2 -1) (> h3 -1) (> h4 -1)
	     (> h5 -1) (> h6 -1) (> h7 -1) (> h8 -1))
	(list h1 h2 h3 h4 h5 h6 h7 h8)
      (list 0 0 0 0 0 0 0 0))))

(defun idea-4hex-to-int (hex)
  "Convert four character long HEXSTRING to int"
  (let* ((x0 (idea-hex-char-to-int (elt hex 3)))
	 (x1 (idea-hex-char-to-int (elt hex 2)))
	 (x2 (idea-hex-char-to-int (elt hex 1)))
	 (x3 (idea-hex-char-to-int (elt hex 0))))
    (if (and (> x0 -1) (> x1 -1) (> x2 -1) (> x3 -1))
	(+ x0
	   (* x1 16)
	   (* x2 256)
	   (* x3 4096))
      -1)))

(defun idea-2hex-to-int (hex)
  "Convert two character long HEXSTRING to int"
  (let* ((x0 (idea-hex-char-to-int (elt hex 1)))
	 (x1 (idea-hex-char-to-int (elt hex 0))))
    (if (and (> x0 -1) (> x1 -1))
	(+ x0
	   (* x1 16))
      -1)))

(defun idea-crypt-transform-block (data key)
  "Make IDEA decryption transformation to DATA with key context KEY"
  ;;; Make 8 basic encryption rounds.
  ;;; Swap inner subblocks between basic rounds.
  ;;; Perform final transformation.
  (idea-final-transform
   (idea-encryption-round  
     (idea-encryption-round  
       (idea-encryption-round  
	 (idea-encryption-round  
	   (idea-encryption-round  
	     (idea-encryption-round  
	       (idea-encryption-round  
		 (idea-encryption-round data (nth 0 key))
		(nth 1 key))
	      (nth 2 key))
	    (nth 3 key))
	  (nth 4 key))
	(nth 5 key))
      (nth 6 key))
    (nth 7 key))
   (nth 8 key)))

(defun idea-encrypt-block (data key &optional version)
  "Encrypt single data block (4 * 16bits) with key"
  (let ((my-key (if (listp key) key 
		  (if (stringp key) (idea-build-encryption-key key version)
		    (error 
		     "IDEA key has to be key-context or string.")))))
    (idea-crypt-transform-block data my-key)))

(defun idea-decrypt-block (data key &optional version)
  "Decrypt single data block (4 * 16bits) with key"
  (let ((my-key (if (listp key) key 
		  (if (stringp key) (idea-build-decryption-key key version)
		    (error 
		     "IDEA key has to be key-context or string.")))))
    (idea-crypt-transform-block data my-key)))

(defun idea-fixed-string-to-block (str)
  "Convert 8 byte STRING to idea block"
  (list (idea-| (idea-<< (elt str 0) 8) (elt str 1))
	(idea-| (idea-<< (elt str 2) 8) (elt str 3))
	(idea-| (idea-<< (elt str 4) 8)	(elt str 5))
	(idea-| (idea-<< (elt str 6) 8)	(elt str 7))))

(defun idea-block-to-fixed-string (block)
  "Convert idea BLOCK to 8 byte string"
  (let ((c1 (idea->> (nth 0 block) 8))
	(c2 (idea-& (nth 0 block) 255))
	(c3 (idea->> (nth 1 block) 8))
	(c4 (idea-& (nth 1 block) 255))
	(c5 (idea->> (nth 2 block) 8))
	(c6 (idea-& (nth 2 block) 255))
	(c7 (idea->> (nth 3 block) 8))
	(c8 (idea-& (nth 3 block) 255)))
    (concat (char-to-string c1) (char-to-string c2) (char-to-string c3)
	    (char-to-string c4) (char-to-string c5) (char-to-string c6)
	    (char-to-string c7) (char-to-string c8))))

(defun idea-add-padding (str rndpad)
  "Add block padding to the STRING."
  (let* ((len (length str))
	 (pad (- 8 (% len 8)))
	 (pad (if (> pad 0) pad 8))
	 (p1 (idea-| (idea-& (if rndpad (idea-random-char) 0) 31) 
		     (idea-<< (- pad 1) 5)))
	 (p2 (if rndpad (idea-random-char) 0))
	 (p3 (if rndpad (idea-random-char) 0))
	 (p4 (if rndpad (idea-random-char) 0))
	 (p5 (if rndpad (idea-random-char) 0))
	 (p6 (if rndpad (idea-random-char) 0))
	 (p7 (if rndpad (idea-random-char) 0))
	 (p8 (if rndpad (idea-random-char) 0))
	 (ps (concat (char-to-string p1) (char-to-string p2)
		     (char-to-string p3) (char-to-string p4)
		     (char-to-string p5) (char-to-string p6)
		     (char-to-string p7) (char-to-string p8)))
	 (ps (substring ps 0 pad)))
    (concat ps str)))

(defun idea-remove-padding (str)
  "Remove block padding from the STRING"
  (let* ((len (length str)))
    (if (< len 8)
	nil
      (substring str (+ 1 (idea->> (elt str 0) 5)) len))))

(defun idea-cleartext-string-to-block-list (str &optional rndpad)
  "Convert cleartext STRING the list of encryption blocks (CLEARTEXT)"
  (let* ((str (idea-add-padding (idea-add-crc str) rndpad))
	 (l (length str))
	 (r (make-vector (/ l 8) 0))
	 (i 0))
    (while (< i l)
      (aset r (/ i 8) (idea-fixed-string-to-block (substring str i (+ i 8))))
      (setq i (+ 8 i)))
    r))

(defun idea-ciphertext-string-to-block-list (str)
  "Convert ciphertext STRING the list of encryption blocks (CIPHERTEXT)"
  (let* ((l (length str))
	 (r (make-vector (/ l 8) 0))
	 (i 0))
    (while (< i l)
      (aset r (/ i 8) (idea-fixed-string-to-block (substring str i (+ i 8))))
      (setq i (+ 8 i)))
    r))

(defun idea-ecb-encrypt-string (str key &optional version)
  "Encrypt STRING with KEY (either key context or string)"
  (let* ((my-key-type (idea-legal-key key))
	 (my-key (cond ((equal 'key-complete-decryption my-key-type)
			(error "KEY is not an encryption key."))
		       ((equal 'key-complete-encryption my-key-type)
			key)
		       ((or (equal 'key-string my-key-type)
			   (equal 'key-intlist my-key-type))
			(idea-build-encryption-key key version))
		       (t (error "Invalid key."))))
	 (data (idea-cleartext-string-to-block-list str '()))
	 (l (length data))
	 (r (make-string (* l 8) 0))
	 (i 0))
    (while (< i l)
      (let ((b (idea-block-to-fixed-string (idea-encrypt-block (elt data i) 
							       my-key)))
	    (so (* i 8)))
	(aset r so (elt b 0))
	(aset r (+ so 1) (elt b 1))
	(aset r (+ so 2) (elt b 2))
	(aset r (+ so 3) (elt b 3))
	(aset r (+ so 4) (elt b 4))
	(aset r (+ so 5) (elt b 5))
	(aset r (+ so 6) (elt b 6))
	(aset r (+ so 7) (elt b 7)))
      (setq i (+ 1 i)))
    (b64-encode-string r)))

(defun idea-ecb-decrypt-string (cipstr key &optional version)
  "Decrypt STRING with KEY (either key context or string)"
  (let ((str (b64-decode-string cipstr)))
    (if (and str (= 0 (% (length str) 8)))
	(let* ((my-key-type (idea-legal-key key))
	       (my-key (cond ((equal 'key-complete-encryption my-key-type)
			      (error "KEY is not an decryption key."))
			     ((equal 'key-complete-decryption my-key-type)
			      key)
			     ((or (equal 'key-string my-key-type)
				  (equal 'key-intlist my-key-type))
			      (idea-build-decryption-key key version))
			     (t (error "Invalid key."))))
	       (data (idea-ciphertext-string-to-block-list str))
	       (l (length data))
	       (r (make-string (* l 8) 0))
	       (i 0))
	  (while (< i l)
	    (let ((b (idea-block-to-fixed-string (idea-encrypt-block 
						  (elt data i) 
						  my-key)))
		  (so (* i 8)))
	      (aset r so (elt b 0))
	      (aset r (+ so 1) (elt b 1))
	      (aset r (+ so 2) (elt b 2))
	      (aset r (+ so 3) (elt b 3))
	      (aset r (+ so 4) (elt b 4))
	      (aset r (+ so 5) (elt b 5))
	      (aset r (+ so 6) (elt b 6))
	      (aset r (+ so 7) (elt b 7)))
	    (setq i (+ 1 i)))
	  (let ((ret (idea-check-crc (idea-remove-padding r))))
	    (if ret
		ret
	      '())))
      '())))

(defun idea-cbc-encrypt-string (str key &optional version)
  "Encrypt STRING with KEY (either key context or string) cbc mode."
  (let* ((my-key-type (idea-legal-key key))
	 (my-key (cond ((equal 'key-complete-decryption my-key-type)
			(error "KEY is not an encryption key."))
		       ((equal 'key-complete-encryption my-key-type)
			key)
		       ((or (equal 'key-string my-key-type)
			   (equal 'key-intlist my-key-type))
			(idea-build-encryption-key key version))
		       (t (error "Invalid key."))))
	 (data (idea-cleartext-string-to-block-list str t))
	 (context '(0 0 0 0))
	 (l (length data))
	 (r (make-string (* l 8) 0))
	 (i 0))
    (while (< i l)
      (let* ((c (idea-encrypt-block (idea-xor-blocks (elt data i) context) 
				    my-key))
	     (b (idea-block-to-fixed-string c))
	     (so (* i 8)))
	(aset r so (elt b 0))
	(aset r (+ so 1) (elt b 1))
	(aset r (+ so 2) (elt b 2))
	(aset r (+ so 3) (elt b 3))
	(aset r (+ so 4) (elt b 4))
	(aset r (+ so 5) (elt b 5))
	(aset r (+ so 6) (elt b 6))
	(aset r (+ so 7) (elt b 7))
	(setq context c))
      (setq i (+ 1 i)))
    (b64-encode-string r)))

(defun idea-cbc-decrypt-string (cipstr key &optional version)
  "Decrypt STRING with KEY (either key context or string) cbc mode."
  (let ((str (b64-decode-string cipstr)))
    (if (and str (= 0 (% (length str) 8)))
	(let* ((my-key-type (idea-legal-key key))
	       (my-key (cond ((equal 'key-complete-encryption my-key-type)
			      (error "KEY is not an decryption key."))
			     ((equal 'key-complete-decryption my-key-type)
			      key)
			     ((or (equal 'key-string my-key-type)
				  (equal 'key-intlist my-key-type))
			      (idea-build-decryption-key key version))
			     (t (error "Invalid key."))))
	       (data (idea-ciphertext-string-to-block-list str))
	       (context '(0 0 0 0))
	       (l (length data))
	       (r (make-string (* l 8) 0))
	       (i 0))
	  (while (< i l)
	    (let* ((c (elt data i))
		   (b (idea-block-to-fixed-string 
		       (idea-xor-blocks (idea-encrypt-block c my-key) 
					context)))
		   (so (* i 8)))
	      (aset r so (elt b 0))
	      (aset r (+ so 1) (elt b 1))
	      (aset r (+ so 2) (elt b 2))
	      (aset r (+ so 3) (elt b 3))
	      (aset r (+ so 4) (elt b 4))
	      (aset r (+ so 5) (elt b 5))
	      (aset r (+ so 6) (elt b 6))
	      (aset r (+ so 7) (elt b 7))
	      (setq context c))
	    (setq i (+ 1 i)))
	  (let ((ret (idea-check-crc (idea-remove-padding r))))
	    (if ret
		ret
	      '())))
      '())))

(defun idea-add-crc (str)
  "Add 32bit crc to STRING"
  (concat (crc32-string str) str))

(defun idea-check-crc (str)
  "Check crc of the STRING.  Return string without crc if ok, otherwise '()"
  (if (< (length str) 8)
      '()
    (let ((realstr (substring str 8 (length str))))
      (if (string= (substring str 0 8) (crc32-string realstr))
	  realstr
	'()))))

(defun idea-hex-char-to-int (x)
  (if (and (>= x ?0) (<= x ?9))
      (- x ?0)
    (if (and (>= x ?a) (<= x ?f))
	(+ (- x ?a) 10)
      (if (and (>= x ?A) (<= x ?F))
	  (+ (- x ?A) 10)
	-1))))

(defun idea-build-key-annotation (key type &optional version)
  "Build annotation table of KEY that is of TYPE \"e\" or \"\d\"."
  (if (null version) (setq version idea-default-key-expand-version))
  (cond ((= version 1) (idea-build-key-annotation-version-1 key type))
	((= version 2) (idea-build-key-annotation-version-2 key type))
	((= version 3) (idea-build-key-annotation-version-3 key type))
	(t (error "Unknown key expansion version"))))

(defun idea-build-key-annotation-version-1 (key type)
  "Build annotation table of KEY that is of TYPE \"e\" or \"\d\"."
  (let ((r (make-string 16 0)))
    (aset r 15 (idea-& (nth 0 key) 255))
    (aset r 14 (idea-& (idea->> (nth 0 key) 8) 255))
    (aset r 13 (idea-& (nth 1 key) 255))
    (aset r 12 (idea-& (idea->> (nth 1 key) 8) 255))
    (aset r 11 (idea-& (nth 2 key) 255))
    (aset r 10 (idea-& (idea->> (nth 2 key) 8) 255))
    (aset r 9  (idea-& (nth 3 key) 255))
    (aset r 8  (idea-& (idea->> (nth 3 key) 8) 255))
    (aset r 7  (idea-& (nth 4 key) 255))
    (aset r 6  (idea-& (idea->> (nth 4 key) 8) 255))
    (aset r 5  (idea-& (nth 5 key) 255))
    (aset r 4  (idea-& (idea->> (nth 5 key) 8) 255))
    (aset r 3  (idea-& (nth 6 key) 255))
    (aset r 2  (idea-& (idea->> (nth 6 key) 8) 255))
    (aset r 1  (idea-& (nth 7 key) 255))
    (aset r 0  (idea-& (idea->> (nth 7 key) 8) 255))
    (concat type ":" (crc32-string r))))

(defun idea-build-key-annotation-version-2 (key type)
  "Build annotation table of KEY that is of TYPE \"e\" or \"\d\"."
  (if (and (= 0 (nth 0 key))
	   (= 0 (nth 1 key))
	   (= 0 (nth 2 key))
	   (= 0 (nth 3 key))
	   (= 0 (nth 4 key))
	   (= 0 (nth 5 key))
	   (= 0 (nth 6 key))
	   (= 0 (nth 7 key)))
      (concat type ":000000000000")
    (let ((r (make-string 9 0))
	  (s (make-string 9 0)))
      (aset r 8 0)
      (aset r 7 (idea-& (nth 0 key) 255))
      (aset r 6 (idea-& (idea->> (nth 0 key) 8) 255))
      (aset r 5 (idea-& (nth 1 key) 255))
      (aset r 4 (idea-& (idea->> (nth 1 key) 8) 255))
      (aset r 3 (idea-& (nth 2 key) 255))
      (aset r 2 (idea-& (idea->> (nth 2 key) 8) 255))
      (aset r 1 (idea-& (nth 3 key) 255))
      (aset r 0 (idea-& (idea->> (nth 3 key) 8) 255))
      (aset s 8 255)
      (aset s 7 (idea-& (nth 4 key) 255))
      (aset s 6 (idea-& (idea->> (nth 4 key) 8) 255))
      (aset s 5 (idea-& (nth 5 key) 255))
      (aset s 4 (idea-& (idea->> (nth 5 key) 8) 255))
      (aset s 3 (idea-& (nth 6 key) 255))
      (aset s 2 (idea-& (idea->> (nth 6 key) 8) 255))
      (aset s 1 (idea-& (nth 7 key) 255))
      (aset s 0 (idea-& (idea->> (nth 7 key) 8) 255))
      (setq s (concat (crc32-string (concat r s) 'raw-string) s))
      (setq r (concat (crc32-string (concat s r) 'raw-string) r))
      (concat type 
	      ":"
	      (substring (crc32-string r) 0 6)
	      (substring (crc32-string s) 0 6)))))

(defun idea-build-key-annotation-version-3 (key type)
  "Build annotation table of KEY that is of TYPE \"e\" or \"\d\"."
  (if (and (= 0 (nth 0 key))
           (= 0 (nth 1 key))
           (= 0 (nth 2 key))
           (= 0 (nth 3 key))
           (= 0 (nth 4 key))
           (= 0 (nth 5 key))
           (= 0 (nth 6 key))
           (= 0 (nth 7 key)))
      (concat type ":0000000000000000")
    (let ((r (make-string 16 0)))
      (aset r 15 (idea-& (nth 0 key) 255))
      (aset r 14 (idea-& (idea->> (nth 0 key) 8) 255))
      (aset r 13 (idea-& (nth 1 key) 255))
      (aset r 12 (idea-& (idea->> (nth 1 key) 8) 255))
      (aset r 11 (idea-& (nth 2 key) 255))
      (aset r 10 (idea-& (idea->> (nth 2 key) 8) 255))
      (aset r 9  (idea-& (nth 3 key) 255))
      (aset r 8  (idea-& (idea->> (nth 3 key) 8) 255))
      (aset r 7  (idea-& (nth 4 key) 255))
      (aset r 6  (idea-& (idea->> (nth 4 key) 8) 255))
      (aset r 5  (idea-& (nth 5 key) 255))
      (aset r 4  (idea-& (idea->> (nth 5 key) 8) 255))
      (aset r 3  (idea-& (nth 6 key) 255))
      (aset r 2  (idea-& (idea->> (nth 6 key) 8) 255))
      (aset r 1  (idea-& (nth 7 key) 255))
      (aset r 0  (idea-& (idea->> (nth 7 key) 8) 255))
      (let ((ra (idea-expand-string-to-key-version-3 r)))
	(concat type ":" (format "%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c%c"
				 (+ ?a (% (nth 0 ra) 26))
				 (+ ?a (% (nth 1 ra) 26))
				 (+ ?a (% (nth 2 ra) 26))
				 (+ ?a (% (nth 3 ra) 26))
				 (+ ?a (% (nth 4 ra) 26))
				 (+ ?a (% (nth 5 ra) 26))
				 (+ ?a (% (nth 6 ra) 26))
				 (+ ?a (% (nth 7 ra) 26))
				 (+ ?a (% (/ (nth 0 ra) 256) 26))
				 (+ ?a (% (/ (nth 1 ra) 256) 26))
				 (+ ?a (% (/ (nth 2 ra) 256) 26))
				 (+ ?a (% (/ (nth 3 ra) 256) 26))
				 (+ ?a (% (/ (nth 4 ra) 256) 26))
				 (+ ?a (% (/ (nth 5 ra) 256) 26))
				 (+ ?a (% (/ (nth 6 ra) 256) 26))
				 (+ ?a (% (/ (nth 7 ra) 256) 26))))))))

(defun idea-legal-subkey-p (subkey)
  "Is SUBKEY a legal subkey structure?"
  (if (and (listp subkey)
	   (= 6 (length subkey))
	   (integerp (nth 0 subkey))
	   (integerp (nth 1 subkey))
	   (integerp (nth 2 subkey))
	   (integerp (nth 3 subkey))
	   (integerp (nth 4 subkey))
	   (integerp (nth 5 subkey))
	   (<= 0 (nth 0 subkey))
	   (>= 65535 (nth 0 subkey))
	   (<= 0 (nth 1 subkey))
	   (>= 65535 (nth 1 subkey))
	   (<= 0 (nth 2 subkey))
	   (>= 65535 (nth 2 subkey))
	   (<= 0 (nth 3 subkey))
	   (>= 65535 (nth 3 subkey))
	   (<= 0 (nth 4 subkey))
	   (>= 65535 (nth 4 subkey))
	   (<= 0 (nth 5 subkey))
	   (>= 65535 (nth 5 subkey)))
      t
    nil))

(defun idea-legal-finalkey-p (finalkey)
  "Is FINALKEY a legal finalkey structure?"
  (if (and (listp finalkey)
	   (= 4 (length finalkey))
	   (integerp (nth 0 finalkey))
	   (integerp (nth 1 finalkey))
	   (integerp (nth 2 finalkey))
	   (integerp (nth 3 finalkey))
	   (<= 0 (nth 0 finalkey))
	   (>= 65535 (nth 0 finalkey))
	   (<= 0 (nth 1 finalkey))
	   (>= 65535 (nth 1 finalkey))
	   (<= 0 (nth 2 finalkey))
	   (>= 65535 (nth 2 finalkey))
	   (<= 0 (nth 3 finalkey))
	   (>= 65535 (nth 3 finalkey)))
      t
    nil))

(defun idea-legal-key (key)
  "Is KEY a idea key or can one be generated from it?"
  (cond ((stringp key)
	 'key-string)
	((and (listp key)
	      (= 8 (length key))
	      (integerp (nth 0 key))
	      (integerp (nth 1 key))
	      (integerp (nth 2 key))
	      (integerp (nth 3 key))
	      (integerp (nth 4 key))
	      (integerp (nth 5 key))
	      (integerp (nth 6 key))
	      (integerp (nth 7 key))
	      (<= 0 (nth 0 key))
	      (>= 65535 (nth 0 key))
	      (<= 0 (nth 1 key))
	      (>= 65535 (nth 1 key))
	      (<= 0 (nth 2 key))
	      (>= 65535 (nth 2 key))
	      (<= 0 (nth 3 key))
	      (>= 65535 (nth 3 key))
	      (<= 0 (nth 4 key))
	      (>= 65535 (nth 4 key))
	      (<= 0 (nth 5 key))
	      (>= 65535 (nth 5 key))
	      (<= 0 (nth 6 key))
	      (>= 65535 (nth 6 key))
	      (<= 0 (nth 7 key))
	      (>= 65535 (nth 7 key)))
	 'key-intlist)
	((and (listp key)
	      (= 10 (length key))
	      (idea-legal-subkey-p (nth 0 key))
	      (idea-legal-subkey-p (nth 1 key))
	      (idea-legal-subkey-p (nth 2 key))
	      (idea-legal-subkey-p (nth 3 key))
	      (idea-legal-subkey-p (nth 4 key))
	      (idea-legal-subkey-p (nth 5 key))
	      (idea-legal-subkey-p (nth 6 key))
	      (idea-legal-subkey-p (nth 7 key))
	      (idea-legal-finalkey-p (nth 8 key)))
	 (if (equal 'encryption (idea-key-type key))
	     'key-complete-encryption
	   'key-complete-decryption))
	(t nil)))

(defun idea-key-version (key)
  (let* ((my-key-type (idea-legal-key key))
	 (r (cond ((or (equal 'key-complete-decryption my-key-type)
		       (equal 'key-complete-encryption my-key-type))
		   (let ((fingerprint (idea-key-fingerprint key)))
		     (cond ((= (length fingerprint) 8) 1)
			   ((= (length fingerprint) 12) 2)
			   ((= (length fingerprint) 16) 3)
			   (t nil))))
		  ((or (equal 'key-string my-key-type)
		       (equal 'key-intlist my-key-type))
		   'any)
		  (t nil))))
    r))

(defun idea-key-fingerprint (key &optional version)
  "Get an IDEA-KEY fingerprint."
  (let* ((my-key-type (idea-legal-key key))
	 (my-key (cond ((equal 'key-complete-decryption my-key-type)
			key)
		       ((equal 'key-complete-encryption my-key-type)
			key)
		       ((or (equal 'key-string my-key-type)
			   (equal 'key-intlist my-key-type))
			(idea-build-encryption-key key version))
		       (t nil))))
    (if my-key
	(let ((annotation (nth 9 my-key)))
	  (if (not (and (stringp annotation)
			(> (length annotation) 9)))
	      nil
	    (let ((ty (elt annotation 0))
		  (de (elt annotation 1)))
	      (if (= de ?:)
		  (if (or (= ty ?e)
			  (= ty ?d))
		      (substring annotation 2 (length annotation))
		    nil)
		nil))))
      nil)))

(defun idea-key-type (key)
  "Is idea KEY encryption or decryption key?"
  (if (listp key)
      (let ((annotation (nth 9 key)))
	(if (not (and (stringp annotation)
		      (> (length annotation) 9)))
	    nil
	  (let ((ty (elt annotation 0))
		(de (elt annotation 1)))
	    (if (= de ?:)
		(if (= ty ?e)
		    'encryption
		  (if (= ty ?d)
		      'decryption
		    nil))
	      nil))))
    nil))

;;; eof (idea.el)
