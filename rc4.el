;;;   -*- lisp -*-
;;; 
;;;  ----------------------------------------------------------------------
;;;  RC4 encryption in elisp.  Cool, ha?
;;;  ----------------------------------------------------------------------
;;;  Created      : Tue Jun 23 15:02:41 1998 tri
;;;  Last modified: Sat Feb  4 19:57:21 2017 tri
;;;  ----------------------------------------------------------------------
;;;  Copyright © 1998, 2017
;;;  Timo J. Rinne <tri@iki.fi>
;;;  ----------------------------------------------------------------------
;;;  Any express or implied warranties are disclaimed.  In no event
;;;  shall the author be liable for any damages caused (directly or
;;;  otherwise) by the use of this software.
;;;  
;;;  irchat-copyright.el applies only if used with irchat IRC client.
;;;  Contact the author for additional copyright info.
;;;
;;;  $Id: rc4.el,v 1.2 1998/08/10 15:04:17 tri Exp $
;;;

(eval-and-compile  
  (provide 'rc4))

(defun rc4-elt-swap (vector idx1 idx2)
  "Swap elements in VECTOR indexed IDX1 and IDX2."
  (let ((x (elt vector idx1)))
    (aset vector idx1 (elt vector idx2))
    (aset vector idx2 x)))

(defun rc4-make-state (key &optional limit)
  "Build rc4 state from string or integer vector KEY."
  (let* ((limit (if (and (numberp limit) (> limit 0)) limit 256))
	 (S (make-vector limit 0))
	 (K (make-vector limit 0))
	 (l (length key))
	 (i 0)
	 (j 0))
    (if (= l 0)
	(progn
	  (setq l 1)
	  (setq key (make-vector 1 0))))
    (setq i 0)
    (while (< i limit)
      (aset K i (% (elt key (% i l)) limit))
      (aset S i i)
      (setq i (+ i 1)))
    (setq i 0)
    (setq j 0)
    (while (< i limit)
      (setq j (% (+ j (elt S i) (elt K i)) limit))
      (rc4-elt-swap S i j)
      (setq i (+ i 1)))
    (vector 0 0 S)))

(defun rc4-random (state)
  "Generate rc4 stream byte from STATE and update state accordingly."
  (let ((limit (length (elt state 2))))
    (aset state 0 
	  (% (+ (elt state 0) 1) limit))
    (aset state 1 
	  (% (+ (elt state 1) (elt (elt state 2) (elt state 0))) limit))
    (rc4-elt-swap (elt state 2) (elt state 0) (elt state 1))
    (elt (elt state 2)
	 (% (+ (elt (elt state 2) (elt state 0))
	       (elt (elt state 2) (elt state 1))) limit))))

(defun rc4-encrypt (str state)
  "Encrypt STRING with rc4 STATE and update state accordingly."
  (let* ((l (length str))
	 (r (if (stringp str) (make-string l 0) (make-vector l 0)))
	 (i 0))
    (while (< i l)
      (aset r i (logxor (elt str i) (rc4-random state)))
      (setq i (+ i 1)))
    r))

(defun rc4-random-vector-complex (str len &optional skip level limit)
  (if (null skip)
      (setq skip 0))
  (if (null level)
      (setq level 0))
  (let ((r (make-vector len 0))
	(s (rc4-make-state str limit))
	(i 0)
	(j 0))
    (setq i skip)
    (while (> i 0)
      (rc4-random s)
      (setq i (- i 1)))
    (setq j level)
    (while (> j 0)
      (setq i (rc4-random s))
      (while (> i 0)
	(rc4-random s)
	(setq i (- i 1)))
      (setq j (- j 1)))
    (rc4-encrypt r s)))

(defun rc4-random-vector (str len &optional limit)
  (rc4-random-vector-complex str len 0 0 limit))

;;; eof (rc4.el)
