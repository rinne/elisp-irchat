;;;   -*- lisp -*-
;;; 
;;;  ----------------------------------------------------------------------
;;;  RC4 encryption in elisp.  Cool, ha?
;;;  ----------------------------------------------------------------------
;;;  Created      : Tue Jun 23 15:02:41 1998 tri
;;;  Last modified: Tue Jun 23 17:14:35 1998 tri
;;;  ----------------------------------------------------------------------
;;;  Copyright © 1998
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
;;;  $Id: rc4.el,v 1.1 1998/06/23 14:19:52 tri Exp $
;;;

(eval-and-compile  
  (provide 'rc4))

(defun rc4-elt-swap (vector idx1 idx2)
  "Swap elements in VECTOR indexed IDX1 and IDX2."
  (let ((x (elt vector idx1)))
    (aset vector idx1 (elt vector idx2))
    (aset vector idx2 x)))

(defun rc4-make-state (key)
  "Build rc4 state from string or integer vector KEY."
  (let ((S (make-vector 256 0))
	(K (make-vector 256 0))
	(l (length key))
	(i 0)
	(j 0))
    (if (= l 0)
	(progn
	  (setq l 1)
	  (setq key (make-vector 1 0))))
    (setq i 0)
    (while (< i 256)
      (aset K i (% (elt key (% i l)) 256))
      (aset S i i)
      (setq i (+ i 1)))
    (setq i 0)
    (setq j 0)
    (while (< i 256)
      (setq j (% (+ j (elt S i) (elt K i)) 256))
      (rc4-elt-swap S i j)
      (setq i (+ i 1)))
    (vector 0 0 S)))

(defun rc4-random (state)
  "Generate rc4 stream byte from STATE and update state accordingly."
  (aset state 0 (% (+ (elt state 0) 1) 256))
  (aset state 1 (% (+ (elt state 1) (elt (elt state 2) (elt state 0))) 256))
  (rc4-elt-swap (elt state 2) (elt state 0) (elt state 1))
  (elt (elt state 2)
       (% (+ (elt (elt state 2) (elt state 0))
	     (elt (elt state 2) (elt state 1))) 256)))

(defun rc4-encrypt (str state)
  "Encrypt STRING with rc4 STATE and update state accordingly."
  (let* ((l (length str))
	 (r (if (stringp str) (make-string l 0) (make-vector l 0)))
	 (i 0))
    (while (< i l)
      (aset r i (logxor (elt str i) (rc4-random state)))
      (setq i (+ i 1)))
    r))

(defun rc4-random-vector-complex (str len &optional skip level)
  (if (null skip)
      (setq skip 0))
  (if (null level)
      (setq level 0))
  (let ((r (make-vector len 0))
	(s (rc4-make-state str))
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

(defun rc4-random-vector (str len)
  (rc4-random-vector-complex str len 0 0))

;;; eof (rc4.el)
