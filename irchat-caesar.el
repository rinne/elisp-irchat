;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-caesar.el,v 3.3 1998/08/08 14:56:08 tri Exp $
;;;
;;; caesar-region written by phr@prep.ai.mit.edu  Nov 86
;;; Modified by tower@prep Nov 86
;;; Modified by umerin@flab.flab.Fujitsu.JUNET for ROT47.
;;; Modified by tri@iki.fi for irchat (japanese is now optional)

(eval-and-compile (provide 'irchat-caesar))

(defun irchat-caesar-string (str &optional n copy japanese)
  "Caesar rotation of STRING by N, default 13, for decrypting netnews.
ROT47 will be performed for Japanese text in any case."
  (cond ((not (numberp n)) (setq n 13))
	((< n 0) (setq n (- 26 (% (- n) 26))))
	(t (setq n (% n 26))))		;canonicalize N
  (if (not (zerop n))			; no action needed for a rot of 0
      (progn 
	(if (and (not japanese)
		 (or (not (boundp 'caesar-translate-table-1))
		     (/= (aref caesar-translate-table-1 ?a) (+ ?a n))))
	    (let ((i 0) (lower "abcdefghijklmnopqrstuvwxyz") upper)
	      (message "Building caesar-translate-table-1...")
	      (setq caesar-translate-table-1 (make-vector 256 0))
	      (while (< i 256)
		(aset caesar-translate-table-1 i i)
		(setq i (1+ i)))
	      (setq lower (concat lower lower) upper (upcase lower) i 0)
	      (while (< i 26)
		(aset caesar-translate-table-1 (+ ?a i) (aref lower (+ i n)))
		(aset caesar-translate-table-1 (+ ?A i) (aref upper (+ i n)))
		(setq i (1+ i)))
	      (message "Building caesar-translate-table 1... done")))
	(if (and japanese
		 (or (not (boundp 'caesar-translate-table-2))
		     (/= (aref caesar-translate-table-2 ?a) (+ ?a n))))
	    (let ((i 0) (lower "abcdefghijklmnopqrstuvwxyz") upper)
	      (message "Building caesar-translate-table-2...")
	      (setq caesar-translate-table-2 (make-vector 256 0))
	      (while (< i 256)
		(aset caesar-translate-table-2 i i)
		(setq i (1+ i)))
	      (setq lower (concat lower lower) upper (upcase lower) i 0)
	      (while (< i 26)
		(aset caesar-translate-table-2 (+ ?a i) (aref lower (+ i n)))
		(aset caesar-translate-table-2 (+ ?A i) (aref upper (+ i n)))
		(setq i (1+ i)))
	      ;; ROT47 for Japanese text.
	      ;; Thanks to ichikawa@flab.fujitsu.junet.
	      (setq i 161)
	      (let ((t1 (logior ?O 128))
		    (t2 (logior ?! 128))
		    (t3 (logior ?~ 128)))
		(while (< i 256)
		  (aset caesar-translate-table-2 i
			(let ((v (aref caesar-translate-table-2 i)))
			  (if (<= v t1) (if (< v t2) v (+ v 47))
			    (if (<= v t3) (- v 47) v))))
		  (setq i (1+ i))))
	      (message "Building caesar-translate-table 2... done")))
	(let ((caesar-translate-table (if japanese 
					  caesar-translate-table-2
					caesar-translate-table-1))
	      (str (if copy (substring str 0 (length str)) str))
	      (i 0) 
	      len)
	  (setq len (length str))
	  (while (< i len)
	    (aset str i (aref caesar-translate-table (aref str i)))
	    (setq i (1+ i)))
	  str))))


(defun irchat-caesar-region (str &optional n &optional japanese)
  "Caesar rotation of region by N, default 13, for decrypting netnews.
ROT47 will be performed for Japanese text in any case."
  (interactive (if current-prefix-arg	; Was there a prefix arg?
		   (list (prefix-numeric-value current-prefix-arg))
		 (list nil nil)))
  (let* ((from (region-beginning))
	 (to (region-end))
	 (str (buffer-substring from to)))
    (setq str (irchat-caesar-string str n nil japanese))
    (goto-char from)
    (delete-region from to)
    (insert str)))
  
;;;
;;; eof
;;;
