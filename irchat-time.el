;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-time.el,v 1.1.2.1 2002/04/25 17:26:22 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

(defun irchat-string-int-div-2 (n)
  "Divide integer represented by STRING by two and return pair of result string and boolean marking nonzero remainder"
  (if (string-match "^[0-9][0-9]*$" n)
      (let ((x '())
	    (y '())
	    (r "")
	    (odd 0))
	(while (string-match "^\\([0-9]*\\)\\([0-9]\\)$" n)
	  (setq x (cons (string-to-int (matching-substring n 2)) x))
	  (setq n (matching-substring n 1)))
	(while x
	  (let ((z (irchat-string-int-div-2-small (car x))))
	    (setq y (cons (car z) y))
	    (if (= 0 (cdr z))
		(setq x (cdr x))
	      (if (null (cdr x))
		  (setq x '()
			odd 1)
		(setq x (cons (+ 10 (car (cdr x))) (cdr (cdr x))))))))
	(while y
	  (setq r (format "%d%s" (car y) r))
	  (setq y (cdr y)))
	(if (string-match "^0*\\([0-9][0-9]*\\)$" r)
	    (setq r (matching-substring r 1)))
	(cons r odd))
    nil))

(defun irchat-string-int-div-2-small (x)
  "Divide INTEGER <20 by two and return pair of result and remainder"
  (nth x (list (cons 0 0) (cons 0 1) (cons 1 0)
	       (cons 1 1) (cons 2 0) (cons 2 1)
	       (cons 3 0) (cons 3 1) (cons 4 0)
	       (cons 4 1) (cons 5 0) (cons 5 1)
	       (cons 6 0) (cons 6 1) (cons 7 0)
	       (cons 7 1) (cons 8 0) (cons 8 1)
	       (cons 9 0) (cons 9 1) (cons 10 0))))

(defun irchat-string-to-16bit-int-list (n)
  "Convert STRING integer to list of 16 bit integers"
  (if (string-match "^[0-9][0-9]*$" n)  
      (let ((r '()))
	(while (not (string= n "0"))
	  (let* ((p n)
		 (m (irchat-string-int-div-2 p))
		 (b0 (if (= 0 (cdr m)) 0 1)) (p (car m))
		 (m (irchat-string-int-div-2 p))
		 (b1 (if (= 0 (cdr m)) 0 2)) (p (car m))
		 (m (irchat-string-int-div-2 p))
		 (b2 (if (= 0 (cdr m)) 0 4)) (p (car m))
		 (m (irchat-string-int-div-2 p))
		 (b3 (if (= 0 (cdr m)) 0 8)) (p (car m))
		 (m (irchat-string-int-div-2 p))
		 (b4 (if (= 0 (cdr m)) 0 16)) (p (car m))
		 (m (irchat-string-int-div-2 p))
		 (b5 (if (= 0 (cdr m)) 0 32)) (p (car m))
		 (m (irchat-string-int-div-2 p))
		 (b6 (if (= 0 (cdr m)) 0 64)) (p (car m))
		 (m (irchat-string-int-div-2 p))
		 (b7 (if (= 0 (cdr m)) 0 128)) (p (car m))
		 (m (irchat-string-int-div-2 p))
		 (b8 (if (= 0 (cdr m)) 0 256)) (p (car m))
		 (m (irchat-string-int-div-2 p))
		 (b9 (if (= 0 (cdr m)) 0 512)) (p (car m))
		 (m (irchat-string-int-div-2 p))
		 (ba (if (= 0 (cdr m)) 0 1024)) (p (car m))
		 (m (irchat-string-int-div-2 p))
		 (bb (if (= 0 (cdr m)) 0 2048)) (p (car m))
		 (m (irchat-string-int-div-2 p))
		 (bc (if (= 0 (cdr m)) 0 4096)) (p (car m))
		 (m (irchat-string-int-div-2 p))
		 (bd (if (= 0 (cdr m)) 0 8192)) (p (car m))
		 (m (irchat-string-int-div-2 p))
		 (be (if (= 0 (cdr m)) 0 16384)) (p (car m))
		 (m (irchat-string-int-div-2 p))
		 (bf (if (= 0 (cdr m)) 0 32768)) (p (car m)))
	    (setq n p)
	    (setq r (cons (+ b0 b1 b2 b3 b4 b5 b6 b7 b8 b9 ba bb bc bd be bf)
			  r))))
	r)
    nil))

(defun irchat-time-val-string-to-current-time-format (x)
  "Convert STRING integer time value to current-time format"
  (let ((n (irchat-string-to-16bit-int-list x)))
    (if (null n)
	(list 0 0 0)
      (append n '(0)))))

;;;
;;; eof
;;;
