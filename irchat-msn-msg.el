;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-msn-msg.el,v 3.3 2002/06/04 23:20:44 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;;
;;; MSN Messenger Client implementation by tri@iki.fi.
;;;

;;; This is far from being complete.  But it'll do.
;;; Full table would me annoyingly slow.
(defconst irchat-msn-iso8859-1-to-utf8-table '(("\344" . "\303\244")
					       ("\345" . "\303\245")
					       ("\366" . "\303\266")
					       ("\304" . "\303\204")
					       ("\305" . "\303\205")
					       ("\326" . "\303\226")
					       ("\321" . "\303\221")
					       ("\361" . "\303\261")
					       ("\350" . "\303\250")
					       ("\351" . "\303\251")
					       ("\374" . "\303\234")
					       ("\334" . "\303\234")
					       ("\367" . "\303\267")
					       ("\244" . "\302\244")
					       ("\247" . "\302\247")
					       ("\275" . "\302\275")
					       ("\243" . "\302\243")
					       ("\241" . "\302\241") 
					       ("\242" . "\302\242") 
					       ("\245" . "\302\245") 
					       ("\251" . "\302\251") 
					       ("\253" . "\302\253") 
					       ("\256" . "\302\256") 
					       ("\261" . "\302\261") 
					       ("\262" . "\302\262") 
					       ("\263" . "\302\263") 
					       ("\265" . "\302\265") 
					       ("\271" . "\302\271") 
					       ("\273" . "\302\273") 
					       ("\277" . "\302\277") 
					       ("\260" . "\302\260") 
					       ("EURO" . "\342\202\254") ;;; Kludge alert, there is no euro sign in latin1
					      ))

(defun irchat-msn-iso8859-1-to-utf8 (msg &optional reverse)
  (let ((m msg)
	(r ""))
    (while (> (length m) 0)
      (let ((l irchat-msn-iso8859-1-to-utf8-table)
	    (d nil))
	(if (< (char-to-int (elt m 0 )) 128)
	    (setq l '()))
	(while l
	  (if (and (not (< (length m) 
			   (length (if reverse (cdr (car l)) (car (car l))))))
		   (string-equal (if reverse (cdr (car l)) (car (car l)))
				 (substring m 0 (length (if reverse (cdr (car l)) (car (car l)))))))
	      (setq r (concat r (if reverse (car (car l)) (cdr (car l))))
		    m (substring m (length (if reverse (cdr (car l)) (car (car l)))) (length m))
		    l '()
		    d t)
	    (setq l (cdr l))))
	(if (null d)
	    (setq r (concat r (substring m 0 1))
		  m (substring m 1 (length m))))))
    r))

(defun irchat-msn-make-typing-notification ()
  (let ((msg (concat "MIME-Version: 1.0\n"
		     "Content-Type: text/x-msmsgscontrol\n"
		     "TypingUser: " irchat-msn-uid "\n"
		     "\n"
		     "\n")))
    (setq msg (replace-in-string msg "\n" "\r\n"))
    msg))

(defun irchat-msn-make-message (msg &optional headers)
  (setq msg (replace-in-string msg "\r\n" "\n"))
  (setq msg (irchat-msn-iso8859-1-to-utf8 msg))
  (let ((len (length msg)))
    (if (string-equal "\n" (substring msg (- len 1) len))
	(setq msg (substring msg 0 (- len 1)))))
  (setq msg (concat "MIME-Version: 1.0\n"
		    "Content-Type: text/plain; charset=UTF-8\n"
		    "X-MMS-IM-Format: FN=Microsoft%20Sans%20Serif; EF=; CO=000000; CS=0; PF=22\n"
		    "\n"
		    msg))
  (setq msg (replace-in-string msg "\n" "\r\n"))
  msg)

(defun irchat-msn-parse-message (msg)
  (let ((m (replace-in-string msg "\r" "")))
    (let ((z (string-match "\n\n" m)))
      (if z
	  (let ((head (substring m 0 z))
		(body (substring m (+ z 2) (length m)))
		(headers '()))
	    (if (string-equal "\n" body)
		(setq body ""))
	    (while (or (string-match "^\\(\\([^ \t]*\\):[ \t]*\\([^\n]*\\)\n\\)" head)
		       (string-match "^\\(\\([^ \t]*\\):[ \t]*\\([^\n]*\\)\\)" head))
	      (let ((n (matching-substring head 2))
		    (v (matching-substring head 3))
		    (l (length (matching-substring head 1))))
		(setq head (substring head l (length head))
		      headers (cons (cons n v) headers))))
	    (cons headers body))))))

(defun irchat-msn-message-header-val (header-name parsed-message)
  (let ((l (car parsed-message))
	(h (upcase header-name))
	(r nil))
    (while l
      (if (string-equal h (upcase (car (car l))))
	  (setq r (cdr (car l))
		l '())
	(setq l (cdr l))))
    r))

(eval-and-compile (provide 'irchat-msn-msg))

;;; eof (irchat-msn-meg.el)
