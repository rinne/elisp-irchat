;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-misc.el,v 3.32 1998/03/24 09:25:17 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

(eval-when-compile (require 'irchat-inlines))
(eval-and-compile  
  (require 'irchat-filter))

(defun irchat-ignore-this-p (nick uah &optional message)
  (let ((mylist irchat-ignore-nickname)
        (time (current-time)))
    (while mylist
      (let ((expiretime (if (cdr (car mylist))
                            (irchat-time-difference time (cdr (car mylist)))
                          1)))
        (if (< expiretime 0)
            (progn
              (setq irchat-ignore-nickname (remassoc (car (car mylist))
						     irchat-ignore-nickname)
                    irchat-save-vars-is-dirty t)
              (if (= (car (cdr (cdr (cdr (car mylist))))) 0)
                  (irchat-w-insert irchat-D-buffer
                                   (format "%sIgnore timeout for %s expired.\n"
                                           irchat-info-prefix
                                           (car (car mylist)))))))
        (setq mylist (cdr mylist)))))
  (let ((mylist irchat-ignore-message-regexp)
        (time (current-time)))
    (while mylist
      (let ((expiretime (if (cdr (car mylist))
                            (irchat-time-difference time (cdr (car mylist)))
                          1)))
        (if (< expiretime 0)
            (progn
              (setq irchat-ignore-message-regexp (remassoc (car (car mylist))
							   irchat-ignore-message-regexp)
                    irchat-save-vars-is-dirty t)
              (if (= (car (cdr (cdr (cdr (car mylist))))) 0)
                  (irchat-w-insert irchat-D-buffer
                                   (format
		    "%sIgnore timeout for messages matching \"%s\" expired.\n"
				    irchat-info-prefix
				    (car (car mylist)))))))
        (setq mylist (cdr mylist)))))
  (if (or (and (fboundp 'irchat-custom-ignore-this-p)
	       (irchat-custom-ignore-this-p nick uah))
	  (and message
	       (fboundp 'irchat-custom-ignore-this-message-p)
	       (irchat-custom-ignore-this-message-p nick uah message)))
      t
    (let ((killit nil)
          (case-fold-search t))
      (mapcar (function 
               (lambda (kill)
                 (if (or (string-ci-equal (car kill) nick)
			 (and (string-match (upcase (car kill)) (upcase nick))
                              (= (match-beginning 0) 0)
                              (= (match-end 0) (length nick)))
			 (and (string-match "@" (car kill))
			      (or (string-ci-equal (car kill) uah)
				  (and (string-match (upcase (car kill))
						     (upcase uah))
				       (= (match-beginning 0) 0)
				       (= (match-end 0) (length uah))))))
                     (setq killit t))))
              irchat-ignore-nickname)
      (if message
	  (mapcar (function 
		   (lambda (kill)
		     (if (string-match (upcase (car kill)) (upcase message))
			 (setq killit t))))
		  irchat-ignore-message-regexp))
      killit)))


(defun irchat-split-string-with-separator (string separator size)
  "Split STRING to list of SIZE sized pieces with SEPARATORs."
  (if (not (stringp string))
      nil
    (if (or (not (stringp separator))
	    (<= size (* 2 (length separator))))
	(list string)
      (let ((s string)
	    (first 1)
	    (r '()))
	(while s
	  (if (<= (+ (length s)
		     (if first 0 (length separator)))
		  size)
	      (progn
		(setq r (cons (concat (if first "" separator) s)
			      r))
		(setq s nil))
	    (let ((l (- size (+ (if first 0 (length separator))
				(length separator)))))
	      (setq r (cons (concat (if first "" separator) 
				    (substring s 0 l)
				    separator)
			    r))
	      (setq s (substring s l (length s)))))
	  (setq first nil))
	(reverse r)))))


(defun irchat-parse-^G-channel-name (chnl)
  "Parse CHNL to pair e.g. #42 -> (#42 . nil) and #42^Gov -> (#42 . +o +v)"
  (if (string-match "^\\([^ ][^ ]*\\)[\007 \t]\\(.*\\)$" chnl)
      (let* ((body (matching-substring chnl 1))
             (args (matching-substring chnl 2))
             (niceargs "")
             (l (length args))
             (i 0))
        (while (< i l)
          (setq niceargs (concat niceargs
                                 (if (> (length niceargs) 0) " +" "+")
                                 (char-to-string (elt args i))))
          (setq i (+ i 1)))
        (if (> (length niceargs) 0)
	    (cons body niceargs)
          (cons body nil)))
    (cons chnl nil)))


(defun irchat-get-^G-channel-flags (chnl)
  "Convert CHNL e.g. #42 -> nil and #42^Go -> +o"
  (cdr (irchat-parse-^G-channel-name chnl)))


(defun irchat-convert-^G-channel-name (chnl)
  "Convert CHNL e.g. #42 -> #42 and #42^Go -> #42 (+o)"
  (let* ((parse (irchat-parse-^G-channel-name chnl))
	 (body (car parse))
	 (flags (cdr parse)))
    (if flags
	(format "%s (%s)" body flags)
      body)))

(defun irchat-Dialogue-insert-message (buffer 
				       absolute-prefix 
				       format-string
				       sender
				       message 
				       &optional channel
				       force)
  "Insert incoming message into dialog buffer(s)."
  (if (and (> (length message) 0)
	   (string-match (concat "^" 
				 "\\("
				 (if irchat-message-split-^C-compat
				     (concat (regexp-quote
					      irchat-message-split-separator)
					     "\\|"
					     (regexp-quote ""))
				   (regexp-quote
				    irchat-message-split-separator))
				 "\\)"
				 "\\(.*\\)")
			 message)
	   (null force))
      (let* ((n (matching-substring message 2))
	     (m (concat "^"
			absolute-prefix
			(regexp-quote (format format-string
					      sender
					      channel))
			" .*\\("
			(if irchat-message-split-^C-compat
			    (concat (regexp-quote
				     irchat-message-split-separator)
				    "\\|"
				    (regexp-quote ""))
			  (regexp-quote
			   irchat-message-split-separator))
		       "\\)$"))
	     (d (concat absolute-prefix
			(format format-string 
				sender 
				channel)
			" ... "
			n
			"\n"))
	     (o (concat "\\("
			(if irchat-message-split-^C-compat
			    (concat (regexp-quote
				     irchat-message-split-separator)
				    "\\|"
				    (regexp-quote ""))
			  (regexp-quote
			   irchat-message-split-separator))
			"\\)"
			"$")))
	(irchat-w-replace buffer m d o n 24))
    (irchat-w-insert buffer (concat absolute-prefix
				    (format format-string sender channel)
				    " "
				    message
				    "\n"))))


(defun irchat-Dialogue-buffer-p (buffer)
  "Is BUFFER the irchat-Dialogue-buffer or it's name?" 
  (cond ((and (stringp buffer)
	      (stringp irchat-Dialogue-buffer)
	      (string= buffer irchat-Dialogue-buffer))
	 t)
	((and (bufferp buffer)
	      (stringp irchat-Dialogue-buffer))
	 (equal buffer (get-buffer irchat-Dialogue-buffer)))
	((and (stringp buffer)
	      (bufferp irchat-Dialogue-buffer))
	 (equal (get-buffer buffer) irchat-Dialogue-buffer))
	(t nil)))


(defun irchat-own-message (message)
  (irchat-w-insert (irchat-pick-buffer irchat-current-channel)
		   (format "%s\n" message))
  (if (not irchat-ownfreeze)
      (let ((chan-buffer (irchat-pick-buffer irchat-current-channel)))
	(irchat-freeze-toggle (car chan-buffer))
	(irchat-w-insert chan-buffer "")
	(irchat-freeze-toggle (car chan-buffer)))))


(defun irchat-own-private-message (message)
  (irchat-w-insert irchat-P-buffer (format "%s\n" message)))

(defun irchat-send (&rest args)
  (irchat-reset-idle)
  (let ((item (irchat-encode-coding-string (apply 'format args)))
	ditem)
    (let ((conv-list irchat-send-convert-list))
      (while conv-list
        (setq item (irchat-replace-in-string item (car (car conv-list))
                                      (car (cdr (car conv-list)))))
        (setq conv-list (cdr conv-list))))
    (let* ((sndstr (concat item "\r"))
	   (len (length sndstr)))
      (if (> len 512)
	  (progn
	    (message (format "Protocol message too long (%d).  Truncated."
			     (length sndstr)))
	    (if irchat-beep-on-bells
		(beep))))
      (process-send-string irchat-server-process sndstr))
    (setq ditem (downcase item))
    (if (string-match "^list" (downcase ditem))
        (if (string-match "\\(^list\\) \\(.+\\)" ditem)
            (setq irchat-channel-filter (matching-substring ditem 2))
          (setq irchat-channel-filter "")))))


(defun irchat-clean-hostname (hostname)
  "Return the arg HOSTNAME, but if is a dotted-quad, put brackets around it."
  (let ((data (match-data)))
    (unwind-protect
	(if (string-match "[0-9]+\\.[0-9]+\\.[0-9]+\\.[0-9]+" hostname)
	    (concat "[" hostname "]")
	  hostname)
      (store-match-data data))))


(defun irchat-current-nickname ()
  "Our current nickname."
  irchat-real-nickname)


(defun irchat-user-on-my-channels (user)
  "Return the list of channels that user (and I) is on."
  (let ((r '())
	(l irchat-current-channels))
    (while l
      (if (irchat-user-on-this-channel user (car l))
	  (setq r (cons (car l) r)))
      (setq l (cdr l)))
    r))


(defun irchat-replace-in-string (str regexp newtext)
  (if (string-match "XEmacs" emacs-version)
      (replace-in-string str regexp newtext t)
    (save-excursion
      (let ((buf (get-buffer-create "*replace-in-string*")) res)
        (set-buffer buf)
        (erase-buffer)
        (insert str)
        (goto-char (point-min))
        (while (re-search-forward regexp nil t)
          (replace-match newtext t t))
        (setq res (buffer-substring-no-properties (point-min) (point-max)))
        (kill-buffer buf)
        res)
      )))

(defun irchat-scroll-if-visible (window)
  (if window (set-window-point window (point-max))))


(defun irchat-completing-default-read 
  (prompt table predicate require-match initial-input)
  "completing-read w/ default argument like in 'kill-buffer'"
  (let ((default-read
	  (completing-read
	   (if initial-input
	       (format "%s(default %s) "
		       prompt initial-input)
	     (format "%s" prompt))
	   table predicate require-match nil)))
    (if (and (string= default-read "") initial-input)
	initial-input
      default-read)))


(defun irchat-greet-user (user chnl)
  ;; actually user has already been interned, but who cares. 
  (let ((u (intern user irchat-obarray)))
    (if (get u 'irchat-greeting)
	(progn 
	  (beep t)
	  ;; tell me that she has arrived
	  (message "IRCHAT: %s has entered! (%s)"
		   user
		   (if (string= chnl "0") 
		       "on no channel yet"
		     (concat "on channel " chnl)))
	  ;; and send her a greeting
	  (irchat-send "PRIVMSG %s :%s" user (get u 'irchat-greeting))
	  ;; ... done
	  (put u 'irchat-waited-for nil)
	  (put u 'irchat-greeting nil)))))


(defun irchat-greet-author ()
  (setq irchat-greet-author nil)
  (irchat-send "PRIVMSG %s :%s <%s@%s> is using irchat version %s"
	       irchat-author-nickname 
	       (user-full-name) 
	       (user-login-name) 
	       irchat-system-fqdname
	       irchat-version))


(defun irchat-change-nick-of (old new)
  (if (and (stringp old)
	   (stringp new)
	   (not (string-ci-equal old new)))
      (let ((uah-pair (irchat-nick-to-uah-raw old)))
	(if uah-pair
	    (progn
	      (irchat-nick-to-uah-append new 
					 (nth 0 uah-pair)
					 (nth 1 uah-pair))
	      (irchat-nick-to-uah-append old
					 (nth 0 uah-pair)
					 'invalid)))))
  (let ((pair (assoc old irchat-nick-alist)))
    (if new
	(if pair
	    (rplaca pair new)
	  (setq irchat-nick-alist (cons (cons new nil) irchat-nick-alist)))
      (setq irchat-nick-alist (string-list-ci-delete old 
						     irchat-nick-alist)))))


(defun irchat-convert-seconds (time)
  "Convert seconds to printable string."
  (let* ((seconds (string-to-int time))
	 (minutes (/ seconds 60))
	 (seconds (if minutes (% seconds 60) seconds))
	 (hours (/ minutes 60))
	 (minutes (if hours (% minutes 60) minutes))
	 (days (/ hours 24))
	 (hours (if days (% hours 24) hours))
	 (ds (and (/= 0 days)
		  (format "%d day%s, " days
			  (if (> days 1) "s" ""))))
	 (hs (and (/= 0 hours)
		  (format "%d hour%s, " hours
			  (if (> hours 1) "s" ""))))
	 (ms (and (/= 0 minutes)
		  (format "%d minute%s " minutes
			  (if (> minutes 1) "s" ""))))
	 (ss (format "%d seconds" seconds)))
    (concat ds hs ms (if seconds ss ""))))

(defvar irchat-idle-point nil "Timestamp of last idle reset")

(defun irchat-reset-idle ()
  "Reset idle counter and return last idle."
  (let ((r (irchat-idle)))
    (setq irchat-idle-point (current-time))
    r))

(defun irchat-idle ()
  "How long has irchat been idle"
  (if irchat-idle-point
      (irchat-time-difference irchat-idle-point (current-time))
    9999999))

(defun irchat-ping-if-idle (&optional limit)
  (if (null limit)
      (setq limit 120))
  (if (> (irchat-idle) limit)
      (progn
	(irchat-Command-ping)
	t)
    nil))

(defun irchat-msg-from-ignored (prefix rest)
  (save-excursion
    (let ((buf (current-buffer)))
      (irchat-w-insert irchat-I-buffer 
		       (format "%s%s::%s\n" 
			       (if (not irchat-timestamp-irc-I-buffer-p)
				   ""
				 (format "[%s] " 
					 (current-time-string)))
			       prefix 
			       rest))
      t)))

(defun irchat-window-height (win)   ;;; No w-v-h in GNU-Emacs //tri
  "Get the visible window height portably with GNU-Emacs and XEmacs." 
  (if (fboundp 'window-displayed-height)
      (window-displayed-height win)
    (window-height win)))

(defun irchat-time-difference (t0 t1)
  "Difference in seconds of two `three integer lists' returned by current-time function."
  (+ (* (- (car t1) (car t0)) 65536) (- (car (cdr t1)) (car (cdr t0)))))

(defun irchat-time-add (t0 t1)
  "Add t1 seconds to time t0. t0 is in `three integer lists'-format returned by current-time function."
  (list (+ (car t0) (/ (+ (car (cdr t0)) t1) 65536))
	(% (+ (car (cdr t0)) t1) 65536)
	0))

(defun irchat-generate-hex-timestamp (&optional time)
  "Generate timestamp string as hexadecimal"
  (let ((x (if time time (current-time))))
	(format "%04x%04x" (car x) (car (cdr x)))))

(defun irchat-hex-timestamp-valid (timestamp limit)
  "Is TIMESTAMP valid within LIMIT?"
  (if (not (and (stringp timestamp)
		(string-match "^[0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f][0-9a-f]$" timestamp)))
      nil
    (let* ((t1 (irchat-hex-to-int (substring timestamp 0 4)))
	   (t2 (irchat-hex-to-int (substring timestamp 4 8)))
	   (diff (irchat-time-difference (list t1 t2 0)
					 (current-time))))
      (if (> limit 0)
	  (if (and (< diff limit)
		   (> diff (- 0 limit)))
	      t
	    nil)
	t))))

(defun irchat-hex-to-int (x)
  "Convert a HEX-STRING like ffff to the decimal integer"
  (if (string-match "^[0-9a-fA-F][0-9a-fA-F]*$" x)
      (let ((i 0)
	    (l (length x))
	    (r 0))
	(while (< i l)
	  (setq r (+ (* 16 r) (irchat-hex-digit-to-int (elt x i))))
	  (setq i (+ i 1)))
	r)
    -1))

(defun irchat-hex-digit-to-int (x)
  "Convert single HEX-DIGIT (char or string) to integer"
  (cond ((= x ?0) 0)
        ((= x ?1) 1)
        ((= x ?2) 2)
        ((= x ?3) 3)
        ((= x ?4) 4)
        ((= x ?5) 5)
        ((= x ?6) 6)
        ((= x ?7) 7)
        ((= x ?8) 8)
        ((= x ?9) 9)
        ((or (= x ?a) (= x ?A)) 10)
        ((or (= x ?b) (= x ?B)) 11)
        ((or (= x ?c) (= x ?C)) 12)
        ((or (= x ?d) (= x ?D)) 13)
        ((or (= x ?e) (= x ?E)) 14)
        ((or (= x ?f) (= x ?F)) 15)
	((string= x "0") 0)
        ((string= x "1") 1)
        ((string= x "2") 2)
        ((string= x "3") 3)
        ((string= x "4") 4)
        ((string= x "5") 5)
        ((string= x "6") 6)
        ((string= x "7") 7)
        ((string= x "8") 8)
        ((string= x "9") 9)
        ((or (string= x "a") (string= x "A")) 10)
        ((or (string= x "b") (string= x "B")) 11)
        ((or (string= x "c") (string= x "C")) 12)
        ((or (string= x "d") (string= x "D")) 13)
        ((or (string= x "e") (string= x "E")) 14)
        ((or (string= x "f") (string= x "F")) 15)
        (t -1)))

(if (not (fboundp 'remassoc))
    (defun remassoc (key lst)
      (let ((r '())
	    (i (length lst)))
	(while (> i 0)
	  (setq i (- i 1))
	  (let ((current (nth i lst)))
	    (if (not (equal (car current) key))
		(setq r (cons current r)))))
	r)))

(if (not (fboundp 'assoc-if))
    (defun assoc-if (pred lst)
      (let ((r nil))
	(while lst
	  (if (eval (list pred (car (car lst))))
	      (progn
		(setq r (car lst))
		(setq lst '()))
	    (setq lst (cdr lst))))
	r)))

(if (not (fboundp '<<))
    (defun << (x n)
      (while (> n 0)
	(setq x (* x 2))
	(setq n (- n 1)))
      x))

(if (not (fboundp '>>))
    (defun >> (x n)
      (while (> n 0)
	(setq x (/ x 2))
	(setq n (- n 1)))
      x))

(defun irchat-encode-coding-string (string &optional coding)
  "String encoding for MULE systems (emacs & xemacs 20 with mule option)."
  (let ((coding (if coding coding irchat-send-coding-system)))
    (if (and coding
	     (fboundp 'encode-coding-string))
	(encode-coding-string string coding)
      string)))

(defun irchat-decode-coding-string (string &optional coding)
  "String decoding for MULE systems (emacs & xemacs 20 with mule option)."
  (let ((coding (if coding coding irchat-receive-coding-system)))
    (if (and coding
	     (fboundp 'decode-coding-string))
	(decode-coding-string string coding)
      string)))

(eval-and-compile (provide 'irchat-misc))
;;;
;;; eof
;;;
