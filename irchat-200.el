;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-200.el,v 1.1 1996/12/19 14:54:46 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

(eval-when-compile
  (require 'irchat-globals)  
  (require 'irchat-vars)
  (require 'irchat-inlines))

;;;
;;;  200 replies
;;;
(defun irchat-handle-200-msgs (number prefix rest)
  (if (string-match "[^ ]* \\([^ :]*\\) *\\([^ :]*\\) *:\\(.*\\)" rest)
      (let ((target1 (matching-substring rest 1))
	    (target2 (matching-substring rest 2))
	    (msg (matching-substring rest 3)))
	(cond ((string-equal target1 "")
	       (irchat-w-insert irchat-200-buffer 
				(format "*** %s\n" msg)))
	      ((string-equal target2 "")
	       (irchat-w-insert irchat-200-buffer 
				(format "*** %s %s\n" target1 msg)))
	      (t
	       (irchat-w-insert irchat-200-buffer 
				(format "*** %s %s (%s)\n" 
					target1 msg target2)))))
    (message "IRCHAT: Strange %s reply" number)))


(defun irchat-handle-200-msg (prefix rest)
  "200 TRACELINK Link <version & debug level> <destination> <next server>"
  (if (string-match "Link \\([^ ]*\\)[ :]*\\([^ ]*\\)[ :]*\\(.*\\)" rest)
      (let ((version (matching-substring rest 1))
	    (dest (matching-substring rest 2))
	    (next (matching-substring rest 3)))
	(irchat-w-insert irchat-200-buffer 
			 (format "*** Link %s (%s) ==> %s (next %s)\n"
				 prefix version dest next)))
    (message "IRCHAT: Strange 200 message")))


(defun irchat-handle-201-msg (prefix rest)
  "201 TRACECONNECTING Try. <class> <server>"
  (if (string-match "[^ ]* [^ ]* \\([0-9]*\\)[ :]*\\(.*\\)" rest)
      (let ((class (matching-substring rest 1))
	    (server (matching-substring rest 2)))
	(irchat-w-insert irchat-200-buffer 
			 (format "*** %s Trying to connect to %s (class %s)\n" 
				 prefix server class)))
    (message "IRCHAT: Strange 201 message")))


(defun irchat-handle-202-msg (prefix rest)
  "202 RPL_TRACEHANDSHAKE H.S. <class> <server>"
  (if (string-match "[^ ]* [^ ]* \\([0-9]*\\)[ :]*\\(.*\\)" rest)
      (let ((class (matching-substring rest 1))
	    (server (matching-substring rest 2)))
	(irchat-w-insert irchat-200-buffer
			 (format "*** %s Handshaking with %s (class: %s)\n" 
				 prefix server class)))
    (message "IRCHAT: Strange 202 message")))


(defun irchat-handle-203-msg (prefix rest)
  "203 RPL_TRACEUNKNOWN ???? <class> [<client IP address in dot form>]"
  (if (string-match "\\([^ ]*\\) \\([^ ]*\\) \\([^ ]*\\)[ :]+\\(.*\\)" rest)
      (let ((status (matching-substring rest 2))
	    (class (matching-substring rest 3))
	    (who (matching-substring rest 4)))
	(irchat-w-insert irchat-200-buffer
			 (format "*** %s Class[%s] ==> %s\n"
				 status class who)))
    (message "IRCHAT: Strange 203 message")))


(defun irchat-handle-204-msg (prefix rest)
  "204 RPL_TRACEOPERATOR Oper <class> <nick>"
  (if (string-match "\\([^ ]*\\) \\([^ ]*\\) \\([^ ]*\\)[ :]+\\(.*\\)" rest)
      (let ((status (matching-substring rest 2))
	    (class (matching-substring rest 3))
	    (who (matching-substring rest 4)))
	(irchat-w-insert irchat-200-buffer
			 (format "*** %s Class[%s] ==> %s\n" 
				 status class who)))
  (message "IRCHAT: Strange 204 message")))


(defun irchat-handle-205-msg (prefix rest)
  "205 RPL_TRACEUSER User %d %s"
  (if (string-match "[^ ]* \\([^ ]*\\) \\([0-9]*\\)[ :]*\\(.*\\)" rest)
      (let ((kind (matching-substring rest 1))
	    (hops (matching-substring rest 2))
	    (where (matching-substring rest 3)))
	(irchat-w-insert irchat-200-buffer
			 (format "*** %s Class[%s] ==> %s\n" kind hops where)))
    (message "IRCHAT: Strange 205 message")))


(defun irchat-handle-206-msg (prefix rest)
  "206 RPL_TRACESERVER Serv %d %dS %dC %s %s!%s@%s"
  (if (string-match "Serv \\([^ ]*\\) \\(.*\\)" rest)
      (let ((class (matching-substring rest 1))
	    (pars (matching-substring rest 2)))
	(if (string-match "^[ :]*\\(.*\\)" pars)
	    (irchat-w-insert irchat-200-buffer
			     (format "*** Serv %s (%s) ==> %s\n" 
				     prefix class (matching-substring pars 1)))
	  (if (string-match 
	       "[ :]*\\([0-9]*\\)*C \\([0-9]*\\)*S[ :]*\\(.*\\)"
	       pars)
	      (irchat-w-insert irchat-200-buffer (format
		       "*** Serv %s (%s) ==> %s (%sC, %sS)\n"
		       prefix class
		       (matching-substring pars 3)
		       (matching-substring pars 1)
		       (matching-substring pars 2)))
	    (irchat-w-insert irchat-200-buffer 
			     (format "*** Serv %s (%s) ==> %s\n" 
				     prefix class pars)))))
    (message "IRCHAT: Strange 206 message")))


(defun irchat-handle-207-msg (prefix rest)
  "207 RPL_TRACESERVICE Service %d %s"
  (if (string-match "[^ ]* Service \\([0-9]*\\) \\(.*\\)" rest)
      (let ((class (matching-substring rest 1))
	    (service (matching-substring rest 2)))
	(irchat-w-insert irchat-200-buffer (format
		 "*** Service %s (class %s)\n" service class)))
    (message "IRCHAT: Strange 207 message")))


(defun irchat-handle-208-msg (prefix rest)
  "208 RPL_TRACENEWTYPE <newtype> 0 %s"
  (irchat-w-insert irchat-200-buffer 
		   (format "*** %s: RPL_TRACENEWTYPE: Why this?\n"
			   prefix))
  nil)


(defun irchat-handle-209-msg (prefix rest)
  "RPL_TRACECLASS Class %d %d"
  (if (string-match "[^ ]* Class \\([0-9]*\\) \\([0-9]*\\)" rest)
      (let ((class (matching-substring rest 1))
	    (entries (matching-substring rest 2)))
	(irchat-w-insert irchat-200-buffer 
			 (format "*** Class %s Entries linked: %s\n" 
				 class entries)))
    (message "IRCHAT: Strange 209 message")))


(defun irchat-handle-211-msg (prefix rest)
  "NOTICE %s :%-15.15s%5u%7u%10u%7u%10u %s"
  (if (string-match "\\([^ ]*\\) \\([^ ]*\\) \\([^ ]*\\) \\([^ ]*\\) \\([^ ]*\\) \\([^ ]*\\) \\([^ ]*\\)[ :]+\\(.*\\)" rest)
      (let ((link (matching-substring rest 2))
	    (sendq (matching-substring rest 3))
	    (sendm (matching-substring rest 4))
	    (sendb (matching-substring rest 5))
	    (rcvem (matching-substring rest 6))
	    (rcveb (matching-substring rest 7))
	    (open (matching-substring rest 8)))
	(irchat-w-insert irchat-200-buffer 
	 (format "%-35s*** %s: %5s%7s%10s%7s%10s %s\n" link
		 prefix sendq sendm sendb rcvem rcveb open)))
    (message "IRCHAT: Strance 211 message")))


(defun irchat-handle-212-msg (prefix rest)
  "212 RPL_STATSCOMMANDS %s %u %u"
  (if (string-match "[^ ]* \\([^ ]*\\) \\([0-9]*\\)" rest)
      (let ((cmd (matching-substring rest 1))
	    (times (matching-substring rest 2)))
	(irchat-w-insert irchat-200-buffer 
			 (format "%s has been used %s times after startup\n"
				 cmd times)))
    (message "IRCHAT: Strange 212 message")))


(defun irchat-handle-213-msg (prefix rest)
  "213 RPL_STATSCLINE %c %s * %s %d %d"
  (if (string-match "[^ ]* \\(.\\) \\([^ ]*\\) \\(.\\) \\([^ ]*\\) \\([0-9]*\\) \\([0-9]*\\)" rest)
      (let ((cn (matching-substring rest 1))
	    (canon (matching-substring rest 2))
	    (pass (matching-substring rest 3))
	    (name (matching-substring rest 4))
	    (port (matching-substring rest 5))
	    (hmmm (matching-substring rest 6)))
	(irchat-w-insert irchat-200-buffer 
			 (format "%s:%s:%s:%s:%s:%s\n" 
				 cn canon pass name port hmmm)))
    (message "IRCHAT: Strange 213 message")))


(defun irchat-handle-214-msg (prefix rest)
  "214 RPL_STATSNLINE %c %s * %s %d %d"
  (if (string-match "[^ ]* \\(.\\) \\([^ ]*\\) \\(.\\) \\([^ ]*\\) \\([0-9]*\\) \\([0-9]*\\)" rest)
      (let ((cn (matching-substring rest 1))
	    (canon (matching-substring rest 2))
	    (pass (matching-substring rest 3))
	    (name (matching-substring rest 4))
	    (port (matching-substring rest 5))
	    (hmmm (matching-substring rest 6)))
	(irchat-w-insert irchat-200-buffer 
			 (format "%s:%s:%s:%s:%s:%s\n" 
				 cn canon pass name port hmmm)))
    (message "IRCHAT: Strange 214 message")))


(defun irchat-handle-215-msg (prefix rest)
  "215 RPL_STATSILINE k2 I * * * 0 0"
  (if (string-match "[^ ]* I \\([^ ]*\\) \\(.\\) \\([^ ]*\\)" rest)
      (let ((domain (matching-substring rest 1))
            (passwd (matching-substring rest 2))
            (redomain (matching-substring rest 3)))
        (irchat-w-insert irchat-200-buffer 
			 (format "*** I:%s:%s:%s\n" 
				 domain passwd redomain)))
    (message "IRCHAT: Strange 215 message")))
	  

(defun irchat-handle-216-msg (prefix rest)
  "216 RPL_STATSKLINE k2 K *.hut.fi * tsh 0 -1"
  (if (or
       (string-match "[^ ]* K \\([^ ]*\\) \\(.\\) \\([^ ]*\\) 0 -1" rest)
       (string-match "[^ ]* K \\([^ ]*\\) \\([^ ]*\\) \\([^ ]*\\) 0 [:]*-1" rest))
      (let ((host (matching-substring rest 1))
	    (pass (matching-substring rest 2))
	    (user (matching-substring rest 3)))
	(irchat-w-insert irchat-200-buffer 
			 (format "*** K:%s:%s:%s\n" host pass user)))
    (message "IRCHAT: Strange 216 message")))


(defun irchat-handle-217-msg (prefix rest)
  "217 RPL_STATSQLINE k2 Q <NULL> * eris.berkeley.edu 0 -1"
  (if (string-match "[^ ]* Q \\([^ ]*\\) \\(.\\) \\([^ ]*\\) \\(.*\\)" rest)
      (let ((reason (matching-substring rest 1))
	    (star (matching-substring rest 2))
	    (host (matching-substring rest 3))
	    (stuff (matching-substring rest 4)))
	(irchat-w-insert irchat-200-buffer 
			 (format "*** Q:%s:%s:%s:%s\n"
				 reason star host stuff)))
    (message "IRCHAT: Strange 217 message")))


(defun irchat-handle-218-msg (prefix rest)
  "218 RPL_STATSYLINE k2 Y 1 90 300 10"
  (if (string-match "[^ ]* Y \\([0-9]*\\) \\([0-9]*\\) \\([0-9]*\\) \\([0-9]*\\)" 
		    rest)
      (let ((class (matching-substring rest 1))
	    (pingfreq (matching-substring rest 2))
	    (confreq (matching-substring rest 3))
	    (maxlinks (matching-substring rest 4)))
	(irchat-w-insert irchat-200-buffer
			 (format "*** Class %s: PingFreq %s, ConFreq %s, MaxLinks %s\n"
				 class pingfreq confreq maxlinks)))
    (message "IRCHAT: Strange 218 message")))

    
(defun irchat-handle-219-msg (prefix rest)
  "219 RPL_ENDOFSTATS %c :End of /STATS report" 
  nil)


(defun irchat-handle-221-msg (prefix rest) 
  "221 RPL_UMODEIS %s"
  (if (string-match "[^ ]* \\(.*\\)" rest)
      (let ((str (matching-substring rest 1)))
	(irchat-w-insert irchat-200-buffer
			 (format "*** Mode for you is %s\n" str)))
    (message (format "IRCHAT: Strange 324 reply '%s'" rest))))


;;;
;;; 230 series not implemented as 7/94
;;;
(defun irchat-handle-231-msg (prefix rest)
  "231 RPL_SERVICEINFO"
  nil)


(defun irchat-handle-232-msg (prefix rest)
  "232 RPL_ENDOFSERVICES"
  nil)


(defun irchat-handle-233-msg (prefix rest)
  "233 RPL_SERVICE"
  nil)


(defun irchat-handle-234-msg (prefix rest)
  "234 RPL_SERVLIST"
  nil)


(defun irchat-handle-235-msg (prefix rest)
  "235 RPL_SERVLISTEND"
  nil)


(defun irchat-handle-241-msg (prefix rest)
  "241 RPL_STATSLLINE %c %s * %s %d %d"
  (if (string-match "[^ ]* \\(.*\\)" rest)
      (let ((msg (matching-substring rest 1)))
	(irchat-w-insert irchat-200-buffer (format "*** %s\n" msg)))
    (message "IRCHAT: Strange 241 reply")))


(defun irchat-handle-242-msg (prefix rest)
  "242 RPL_STATSUPTIME :Server Up %d days, %d:%02d:%02d"
  (if (string-match "[^ ]* \\(.*\\)" rest)
      (let ((msg (matching-substring rest 1)))
	(irchat-w-insert irchat-200-buffer (format "*** %s\n" msg)))
    (message "IRCHAT: Strange 242 reply")))


(defun irchat-handle-243-msg (prefix rest)
  "243 RPL_STATSOLINE %c %s * %s %d %d"
  (if (string-match "[^ ]* \\(.*\\)" rest)
      (let ((msg (matching-substring rest 1)))
	(irchat-w-insert irchat-200-buffer (format "*** %s\n" msg)))
    (message "IRCHAT: Strange 243 reply")))


(defun irchat-handle-244-msg (prefix rest)
  "244 RPL_STATSHLINE %c %s * %s %d %d"
  (if (string-match "[^ ]* \\(.*\\)" rest)
      (let ((msg (matching-substring rest 1)))
	(irchat-w-insert irchat-200-buffer (format "*** %s\n" msg)))
    (message "IRCHAT: Strange 244 reply")))


(defun irchat-handle-245-msg (prefix rest)
  "245 RPL_STATSSLINE %c %s * %s %d %d"
  (if (string-match "[^ ]* \\(.*\\)" rest)
      (let ((msg (matching-substring rest 1)))
	(irchat-w-insert irchat-200-buffer (format "*** %s\n" msg)))
    (message "IRCHAT: Strange 245 reply")))

;;;
;;; eof
;;;
