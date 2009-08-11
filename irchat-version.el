;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-version.el,v 3.258 2009/08/11 20:32:56 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;; DO NOT TOUCH THIS LINE: ZZZXXX>>>1250022776<<<XXXZZZ

;;;
;;; Version number constants
;;;

(defconst irchat-client-version-major "5"
  "*Major version number. Major functional changes change this number.")

(defconst irchat-client-version-minor "02"
  "*Minor version number. Server Protocol changes and bug fixes change this number.")

(defconst irchat-client-version-beta-p t
  "*Is this version a beta version?")

(defconst irchat-client-version-alpha-p t
  "*Is this version an alpha version?")

(defconst irchat-client-version-rcs-snap t
  "*Is this just a snapshot from rcs?")

(defconst irchat-client-version-release "$Date: 2009/08/11 20:32:56 $"
  "*Version release date")

(defconst irchat-client-name "Irchat"
  "*Name of this program.")

(defconst irchat-client-distribution "none"
  "*What is the buld name of this distribution?")

(defconst irchat-emacs-version-major
  (cond ((and (boundp 'emacs-major-version)
	      (integerp emacs-major-version))
	 emacs-major-version)
	(t (let ((e-v (cond ((fboundp 'emacs-version) (emacs-version))
			    ((boundp 'emacs-version) emacs-version)
			    (t "")))
		 (case-fold-search t))
	     (if (string-match 
		  "\\([1-9][0-9]*\\)\.[0-9][0-9]*"
		  e-v)
		 (string-to-int (substring e-v
					   (match-beginning 1)
					   (match-end 1)))
	       0))))
  "*Major version number of emacs running this irchat.")

(defconst irchat-emacs-version-minor
  (cond ((and (boundp 'emacs-minor-version)
	      (integerp emacs-minor-version))
	 emacs-minor-version)
	(t (let ((e-v (cond ((fboundp 'emacs-version) (emacs-version))
			    ((boundp 'emacs-version) emacs-version)
			    (t "")))
		 (case-fold-search t))
	     (if (string-match 
		  "[1-9][0-9]*\.\\([0-9][0-9]*\\)"
		  e-v)
		 (string-to-int (substring e-v
					   (match-beginning 1)
					   (match-end 1)))
	       0))))
  "*Minor version number of emacs running this irchat.")

(defconst irchat-emacs-version-number
  (format "%d.%d"
	  irchat-emacs-version-major
	  irchat-emacs-version-minor)
  "*Version number of Emacs running this Irchat")

(defconst irchat-emacs-subtype
  (let ((e-v (cond ((fboundp 'emacs-version) (emacs-version))
		   ((boundp 'emacs-version) emacs-version)
		   (t "")))
	(case-fold-search t))
    (cond ((string-match "xemacs" e-v) "XEmacs")
	  ((string-match "lucid" e-v) "Lucid Emacs")
	  ((string-match "epoch" e-v) "Epoch")
	  ((or (string-match "gnu.emacs" e-v)
	       (string-match "gnuemacs" e-v)) "GNU Emacs")
	  ((string-match "mule" e-v) "Mule")
	  ((string-match "emacs" e-v) "Emacs")
	  (t "?????")))
  "*Type of Emacs running this Irchat")

(defconst irchat-emacs-version-name
  (format "%s %s"
	  irchat-emacs-subtype
	  irchat-emacs-version-number)
  "*Version of Emacs running this Irchat")

(defconst irchat-version
  (format "%s %s.%s%s %s" 
	  irchat-client-name 
	  irchat-client-version-major
	  irchat-client-version-minor
	  (cond (irchat-client-version-rcs-snap "current")
		(irchat-client-version-alpha-p  "alpha")
		(irchat-client-version-beta-p   "beta")
		(t ""))
	  (if (string-match ".*: \\([^ ]*\\) .*"
			    irchat-client-version-release)
	      (concat "("
		      (substring irchat-client-version-release 
				 (match-beginning 1)
				 (match-end 1))
		      ")")
	    ""))
  "*The version of irchat you are using.")

(defun irchat-version (&optional int)
  "Irchat version identifier string."
  (interactive '(t))
  (if int
      (message irchat-version))
  irchat-version)

(eval-and-compile
  (provide 'irchat-version))

;;;
;;; eof
;;;
