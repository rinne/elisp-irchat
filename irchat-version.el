;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-version.el,v 3.163 2002/09/02 20:28:33 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;; DO NOT TOUCH THIS LINE: ZZZXXX>>>1030998503<<<XXXZZZ

;;;
;;; Version number constants
;;;

(defconst irchat-client-version-major "4"
  "*Major version number. Major functional changes change this number.")

(defconst irchat-client-version-minor "00"
  "*Minor version number. Server Protocol changes and bug fixes change this number.")

(defconst irchat-client-version-beta-p t
  "*Is this version a beta version?")

(defconst irchat-client-version-alpha-p t
  "*Is this version an alpha version?")

(defconst irchat-client-version-rcs-snap t
  "*Is this just a snapshot from rcs?")

(defconst irchat-client-version-release "$Date: 2002/09/02 20:28:33 $"
  "*Version release date")

(defconst irchat-client-name "Irchat"
  "*Name of this program.")

(defconst irchat-client-distribution "none"
  "*What is the buld name of this distribution?")

(defconst irchat-emacs-version (let ((e-v-s (if (boundp 'emacs-version) 
						emacs-version
					      nil))
				     (e-v-l (if (fboundp 'emacs-version) 
						(emacs-version)
					      nil)))
				 (cond ((and (boundp 'emacs-major-version)
					     (integerp emacs-major-version))
					emacs-major-version)
				       ((and (stringp e-v-l)
					     (string-match 
					      "\\([1-9][0-9]*\\)\.[0-9][0-9]*"
					      e-v-l))
					(string-to-int
					 (substring e-v-l
						    (match-beginning 1)
						    (match-end 1))))
				       ((and (stringp e-v-s)
					     (string-match 
					      "\\([1-9][0-9]*\\)\.[0-9][0-9]*"
					      e-v-s))
					(string-to-int
					 (substring e-v-s
						    (match-beginning 1)
						    (match-end 1))))
				       (t 0)))
  "*Major number of emacs running this irchat.")


(defconst irchat-emacs-version-name
  (let ((e-v (emacs-version))
	(case-fold-search t)
	irchat-emacs-subtype
	irchat-emacs-version-number)
    (setq irchat-emacs-subtype
	(cond ((string-match "xemacs" e-v) "XEmacs")
	      ((string-match "lucid" e-v) "Lucid Emacs")
	      ((string-match "epoch" e-v) "Epoch")
	      ((or (string-match "gnu.emacs" e-v)
		   (string-match "gnuemacs" e-v)) "GNU Emacs")
	      ((string-match "mule" e-v) "Mule")
	      ((string-match "emacs" e-v) "Emacs")
	      (t "?????")))
    (setq irchat-emacs-version-number
	  (if (and (boundp 'emacs-major-version)
		   (boundp 'emacs-minor-version))
	      (format "%d.%d" emacs-major-version emacs-minor-version)
	    (if (string-match "[1-9][0-9]*\.[0-9][0-9]*" e-v)
		(substring e-v (match-beginning 0) (match-end 0))
	      "##.##")))
    (concat irchat-emacs-subtype " " irchat-emacs-version-number))
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

(provide 'irchat-version)

;;;
;;; eof
;;;
