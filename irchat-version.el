;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-version.el,v 3.77 1998/05/25 15:02:48 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;;
;;; Version number constants
;;;

(defconst irchat-client-version-major "3"
  "Major version number. Major functional changes change this number.")

(defconst irchat-client-version-minor "04"
  "Minor version number. Server Protocol changes and bug fixes change this number.")

(defconst irchat-client-version-beta-p t
  "Is this version a beta version?")

(defconst irchat-client-version-alpha-p t
  "Is this version an alpha version?")

(defconst irchat-client-version-rcs-snap nil
  "If a `current' release, what's the date?")

(defconst irchat-client-version-release "$Date: 1998/05/25 15:02:48 $"
  "version release date")

(defconst irchat-client-name "Irchat"
  "*Name of this program.")

(defconst irchat-version
  (format "%s %s.%s%s %s" 
	  irchat-client-name 
	  irchat-client-version-major
	  irchat-client-version-minor
	  (cond (irchat-client-version-rcs-snap "current")
		(irchat-client-version-alpha-p  "alpha")
		(irchat-client-version-beta-p   "beta")
		(t ""))
	  (cond ((stringp irchat-client-version-rcs-snap)
		 (concat "(" irchat-client-version-rcs-snap ")"))
		(irchat-client-version-rcs-snap
		 "(unknown date)")
		(t
		 (if (string-match ".*: \\([^ ]*\\) .*"
				   irchat-client-version-release)
		     (concat "("
			     (substring irchat-client-version-release 
					(match-beginning 1)
					(match-end 1))
			     ")")
		   ""))))
  "The version of irchat you are using.")

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
