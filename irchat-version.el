;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-version.el,v 1.10 1997/02/21 18:40:13 too Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;;
;;; Version number constants
;;;

(defconst irchat-client-version-major "2"
  "Major version number. Major functional changes change this number.")

(defconst irchat-client-version-minor "54"
  "Minor version number. Server Protocol changes and bug fixes change this number.")

(defconst irchat-client-version-beta-p nil
  "Is this version a beta version?")

(defconst irchat-client-version-alpha-p nil
  "Is this version an alpha version?")

(defconst irchat-client-version-release "$Date: 1997/02/21 18:40:13 $"
  "version release date")

(defconst irchat-client-name "Irchat"
  "*Name of this program.")

(defconst irchat-version
  (format "%s %s.%s%s %s" 
	  irchat-client-name 
	  irchat-client-version-major
	  irchat-client-version-minor
	  (cond (irchat-client-version-alpha-p "alpha")
		(irchat-client-version-beta-p  "beta")
		(t ""))
	  (if (string-match ".*: \\([^ ]*\\) .*"
			    irchat-client-version-release)
	      (concat "("
		      (substring irchat-client-version-release 
				 (match-beginning 1)
				 (match-end 1))
		      ")")
	    ""))
  "The version of irchat you are using.")

(provide 'irchat-version)
;;;
;;; eof
;;;
