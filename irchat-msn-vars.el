;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-msn-vars.el,v 3.3 2002/06/04 20:21:39 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;;
;;; MSN Messenger Client implementation by tri@iki.fi.
;;;

;;;
;;; Public variables.
;;;
(defvar irchat-msn-server '("64.4.13.58")
  "*Primary MSN Messenger server(s).  List can contain many addresses.")
(defvar irchat-msn-service 1863
  "*MSN Messenger port")
(defvar irchat-msn-uid nil
  "*MSN Messenger user id.  Usually a hotmail address.")
(defvar irchat-msn-uid-password nil
  "*MSN Messenger password of the account.  User will be prompted if nil.")
(defvar irchat-msn-auto-add-reverse-contacts t
  "*Add new reverse contact automatically (if not blocked).")
(defvar irchat-msn-show-lists-in-startup nil
  "*If non-nil, various MSN lists are dumped to screen on startup.")
  
;;;
;;; Format strings
;;;
(defvar irchat-msn-info-prefix "$$$ MSN-Info: "
  "*String to add before any MSN informational message")
(defvar irchat-msn-error-prefix "$$$ MSN-Error: "
  "*String to add before any MSN error message")
(defvar irchat-msn-change-prefix "$$$ Change: "
  "*String to add before any change msg.")
(defvar irchat-msn-format-string-in "<%s>"
  "*Format string for incoming message with one parameter.")
(defvar irchat-msn-format-string-out ">%s<"
  "*Format string for outgoing message with one parameter.")
(defvar irchat-msn-format-string-in2 "<%s:%s>"
  "*Format string for incoming message with two parameters.")
(defvar irchat-msn-format-string-out2 ">%s:%s<"
  "*Format string for outgoing message with two parameters.")

(eval-and-compile (provide 'irchat-msn-vars))

;;; eof (irchat-msn-vars.el)
