;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-msn-vars.el,v 3.9 2002/11/09 21:09:56 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;;
;;; MSN Messenger Client implementation by tri@iki.fi.
;;;

;;;
;;; Public variables.
;;;
(defvar irchat-msn-server '("messenger.hotmail.com")
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
(defvar irchat-msn-send-typing-notifications nil
  "*If non-nil, send typing notification, while writing a message.")
(defvar irchat-msn-show-typing-notifications nil
  "*If non-nil, show incoming typing notifications in message line.")
(defvar irchat-msn-show-status-changes t
  "*If non-nil, show whenever your contact's status changes.")

;;;
;;; Fake version string to messenger server.  Don't change contents 
;;; of this string.  Anyway, you can set this to nil, in order not to
;;; send fake id to the server.  It seems to work OK without it, but
;;; I don't know what may happen, if you do so.
;;;
(defvar irchat-msn-fake-client-version "0x0409 winnt 5.0 i386 MSMSGS 4.6.0042 MSMSGS"
  "*Send this information as our client version.  Don't change this!")
  
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
;;;
;;; Format strings for encrypted stuff.
;;;
(defvar irchat-msn-format-string-in-e "<<%s>>"
  "*Format string for encrypted incoming message with one parameter.")
(defvar irchat-msn-format-string-out-e ">>%s<<"
  "*Format string for encrypted outgoing message with one parameter.")
(defvar irchat-msn-format-string-in2-e "<<%s:%s>>"
  "*Format string for encrypted incoming message with two parameters.")
(defvar irchat-msn-format-string-out2-e ">>%s:%s<<"
  "*Format string for encrypted outgoing message with two parameters.")

;;;
;;; Variables for Irchat Messenger gateway (kny@iki.fi)
;;;
;;; This extension provides a way to connect to your favourite
;;; irchat session from any MSN Messenger client. You need
;;; two MSN Messenger accounts for this service:
;;; 1. Your primary MSN Messenger account
;;; 2. A private secondary MSN Messenger account for the gateway service
;;;
;;; There are two ways to use this service:
;;; 1. Primary MSN Messenger account used in irchat, secondary account
;;;    used to contact your irchat session from any MSN Messenger client
;;; 2. Secondary MSN Messenger account used in irchat, primary account
;;;    used in any MSN Messenger client
;;;
;;; To initiate a MSN Messenger - Irchat relay you start a discussion
;;; with the MSN Messenger account used within Irchat. All messages
;;; are forwarded both ways. If you prefix your line with a "/" in the
;;; MSN Messenger client, the rest of the line is sent directly to the
;;; irc server. This can be used to send private messages, join and part
;;; channels etc.
;;;

(defvar irchat-msn-gwcontact nil
  "*Contact that is allowed to use the MSN relay (the MSN login name)")

(eval-and-compile (provide 'irchat-msn-vars))

;;; eof (irchat-msn-vars.el)
