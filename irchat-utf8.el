;;;  -*- emacs-lisp -*-
;;;
;;;  $Id: irchat-utf8.el,v 3.12 2009/08/01 00:42:18 tri Exp $
;;;
;;; see file irchat-copyright.el for change log and copyright info

;;;
;;; UTF-8 kludge implementation by tri@iki.fi.
;;;

(eval-and-compile
  (require 'irchat-utf8-table))

(defconst irchat-utf8-kludge-max-unicode-val 1114111)

(defun irchat-utf8-kludge-get-char-with-symbolic-name (str)
  (let ((lst irchat-utf8-kludge-table)
	(r nil))
    (while lst
      (if (string= str (car (cdr (car lst))))
	  (setq r (car (car lst))
		lst nil)
	(setq lst (cdr lst))))
    r))

(defun irchat-utf8-kludge-nth-char (str n)
  "Get Nth character from STR as integer or nil if not in range."
  (if (and (or (stringp str)
	       (vectorp str))
	   (>= n 0)
	   (> (length str) n))
      ; For reason that escapes from me, gnu emacs 22 seems to do
      ; weird things for latin-1 characters in strings.  Function
      ; elt returns those codes 128-255 to be in range 2176-2303
      ; (i.e. 0x8## instead of 0x##).  This page in UTF-8 is unused
      ; but reserved, so let's just handle those specially.  This
      ; will certainly break big time if page 8 is sometime used.
      (let ((c (elt str n)))
	(if (or (numberp c)
		(and (fboundp 'characterp) (characterp c)))
	    (let ((c (+ 0 c)))
	      (if (and (>= c 2176) (<= c 2303))
		  (logand c 255)
		c))
	  nil))
    nil))

(defun irchat-utf8-kludge-decode-first (str)
  "Decode the first character from STR as utf-8 if possible and return
as (code . rest) pair containing the character code of the first
character and the rest of the string"
  (let ((b1 (irchat-utf8-kludge-nth-char str 0))
	(b2 (irchat-utf8-kludge-nth-char str 1))
	(b3 (irchat-utf8-kludge-nth-char str 2))
	(b4 (irchat-utf8-kludge-nth-char str 3)))
    (cond ((or (null b1)
	       (= b1 0))
	   ; This is empty string or string that contains NIL characters
	   ; and it simply means that caller is out of luck
	   nil)
	  ((<= b1 127)
	   ; This is ASCII
	   (cons b1 (substring str 1)))
	  ((and (not (null b1)) (>= b1 192) (<= b1 223)
		(not (null b2)) (>= b2 128) (<= b2 191))
	   ; This is 2 byte UTF-8 character if in range 0x80-0x7ff
	   (let ((v (logior (lsh (logand b1 (lognot 192)) 6)
			    (logand b2 (lognot 128)))))
	     (if (and (>= v 128) (<= v 2047))
		 (cons v (substring str 2))
	       (cons b1 (substring str 1)))))
	  ((and (not (null b1)) (>= b1 224) (<= b1 239)
		(not (null b2)) (>= b2 128) (<= b2 191)
		(not (null b3)) (>= b3 128) (<= b3 191))
	   ; This is 3 byte UTF-8 character if in range 0x800-0xffff
	   (let ((v (logior (lsh (logand b1 (lognot 224)) 12)
			    (lsh (logand b2 (lognot 128)) 6)
			    (logand b3 (lognot 128)))))
	     (if (and (>= v 2048) (<= v 65565))
		 (cons v (substring str 3))
	       (cons b1 (substring str 1)))))
	  ((and (not (null b1)) (>= b1 240) (<= b1 247)
		(not (null b2)) (>= b2 128) (<= b2 191)
		(not (null b3)) (>= b3 128) (<= b3 191)
		(not (null b4)) (>= b4 128) (<= b4 191))
	   ; This is 4 byte UTF-8 character if in range 0x10000-0x10ffff
	   (let ((v (logior (lsh (logand b1 (lognot 240)) 18)
			    (lsh (logand b2 (lognot 128)) 12)
			    (lsh (logand b3 (lognot 128)) 6)
			    (logand b4 (lognot 128)))))
	     (if (and (>= v 65566) (<= v irchat-utf8-kludge-max-unicode-val))
		 (cons v (substring str 4))
	       (cons b1 (substring str 1)))))
	  (t
	   ; This is NOT UTF-8 encoding at all and is returned as is
	   (cons b1 (substring str 1))))))

(defun irchat-utf8-kludge-visible-char (ch)
  "Convert a character code into a visible string."
  (let ((c (assq (string-to-int (format "%d" ch)) irchat-utf8-kludge-table))
	(r (irchat-utf8-kludge-code-range ch))
	(v))
    (cond ((setq v (nth 2 c))
	   v)
	  ((setq v (nth 1 c))
	   (format "[%s]" v))
	  ((not (null r))
	   (format "[U+%04x %s]" ch r))
	  (t
	   (format "[U+%04x]" ch)))))

(defun irchat-utf8-kludge-decode (str)
  "Decode STR as utf-8 opportunisticly."
  (let ((r "")
	(e nil))
    (while (setq e (irchat-utf8-kludge-decode-first str))
	(setq r (concat r (irchat-utf8-kludge-visible-char (car e))))
	(setq str (cdr e)))
    r))

(defun irchat-utf8-kludge-encode-char (c)
  "Encode character value C to utf-8 string."
  (cond ((and (>= c 0) (<= c 127))
	 (string c))
	((and (>= c 128) (<= c 2047))
	 (string (logior 192 (logand (lsh c -6) 31))
		 (logior 128 (logand c 63))))
	((and (>= c 2048) (<= c 65565))
	 (string (logior 224 (logand (lsh c -12) 15))
		 (logior 128 (logand (lsh c -6) 63))
		 (logior 128 (logand c 63))))
	((and (>= c 65566) (<= c irchat-utf8-kludge-max-unicode-val))
	 (string (logior 240 (logand (lsh c -18) 7))
		 (logior 128 (logand (lsh c -12) 63))
		 (logior 128 (logand (lsh c -6) 63))
		 (logior 128 (logand c 63))))
	(t nil)))

(defun irchat-utf8-kludge-encode (str)
  "Encode a STR which can be either a string or vector of numbers to UTF-8."
  (let ((r "")
	(i 0)
	(len (length str)))
    (while (< i len)
      (let ((c (irchat-utf8-kludge-nth-char str i)))
	(if (not (null c))
	    (let ((x (irchat-utf8-kludge-encode-char c)))
	      (if (not (null x))
		  (setq r (concat r x)
			i (+ i 1))
		(setq r nil
		      i len)))
	  (setq r nil
		i len))))
    r))

(defun irchat-utf8-kludge-encode-extended (str)
  "Encode a STR which can be either a string or vector of numbers to UTF-8. Extended characters presented as [U+#] where # is a hexadecimal number are also converted to their corresponding UTF-8 encoding."
  (let ((r ""))
    (while (> (length str) 0)
      (let ((c nil)
	    (len 0)
	    (x nil))
	(if (and (stringp str)
		 (string-match
		  "^\\(\\[U\\+\\([0-9a-fA-F][0-9a-fA-F]*\\)\\]\\)"
		  str))
	    (let ((s1 (matching-substring str 1))
		  (s2 (matching-substring str 2)))
	      (progn
		(setq len (length s1))
		(setq c (hex-to-int s2))
		(setq x (irchat-utf8-kludge-encode-char c)))))
	(if (and (null x)
		 (stringp str)
		 (string-match
		  "^\\[\\([^\]][^\]]*\\)\\]"
		  str))
	    (let ((s1 (matching-substring str 1)))
	      (setq len (length s1))
	      (setq c (irchat-utf8-kludge-get-char-with-symbolic-name s1))
	      (if (not (null c))
		  (setq x (irchat-utf8-kludge-encode-char c)))))
	(if (null x)
	    (progn
	      (setq len 1)
	      (setq c (irchat-utf8-kludge-nth-char str 0))
	      (setq x (irchat-utf8-kludge-encode-char c))))
	(if (not (null x))
	    (progn
	      (setq str (substring str len))
	      (setq r (concat r x)))
	  (progn
	    (setq str "")
	    (setq nil)))))
    r))

(defun irchat-utf8-kludge-code-range (n)
  (cond ((and (>= n 0) (<= n 127))
	 "Basic Latin")
        ((and (>= n 128) (<= n 255))
	 "Latin-1 Supplement")
        ((and (>= n 256) (<= n 383))
	 "Latin Extended A")
        ((and (>= n 384) (<= n 591))
	 "Latin Extended B")
        ((and (>= n 592) (<= n 687))
	 "IPA Extension")
        ((and (>= n 688) (<= n 767))
	 "Spacing Modifier Letter")
        ((and (>= n 768) (<= n 879))
	 "Combining Diacritical Mark")
        ((and (>= n 880) (<= n 1023))
	 "Greek and Coptic")
        ((and (>= n 1024) (<= n 1279))
	 "Cyrillic")
        ((and (>= n 1280) (<= n 1327))
	 "Cyrillic Supplement")
        ((and (>= n 1328) (<= n 1423))
	 "Armenian")
        ((and (>= n 1424) (<= n 1535))
	 "Hebrew")
        ((and (>= n 1536) (<= n 1791))
	 "Arabic")
        ((and (>= n 1792) (<= n 1871))
	 "Syriac")
        ((and (>= n 1872) (<= n 1919))
	 "Arabic Supplement")
        ((and (>= n 1920) (<= n 1983))
	 "Thaana")
        ((and (>= n 1984) (<= n 2047))
	 "NKo")
        ((and (>= n 2304) (<= n 2431))
	 "Devanagari")
        ((and (>= n 2432) (<= n 2559))
	 "Bengali")
        ((and (>= n 2560) (<= n 2687))
	 "Gurmukhi")
        ((and (>= n 2688) (<= n 2815))
	 "Gujarati")
        ((and (>= n 2816) (<= n 2943))
	 "Oriya")
        ((and (>= n 2944) (<= n 3071))
	 "Tamil")
        ((and (>= n 3072) (<= n 3199))
	 "Telugu")
        ((and (>= n 3200) (<= n 3327))
	 "Kannada")
        ((and (>= n 3328) (<= n 3455))
	 "Malayalam")
        ((and (>= n 3456) (<= n 3583))
	 "Sinhala")
        ((and (>= n 3584) (<= n 3711))
	 "Thai")
        ((and (>= n 3712) (<= n 3839))
	 "Lao")
        ((and (>= n 3840) (<= n 4095))
	 "Tibetan")
        ((and (>= n 4096) (<= n 4255))
	 "Myanmar")
        ((and (>= n 4256) (<= n 4351))
	 "Georgian")
        ((and (>= n 4352) (<= n 4607))
	 "Hangul Jamo")
        ((and (>= n 4608) (<= n 4991))
	 "Ethiopic")
        ((and (>= n 4992) (<= n 5023))
	 "Ethiopic Supplement")
        ((and (>= n 5024) (<= n 5119))
	 "Cherokee")
        ((and (>= n 5120) (<= n 5759))
	 "Unified Canadian Aboriginal Syllabic")
        ((and (>= n 5760) (<= n 5791))
	 "Ogham")
        ((and (>= n 5792) (<= n 5887))
	 "Runic")
        ((and (>= n 5888) (<= n 5919))
	 "Tagalog")
        ((and (>= n 5920) (<= n 5951))
	 "Hanunoo")
        ((and (>= n 5952) (<= n 5983))
	 "Buhid")
        ((and (>= n 5984) (<= n 6015))
	 "Tagbanwa")
        ((and (>= n 6016) (<= n 6143))
	 "Khmer")
        ((and (>= n 6144) (<= n 6319))
	 "Mongolian")
        ((and (>= n 6400) (<= n 6479))
	 "Limbu")
        ((and (>= n 6480) (<= n 6527))
	 "Tai Le")
        ((and (>= n 6528) (<= n 6623))
	 "New Tai Lue")
        ((and (>= n 6624) (<= n 6655))
	 "Khmer Symbol")
        ((and (>= n 6656) (<= n 6687))
	 "Buginese")
        ((and (>= n 6912) (<= n 7039))
	 "Balinese")
        ((and (>= n 7040) (<= n 7103))
	 "Sundanese")
        ((and (>= n 7168) (<= n 7247))
	 "Lepcha")
        ((and (>= n 7248) (<= n 7295))
	 "Ol Chiki")
        ((and (>= n 7424) (<= n 7551))
	 "Phonetic Extension")
        ((and (>= n 7552) (<= n 7615))
	 "Phonetic Extension Supplement")
        ((and (>= n 7616) (<= n 7679))
	 "Combining Diacritical Mark Supplement")
        ((and (>= n 7680) (<= n 7935))
	 "Latin Extended Additional")
        ((and (>= n 7936) (<= n 8191))
	 "Greek Extended")
        ((and (>= n 8192) (<= n 8303))
	 "General Punctuation")
        ((and (>= n 8304) (<= n 8351))
	 "Superscript or Subscript")
        ((and (>= n 8352) (<= n 8399))
	 "Currency Symbol")
        ((and (>= n 8400) (<= n 8447))
	 "Combining Diacritical Mark for Symbol")
        ((and (>= n 8448) (<= n 8527))
	 "Letterlike Symbol")
        ((and (>= n 8528) (<= n 8591))
	 "Number Form")
        ((and (>= n 8592) (<= n 8703))
	 "Arrow")
        ((and (>= n 8704) (<= n 8959))
	 "Mathematical Operator")
        ((and (>= n 8960) (<= n 9215))
	 "Miscellaneous Technical")
        ((and (>= n 9216) (<= n 9279))
	 "Control Picture")
        ((and (>= n 9280) (<= n 9311))
	 "Optical Character Recognition")
        ((and (>= n 9312) (<= n 9471))
	 "Enclosed Alphanumeric")
        ((and (>= n 9472) (<= n 9599))
	 "Box Drawing")
        ((and (>= n 9600) (<= n 9631))
	 "Block Element")
        ((and (>= n 9632) (<= n 9727))
	 "Geometric Shape")
        ((and (>= n 9728) (<= n 9983))
	 "Miscellaneous Symbol")
        ((and (>= n 9984) (<= n 10175))
	 "Dingbat")
        ((and (>= n 10176) (<= n 10223))
	 "Miscellaneous Mathematical Symbol A")
        ((and (>= n 10224) (<= n 10239))
	 "Supplemental Arrow A")
        ((and (>= n 10240) (<= n 10495))
	 "Braille Pattern")
        ((and (>= n 10496) (<= n 10623))
	 "Supplemental Arrow B")
        ((and (>= n 10624) (<= n 10751))
	 "Miscellaneous Mathematical Symbol B")
        ((and (>= n 10752) (<= n 11007))
	 "Supplemental Mathematical Operator")
        ((and (>= n 11008) (<= n 11263))
	 "Miscellaneous Symbol or Arrow")
        ((and (>= n 11264) (<= n 11359))
	 "Glagolitic")
        ((and (>= n 11360) (<= n 11391))
	 "Latin Extended C")
        ((and (>= n 11392) (<= n 11519))
	 "Coptic")
        ((and (>= n 11520) (<= n 11567))
	 "Georgian Supplement")
        ((and (>= n 11568) (<= n 11647))
	 "Tifinagh")
        ((and (>= n 11648) (<= n 11743))
	 "Ethiopic Extended")
        ((and (>= n 11744) (<= n 11775))
	 "Cyrillic Extended A")
        ((and (>= n 11776) (<= n 11903))
	 "Supplemental Punctuation")
        ((and (>= n 11904) (<= n 12031))
	 "CJK Radical Supplement")
        ((and (>= n 12032) (<= n 12255))
	 "Kangxi Radical")
        ((and (>= n 12272) (<= n 12287))
	 "Ideographic Description Character")
        ((and (>= n 12288) (<= n 12351))
	 "CJK Symbol or Punctuation")
        ((and (>= n 12352) (<= n 12447))
	 "Hiragana")
        ((and (>= n 12448) (<= n 12543))
	 "Katakana")
        ((and (>= n 12544) (<= n 12591))
	 "Bopomofo")
        ((and (>= n 12592) (<= n 12687))
	 "Hangul Compatibility Jamo")
        ((and (>= n 12688) (<= n 12703))
	 "Kanbun")
        ((and (>= n 12704) (<= n 12735))
	 "Bopomofo Extended")
        ((and (>= n 12736) (<= n 12783))
	 "CJK Stroke")
        ((and (>= n 12784) (<= n 12799))
	 "Katakana Phonetic Extension")
        ((and (>= n 12800) (<= n 13055))
	 "Enclosed CJK Letter or Month")
        ((and (>= n 13056) (<= n 13311))
	 "CJK Compatibility")
        ((and (>= n 13312) (<= n 19903))
	 "CJK Unified Ideograph Extension A")
        ((and (>= n 19904) (<= n 19967))
	 "Yijing Hexagram Symbol")
        ((and (>= n 19968) (<= n 40959))
	 "CJK Unified Ideograph")
        ((and (>= n 40960) (<= n 42127))
	 "Yi Syllable")
        ((and (>= n 42128) (<= n 42191))
	 "Yi Radical")
        ((and (>= n 42240) (<= n 42559))
	 "Vai")
        ((and (>= n 42560) (<= n 42655))
	 "Cyrillic Extended B")
        ((and (>= n 42752) (<= n 42783))
	 "Modifier Tone Letter")
        ((and (>= n 42784) (<= n 43007))
	 "Latin Extended D")
        ((and (>= n 43008) (<= n 43055))
	 "Syloti Nagri")
        ((and (>= n 43072) (<= n 43135))
	 "Phags-pa")
        ((and (>= n 43136) (<= n 43231))
	 "Saurashtra")
        ((and (>= n 43264) (<= n 43311))
	 "Kayah Li")
        ((and (>= n 43312) (<= n 43359))
	 "Rejang")
        ((and (>= n 43520) (<= n 43615))
	 "Cham")
        ((and (>= n 44032) (<= n 55215))
	 "Hangul Syllable")
        ((and (>= n 55296) (<= n 56191))
	 "High Surrogate")
        ((and (>= n 56192) (<= n 56319))
	 "High Private Use Surrogate")
        ((and (>= n 56320) (<= n 57343))
	 "Low Surrogate")
        ((and (>= n 57344) (<= n 63743))
	 "Private Use Area")
        ((and (>= n 63744) (<= n 64255))
	 "CJK Compatibility Ideograph")
        ((and (>= n 64256) (<= n 64335))
	 "Alphabetic Presentation Form")
        ((and (>= n 64336) (<= n 65023))
	 "Arabic Presentation Forms A")
        ((and (>= n 65024) (<= n 65039))
	 "Variation Selector")
        ((and (>= n 65040) (<= n 65055))
	 "Vertical Form")
        ((and (>= n 65056) (<= n 65071))
	 "Combining Half Mark")
        ((and (>= n 65072) (<= n 65103))
	 "CJK Compatibility Form")
        ((and (>= n 65104) (<= n 65135))
	 "Small Form Variant")
        ((and (>= n 65136) (<= n 65279))
	 "Arabic Presentation Forms B")
        ((and (>= n 65280) (<= n 65519))
	 "Halfwidth or Fullwidth Form")
        ((and (>= n 65520) (<= n 65535))
	 "Special")
        ((and (>= n 65536) (<= n 65663))
	 "Linear B Syllabary")
        ((and (>= n 65664) (<= n 65791))
	 "Linear B Ideogram")
        ((and (>= n 65792) (<= n 65855))
	 "Aegean Number")
        ((and (>= n 65856) (<= n 65935))
	 "Ancient Greek Number")
        ((and (>= n 65936) (<= n 65999))
	 "Ancient Symbol")
        ((and (>= n 66000) (<= n 66047))
	 "Phaistos Disc")
        ((and (>= n 66176) (<= n 66207))
	 "Lycian")
        ((and (>= n 66208) (<= n 66271))
	 "Carian")
        ((and (>= n 66304) (<= n 66351))
	 "Old Italic")
        ((and (>= n 66352) (<= n 66383))
	 "Gothic")
        ((and (>= n 66432) (<= n 66463))
	 "Ugaritic")
        ((and (>= n 66464) (<= n 66527))
	 "Old Persian")
        ((and (>= n 66560) (<= n 66639))
	 "Deseret")
        ((and (>= n 66640) (<= n 66687))
	 "Shavian")
        ((and (>= n 66688) (<= n 66735))
	 "Osmanya")
        ((and (>= n 67584) (<= n 67647))
	 "Cypriot Syllabary")
        ((and (>= n 67840) (<= n 67871))
	 "Phoenician")
        ((and (>= n 67872) (<= n 67903))
	 "Lydian")
        ((and (>= n 68096) (<= n 68191))
	 "Kharoshthi")
        ((and (>= n 73728) (<= n 74751))
	 "Cuneiform")
        ((and (>= n 74752) (<= n 74879))
	 "Cuneiform Number or Punctuation")
        ((and (>= n 118784) (<= n 119039))
	 "Byzantine Musical Symbol")
        ((and (>= n 119040) (<= n 119295))
	 "Musical Symbol")
        ((and (>= n 119296) (<= n 119375))
	 "Ancient Greek Musical Notation")
        ((and (>= n 119552) (<= n 119647))
	 "Tai Xuan Jing Symbol")
        ((and (>= n 119648) (<= n 119679))
	 "Counting Rod Numeral")
        ((and (>= n 119808) (<= n 120831))
	 "Mathematical Alphanumeric Symbol")
        ((and (>= n 126976) (<= n 127023))
	 "Mahjong Tile")
        ((and (>= n 127024) (<= n 127135))
	 "Domino Tile")
        ((and (>= n 131072) (<= n 173791))
	 "CJK Unified Ideograph Extension B")
        ((and (>= n 194560) (<= n 195103))
	 "CJK Compatibility Ideograph Supplement")
        ((and (>= n 917504) (<= n 917631))
	 "Tag")
        ((and (>= n 917760) (<= n 917999))
	 "Variation Selector Supplement")
        ((and (>= n 983040) (<= n 1048575))
	 "Supplementary Private Use Area A")
        ((and (>= n 1048576) (<= n 1114111))
	 "Supplementary Private Use Area B")
	(t nil)))

(eval-and-compile
  (provide 'irchat-utf8))
;;;
;;; eof
;;;
