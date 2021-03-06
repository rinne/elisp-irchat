;;;   -*- lisp -*-
;;; 
;;;  ----------------------------------------------------------------------
;;;  CRC32 compatible crc in elisp.  Cool, ha?
;;;  ----------------------------------------------------------------------
;;;  Created      : Fri Dec 16 13:47:25 1995 tri
;;;  Last modified: Sat Feb  4 20:21:44 2017 tri
;;;  ----------------------------------------------------------------------
;;;  Copyright © 1995-1997, 2003, 2017
;;;  Timo J. Rinne <tri@iki.fi>
;;;  ----------------------------------------------------------------------
;;;  Any express or implied warranties are disclaimed.  In no event
;;;  shall the author be liable for any damages caused (directly or
;;;  otherwise) by the use of this software.
;;;  
;;;  irchat-copyright.el applies only if used with irchat IRC client.
;;;  Contact the author for additional copyright info.
;;;

(eval-and-compile  
  (provide 'crc32))

;;; Interactive functions
(defun crc32-region (&optional raw)
  "Calculate crc32 of the current region"
  (interactive)
  (if mark-active
      (let* ((s (buffer-substring (mark) (point)))
	     (crc (crc32-string s raw)))
	(message "Length = %d, CRC32 = %s" (length s) crc)
	crc)
    (error "Mark is not set")))

(defun crc32-buffer (&optional raw)
  "Calculate crc32 of the current buffer"
  (interactive)
  (let* ((s (buffer-substring (point-min) (point-max)))
	 (crc (crc32-string s raw)))
    (message "Length = %d, CRC32 = %s" (length s) crc)
    crc))

;;; Lookup table
(defconst crc32-lookup-table
  [ (0 . 0) (30471 . 12438) (60942 . 24876) (39177 . 20922) (1901 . 50201)
    (28778 . 62607) (59747 . 42293) (40548 . 38307) (3803 . 34866)
    (31196 . 47268) (57557 . 59678) (38866 . 55688) (2486 . 19499)
    (32433 . 31933) (59320 . 11527) (37055 . 7569) (7607 . 4196) (27312 . 8434)
    (62393 . 29000) (33982 . 16862) (6874 . 54397) (28125 . 58603)
    (62676 . 46417) (33747 . 34247) (4972 . 38998) (25707 . 43200)
    (64866 . 63866) (35429 . 51692) (5121 . 23631) (25350 . 27865)
    (64015 . 15715) (36104 . 3573) (15214 . 8392) (19561 . 4190)
    (54624 . 16868) (41575 . 29042) (15363 . 58577) (19204 . 54343)
    (53773 . 34301) (42250 . 46443) (13749 . 43258) (17074 . 39020)
    (56251 . 51670) (44220 . 63808) (13016 . 27875) (17887 . 23669)
    (56534 . 3535) (43985 . 15705) (9945 . 12460) (20958 . 58) (51415 . 20864)
    (49104 . 24854) (8628 . 62645) (22195 . 50211) (53178 . 38297)
    (47293 . 42255) (10242 . 47262) (24325 . 34824) (50700 . 55730)
    (45323 . 59684) (12143 . 31879) (22632 . 19473) (49505 . 7595)
    (46694 . 11581) (30428 . 16784) (475 . 28934) (39122 . 8380) (61397 . 4138)
    (29105 . 34185) (1718 . 46367) (40895 . 58533) (59576 . 54323)
    (30727 . 51618) (3840 . 63796) (38409 . 43150) (57614 . 38936)
    (32618 . 3515) (2157 . 15661) (37220 . 27799) (58979 . 23553)
    (27499 . 20980) (7276 . 24930) (34149 . 12504) (62050 . 78) (27654 . 38381)
    (6913 . 42363) (33288 . 62657) (62735 . 50263) (26032 . 55750)
    (4791 . 59728) (35774 . 47338) (64697 . 34940) (25309 . 7647)
    (5594 . 11593) (36051 . 31987) (64468 . 19557) (19890 . 24920)
    (15029 . 20942) (41916 . 116) (54459 . 12514) (19167 . 42305)
    (15832 . 38359) (42193 . 50285) (54230 . 62715) (17257 . 59754)
    (13422 . 55804) (44391 . 34886) (55904 . 47312) (17412 . 11635)
    (13059 . 7653) (43530 . 19551) (56589 . 31945) (20485 . 28988)
    (9986 . 16810) (48651 . 4112) (51468 . 8326) (22376 . 46373) (8303 . 34227)
    (47462 . 54281) (52833 . 58527) (24286 . 63758) (10713 . 51608)
    (45264 . 38946) (51159 . 43188) (22963 . 15639) (11956 . 3457)
    (47037 . 23611) (49338 . 27821) (60856 . 33568) (39615 . 46006)
    (950 . 57868) (29873 . 53914) (60117 . 18233) (40402 . 30639) (1243 . 9749)
    (29660 . 5763) (58211 . 2834) (37988 . 15236) (3437 . 27198)
    (31338 . 23208) (58382 . 53003) (37641 . 65437) (2560 . 44583)
    (32007 . 40625) (61455 . 37700) (34568 . 41938) (7681 . 62056)
    (26886 . 49918) (63330 . 22365) (32869 . 26571) (6508 . 13937)
    (28267 . 1767) (65236 . 7030) (35283 . 11232) (4314 . 31322)
    (26589 . 19148) (63929 . 57199) (36542 . 61433) (6071 . 48707)
    (24752 . 36565) (54998 . 41960) (41425 . 37758) (14552 . 49860)
    (20447 . 62034) (53691 . 26609) (42684 . 22375) (16309 . 1757)
    (18610 . 13899) (55309 . 11226) (44810 . 6988) (13827 . 19190)
    (16644 . 31328) (57184 . 61379) (43111 . 57173) (12654 . 36591)
    (18025 . 48761) (52065 . 45964) (48230 . 33562) (9583 . 53920)
    (21096 . 57910) (52236 . 30613) (47883 . 18179) (8706 . 5817)
    (21765 . 9775) (50618 . 15294) (45757 . 2856) (11188 . 23186)
    (23731 . 27140) (49879 . 65447) (46544 . 53041) (11481 . 40587)
    (23518 . 44573) (39780 . 49840) (60515 . 61990) (30058 . 41884)
    (621 . 37642) (39945 . 1705) (60174 . 13887) (29191 . 26501) (1280 . 22291)
    (38335 . 19074) (58040 . 31252) (31665 . 11182) (3254 . 6968)
    (37586 . 36507) (58837 . 48653) (31964 . 61367) (3035 . 57121)
    (34515 . 53972) (61908 . 57922) (26845 . 46072) (8154 . 33646)
    (33214 . 5837) (63161 . 9819) (28592 . 30689) (6327 . 18295)
    (34824 . 23270) (65295 . 27248) (26118 . 15306) (4353 . 2908)
    (36709 . 40703) (63586 . 44649) (24939 . 65491) (5740 . 53061)
    (40970 . 57976) (55053 . 53998) (19972 . 33620) (14595 . 46018)
    (42855 . 9825) (53344 . 5879) (18793 . 18253) (15982 . 30683)
    (44753 . 27210) (55766 . 23260) (16607 . 2918) (14296 . 15344)
    (43452 . 44627) (57019 . 40645) (18354 . 53119) (12469 . 65513)
    (48573 . 61980) (51898 . 49802) (21427 . 37680) (9396 . 41894)
    (47824 . 13829) (52695 . 1683) (21726 . 22313) (9177 . 26559)
    (45926 . 31278) (50273 . 19128) (23912 . 6914) (10863 . 11156)
    (46091 . 48695) (49932 . 36513) (23045 . 57115) (11522 . 61325) ]
  "Lookup table for crc32 routines")

;;; Arithmetics
(defun crc32-^ (x1 x2)
  "Bitwise xor for crc32 routine"
  (let ((a (if (consp x1) x1 (cons 0 x1)))
	(b (if (consp x2) x2 (cons 0 x2))))
    (cons (logxor (car a) (car b)) (logxor (cdr a) (cdr b)))))

(defun crc32-& (x1 x2)
  "Bitwise xor for crc32 routine"
  (let ((a (if (consp x1) x1 (cons 0 x1)))
	(b (if (consp x2) x2 (cons 0 x2))))
    (cons (logand (car a) (car b)) (logand (cdr a) (cdr b)))))

(defun crc32-mask-8bit (x)
  "Mask crc32 PAIR to 8bit integer"
  (logand 255 (cdr x)))

(defun crc32-shr-8 (x)
  "Shift crc32 PAIR left 8 bits"
  (let* ((h (car x)) 
	 (l (cdr x)))
    (cons (/ h 256)
	  (logior (* (logand h 255) 256) (/ l 256)))))

;;; CRC32 function
(defun crc32-string (str &optional raw)
  "Calculate 32bit crc of STRING"
  (let ((r (cons 0 0))
	(l (length str))
	(j 0))
    (while (< j l)
      (let* ((i (crc32-mask-8bit (crc32-^ r (logand (elt str j) 255)))))
	(setq r (crc32-^ (elt crc32-lookup-table i)
			 (crc32-shr-8 r)))
	(setq j (+ 1 j))))
    (cond ((or (null raw) (equal raw 'hex-string))
	   (format "%04.4x%04.4x" (car r) (cdr r)))
	  ((equal raw 'raw-vector)
	   (let ((v [0 0 0 0]))
	     (aset v 0 (logand (/ (car r) 256) 255))
	     (aset v 1 (logand (car r) 255))
	     (aset v 2 (logand (/ (cdr r) 256) 255))
	     (aset v 3 (logand (cdr r) 255))
	     v))
	  ((or (equal raw 'raw-string) (equal raw t))
	   (let ((v "xxxx"))
	     (aset v 0 (logand (/ (car r) 256) 255))
	     (aset v 1 (logand (car r) 255))
	     (aset v 2 (logand (/ (cdr r) 256) 255))
	     (aset v 3 (logand (cdr r) 255))
	     v))
	  (t (error "Unknown return type in crc32-string")))))

;;; eof (crc32.el)






