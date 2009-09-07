;;; skk-dinsert.el --- SKK dynamic insert -*- coding: iso-2022-jp -*-

;; Copyright (C) 2002 Eiji Obata <obata@suzuki.kuee.kyoto-u.ac.jp>

;; Author: Eiji Obata <obata@suzuki.kuee.kyoto-u.ac.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-dinsert.el,v 1.10 2007/04/22 02:38:28 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2007/04/22 02:38:28 $

;; This file is part of Daredevil SKK.

;; Daredevil SKK is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.

;; Daredevil SKK is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Daredevil SKK, see the file COPYING.  If not, write to
;; the Free Software Foundation Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; $B%+!<%=%k$NA08e$NJ8;zNs$d!"G$0U$N(B elisp $B$NI>2ACM$K$h$C$F%P%C%U%!$KA^(B
;; $BF~$9$kJ8;z$rF0E*$K7hDj$9$k%W%m%0%i%`$G$9!#(B
;;
;; skk-dinsert-rule-list $B$K!"%-!<F~NO$H$=$l$KBP1~$9$k>r7o$N%j%9%H$r=q(B
;; $B$$$F$/$@$5$$!#?t;z$ND>8e$G$N$_(B [-,.]$B$r(B[$B!<!"!#(B]$B$G$J$/$=$N$^$^F~NO$7(B
;; $B$?$$>l9g$K$O<!$N$h$&$K(B .skk $B$K=q$$$F$/$@$5$$!#(B
;;
;;   (setq skk-dinsert-rule-list
;;         '(("." nil
;;            (("[0-9]" . ".")
;;             (t . skk-current-kuten)))
;;           ("," nil
;;            (("[0-9]" . ",")
;;             (t . skk-current-touten)))
;;           ("-" nil
;;            (("[0-9]" . "-")
;;             (t . "$B!<(B")))))
;;
;; $B$^$?!"(BSKK $BK\BN$KE}9g$5$l$k$^$G$O!"(Bskk-dinsert-rule-list $B$N@_Dj$h$j(B
;; $B$b2<$NJ}$K0J2<$N%3!<%I$rDI2C$7$F2<$5$$!#(B
;;
;;   (when (locate-library "skk-dinsert")
;;     (require 'skk-dinsert)
;;     (setq skk-rom-kana-rule-list
;;           (append skk-rom-kana-rule-list
;;                   (let ((count -1))
;;                     (mapcar #'(lambda (r)
;;                                 (setq count (1+ count))
;;                                 (list (car r)
;;                                       (cadr r)
;;                                       `(lambda (arg)
;;                                          (skk-dinsert arg ,count))))
;;                             skk-dinsert-rule-list)))))
;;
;; $B$5$i$K!"$3$N%U%!%$%k$r(B load-path $B$NDL$C$?%G%#%l%/%H%j$KCV$$$F2<$5$$!#(B
;;
;;
;; $BF0E*$JF~NO$rL58z$K$7$?$$;~$O!"(BM-x skk-toggle-dinsert $B$7$F2<$5$$!#(B
;; $BKt!"0l;~E*$KL58z$K$7$?$$;~$O!"(BQ $B$NF~NO$K$h$j"&%b!<%I$KF~$C$F2<$5$$!#(B
;; $BB3$/0lJ8;zL\$K$D$$$F$OL58z$K$J$j$^$9!#(B

;;; Code:

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars))

(defvar skk-dinsert-mode t
  "*Non-nil $B$G$"$l$P!"(B`skk-dinsert' $B$K$h$kF0E*$JF~NO$rM-8z$K$9$k!#(B
nil $B$G$"$l$P(B `skk-dinsert-rule-list' $B$G$N(B t $B$KBP1~$9$kCM$rMQ$$$k!#(B

isearch $B$N:]$K$O!"$3$NJQ?t$NCM$K$h$i$:!"F0E*$JF~NO$OL58z$K$J$k!#Kt!"(B
Q (skk-set-henkan-point-subr) $B$NF~NO$K$h$C$F"&%b!<%I$KF~$k$H!"D>8e$N(B
$BF~NO$K8B$jL58z$K$J$k!#(B")

(defvar skk-dinsert-ignore-lf t
  "*Non-nil $B$G$"$l$P!"%+!<%=%k0JA0$NJ8;zNs$K$h$k>r7oH=Dj$r9TF,$G9T$J$C$?>l9g!"2~9T$rL5;k$9$k!#(B
`skk-dinsert-rule-list' $B$K$FBP1~$9$k%*%W%7%g%s$,;XDj$7$F$"$k$H!"$=$A$i$,M%@h$5$l$k!#(B")

(defvar skk-dinsert-rule-list nil
  "*`skk-dinsert' $B$K$h$kF0E*$JF~NO$N>r7o%j%9%H!#(B

$B%j%9%H$N3FMWAG$O!"$=$l$>$l$,0l$D$N5,B'$G$"$j!"2<5-$N7A<0$rK~$?$7$F$$$J$1$l$P(B
$B$J$i$J$$!#(B

 (INPUT-STATE NEXT-STATE RULE-ALIST)

INPUT-STATE, NEXT-STATE $B$N0UL#$O(B `skk-rom-kana-base-rule-list' $B$HF1$8$G$"$j!"(B
SKK $B$O(B INPUT-STATE $B$r8!=P$9$k$H!"(BRULE-ALIST $B$K4p$E$$$F%P%C%U%!$KJ8;z$rA^F~$7!"(B
$BB3$$$F(B NEXT-STATE $B$K>uBV$r0\$7$?$&$($G!"F~NOBT$A>uBV$H$J$k!#(B

RULE-ALIST $B$O>r7o$H!"$=$l$,@.N)$7$?;~$K=PNO$5$l$kCM$NO"A[%j%9%H$G$"$k!#(B
$B$=$l$>$l$N%k!<%k$O(B

  (REGEXP looking-at b-regexp/ignore-lf limit s-exp . VAL)

$BKt$O(B

  (S-EXP . VAL)

$B$N7A<0$r$H$k!#(B(REGEXP, S-EXP, VAL $B0J30$O>JN,2DG=(B)

$B=i$a$K!":G=i$NMWAG$,J8;zNs$G$"$k>l9g$K$D$$$F@bL@$9$k!#(B
$B%j%9%H$N3FMWAG$N0UL#$O2<5-$NDL$j!#(B

  0th: $BJ8;zNs$r@55,I=8=$H$7$F07$$!"%+!<%=%k<~0O$NJ8;zNs$,$3$l$K%^%C%A$9$k$+H=Dj(B
       $B$9$k!#(B
  1th: Non-nil $B$G$"$l$P(B `looking-at' $B$rMQ$$$F%+!<%=%k0J8e$NJ8;zNs$,(B REGEXP $B$K%^%C(B
       $B%A$9$k$+H=Dj$9$k!#(Bnil $B$G$"$l$P(B `re-search-backward' $B$K$h$j%+!<%=%kD>A0$N(B
       $BJ8;zNs$,(B REGEXP $B$K%^%C%A$9$k$+H=Dj$9$k!#>JN,$9$k$H(B nil $B$H$7$F07$&!#(B
  2th: 1th $B$N(B looking-at $B$NCM$K$h$j0[$J$k0UL#$r;}$D!#(B
         looking-at $B$N;XDj$,(B Non-nil $B$N;~(B:
           nil $B$G$"$l$P!"%+!<%=%kD>8e$NJ8;zNs$,(B REGEXP $B$K%^%C%A$9$k$+H=Dj$9$k!#(B
           Non-nil $B$G$"$l$P!"$3$l$r@55,I=8=$H$7$F07$$!"(B`re-search-backward' $B$K$h$C(B
           $B$F%+!<%=%k0\F0$r$9$k!#$=$N8e(B `looking-at' $B$r9T$J$$(B REGEXP $B$,J8;zNs$K(B
           $B%^%C%A$9$k$+H=Dj$9$k!#$3$l$O%3%s%F%-%9%H$K1~$8$?F~NO$r$9$k>l9g$KM-8z(B
           $B$G$"$k(B($B$H;W$o$l$k(B)$B!#(B
         looking-at $B$N;XDj$,(B nil $B$N;~(B(`re-search-backward' $B$K$h$C$FH=Dj$9$k;~(B):
           Non-nil $B$G$"$l$P!"9TF,$G$NF~NO$K$*$$$F!"$=$ND>A0$NJ8;z(B($B$9$J$o$A2~9T%3!<(B
           $B%I(B)$B$rL5;k$7$F(B REGEXP $B$H%^%C%A$9$k$+H=Dj$9$k!#;XDj$,L5$1$l$P(B
           `skk-dinsert-ignore-lf' $B$NCM$rMQ$$$k!#(B
  3th: `re-search-backward' $B$r9T$J$&:]$N8!:wHO0O$r;XDj$9$k!#$3$l$O%^%C%A%s%0$N>r(B
       $B7o$r9J$C$?$j!"%Q%U%)!<%^%s%9$NDc2<$rM^$($kL\E*$KMxMQ$5$l$k!#(B
       $B?tCM$,;XDj$5$l$k$H!"8=:_$N%+!<%=%k0LCV$h$j(B limit $BJ8;zA0$^$G$rHO0O$H$7$F8!(B
       $B:w$9$k!#6qBNE*$K$O(B (- (point) limit) $B$r(B `re-search-backward' $B$KEO$9!#(B
       $B$=$l0J30$G$"$k$H!"(BS $B<0$H$7$FI>2A$7$?CM$rMQ$$$k!#(B
         note:
           * $B?tCM$r$=$N$^$^EO$7$?$$>l9g$K$O(B (quote 1) $BEy$H$9$kI,MW$,$"$k!#(B
           * $BI>2A$7$?CM$,%+!<%=%k0LCV$N(B point $B$h$jBg$-$$$H%(%i!<$K$J$k!#(B
  4th: REGEXP $B$K$h$k%^%C%A%s%0$,@.8y$7$?:]!"$3$N(B s-exp $B$rI>2A$9$k!#(B
       S $B<0$NCf$G$O(B arg $B$H(B m-d $B$rMxMQ$9$k;v$,$G$-!"(Barg $B$K$O(B `skk-dinsert' $B$N0z?t(B
       $B$,!"(Bm-d $B$K$O(B REGEXP $B$K$h$k%^%C%A%s%0$N:]$N(B (match-data) $B$NFbMF$,F~$C$F$$(B
       $B$k!#(Bs-exp $B$NI>2ACM$,(B nil $B$G$"$k$H!"$3$N%k!<%k$OE,MQ$5$l$J$$!#(B
  5th: REGEXP $B$K$h$k%^%C%A%s%0$,@.N)$7!"$+$D(B s-exp($B;XDj$5$l$F$$$l$P(B)$B$N(B
       $BI>2ACM$,(B Non-nil $B$G$"$C$?;~!"$3$N(B VAL $B$,%P%C%U%!$KA^F~$5$l$k!#(B

$B<!$K:G=i$NMWAG$,J8;zNs0J30$N>l9g$G$"$k$,!"$3$l$O(B S $B<0$H$7$FI>2A$5$l$k!#(B
$BI>2ACM$,(B Non-nil $B$G$"$l$P%k!<%k$,E,MQ$5$l!"BP1~$9$k(B VAL $B$,%P%C%U%!$KA^F~$5$l$k!#(B

$B%k!<%k$O>e$+$i=g$K;n$5$l!":G=i$K@.N)$7$?$b$N$,E,MQ$5$l$k!#(B

VAL $B$K$O!"0J2<$N(B 3$B$D$N7A<0$r;XDj$G$-$k!#(B

$BJ8;zNs(B -- $B$3$l$,%P%C%U%!$KA^F~$5$l$k!#(B
$B4X?tL>%7%s%\%k(B
       -- `skk-rom-kana-rule-list' $B$G;XDj$7$?>l9g$HF1MM!"0z?tIU$-$G8F$P$l$k!#(B
          $BJV$jCM$,J8;zNs$G$"$l$P$=$l$,%P%C%U%!$KA^F~$5$l$k!#(B
$BJQ?tL>%7%s%\%k(B
       -- (`format' \"%S\" VAL) $B$7$?CM$,%P%C%U%!$KA^F~$5$l$k!#(B

$BFCJL$J>l9g$H$7$F(B nil $B$r;XDj$9$k;v$,$G$-$k!#$3$N;~$K$O!"(BS-EXP $BKt$O(B s-exp $B$NI>2ACM(B
$B$,MQ$$$i$l!"$3$l$O>e$N(B 3$B$D$N7A<0$N2?$l$+$G$"$kI,MW$,$"$k!#(B

`skk-rom-kana-rule-list' $B$H$O0[$J$j!"%"%H%`$G$J$$MWAG$O;XDj$G$-$J$$!#$3$N$?$a(B
\(\"$B%+%J(B\" . \"$B$+$J(B\") $B$H$O;XDj$G$-$J$$!#$+$J%b!<%I!"%+%J%b!<%I$K$h$C$F>r7oJ,$1(B
$B$7$?$$>l9g$K$O!"JQ?t(B `skk-hiragana', `skk-katakana' $B$K$h$C$FD4$Y$k;v$,$G$-$k!#(B
  note: `skk-hiragana' $B$O(B `skk-dinsert' $B$NCf$K$*$$$F$N$_M-8z$J%m!<%+%kJQ?t$G$"$k!#(B")


(defun skk-toggle-dinsert (&optional arg)
  "$BF0E*$JF~NO$NM-8z(B/$BL58z$r@Z$jBX$($k!#(B"
  (interactive "P")
  (setq skk-dinsert-mode (cond ((null arg)
				(not skk-dinsert-mode))
			       ((> (prefix-numeric-value arg) 0)
				t)
			       (t
				nil))))

(defun skk-dinsert (arg idx)
  (let ((rule-alist (nth 2 (nth idx skk-dinsert-rule-list)))
	;; VAL $B$K(B ("$B%+%J(B" . "$B$+$J(B") $B$N7A<0$,=q$1$J$$$N$G(B
	;; $B$;$a$FJQ?t$rDs6!$7$F$_$k(B
	(skk-hiragana (and (not skk-katakana) skk-j-mode))
	val cnd)
    (if (or (not skk-dinsert-mode)
	    (and skk-henkan-mode
		 (= skk-henkan-start-point skk-kana-start-point))
	    (and skk-isearch-switch
		 (buffer-live-p skk-isearch-current-buffer)))
	;; isearch $BKt$O(B $BF0E*$JF~NO$r$7$J$$$J$i(B t $B$KBP1~$9$kCM$r;H$&(B
	(setq val (cdr (assq t rule-alist)))
      (catch 'return
	(dolist (cur-rule rule-alist)
	  (setq cnd (car cur-rule))
	  (cond
	   ((stringp cnd)		; REGEXP
	    (let (found s-exp m-d)
	      (setq val
		    (save-match-data
		      (save-excursion
			(let* ((i 0)
			       (regexp cnd)
			       (r cur-rule)
			       (v (progn
				    (while (not (atom r))
				      (setq i (1+ i)
					    r (cdr r)))
				    r))
			       l-a i-lf b-regexp lim pos)
			  ;; (0 1 2 3 4 . 5) $B$N7A<0$r:NMQ$7$F$$$k$N$G(B
			  ;; (nth n LIST) $B$r$9$k$K$O(B i > n $B$G$"$k;v$,I,MW(B
			  (ignore-errors
			    (setq l-a (nth 1 cur-rule)
				  i-lf (nth 2 cur-rule)
				  b-regexp i-lf
				  lim (nth 3 cur-rule)
				  s-exp (nth 4 cur-rule)))
			  ;; re-search-backward $B$N(B limit $BD4@0(B
			  (when lim
			    (setq lim
				  (if (numberp lim)
				      (- (point) lim) ; $BIi$K$J$C$F$b(B ok
				    (eval lim))))
			  (cond
			   (l-a		; looking-at
			    (when b-regexp
			      (re-search-backward b-regexp lim t))
			    (when (looking-at regexp)
			      (setq found t
				    m-d (match-data))
			      v))
			   (t		; re-search-backward
			    (when (and (not (bobp))
				       (bolp)
				       (if (> i 2)
					   i-lf
					 skk-dinsert-ignore-lf))
			      (backward-char))
			    (setq pos (point))
			    (when (and (re-search-backward regexp lim t)
				       (= pos
					  (match-end 0)))
			      (setq found t
				    m-d (match-data))
			      v)))))))
	      ;; match-data $B$rMQ$$$F=PNO$r@8@.(B or $B>r7oH=Dj(B
	      ;; skk-dinsert $B<+?H$N0z?t$O(B arg
	      ;; match-data $B$NFbMF$O(B m-d
	      (when (and found
			 s-exp)
		(setq val
		      (let ((retval (eval s-exp)))
			(when retval
			  (or val
			      retval))))))
	    (when val
	      (throw 'return nil)))
	   (t				; S-EXP
	    (let ((retval (eval cnd))
		  (v (cdr cur-rule)))
	      (setq val
		    (when retval
		      (or v
			  retval))))
	    (when val
	      (throw 'return nil)))))))
    (cond ((stringp val)
	   val)
	  ((functionp val)
	   (funcall val arg))
	  (t
	   (format "%S" val)))))


(provide 'skk-dinsert)
