;;; skk-correct.el --- correct key word for conversion. -*- coding: iso-2022-jp -*-
;; Copyright (C) 1999 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-correct.el,v 1.7 2007/04/22 02:38:28 skk-cvs Exp $
;; Keywords: japanese
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

;;; Commentary

;; $B<-=qCf$K$J$$FI$_$N%V%l$?8+=P$78l$GJQ49$7$?:]$K!"FbIt$G(B skk-correct-table
;; $B$r;2>H$7$F@5$7$$8+=P$78l$KCV$-49$(8uJd$r8!:w$9$k%W%m%0%i%`$G$9!#(B
;; skk-correct-search $B$N0z?t$K!"%V%lJd@58e$K8!:w$7$?$$<-=q$r!"(B
;; skk-search-prog-list $B$HF1$8MWNN$G%j%9%H$GI=$7!"8!:w%W%m%0%i%`$H$H$b$K5-:\(B
;; $B$7$F2<$5$$!#Nc$($P$3$s$J46$8$G$9!#(B
;;
;;   (skk-correct-search
;;    '((skk-search-jisyo-file skk-jisyo 0 t)
;;      (skk-search-server skk-aux-large-jisyo 10000)
;;      (skk-okuri-search)))
;;
;;     * $B>e5-$NNc$G$O!"Jd@5$7$?8+=P$78l$KBP$7!"8D?M<-=q!"%5!<%P!<!"<+F0Aw$j(B
;;       $B=hM}$7$F8D?M<-=q$H$$$&8!:w$r9T$J$$$^$9$,!"$I$3$+$GJd@58e$N8+=P$78l(B
;;       $B$KBP$9$k8uJd$,8+$D$+$l$P$=$l0J>e$O8!:w$r9T$J$$$^$;$s!#(B
;;
;; $B$3$l$r99$K(B skk-search-prog-list $B$NCf$NE,Ev$J8D=j$KF~$l$^$7$g$&!#2<5-$N$h(B
;; $B$&$K8D?M<-=q8!:wD>8e$K!"0lEY8+=P$78l$rJd@5$7$F8D?M<-=q$r8!:w$7D>$7!"%5!<(B
;; $B%P!<8!:w8e$K8+$D$+$i$J$+$C$?$i:FEYJd@5$7$F%5!<%P!<$r8!:w$9$k!"$H$$$&$N$b(B
;; $B0l0F$G$9!#$"$k$$$O!"8D?M$NJJ$r$b$m$KH?1G$7$F$$$k8D?M<-=q$N%V%lJd@5$O;_$a(B
;; $B$F!"%5!<%P!<$N$_Jd@58e8!:w$r$9$k!"$H$$$&$N$bNI$$$G$7$g$&!#(B
;;
;;   (setq skk-search-prog-list
;;         '((skk-search-jisyo-file skk-jisyo 0 t)
;;	     (skk-correct-search '((skk-search-jisyo-file skk-jisyo 0 t)))
;;	     (skk-search-server skk-aux-large-jisyo 10000)
;;           (skk-okuri-search)
;;	     (skk-correct-search
;;	      '((skk-search-server skk-aux-large-jisyo 10000)
;;	        (skk-okuri-search)))
;;	     ))
;;
;; $B8=:_$N$H$3$m!"(Bskk-correct-table $B$N3FMWAG$r:G=i$+$i=g$K<h$j=P$7!"%V%l$?8+(B
;; $B=P$78l$,$J$$$+$I$&$+$rD4$Y$F!"%V%l$,8+$D$+$C$?$i$=$l0J>e(B skk-correct-table
;; $B$N8eH>ItJ,$O8+$J$$;EMM$K$J$C$F$$$^$9!#(B
;; $B$H$j$"$($:(B Naoki Wakamatsu <m5032106@u-aizu.ac.jp> $B$5$s$,(B Message-Id:
;;  <200001260732.QAA00868@ring.etl.go.jp> $B$GNs5s$7$F$/$@$5$C$?FI$_$N%V%l$r(B
;; $BC1=c$K(B sort $B$7$F!"(B2 $BJ8;z$N$b$N(B, 1 $BJ8;z$N$b$N$H$$$&=g$G(B skk-correct-table
;; $B$K<}$a$F$$$^$9$,!">e5-$N$h$&$K2?$+0l$D%V%l$,8+$D$+$C$?$i$=$l0J>e$O%F!<%V%k(B
;; $B$r8+$F$$$J$$$N$G!"$3$N%F!<%V%k$N%V%l$NM%@h=g0L$O8!F$$7$?J}$,NI$$$G$7$g$&!#(B
;;
;; $B:#8e$N2~NI$N%"%$%G%#%"$N0l$D$H$7$F$O!"JQ49>r7o$K1~$8$F8+$k%V%l$H8+$J$$%V%l(B
;; $B$r:n$k$N$bNI$$$+$b$7$l$^$;$s!#Nc$($P!"$3$N%V%l$OAw$j$"$jJQ49$N$H$-$@$1$7$+(B
;; $B8!:w$7$J$$!"$J$I$H$$$&$b$N$G$9!#$3$NJU$j$O<B:]$K;H$C$F$_$F$40U8+$r$*J9$+$;(B
;; $B2<$5$$!#(B
;;
;; $B$J$*!"JQ49;~$KEv=i<j$GF~NO$7$?%V%l$?8+=P$78l$O!"$=$N$^$^8D?M<-=q$K<h$j9~$^(B
;; $B$l$^$9$,!"$3$l$O;EMM$G$9!#2?8N$J$i!"3NDj;~$K$O!"3NDj$5$l$?8uJd$,!"8+=P$78l(B
;; $B$N%V%l$rJd@5$7$F8+$D$1$?8uJd$+$I$&$+$r3NG'$9$k=Q$,8=:_$N$H$3$mDs6!$5$l$F$$(B
;; $B$J$$$+$i$G$9!#(B

;;; Code:

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars))

(defgroup skk-correct nil "SKK correct related customization."
  :prefix "skk-correct-"
  :group 'skk)

(defvar skk-correct-table
  '(
    ;; 2 chars
    ("$B$*!<(B" . "$B$*$&(B")			; $BBg$-$$(B
    ("$B$*!<(B" . "$B$*$*(B")
    ("$B$*$&(B" . "$B$*$*(B")
    ("$B$H$&(B" . "$B$H$*(B")			; $BDL$k(B
    ("$B$I$&(B" . "$B$I$*(B")
    ("$B$d$&(B" . "$B$h$&(B")			; $B$d$&$d(Bk /$BA2(B/ -> $B$h$&$d(Bk /$BA2(B/
    ("$B$i$&(B" . "$B$m$&(B")			; $B$i$&(Bs /$BO+(B/ -> $B$m$&(Bs /$BO+(B/
    ;; 1 char
    ("$B$$(B" . "$B$f(B")			; $B$$$-$I(Bm /$B9T$-;_(B/ -> $B$f$-$I(Bm /$B9T$-;_(B/
    ("$B$$(B" . "$B$h(B")			; $B$$(Bi /$BNI(B/ -> $B$h(Bi /$BNI(B/
    ("$B$*(B" . "$B$&(B")			; $B$[$*(Bt /$BJ|(B/ -> $B$[$&(Bt /$BJ|(B/
    ("$B$*(B" . "$B$[(B")			; $B$h$=$*(Bu /$Bjf(B/ -> $B$h$=$[(Bu /$Bjf(B/
    ("$B$*(B" . "$B$r(B")			; $B$/$A$*(Bs /$B8}@K(B/ -> $B$/$A$r(Bs /$B8}@K(B/
    ("$B$+(B" . "$B$,(B")			; $B$^$L$+(Br /$BLH(B/ -> $B$^$L$,(Br /$BLH(B/
    ("$B$6(B" . "$B$5(B")			; $B$O$@$6$`(Bk /$BH)4((B/ -> $B$O$@$5$`(Bi /$BH)4((B/
    ("$B$8(B" . "$B$B(B")			; $B$_$8$+(Bn /$B?H6a(B/ -> $B$_$B$+(Bn /$B?H6a(B/
    ("$B$=(B" . "$B$>(B")			; $B$_$.$=$m(Be /$B1&B7(B/ -> $B$_$.$>$m(Be /$B1&B7(B/
    ("$B$?(B" . "$B$@(B")			; $B$d$/$?(Bt /$BLrN)(B/ -> $B$d$/$@(Bt /$BLrN)(B/
    ("$B$E(B" . "$B$:(B")			; $B$o$E$i(Bw /$BHQ(B/ -> $B$o$:$i(Bw /$BHQ(B/ ; $B$`$E$+(Bs /$BFq(B/ -> $B$`$:$+(Bs /$BFq(B/
    ("$B$H(B" . "$B$I(B")			; $B$b$H(Br /$BLa(B/ -> $B$b$I(Br /$BLa(B/
    ("$B$O(B" . "$B$o(B")			; $B$a$6$O(Br /$BL\>c(B/ -> $B$a$6$o(Br /$BL\>c(B/
    ("$B$P(B" . "$B$O(B")			; $B$O$i(Bi /$BJ'(B/  -> $B$P$i(Bi /$BJ'(B/
    ("$B$Q(B" . "$B$O(B")			; $B$Q(Bt /$BD%(B/ -> $B$O(Bt /$BD%(B/
    ("$B$R(B" . "$B$$(B")			; $B$U$,$R$J(Bi /$Bg%9CHeL5(B/ -> $B$U$,$$$J(Bi /$Bg%9CHeL5(B/
    ("$B$R(B" . "$B$S(B")			; $B$j$g$&$R$i(Bk /$BN>3+(B/ -> $B$j$g$&$S$i(Bk /$BN>3+(B/
    ("$B$U(B" . "$B$V(B")			; $B$U$+(Bk /$B?<(B/ -> $B$V$+(Bk /$B?<(B/
    ("$B$V(B" . "$B$U(B")			; $B$1$V$+(Bi /$BLS?<(B/ -> $B$1$U$+(Bi /$BLS?<(B/
    ("$B$`(B" . "$B$s(B")			; $B$d$`$4$H$J(Bs /$B;_;vL5(B/ -> $B$d$s$4$H$J(Bs /$B;_;vL5(B/
    ("$B$f(B" . "$B$$(B")			; $B$f$-$I(Bm /$B9T$-;_(B/ -> $B$$$-$I(Bm /$B9T$-;_(B/
    ("$B$h(B" . "$B$$(B")			; $B$h(Bi /$BNI(B/ -> $B$$(Bi /$BNI(B/
    ("$B$p(B" . "$B$$(B")			; $B$p(Br /$B5o(B/ -> $B$$(Br /$B5o(B/
    )
  "*$B8+=P$78lJQ49$N$?$a$N%F!<%V%k!#(B
$B3FMWAG$N%G!<%?9=B$$O!"(B\(\"$BFI$_$,$V$l$?8+=P$78l(B\" . \"$B@5$7$$8+=P$78l(B\"\)$B!#(B")

;; internal variable
(defvar skk-correct-current-table nil)

;; functions.
;;;###autoload
(defun skk-correct-search (search-method-list)
  (let ((henkan-key skk-henkan-key)
	search-list skk-henkan-key v)
    (setq skk-correct-current-table skk-correct-table)
    (while (and (not v) (setq search-list search-method-list
			      skk-henkan-key (skk-correct henkan-key)))
      (while (and search-list (not (setq v (eval (car search-list)))))
	(setq search-list (cdr search-list))))
    v))

(defun skk-correct (string)
  ;; STRING $BCf$K%V%l$,$"$C$?$i$=$NItJ,$r@5$7$$J8;zNs$KCV$-49$($FJV$9!#(B
  (let (v)
    (save-match-data
      (while (and (not v) skk-correct-current-table)
	(if (string-match (car (car skk-correct-current-table)) string)
	    (setq v (concat (substring string 0 (match-beginning 0))
			    (cdr (car skk-correct-current-table))
			    (substring string (match-end 0)))))
	(setq skk-correct-current-table (cdr skk-correct-current-table)))
      v)))

(require 'product)
(product-provide (provide 'skk-correct) (require 'skk-version))
;;; Local Variables:
;;; End:
;;; skk-correct.el ends here
