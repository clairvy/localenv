;;; skk-oasys.el --- SKK $B$K(B OASYS $BIwF~NO4D6-$rDs6!(B -*- coding: iso-2022-jp -*-

;; Copyright (C) 1996, 1997, 1998, 1999, 2000
;;   Itsushi Minoura <minoura@eva.hi-ho.ne.jp>

;; Author: Itsushi Minoura <minoura@eva.hi-ho.ne.jp>
;;      Tetsuo Tsukamoto <czkmt@remus.dti.ne.jp>
;; Keywords: hardware, japanese, mule, input method

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

;; $B$3$N%U%!%$%k$O(B $BIY;NDL$N%o!<%I%W%m%;%C%5(B OASYS $BIw$N%-!<G[Ns$H$=$l$r<B8=$9$k(B
;; $B$?$a$N%k!<%k$rDs6!$7$^$9!#(B

;;; Code:

(eval-when-compile
  (require 'cl)
  (require 'skk-kanagaki-util))

(require 'skk-kanagaki)


;; OASYS $BIwG[Ns(B

(defvar skk-kanagaki-oasys-base-rule-list
  '(("1" nil skk-nicola-insert)  ("2" nil skk-nicola-insert)
    ("3" nil skk-nicola-insert)  ("4" nil skk-nicola-insert)
    ("5" nil skk-nicola-insert)
    ;;
    ("6" nil skk-nicola-insert) ("7" nil skk-nicola-insert)
    ("8" nil skk-nicola-insert) ("9" nil skk-nicola-insert)
    ("0" nil skk-nicola-insert) ("-" nil skk-nicola-insert)
    ("^" nil skk-nicola-insert) ("\\" nil skk-nicola-insert)
    ;;
    ("q" nil skk-nicola-insert) ("w" nil skk-nicola-insert)
    ("e" nil skk-nicola-insert) ("r" nil skk-nicola-insert)
    ("t" nil skk-nicola-insert)
    ;;
    ("y" nil skk-nicola-insert)  ("u" nil skk-nicola-insert)
    ("i" nil skk-nicola-insert)  ("o" nil skk-nicola-insert)
    ("p" nil skk-nicola-insert)  ("@" nil skk-nicola-insert)
    ;;
    ("a" nil skk-nicola-insert) ("s" nil skk-nicola-insert)
    ("d" nil skk-nicola-insert) ("f" nil skk-nicola-insert)
    ("g" nil skk-nicola-insert)
    ;;
    ("h" nil skk-nicola-insert)  ("j" nil skk-nicola-insert)
    ("k" nil skk-nicola-insert)  ("l" nil skk-nicola-insert)
    (";" nil skk-nicola-insert)
    ;;
    ("z" nil skk-nicola-insert) ("x" nil skk-nicola-insert)
    ("c" nil skk-nicola-insert) ("v" nil skk-nicola-insert)
    ("b" nil skk-nicola-insert)
    ;;
    ("n" nil skk-nicola-insert)  ("m" nil skk-nicola-insert)
    ("," nil skk-nicola-insert)  ("." nil skk-nicola-insert)
    ("/" nil skk-nicola-insert)
    ;;
    ("!" nil "$B!*(B") ("\"" nil "$B!I(B") ("#" nil "$B!t(B") ("$" nil "$B!p(B") ("%" nil "$B!s(B")
    ;;
    ("&" nil "$B!u(B") ("'" nil "$B!G(B") ("(" nil "$B!J(B") (")" nil "$B!K(B") ("~" nil "$B!1(B")
    ("=" nil "$B!a(B")
    ;; ("~" nil "$B!v(B")
    ("|" nil "$B!C(B")
    ;;
    ("Q" nil "$B!#(B") ("W" nil ("$B%+(B" . "$B$q(B")) ("E" nil ("$B%?(B" . "$B%u(B"))
    ("R" nil ("$B%3(B" . "$B$3(B")) ("T" nil ("$B%5(B" . "$B$5(B"))
    ;;
    ("Y" nil ("$B%i(B" . "$B$i(B")) ("U" nil ("$B%A(B" . "$B$A(B")) ("I" nil ("$B%/(B" . "$B$/(B"))
    ("O" nil ("$B%D(B" . "$B$D(B")) ("P" nil "$B!$(B") ("`" nil "$B!"(B")
    ;;
    ("A" nil ("$B%&(B" . "$B$&(B")) ("S" nil ("$B%7(B" . "$B$7(B")) ("D" nil ("$B%F(B" . "$B$F(B"))
    ("F" nil ("$B%1(B" . "$B%v(B")) ("G" nil ("$B%;(B" . "$B$;(B"))
    ;;
    ("H" nil ("$B%Q(B" . "$B$Q(B")) ("J" nil ("$B%H(B" . "$B$H(B")) ("K" nil ("$B%-(B" . "$B$-(B"))
    ("L" nil ("$B%$(B" . "$B$p(B")) ("+" nil ("$B%s(B" . "$B$s(B"))
    ;;
    ("Z" nil "$B!%(B") ("X" nil ("$B%T(B" . "$B$T(B")) ("C" nil ("$B%9(B" . "$B$9(B"))
    ("V" nil ("$B%W(B" . "$B$W(B")) ("B" nil ("$B%Z(B" . "$B$Z(B"))
    ;;
    ("N" nil ("$B%a(B" . "$B$a(B")) ("M" nil ("$B%=(B" . "$B$=(B")) ("<" nil ("$B%M(B" . "$B$M(B"))
    (">" nil ("$B%](B" . "$B$](B")) ("?" nil "$B!&(B"))
  "$BF|K\8l(B 106 $B%-!<%\!<%I$G(B OASYS $BIwF~NO$r$9$k$?$a$N4pK\%k!<%k!#(B")

(defconst skk-oasys-keymap-display 'dummy
  "$B0J2<$O!"F|K\8l(B 106 $B%-!<%\!<%I$G(B OASYS $BIwF~NO$r$9$k$?$a$N%-!<G[Ns?^$G$9!#(B

$B(#(!(!(((!(!(((!(!(((!(!(((!(!($(B $B(#(!(!(((!(!(((!(!(((!(!(((!(!(((!(!(((!(!($(B
$B("!!!)("!!!?("!!!A("!!!V("!!!W("(B $B("!!!N("!!!O("!!!J("!!!K("!!!X("!!!Y("!-!!("(B
$B("(B1 $B#1("(B2 $B#2("(B3 $B#3("(B4 $B#4("(B5 $B#5("(B $B("(B6 $B#6("(B7 $B#7("(B8 $B#8("(B9 $B#9("(B0 $B#0("(B- $B!]("(B^ $B!'("(B
$B('(!(!(+(!(!(+(!(!(+(!(!(+(!(!()(B $B('(!(!(+(!(!(+(!(!(+(!(!(+(!(!(+(!(!(+(!(!(%(B
$B("!,$!("$,$(("$@$j("$4$c("$6$l("(B $B("$Q$h("$B$K("$0$k("$E$^("$T$'("!!!!("(B
$B("(BQ $B!#("(BW $B$+("(BE $B$?("(BR $B$3("(BT $B$5("(B $B("(BY $B$i("(BU $B$A("(BI $B$/("(BO $B$D("(BP $B!$("(B@ $B!"("(B
$B('(!(!(+(!(!(+(!(!(+(!(!(+(!(!()(B $B('(!(!(+(!(!(+(!(!(+(!(!(+(!(!(+(!(!(+(!(!($(B
$B("%t$r("$8$"("$G$J("$2$e("$<$b("(B $B("$P$_("$I$*("$.$N("$]$g("$C$C("8e!!("<h!!("(B
$B("(BA $B$&("(BS $B$7("(BD $B$F("(BF $B$1("(BG $B$;("(B $B("(BH $B$O("(BJ $B$H("(BK $B$-("(BL $B$$("(B; $B$s("(B: $BB`("(B] $B>C("(B
$B('(!(!(+(!(!(+(!(!(+(!(!(+(!(!()(B $B('(!(!(+(!(!(+(!(!(+(!(!(+(!(!(+(!(!(+(!(!(%(B
$B("!%$%("$S!<("$:$m("$V$d("$Y$#("(B $B("$W$L("$>$f("$Z$`("$\$o("!+$)("!@!!("(B
$B("(BZ $B!%("(BX $B$R("(BC $B$9("(BV $B$U("(BB $B$X("(B $B("(BN $B$a("(BM $B$=("(B, $B$M("(B. $B$[("(B/ $B!&("(B\\ $B!o("(B
$B(&(!(!(*(!(!(*(!(!(*(!(!(*(!(!(%(B $B(&(!(!(*(!(!(*(!(!(*(!(!(*(!(!(*(!(!(%(B

$B3FOH$NJ8;z$O0J2<$N$h$&$K=q$+$l$F$$$^$9!#(B

 $B:82<(B $B!D(B ASCII $BJ8;z(B
 $B1&2<(B $B!D(B $B?F;X%7%U%H$7$J$$$GF~NO$5$l$k$Y$-J8;z(B ($BC1FHBG80(B)
 $B1&>e(B $B!D(B $BF1B&?F;X%7%U%H$K$h$jF~NO$5$l$k$Y$-J8;z(B (straight shift)
 $B:8>e(B $B!D(B $BH?BPB&?F;X%7%U%H$K$h$jF~NO$5$l$k$Y$-J8;z(B (cross shift)

\($BC"$72<$+$i(B 2 $BNs$a$N1&$N(B 2 $B$D$N%-!<$O$=$l$>$l8eB`%-!<!"<h>C%-!<$G$"$k$3$H$r(B
$B0UL#$7$^$9!#(B)

$B$3$l$K4p$$$F0J2<$N(B 3 $B$D$N%k!<%k$,7hDj$5$l$^$9!#(B

 `skk-oasys-plain-rule-list'
 `skk-oasys-lshift-rule-list'
 `skk-oasys-rshift-rule-list'

")

(defvar skk-oasys-plain-rule-list
  '((?1 "$B#1(B") (?2 "$B#2(B") (?3 "$B#3(B") (?4 "$B#4(B") (?5 "$B#5(B")
    ;;
    (?6 "$B#6(B") (?7 "$B#7(B") (?8 "$B#8(B") (?9 "$B#9(B") (?0 "$B#0(B") (?- "$B!](B")
    (?^ ("$B!v(B" ."$B!'(B")) (?\\ "$B!o(B")
    ;;
    (?q "$B!#(B") (?w ("$B%q(B" . "$B$+(B")) (?e ("$B%u(B" . "$B$?(B")) (?r ("$B%3(B" . "$B$3(B"))
    (?t ("$B%5(B" . "$B$5(B"))
    ;;
    (?y ("$B%i(B" . "$B$i(B")) (?u ("$B%A(B" . "$B$A(B")) (?i ("$B%/(B" . "$B$/(B")) (?o ("$B%D(B" . "$B$D(B"))
    (?p "$B!$(B") (?@ "$B!"(B")
    ;;
    (?a ("$B%&(B" . "$B$&(B")) (?s ("$B%7(B" . "$B$7(B")) (?d ("$B%F(B" . "$B$F(B")) (?f ("$B%v(B" . "$B$1(B"))
    (?g ("$B%;(B" . "$B$;(B"))
    ;;
    (?h ("$B%O(B" . "$B$O(B")) (?j ("$B%H(B" . "$B$H(B")) (?k ("$B%-(B" . "$B$-(B")) (?l ("$B%p(B" . "$B$$(B"))
    (?\; ("$B%s(B" . "$B$s(B"))
    ;;
    (?z "$B!%(B") (?x ("$B%R(B" . "$B$R(B")) (?c ("$B%9(B" . "$B$9(B")) (?v ("$B%U(B" . "$B$U(B"))
    (?b ("$B%X(B" . "$B$X(B"))
    ;;
    (?n ("$B%a(B" . "$B$a(B")) (?m ("$B%=(B" . "$B$=(B")) (?, ("$B%M(B" . "$B$M(B")) (?. ("$B%[(B" . "$B$[(B"))
    (?/ "$B!&(B"))
  "$BC1FHBG80;~$NF~NO%k!<%k!#(B")

(defvar skk-oasys-rshift-rule-list
  '((?1 "$B!)(B") (?2 "$B!?(B") (?3 "$B!A(B") (?4 "$B!V(B") (?5 "$B!W(B")
    ;;
    (?6 "$B!N(B") (?7 "$B!O(B") (?8 "$B!J(B") (?9 "$B!K(B") (?0 "$B!X(B") (?- "$B!Y(B") (?^ "$B!!(B")
    (?\\ "$B!!(B")
    ;;
    (?q ("$B%!(B" . skk-kanagaki-handakuten))
    (?w ("$B%,(B" . "$B$,(B")) (?e ("$B%@(B" . "$B$@(B")) (?r ("$B%4(B" . "$B$4(B"))
    (?t ("$B%6(B" . "$B$6(B"))
    ;;
    (?y ("$B%h(B" . "$B$h(B")) (?u ("$B%K(B" . "$B$K(B")) (?i ("$B%k(B" . "$B$k(B")) (?o ("$B%^(B" . "$B$^(B"))
    (?p ("$B%'(B" . "$B$'(B")) (?@ ("$B!"(B" . "$B!!(B"))
    ;;
    (?a "$B%t(B") (?s ("$B%8(B" . "$B$8(B")) (?d ("$B%G(B" . "$B$G(B")) (?f ("$B%2(B" . "$B$2(B"))
    (?g ("$B%<(B" . "$B$<(B"))
    ;;
    (?h ("$B%_(B" . "$B$_(B")) (?j ("$B%*(B" . "$B$*(B")) (?k ("$B%N(B" . "$B$N(B")) (?l ("$B%g(B" . "$B$g(B"))
    (?\; ("$B%C(B" . "$B$C(B"))
    ;;
    (?z ("$B%%(B" . "$B!%(B")) (?x ("$B%S(B" . "$B$S(B")) (?c ("$B%:(B" . "$B$:(B")) (?v ("$B%V(B" . "$B$V(B"))
    (?b ("$B%Y(B" . "$B$Y(B"))
    ;;
    (?n ("$B%L(B" . "$B$L(B")) (?m ("$B%f(B" . "$B$f(B")) (?, ("$B%`(B" . "$B$`(B")) (?. ("$B%o(B" . "$B$o(B"))
    (?/ ("$B!&(B" . "$B$)(B"))
    ;;
    (?\ " "))
  "$B1&?F;X%-!<$,2!$5$l$?$H$-$NF~NO%k!<%k!#(B")

(defvar skk-oasys-lshift-rule-list
  '((?1 "$B!)(B") (?2 "$B!?(B") (?3 "$B!A(B") (?4 "$B!V(B") (?5 "$B!W(B")
    ;;
    (?6 "$B!N(B") (?7 "$B!O(B") (?8 "$B!J(B") (?9 "$B!K(B") (?0 "$B!X(B") (?- "$B!Y(B")
    (?^ ("$B!!(B" . "$B!-(B")) (?\\ ("$B!!(B" . "$B!@(B"))
    ;;
    (?q ("$B%!(B" . "$B$!(B")) (?w ("$B%((B" . "$B$((B")) (?e ("$B%j(B" . "$B$j(B")) (?r ("$B%c(B" . "$B$c(B"))
    (?t ("$B%l(B" . "$B$l(B"))
    ;;
    (?y ("$B%Q(B" . "$B$Q(B")) (?u ("$B%B(B" . "$B$B(B")) (?i ("$B%0(B" . "$B$0(B")) (?o ("$B%D(B" . "$B$E(B"))
    (?p ("$B%T(B" . "$B$T(B")) (?@ "$B!!(B")
    ;;
    (?a ("$B%r(B" . "$B$r(B")) (?s ("$B%"(B" . "$B$"(B")) (?d ("$B%J(B" . "$B$J(B")) (?f ("$B%e(B" . "$B$e(B"))
    (?g ("$B%b(B" . "$B$b(B"))
    ;;
    (?h ("$B%P(B" . "$B$P(B")) (?j ("$B%I(B" . "$B$I(B")) (?k ("$B%.(B" . "$B$.(B")) (?l ("$B%](B" . "$B$](B"))
    (?\; ("$B%C(B" . "$B$C(B"))
    ;;
    (?z ("$B%%(B" . "$B$%(B")) (?x "$B!<(B") (?c ("$B%m(B" . "$B$m(B")) (?v ("$B%d(B" . "$B$d(B"))
    (?b ("$B%#(B" . "$B$#(B"))
    ;;
    (?n ("$B%W(B" . "$B$W(B")) (?m ("$B%>(B" . "$B$>(B")) (?, ("$B%Z(B" . "$B$Z(B")) (?. ("$B%\(B" . "$B$\(B"))
    (?/ ("$B!!(B" . skk-kanagaki-dakuten))
    ;;
    (?\ " "))
  "$B:8?F;X%-!<$,2!$5$l$?$H$-$NF~NO%k!<%k!#(B")

(require 'skk-nicola)

(when skk-nicola-use-koyubi-functions
  (define-key skk-j-mode-map ":" 'skk-kanagaki-bs)
  (define-key skk-j-mode-map "]" 'skk-kanagaki-esc))

(provide 'skk-oasys)

;;; skk-oasys.el ends here
