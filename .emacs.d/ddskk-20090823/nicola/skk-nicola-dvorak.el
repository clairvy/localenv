;;; skk-nicola-dvorak.el --- SKK $B$K(B NICOLA (Dvorak) $BF~NO4D6-$rDs6!(B -*- coding: iso-2022-jp -*-

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

;; $B$3$N%U%!%$%k$O(B NICOLA Dvorak $BG[Ns$H$=$l$r<B8=$9$k$?$a$N%k!<%k$rDs6!$7$^$9!#(B

;;; Code:


;; NICOLA Dvorak $BG[Ns(B

(defvar skk-kanagaki-nicola-dvorak-base-rule-list
  '(("`" nil skk-nicola-insert)
    ;;
    ("1" nil skk-nicola-insert)  ("2" nil skk-nicola-insert)
    ("3" nil skk-nicola-insert)  ("4" nil skk-nicola-insert)
    ("5" nil skk-nicola-insert)
    ;;
    ("6" nil skk-nicola-insert) ("7" nil skk-nicola-insert)
    ("8" nil skk-nicola-insert) ("9" nil skk-nicola-insert)
    ("0" nil skk-nicola-insert) ("[" nil skk-nicola-insert)
    ("]" nil skk-nicola-insert) ("\\" nil skk-nicola-insert)
    ;;
    ("'" nil skk-nicola-insert) ("," nil skk-nicola-insert)
    ("." nil skk-nicola-insert) ("p" nil skk-nicola-insert)
    ("y" nil skk-nicola-insert)
    ;;
    ("f" nil skk-nicola-insert)  ("g" nil skk-nicola-insert)
    ("c" nil skk-nicola-insert)  ("r" nil skk-nicola-insert)
    ("l" nil skk-nicola-insert)  ("/" nil skk-nicola-insert)
    ("=" nil skk-nicola-insert)
    ;;
    ("a" nil skk-nicola-insert) ("o" nil skk-nicola-insert)
    ("e" nil skk-nicola-insert) ("u" nil skk-nicola-insert)
    ("i" nil skk-nicola-insert)
    ;;
    ("d" nil skk-nicola-insert)  ("h" nil skk-nicola-insert)
    ("t" nil skk-nicola-insert)  ("l" nil skk-nicola-insert)
    ("n" nil skk-nicola-insert)  ("s" nil skk-nicola-insert)
    ("-" nil skk-nicola-insert)
    ;;
    (";" nil skk-nicola-insert) ("q" nil skk-nicola-insert)
    ("j" nil skk-nicola-insert) ("k" nil skk-nicola-insert)
    ("x" nil skk-nicola-insert)
    ;;
    ("b" nil skk-nicola-insert)  ("m" nil skk-nicola-insert)
    ("w" nil skk-nicola-insert)  ("v" nil skk-nicola-insert)
    ("z" nil skk-nicola-insert)
    ;;
    ("@" nil skk-today)
    ("$" nil skk-display-code-for-char-at-point)
    ("\"" nil skk-set-henkan-point-subr)
    ("A" nil skk-latin-mode)
    (":" nil skk-jisx0208-latin-mode)
    ("Q" nil skk-purge-from-jisyo)
    ("J" nil skk-input-by-code-or-menu)) "\
ANSI Dvorak $BG[Ns%-!<%\!<%I$G(B NICOLA $BF~NO$9$k$?$a$N4pK\%k!<%k!#(B")

(defconst skk-nicola-dvorak-keymap-display 'dummy "\
$B0J2<$O!"(BANSI Dvorak $BG[Ns%-!<%\!<%I$G(B NICOLA $BF~NO$9$k$?$a$N%-!<G[Ns?^$G$9!#(B

$B(#(!(!($(B
$B("%u!1("(B
$B("(B` $B!.("(B
$B('(!(!(+(!(!(((!(!(((!(!(((!(!($(B $B(#(!(!(((!(!(((!(!(((!(!(((!(!(((!(!(((!(!(((!(!($(B
$B("!)!*("!?!w("!A!t("!V!p("!W!s("(B $B("!G!0("!\!u("!H!v("!I!J("!X!K("!Y!P("!c!Q("!d!C("(B
$B("(B1 $B!!("(B2 $B!!("(B3 $B!!("(B4 $B!!("(B5 $B!!("(B $B("(B6 $B!!("(B7 $B!!("(B8 $B!!("(B9 $B!!("(B0 $B!!("(B[ $B!N("(B] $B!O("(B\\ $B!o("(B
$B('(!(!(+(!(!(+(!(!(+(!(!(+(!(!()(B $B('(!(!(+(!(!(+(!(!(+(!(!(+(!(!(+(!(!(+(!(!(+(!(!(%(B
$B("$p$!("$,$(("$@$j("$4$c("$6$l("(B $B("$Q$h("$B$K("$0$k("$E$^("$T$'("!'!7("!a!,("(B
$B("(B' $B!#("(B, $B$+("(B. $B$?("(BP $B$3("(BY $B$5("(B $B("(BF $B$i("(BG $B$A("(BC $B$/("(BR $B$D("(BL $B!$("(B/ $B!"("(B= $B!+("(B
$B('(!(!(+(!(!(+(!(!(+(!(!(+(!(!()(B $B('(!(!(+(!(!(+(!(!(+(!(!(+(!(!(+(!(!(+(!(!(%(B
$B("%t$r("$8$"("$G$J("$2$e("$<$b("(B $B("$P$_("$I$*("$.$N("$]$g("!($C("!2!D("(B
$B("(BA $B$&("(BO $B$7("(BE $B$F("(BU $B$1("(BI $B$;("(B $B("(BD $B$O("(BH $B$H("(BT $B$-("(BN $B$$("(BS $B$s("(B- $B!]("(B
$B('(!(!(+(!(!(+(!(!(+(!(!(+(!(!()(B $B('(!(!(+(!(!(+(!(!(+(!(!(+(!(!(+(!(!(%(B
$B("$q$%("$S!<("$:$m("$V$d("$Y$#("(B $B("$W$L("$>$f("$Z$`("$\$o("$n$)("(B
$B("(B; $B!%("(BQ $B$R("(BJ $B$9("(BK $B$U("(BX $B$X("(B $B("(BB $B$a("(BM $B$=("(BW $B$M("(BV $B$[("(BZ $B!&("(B
$B(&(!(!(*(!(!(*(!(!(*(!(!(*(!(!(%(B $B(&(!(!(*(!(!(*(!(!(*(!(!(*(!(!(%(B

$B3FOH$NJ8;z$O0J2<$N$h$&$K=q$+$l$F$$$^$9!#(B

 $B:82<(B $B!D(B ASCII $BJ8;z(B
 $B1&2<(B $B!D(B $B?F;X%7%U%H$7$J$$$GF~NO$5$l$k$Y$-J8;z(B ($BC1FHBG80(B)
 $B1&>e(B $B!D(B $BF1B&?F;X%7%U%H$K$h$jF~NO$5$l$k$Y$-J8;z(B (straight shift)
 $B:8>e(B $B!D(B $BH?BPB&?F;X%7%U%H$K$h$jF~NO$5$l$k$Y$-J8;z(B (cross shift)

$B$3$l$K4p$$$F0J2<$N(B 3 $B$D$N%k!<%k$,7hDj$5$l$^$9!#(B

 `skk-nicola-dvorak-plain-rule-list'
 `skk-nicola-dvorak-lshift-rule-list'
 `skk-nicola-dvorak-rshift-rule-list'

")

(defvar skk-nicola-dvorak-plain-rule-list
  '((?` "$B!.(B")
    ;;
    (?1 "1") (?2 "2") (?3 "3") (?4 "4") (?5 "5")
    ;;
    (?6 "6") (?7 "7") (?8 "8") (?9 "9") (?0 "0") (?\[ "$B!N(B") (?\] "$B!O(B")
    (?\\ "$B!o(B")
    ;;
    (?' "$B!#(B") (?, ("$B%+(B" . "$B$+(B")) (?. ("$B%?(B" . "$B$?(B")) (?p ("$B%3(B" . "$B$3(B"))
    (?y ("$B%5(B" . "$B$5(B"))
    ;;
    (?f ("$B%i(B" . "$B$i(B")) (?g ("$B%A(B" . "$B$A(B")) (?c ("$B%/(B" . "$B$/(B")) (?r ("$B%D(B" . "$B$D(B"))
    (?l "$B!$(B") (?/ "$B!"(B") (?= skk-kanagaki-dakuten)
    ;;
    (?a ("$B%&(B" . "$B$&(B")) (?o ("$B%7(B" . "$B$7(B")) (?e ("$B%F(B" . "$B$F(B")) (?u ("$B%1(B" . "$B$1(B"))
    (?i ("$B%;(B" . "$B$;(B"))
    ;;
    (?d ("$B%O(B" . "$B$O(B")) (?h ("$B%H(B" . "$B$H(B")) (?t ("$B%-(B" . "$B$-(B")) (?n ("$B%$(B" . "$B$$(B"))
    (?s ("$B%s(B" . "$B$s(B")) (?- "$B!](B")
    ;;
    (?\; "$B!%(B") (?q ("$B%R(B" . "$B$R(B")) (?j ("$B%9(B" . "$B$9(B")) (?k ("$B%U(B" . "$B$U(B"))
    (?x ("$B%X(B" . "$B$X(B"))
    ;;
    (?b ("$B%a(B" . "$B$a(B")) (?m ("$B%=(B" . "$B$=(B")) (?w ("$B%M(B" . "$B$M(B")) (?v ("$B%[(B" . "$B$[(B"))
    (?z "$B!&(B")
    ;;
    (?\  " ")) "\
$BC1FHBG80;~$NF~NO%k!<%k!#(B")

(defvar skk-nicola-dvorak-lshift-rule-list
  '((?` "$B!1(B")
    ;;
    (?1 "$B!*(B") (?2 "$B!w(B") (?3 "$B!t(B") (?4 "$B!p(B") (?5 "$B!s(B")
    ;;
    (?6 "$B!G(B") (?7 "$B!\(B") (?8 "$B!H(B") (?9 "$B!I(B") (?0 "$B!X(B") (?\[ "$B!Y(B") (?\] "$B!c(B")
    (?\\ "$B!d(B")
    ;;
    (?' ("$B%!(B" . "$B$!(B")) (?, ("$B%((B" . "$B$((B")) (?. ("$B%j(B" . "$B$j(B")) (?p ("$B%c(B" . "$B$c(B"))
    (?y ("$B%l(B" . "$B$l(B"))
    ;;
    (?f ("$B%Q(B" . "$B$Q(B")) (?g ("$B%B(B" . "$B$B(B")) (?c ("$B%0(B" . "$B$0(B")) (?r ("$B%E(B" . "$B$E(B"))
    (?l ("$B%T(B" . "$B$T(B")) (?/ "$B!'(B") (?= "$B!a(B")
    ;;
    (?a ("$B%r(B" . "$B$r(B")) (?o ("$B%"(B" . "$B$"(B")) (?e ("$B%J(B" . "$B$J(B")) (?u ("$B%e(B" . "$B$e(B"))
    (?i ("$B%b(B" . "$B$b(B"))
    ;;
    (?d ("$B%P(B" . "$B$P(B")) (?h ("$B%I(B" . "$B$I(B")) (?t ("$B%.(B" . "$B$.(B")) (?n ("$B%](B" . "$B$](B"))
    (?s "$B!((B") (?- "$B!2(B")
    ;;
    (?\; ("$B%%(B" . "$B$%(B")) (?q "$B!<(B") (?j ("$B%m(B" . "$B$m(B")) (?k ("$B%d(B" . "$B$d(B"))
    (?x ("$B%#(B" . "$B$#(B"))
    ;;
    (?b ("$B%W(B" . "$B$W(B")) (?m ("$B%>(B" . "$B$>(B")) (?w ("$B%Z(B" . "$B$Z(B")) (?v ("$B%\(B" . "$B$\(B"))
    (?z ("$B%n(B" . "$B$n(B"))
    ;;
    (?\  " ")) "\
$B:8?F;X%-!<$,2!$5$l$?$H$-$NF~NO%k!<%k!#(B")

(defvar skk-nicola-dvorak-rshift-rule-list
  '((?` "$B%u(B")
    ;;
    (?1 "$B!)(B") (?2 "$B!?(B") (?3 "$B!A(B") (?4 "$B!V(B") (?5 "$B!W(B")
    ;;
    (?6 "$B!0(B") (?7 "$B!u(B") (?8 "$B!v(B") (?9 "$B!J(B") (?0 "$B!K(B") (?\[ "$B!P(B") (?\] "$B!Q(B")
    (?\\ "$B!C(B")
    ;;
    (?' ("$B%p(B" . "$B$p(B")) (?, ("$B%,(B" . "$B$,(B")) (?. ("$B%@(B" . "$B$@(B")) (?p ("$B%4(B" . "$B$4(B"))
    (?y ("$B%6(B" . "$B$6(B"))
    ;;
    (?f ("$B%h(B" . "$B$h(B")) (?g ("$B%K(B" . "$B$K(B")) (?c ("$B%k(B" . "$B$k(B")) (?r ("$B%^(B" . "$B$^(B"))
    (?l ("$B%'(B" . "$B$'(B")) (?/ "$B!7(B") (?= skk-kanagaki-handakuten)
    ;;
    (?a "$B%t(B") (?o ("$B%8(B" . "$B$8(B")) (?e ("$B%G(B" . "$B$G(B")) (?u ("$B%2(B" . "$B$2(B"))
    (?i ("$B%<(B" . "$B$<(B"))
    ;;
    (?d ("$B%_(B" . "$B$_(B")) (?h ("$B%*(B" . "$B$*(B")) (?t ("$B%N(B" . "$B$N(B")) (?n ("$B%g(B" . "$B$g(B"))
    (?s ("$B%C(B" . "$B$C(B")) (?- "$B!D(B")
    ;;
    (?\; ("$B%q(B" . "$B$q(B")) (?q ("$B%S(B" . "$B$S(B")) (?j ("$B%:(B" . "$B$:(B"))
    (?k ("$B%V(B" . "$B$V(B")) (?x ("$B%Y(B" . "$B$Y(B"))
    ;;
    (?b ("$B%L(B" . "$B$L(B")) (?m ("$B%f(B" . "$B$f(B")) (?w ("$B%`(B" . "$B$`(B")) (?v ("$B%o(B" . "$B$o(B"))
    (?z ("$B%)(B" . "$B$)(B"))
    ;;
    (?\  " ")) "\
$B1&?F;X%-!<$,2!$5$l$?$H$-$NF~NO%k!<%k!#(B")

(require 'skk-nicola)

(require 'product)
(product-provide
    (provide 'skk-nicola-dvorak)
  (require 'skk-version))

;;; skk-nicola-dvorak.el ends here
