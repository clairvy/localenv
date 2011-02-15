;;; skk-rdbms.el --- SKK Relational Data Base Management System. -*- coding: iso-2022-jp -*-
;; Copyright (C) 1998, 2000, 2003 NAKAJIMA Mikio <minakaji@namazu.org>

;; Author: NAKAJIMA Mikio <minakaji@namazu.org>
;; Maintainer: NAKAJIMA Mikio <minakaji@namazu.org>
;; Version: $Id: skk-rdbms.el,v 1.13 2010/08/02 15:21:06 skk-cvs Exp $
;; Keywords: japanese, rdbms
;; Last Modified: $Date: 2010/08/02 15:21:06 $

;; This file is not part of Daredevil SKK yet.

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

;; Currently this program only supports PostgreSQL, but may work with
;; other rdbms (such as mSQL or mySQL) by slightly changes.

;; SQL $B$N%7%'%k$r(B Emacs $B$N%P%C%U%!$NCf$G5/F0$7$F!"(BSQL $B%7%'%k$K%3%^%s%I(B
;; $B$rAw$k!"$H$$$&ItJ,$O!"(BEmacs Calc $B$N(B Gnuplot $B%$%s%?!<%U%'%$%9ItJ,$G(B
;; $B$"$k(B calc-graph.el $B$r;29M$K$7$?!#(B

;;; Code:

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars))

;; User variables.
(defvar skk-rdbms-shell "psql"
  "*$B%G!<%?%Y!<%9K\BN$H@\B3$9$k$?$a$N%7%'%k%W%m%0%i%`!#(B")

(defvar skk-rdbms-shell-args "skk"
  "*skk-rdbms-shell $B$N0z?t!#(B
$B%G%#%U%)%k%H$G$O!"(Bskk $B%G!<%?%Y!<%9$K@\B3$9$k$?$a$N0z?t$r;XDj$7$F$$$k!#(B")

(defvar skk-rdbms-process-coding-system
  (cdr (assoc "euc" skk-coding-system-alist))
  "*skk-rdbms-shell $B$H$N%W%m%;%9DL?.$K;H$&%3!<%G%#%s%0%7%9%F%`!#(B")

(defvar skk-rdbms-shell-prompt-regexp "^skk=> "
  "*skk-rdbms-shell $B$N%W%m%s%W%H$r<($9@55,I=8=!#(B")

(defvar skk-rdbms-shell-version-check-function nil
  "*skk-rdbms-shell $B$N%P!<%8%g%s$r%A%'%C%/$9$k4X?t!#(B
skk-rdbms-shell $B$N%P!<%8%g%s$K$h$C$F0[$J$kF0$-$r$7$?$$>l9g$K;HMQ$9$k!#(B")

;; $B2?8N$+(B process-send-string $B$7$?%3%^%s%I$ND>8e$K5"$C$F$/$kJ8;zNs$N@h(B
;; $BF,$K(B ^M $B$,IU$/!#(B
;; SQL $BJ8$NESCf$K2~9T$rF~$l$k$H(B psql $B$NBh(B 2 $B%W%m%s%W%H$,=P$k!#$3$l$r@55,I=8=$G(B
;; $BI=8=$7$F8!:w$9$k$H%*!<%P!<%X%C%I$,Bg$-$/!"3NDj$7$F$+$i$N<-=q99?7$,CY$/$J$k!#(B
(defvar skk-rdbms-error-regexp "^*ERROR: +\\(.+\\)$"
  "*skk-rdbms-shell $B$,=P$9%(%i!<%a%C%;!<%8$N@55,I=8=!#(B
\(match-string 1\)$B$G%(%i!<$NK\BN$r@Z$j=P$;$k$h$&$K@55,I=8=$r=q$/!#(B")

(defvar skk-rdbms-update-fail-regexp "^*UPDATE 0$"
  "*$B8D?M<-=q%F!<%V%k$N(B UPDATE $B$K<:GT$7$?$H$-$K(B skk-rdbms-shell $B$,=P$9%a%C%;!<%8$N@55,I=8=!#(B")

(defvar skk-rdbms-shell-special-command-regexp "^\\\\[a-z?!]+$"
  "*skk-rdbms-shell $B$N%3%^%s%I$G(B SQL $B%3%^%s%I0J30$N%3%^%s%I$rI=$9@55,I=8=!#(B")

(defvar skk-rdbms-kill-command "\\q"
  "*skk-rdbms-shell $B=*N;$N$?$a$K;HMQ$9$k%3%^%s%I!#(B")

(defvar skk-rdbms-SQL-wildcard "%"
  "*LIKE $B1i;;;R$G%Q%?!<%s%^%C%A%s%0$K;H$o$l$k%o%$%k%I%+!<%I!#(B")

(defvar skk-rdbms-private-jisyo-table (concat (user-login-name) "_private_jisyo")
  "*$B8D?M<-=q$N%F!<%V%kL>!#(B

CREATE TABLE YOUR-LOGIN-NAME_private_jisyo \(
	okuriari int2 NOT NULL,
	yomi varchar\(50\) NOT NULL, -- longest entry of yomi in SKK-JISYO.L is
				   -- '$B$[$/$j$/$;$s$?$s$+$,$/$.$8$e$D$@$$$,$/$$$s$@$$$,$/(B'
	kanji text NOT NULL,
	okurigana varchar\(4\),
	date abstime NOT NULL
\);

$B%F!<%V%kL>$N(B YOUR-LOGIN-NAME $B$N8D=j$O!"(BEmacs $B$N(B user-login-name $B$,JV$9CM$r=q$/!#(B")

(defvar skk-rdbms-public-jisyo-table "large_jisyo"
  "*$B6&M-<-=q$N%F!<%V%kL>!#(B

CREATE TABLE large_jisyo \(
	okuriari int2 NOT NULL,
	yomi varchar\(50\) NOT NULL,
	kanji text NOT NULL
\);")

(defvar skk-rdbms-jis2-jisyo-table "jis2_jisyo"
  "*JIS $BBh(B 2 $B?e=`$NJ8;z$r=8$a$?<-=q%F!<%V%kL>!#(B

CREATE TABLE jis2_jisyo \(
	okuriari int2 NOT NULL,
	yomi varchar\(50\) NOT NULL,
	kanji text NOT NULL
\);")

(defvar skk-rdbms-kakutei-jisyo-table (concat (user-login-name) "_kakutei_jisyo")
  "*$B3NDj<-=q$N%F!<%V%kL>!#(B

CREATE TABLE YOUR-LOGIN-NAME_kakutei_jisyo (
	okuriari int2 NOT NULL,
	yomi varchar(50) NOT NULL,
	kanji text NOT NULL
);

$B%F!<%V%kL>$N(B YOUR-LOGIN-NAME $B$N8D=j$O!"(BEmacs $B$N(B user-login-name $B$,JV$9CM$r=q$/!#(B")

(defvar skk-rdbms-initial-jisyo-table (concat (user-login-name) "_initial_jisyo")
  "*initial $B%5!<%A<-=q$N%F!<%V%kL>!#(B

CREATE TABLE YOUR-LOGIN-NAME_kakutei_jisyo (
	okuriari int2 NOT NULL,
	yomi varchar(50) NOT NULL,
	kanji text NOT NULL
);

$B%F!<%V%kL>$N(B YOUR-LOGIN-NAME $B$N8D=j$O!"(BEmacs $B$N(B user-login-name $B$,JV$9CM$r=q$/!#(B")

(defvar skk-rdbms-kcode-table "kcode"
  "*$BJ8;z$N(B JIS $B%3!<%I5Z$S(B Unicode $B$r=8$a$?%F!<%V%kL>!#(B

CREATE TABLE kcode \(
	kanji varchar\(2\) NOT NULL PRIMARY KEY,
	JIScode char\(4\) NOT NULL UNIQUE,
        Unicode char\(4\) NOT NULL UNIQUE
\);")

(defvar skk-rdbms-stroke-table "stroke"
  "*$B2h?t%G!<%?$r=8$a$?%F!<%V%kL>!#(B

CREATE TABLE stroke \(
	stroke int2 NOT NULL,
	kanji varchar\(2\) NOT NULL PRIMARY KEY
\);")

(defvar skk-rdbms-busyu-base-table "busyu_base"
  "*$BIt<s$N%G!<%?$r=8$a$?%F!<%V%kL>!#(B

CREATE TABLE busyu_base \(
	busyuID int2 NOT NULL, -- radical \(Busyu\) number in the Nelson New
                               -- Japanese-English Character Dictionary.
                               -- why aren't they unique?
	busyuID2 int2, -- classic radical number
	kanji varchar\(2\), -- $BIt<s$N$_$NI85-$,ITG=$JJ8;z$b$"$k$N$G!"(B
                            -- NOT NULL $B>r7o$rIU$1$J$$!#(B
	yomi1 varchar\(16\) NOT NULL,
	yomi2 varchar\(16\),
	yomi3 varchar\(16\),
	yomi4 varchar\(16\)
\);")

(defvar skk-rdbms-busyu-data-table "busyu_data"
  "*$B3F4A;z$NIt<s$K4X$9$k%G!<%?$r=8$a$?%F!<%V%kL>!#(B

CREATE TABLE busyu_data \(
	busyuID int2 NOT NULL, -- radical \(Busyu\) number.
	busyuID2 int2, -- classic radical number
	kanji varchar\(2\) NOT NULL UNIQUE
\);")

(defvar skk-rdbms-hinsi-base-table "hinsi_base"
  "*$BIJ;l$K4X$9$k%G!<%?$r=8$a$?%F!<%V%kL>!#(B

CREATE TABLE hinsi_base \(
	hinsi varchar\(23\) NOT NULL, -- longest is `$B%59T(B($B$9$k(B)&$BL>;l2=@\Hx8l(B'
	hinsiID int2 NOT NULL,
	hinsiID2 int2
\);")

(defvar skk-rdbms-hinsi-data-table "hinsi_data"
  "*$B3F4A;z$NIJ;l$K4X$9$k%G!<%?$r=8$a$?%F!<%V%kL>!#(B

CREATE TABLE hinsi_data \(
	yomi varchar\(26\) NOT NULL, -- longest is `$B$P$C$/$W$m$Q$2!<$7$g$s$[$&(B'
	kanji varchar\(24\) NOT NULL, -- longest is `$B%*%Z%l!<%F%#%s%0%7%9%F%`(B'
	hinsiID int2 NOT NULL,
	hinsiID2 int2
\);")

(defvar skk-rdbms-private-jisyo-dump "~/.skk-jisyo.dump"
  "*skk-rdbms-private-jisyo-table $B$r%@%s%W$7$FJ]B8$9$k%U%!%$%k!#(B")

(defvar skk-rdbms-dump-error "~/.skk-dump.error"
  "*skk-rdbms-private-jisyo-dump $B$r%@%s%W$9$k:]$KH/@8$7$?%(%i!<%a%C%;!<(B
$B%8$,J]B8$5$l$k%U%!%$%k!#(B")

;; $B%f!<%6!<JQ?t$K$9$k$D$b$j$,!"%^%/%m$G$J$$$HF0$+$J$$$3$H$K5$$,IU$$(B
;; $B$?(B...$B!#(Bformat $B$N0z?t$K$J$C$F$$$kJQ?tC#$,%3%s%Q%$%k$N$H$-$O3'(B nil $B$@(B
;; $B$+$i$M!#(BPostgreSQL $B0J30$N(B RDBMS $B$r;H$&?M$K$O%^%/%m$r=q$-BX$($F$b$i(B
;; $B$&(B...$B9s$@$m$&$+!)(B
(defmacro skk-rdbms-SQL-insert-command (word)
  `(if (not skk-henkan-okurigana)
       (format
	"INSERT INTO %s (okuriari,yomi,kanji,date) VALUES (%s, '%s', '%s', '%s');"
	skk-rdbms-private-jisyo-table 0 skk-henkan-key ,word
	(current-time-string))
     (format
      "INSERT INTO %s (okuriari,yomi,kanji,okurigana,date) VALUES (%s, '%s', '%s', '%s', '%s');"
      skk-rdbms-private-jisyo-table 1 skk-henkan-key ,word skk-henkan-okurigana
      (current-time-string))))

(defmacro skk-rdbms-SQL-delete-command (word)
  `(concat
    (format
     "DELETE FROM %s WHERE kanji = '%s' AND yomi = '%s' AND okuriari = %s "
     skk-rdbms-private-jisyo-table ,word skk-henkan-key
     (if skk-okuri-char 1 0))
    (if (and skk-henkan-okuri-strictly skk-henkan-okurigana)
	(format "AND okurigana = '%s'" skk-henkan-okurigana))
    ";"))

(defmacro skk-rdbms-SQL-regexp-delete-command (word)
  `(concat
    ;; $BEvA3$@$1$I!"@55,I=8=$,;H$($J$$(B RDBMS $B$G$O;H$($J$$$M(B...$B!#$I$s(B
    ;; $B$I$s(B PostgreSQL $B4s$j$K$J$C$F$f$/(B...f(^_^;;;$B!#(B
    (format
     "DELETE FROM %s WHERE kanji ~ '%s' AND yomi = '%s' AND okuriari = %s "
     skk-rdbms-private-jisyo-table ,word skk-henkan-key
     (if skk-okuri-char 1 0))
    (if (and skk-henkan-okuri-strictly skk-henkan-okurigana)
	(format "AND okurigana = '%s'" skk-henkan-okurigana))
    ";"))

(defmacro skk-rdbms-SQL-update-command (word)
  `(concat
    ;; SET date = 'now'::abstime $B$C$F(B PostgreSQL $B$@$1$G$7$+DLMQ$7$J$$%3(B
    ;; $B%^%s%I!)(B  SQL92 ($B$H$$$&$N$+$J!)(B) $B$G$O$I$&=q$/$s$G$7$g$&!)(B
    (format
     "UPDATE %s SET date = 'now'::abstime WHERE kanji = '%s' AND yomi = '%s' AND okuriari = %s "
     skk-rdbms-private-jisyo-table ,word skk-henkan-key (if skk-okuri-char 1 0))
    (if (and skk-henkan-okuri-strictly skk-henkan-okurigana)
	(format "AND okurigana = '%s'" skk-henkan-okurigana))
    ";"))

(defmacro skk-rdbms-SQL-search-jisyo-command (table)
  `(format "SELECT kanji FROM %s WHERE yomi = '%s' AND okuriari = %s;"
	   ,table skk-henkan-key (if skk-okuri-char 1 0)))

(defmacro skk-rdbms-SQL-search-private-jisyo-command ()
  `(concat
    (format "SELECT kanji FROM %s WHERE yomi = '%s' AND okuriari = %s "
	    skk-rdbms-private-jisyo-table skk-henkan-key (if skk-okuri-char 1 0))
    (if (and skk-henkan-okurigana skk-henkan-okuri-strictly)
	(format "AND okurigana = '%s' " skk-henkan-okurigana))
    "ORDER BY date DESC;"))

(defmacro skk-rdbms-SQL-search-completion-word-command ()
  `(format
    "SELECT yomi FROM %s WHERE yomi LIKE '%s%s' AND okuriari = 0 GROUP BY yomi ORDER BY max(date) DESC;"
    skk-rdbms-private-jisyo-table skk-completion-word skk-rdbms-SQL-wildcard))

(defmacro skk-rdbms-SQL-search-busyu-command (key)
  `(format
    ;; SQL $BJ8$NESCf$K2~9T$rF~$l$k$H(B psql $B$NBh(B 2 $B%W%m%s%W%H$,=P$k!#$3$l$r@55,I=8=$G(B
    ;; $BI=8=$7$F8!:w$9$k$H%*!<%P!<%X%C%I$,Bg$-$/!"3NDj$7$F$+$i$N<-=q99?7$,CY$/$J$k!#(B
    ;; $B=>$$(B SQL $BJ8$OD9$/$F$bL5M}LpM}(B 1 $B9T$GI=8=$9$k$+!"7y$J$i:Y@Z$l$K@Z$C$F(B
    ;; concat $B$G$D$J$0!#(B
    "SELECT kanji FROM %s WHERE busyuID = (SELECT busyuID FROM %s WHERE yomi1 = '%s' OR yomi2 = '%s' OR yomi3 = '%s' OR yomi4 = '%s');"
    skk-rdbms-busyu-data-table skk-rdbms-busyu-base-table
    ,key ,key ,key ,key))

(defmacro skk-rdbms-SQL-search-stroke-command (stroke)
  `(format "SELECT kanji FROM %s WHERE stroke = %s;"
	   skk-rdbms-stroke-table ,stroke))

(defmacro skk-rdbms-search-sahen-command (dakuten)
  `(format
    "SELECT kanji FROM %s WHERE yomi = '%s' AND hinsiID = (SELECT hinsiID FROM %s WHERE hinsi = '%s');"
    skk-rdbms-hinsi-data-table skk-henkan-key skk-rdbms-hinsi-base-table
    (if dakuten "$B%69T(B($B$:$k(B)" "$B%59T(B($B$9$k(B)")))

(defvar skk-rdbms-cutoff-output-function
  (function
   (lambda ()
     (save-match-data
       (save-excursion
	 (let (candidates pos)
	   (if (not (re-search-backward "^(\\([0-9]+\\) rows*)$"
					skk-rdbms-last-process-point t))
	       (error "")
	     (setq candidates (match-string 1) pos (point))
	     (if (string= candidates "0")
		 nil
	       (forward-line (- (string-to-number candidates)))
	       (beginning-of-line) ; fail safe
	       (split-string (buffer-substring-no-properties pos (point)) " *\n"))))))))
  "*skk-rdbms-shell $B$K$h$C$F8!:w$7$?%"%&%H%W%C%H$r@Z$j=P$7$F2C9)$9$k4X?t!#(B
skk-rdbms-public-jisyo-table $B$d(B skk-rdbms-jis2-jisyo-table $B$J$I$KMxMQ$9$k!#(B
$B8uJd$NJ8;zNs$r%j%9%H$K$7$FJV$9!#(B
skk-rdbms-working-buffer $B$NCf$G%3!<%k$5$l$k!#(B")

(defvar skk-rdbms-cutoff-output-function-2
  (function
   (lambda ()
     (save-match-data
       (save-excursion
 	 (let (candidates var)
  	   (if (not (re-search-backward "^(\\([0-9]+\\) rows*)$"
					skk-rdbms-last-process-point t))
  	       (error "")
 	     (setq candidates (string-to-number (match-string 1)))
 	     (if (= candidates 0)
  		 nil
	       (beginning-of-line)	; fail safe
 	       ;; $B$3$&$d$C$F0l$D$:$D@Z$j=P$9$s$8$c$J$/$F!"@55,I=8=$+$J$s$+(B
 	       ;; $B$G%Q%C$H=hM}$G$-$^$;$s$+$M!#(B
 	       (while (> candidates 0)
		 (forward-line -1)
		 (setq var (cons (buffer-substring-no-properties
				  (point)
				  ;; yomi $B$OESCf$K6uGrJ8;z$r5v$5$J$$$N$G!"(B
				  ;; $BA0$+$i6uGrJ8;z0J30$r%9%-%C%W$7$F$b(B OK$B!#(B
				  (progn (skip-chars-forward "^ ") (point)))
				 var))
		 (beginning-of-line)
		 (setq candidates (1- candidates)))
	       var)))))))
  "*skk-rdbms-shell $B$K$h$C$F8!:w$7$?%"%&%H%W%C%H$r@Z$j=P$7$F2C9)$9$k4X?t!#(B
$B@Z$j=P$9BP>]$N=PNO$O!"J#?t%+%i%`$N:G=i$N%+%i%`$G!"@Z$j=P$9J8;zNs$OESCf$K6uGrJ8;z(B
$B$r5v$5$J$$!"$H$$$&>r7o2<$G;HMQ$9$k!#(B
$B8uJd$NJ8;zNs$r%j%9%H$K$7$FJV$9!#(Bskk-rdbms-working-buffer $B$NCf$G%3!<%k$5$l$k!#(B")

(defvar skk-rdbms-cutoff-output-function-3
  (function
   (lambda ()
     (save-match-data
       (save-excursion
	 (let (candidates)
	   (if (not (re-search-backward "^(\\([0-9]+\\) rows*)$"
					skk-rdbms-last-process-point t))
	       (error "")
	     (setq candidates (match-string 1))
	     (if (not (string= candidates "1"))
		 (skk-error "$B<-=q$N8uJd?t$r?t$($k$N$K<:GT$7$^$7$?(B"
			    "Failed counting jisyo candidates")
	       (forward-line -1)
	       (skip-chars-forward " ")
	       (string-to-number
		(buffer-substring-no-properties (point) (progn (end-of-line) (point)))))))))))
  "*skk-rdbms-shell $B$K$h$C$F8!:w$7$?%"%&%H%W%C%H$r@Z$j=P$7$F2C9)$7!"<-=q8uJd?t$rJV$94X?t!#(B
skk-rdbms-working-buffer $B$NCf$G%3!<%k$5$l$k!#(B")

(defvar skk-rdbms-save-jisyo-function
  (function
   (lambda (quiet)
     (if (not (skk-rdbms-process-alive))
	 (if (not quiet)
	     (progn
	       (skk-message "SKK $B%G!<%?%Y!<%9$rA]=|(B/$B%@%s%W$9$kI,MW$O$"$j$^$;$s(B"
			    "No need to vacuum/dump SKK database")
	       (sit-for 1)))
       (let ((wbuf (get-buffer-create " *SKK private jisyo dump*"))
	     v)
	 (unwind-protect
	     (progn
	       (if (not quiet)
		   (skk-message "SKK $B%G!<%?%Y!<%9$rA]=|$7$F$$$^$9(B..."
				"Vacuuming SKK database..."))
	       (if (eq this-command 'save-buffers-kill-emacs)
		   (skk-record-jisyo-data))
	       ;; vacuum $B$,=*$o$i$J$$Fb$K(B skk-rdbms-kill $B$,%3!<%k$5$l$F$7$^$&$H$I(B
	       ;; $B$&$J$k$s$@$m$&$+!)(B
	       (skk-rdbms-run-SQL-command
		(format "VACUUM %s;" skk-rdbms-private-jisyo-table))
	       (if (not quiet)
		   (progn
		     (skk-message "SKK $B%G!<%?%Y!<%9$rA]=|$7$F$$$^$9(B...$B40N;!*(B"
				  "Vacuuming SKK database...done")
		     (sit-for 1)))
	       (if (and (file-exists-p skk-rdbms-private-jisyo-dump)
			(> 86399
			   (skk-time-difference
			    (nth 5 (file-attributes skk-rdbms-private-jisyo-dump))
			    (current-time))))
		   nil
		 ;; $BA02s%@%s%W$7$?$N$,(B 1 $BF|0J>eA0$@$C$?$i!"%@%s%W$9$k!#(B
		 (if (not quiet)
		     (skk-message "SKK $B%G!<%?%Y!<%9$r%@%s%W$7$F$$$^$9(B..."
				  "Dumping out SKK database..."))
		 (setq v (= (call-process
			     shell-file-name
			     nil (list wbuf skk-rdbms-dump-error) t
			     "-c" (format "pg_dump skk -t %s" skk-rdbms-private-jisyo-table))
			    0))
		 (cond ((and (not quiet) v)
			(save-excursion
			  (set-buffer wbuf)
			  (write-region (point-min) (point-max)
					skk-rdbms-private-jisyo-dump nil 'nomsg))
			(skk-message "SKK $B%G!<%?%Y!<%9$r%@%s%W$7$F$$$^$9(B...$B40N;!*(B"
				     "Dumping out SKK database...done")
			(sit-for 1))
		       ((not v)
			(skk-error "SKK $B%G!<%?%Y!<%9$r%@%s%W$9$k$3$H$K<:GT$7$^$7$?(B"
				   "Failed to dump out SKK database")))))
	   (kill-buffer wbuf))))))
  "*$B%G!<%?%Y!<%9%W%m%;%9$N=*N;;~$K8D?M<-=q$K4X$9$k=hM}$r9T$J$&4X?t!#(B
$B%G%#%U%)%k%H$O(B PostgreSQL $B@lMQ%3%^%s%I$H$J$C$F$$$k!#(B")

(defvar skk-rdbms-load-hook nil
  "*skk-rdbms.el $B$r%m!<%I$7$?$H$-$N%U%C%/!#(B")

;; for test use.
(setq skk-search-prog-list
      '(;;(skk-rdbms-search-kakutei-jisyo-table)
	(skk-rdbms-search-jisyo-table skk-rdbms-private-jisyo-table)
	(skk-rdbms-sahen-search)
	(skk-rdbms-search-jisyo-table skk-rdbms-public-jisyo-table)
	;;(skk-okuri-search)
	(skk-rdbms-search-jisyo-table skk-rdbms-jis2-jisyo-table)))

(setq skk-server-host nil
      skk-servers-list nil)

;; System constants and variables.
(defvar skk-rdbms-last-process-point nil)
(defvar skk-rdbms-process nil)
(defvar skk-rdbms-working-buffer nil)
(defvar skk-rdbms-no-wait nil)
(defvar skk-rdbms-completion-index 1)
(defvar skk-rdbms-completion-list nil)
(defvar skk-rdbms-no-update-command nil)
;;(defvar skk-rdbms-kakutei-word nil)
(setq skk-update-jisyo-function 'skk-rdbms-update-jisyo)
(setq skk-save-jisyo-function 'skk-rdbms-save-jisyo)
(setq skk-count-jisyo-candidates-function 'skk-rdbms-count-jisyo-candidates)
(setq skk-completion-function 'skk-rdbms-completion)
(setq skk-previous-completion-function 'skk-rdbms-previous-completion)
(setq skk-okuri-search-function 'skk-rdbms-okuri-search)
(setq skk-public-jisyo-to-be-searched-function 'skk-rdbms-public-jisyo-to-be-searched)

(defun skk-rdbms-init ()
  (or (skk-rdbms-process-alive)
      (let ((process-connection-type t)
	    (cbuf (current-buffer))
	    origin)
	(if skk-rdbms-process
	    (progn
	      (delete-process skk-rdbms-process)
	      (setq skk-rdbms-process nil)))
	(or (and skk-rdbms-working-buffer
		 (buffer-name skk-rdbms-working-buffer))
	    (setq skk-rdbms-working-buffer (get-buffer-create " *SKK rdbms*")))
	;; $BF0$$$?%]%$%s%H$rJ]B8$9$k$?$a(B save-excursion $B$O;H$o$J$$!#(B
	(unwind-protect
	    (progn
	      (set-buffer skk-rdbms-working-buffer)
              (buffer-disable-undo)
	      (setenv "PAGER" nil)
	      (insert "\nStarting SKK rdbms...\n\n")
	      (skk-message "SKK $B%G!<%?%Y!<%9$r5/F0$7$F$$$^$9(B..."
			   "Starting SKK rdbms...")
	      (setq origin (point))
	      (condition-case nil
		  (progn
		    (setq skk-rdbms-process
			  (start-process "SKK rdbms"
					 skk-rdbms-working-buffer
					 skk-rdbms-shell
					 skk-rdbms-shell-args))
		    (process-kill-without-query skk-rdbms-process)
		    (cond ((eq skk-emacs-type 'xemacs)
			   (set-process-input-coding-system
			    skk-rdbms-process
			    skk-rdbms-process-coding-system)
			   (set-process-output-coding-system
			    skk-rdbms-process
			    skk-rdbms-process-coding-system))
			  (t
			   (set-process-coding-system
			    skk-rdbms-process
			    skk-rdbms-process-coding-system
			    skk-rdbms-process-coding-system))))
		(file-error
		 (skk-error "$B%7%9%F%`>e$K(B \"%s\" $B$,8+$D$+$j$^$;$s(B"
			    "Sorry, can't find \"%s\" on your system"
			    skk-rdbms-shell)))
	      (skk-save-point
	       (while
		   (and
		    (not
		     (progn
		       (goto-char origin)
		       (re-search-forward skk-rdbms-shell-prompt-regexp nil t)))
		    (memq (process-status skk-rdbms-process) '(run stop)))
		 (accept-process-output skk-rdbms-process)))
	      (or (memq (process-status skk-rdbms-process) '(run stop))
		  (skk-error "SKK $B%G!<%?%Y!<%9%W%m%;%9$r%9%?!<%H$9$k$3$H$,$G$-$^$;$s(B"
			     "Unable to start SKK database process"))
	      (and skk-rdbms-shell-version-check-function
		   (funcall skk-rdbms-shell-version-check-function))
	      (goto-char (process-mark skk-rdbms-process))
	      (setq skk-rdbms-last-process-point (point)))
	      (skk-message "SKK $B%G!<%?%Y!<%9$r5/F0$7$F$$$^$9(B...$B40N;(B!"
			   "Starting SKK rdbms...done")
	  (set-buffer cbuf)))))

(defun skk-rdbms-process-alive ()
  (and skk-rdbms-process
       skk-rdbms-working-buffer
       (buffer-name skk-rdbms-working-buffer)
       (memq (process-status skk-rdbms-process) '(run stop))))

(defun skk-rdbms-kill ()
  "SKK $B%G!<%?%Y!<%9$N%W%m%;%9$r;&$9!#(B"
  (interactive)
  (if (not (skk-rdbms-process-alive))
      ;; $BKLEM?@7}$N@$3&$G$9$J(B...$B!#(B
      (skk-message "SKK $B%G!<%?%Y!<%9%W%m%;%9$O4{$K;`$s$G$^$9(B"
		   "SKK database process has already died")
    (or (eq this-command 'save-buffers-kill-emacs)
	(skk-rdbms-save-jisyo))
    (let ((skk-rdbms-no-wait t))
      (skk-rdbms-run-SQL-command skk-rdbms-kill-command)
      (sit-for 1)
      (and (process-status skk-rdbms-process)
	   (delete-process skk-rdbms-process))
      (setq skk-rdbms-process nil)
      (skk-message "SKK $B%G!<%?%Y!<%9%W%m%;%9$,;`$K$^$7$?(B"
		   "SKK database process died"))))

(defun skk-rdbms-search-jisyo-table (table)
  (and skk-use-numeric-conversion
       (setq skk-henkan-key (skk-compute-numeric-henkan-key skk-henkan-key)))
  (let* ((private-table-p (string= table skk-rdbms-private-jisyo-table))
	 (command
	  (cond (private-table-p (skk-rdbms-SQL-search-private-jisyo-command))
		(t (skk-rdbms-SQL-search-jisyo-command table)))))
    (skk-rdbms-run-SQL-command command skk-rdbms-cutoff-output-function)))

(defun skk-rdbms-search-kakutei-jisyo-table ()
  (prog1
      (setq skk-kakutei-henkan-flag
	    (skk-rdbms-run-SQL-command
	     (skk-rdbms-SQL-search-jisyo-command skk-rdbms-kakutei-jisyo-table)
	     skk-rdbms-cutoff-output-function))
    (and skk-kakutei-henkan-flag
	 (setq skk-rdbms-no-update-command t))))

(defun skk-rdbms-run-SQL-command (command &optional cutoff-func)
  (combine-after-change-calls
    (save-match-data
      (if (and (not (string-match ";$" command))
	       skk-rdbms-shell-special-command-regexp
	       (not (string-match skk-rdbms-shell-special-command-regexp command)))
	  (skk-error "%s $B$N%3%^%s%I$KJ8K!%_%9$,$"$j$^$9(B"
		     "A grammatical mistake of %s command exists"
		     skk-rdbms-shell)
	(setq command (concat command " \n")))
      (let ((cbuf (current-buffer))
	    (okuri-char skk-okuri-char)
	    (henkan-okurigana skk-henkan-okurigana)
	    (henkan-key skk-henkan-key)
	    pmark var)
	(skk-rdbms-init)
	(accept-process-output)
	(setq pmark (process-mark skk-rdbms-process))
	;; $BF0$$$?%]%$%s%H$rJ]B8$9$k$?$a(B save-excursion $B$O;H$o$J$$!#(B
	(unwind-protect
	    (catch 'exit
	      (set-buffer skk-rdbms-working-buffer)
	      ;; $B%P%C%U%!%m!<%+%kJQ?t$r0\$7$F$*$/!#(B
	      (setq skk-okuri-char okuri-char
		    skk-henkan-okurigana henkan-okurigana
		    skk-henkan-key henkan-key)
	      ;; $B%]%$%s%H$OJ]B8$5$l$F$$$k$N$GK\MhMW$i$J$$$O$:$@$,!#(B
	      (goto-char pmark)
	      (setq skk-rdbms-last-process-point (point))
	      (insert command)
	      (set-marker pmark (point))
	      (process-send-string skk-rdbms-process command)
	      (accept-process-output (and (not skk-rdbms-no-wait)
					  skk-rdbms-process))
	      (goto-char skk-rdbms-last-process-point)
	      ;; $B$3$&$7$J$$$H(B psql $B$,$b$?$b$?$7$F$$$k$H(B search $B$K<:GT$9$k>l(B
	      ;; $B9g$,$"$k!#(B
	      (while (not (re-search-forward skk-rdbms-shell-prompt-regexp
					     pmark t))
		;; \q $B%3%^%s%I$rAw$C$?$i%W%m%s%W%H$O5"$C$F$3$J$$!#(B
		(if (eq (process-status skk-rdbms-process) 'exit)
		    (throw 'exit nil))
		(accept-process-output))
	      (skk-rdbms-check-for-errors)
	      (goto-char pmark)
	      (and cutoff-func (setq var (funcall cutoff-func))))
	  (set-buffer cbuf)
	  var)))))

(defun skk-rdbms-check-for-errors ()
  ;; $B%(%i!<8e$N%]%$%s%H0LCV$rJ]8n$9$k$?$a$K(B if $B@a$r(B save-excursion $B$G$D$D$`!#(B
  (save-match-data
    (if (save-excursion
	  (set-buffer skk-rdbms-working-buffer)
	  (goto-char skk-rdbms-last-process-point)
	  (re-search-forward skk-rdbms-error-regexp nil t))
	(error "%s" (concat "SQL error: " (match-string 1))))))

(defun skk-rdbms-update-jisyo (word &optional purge)
  (if skk-rdbms-no-update-command
      (setq skk-rdbms-no-update-command nil)
    (and (> skk-okuri-index-min -1) (setq word (skk-remove-common word)))
    (and skk-use-numeric-conversion
	 (setq skk-henkan-key (skk-compute-numeric-henkan-key skk-henkan-key)))
    (let ((command
	   (cond (purge (skk-rdbms-SQL-delete-command word))
		 ;; $B%_%K%P%C%U%!$GEPO?$7$?>l9g(B
		 (skk-henkan-in-minibuff-flag
		  (skk-rdbms-SQL-insert-command word))
		 (t (skk-rdbms-SQL-update-command word))))
	  ignore)
      (skk-rdbms-run-SQL-command command)
      (cond (purge
	     (cond
	      ((skk-public-jisyo-has-entry-p skk-henkan-okurigana word)
	       (skk-rdbms-run-SQL-command
		(skk-rdbms-SQL-regexp-delete-command "^\\\\(skk-ignore-dic-word .*\\\\)"))
	       (setq ignore (car (skk-compose-ignore-entry skk-henkan-list word)))
	       (skk-rdbms-run-SQL-command (skk-rdbms-SQL-insert-command ignore)))))
	    ((and (not skk-henkan-in-minibuff-flag)
		  ;; L $B<-=q$J$I$+$iF@$?%(%s%H%j$@$+$i8D?M<-=q$N(B UPDATE $B$K<:GT$7$?!#(B
		  (skk-rdbms-update-jisyo-failp))
	     ;; $B:G=i$K(B SELECT $B$G%l%3!<%I$,$"$k$+$I$&$+%A%'%C%/$7$F$+$i(B
	     ;; UPDATE $B$9$k$N$,K\Ev$@$m$&$1$I!"$=$&$9$k$H7k9=CY$$!#(B
	     (skk-rdbms-run-SQL-command (skk-rdbms-SQL-insert-command word))))
      (setq skk-henkan-in-minibuff-flag nil))))

(defun skk-rdbms-update-jisyo-failp ()
  (save-match-data
    (save-excursion
      (set-buffer skk-rdbms-working-buffer)
      (goto-char (process-mark skk-rdbms-process))
      (re-search-backward skk-rdbms-update-fail-regexp
			  skk-rdbms-last-process-point t))))

(defun skk-rdbms-save-jisyo (&optional quiet)
  (let ((inhibit-quit t))
    (funcall skk-rdbms-save-jisyo-function quiet)
      (skk-set-cursor-properly)))

(defun skk-rdbms-restore-private-jisyo (force)
  "$B%@%s%W%U%!%$%k$+$i8D?M<-=q%G!<%?%Y!<%9$rI|85$9$k!#(B
C-u M-x skk-rdbms-restore-private-jisyo $B$9$k$H3NG'$J$7$KI|85$9$k!#(B"
  (interactive "P")
  (if (and
       (not force)
       (not (skk-yes-or-no-p
	     "$B%@%s%W%U%!%$%k$+$i8D?M<-=q%G!<%?%Y!<%9$rI|85$7$^$9!#$h$m$7$$$G$9$+!)(B "
	     "Restore private jisyo database from dump file? ")))
      (progn
	(ding)
	(skk-message "$B%@%s%W%U%!%$%k$+$i$N8D?M<-=q%G!<%?%Y!<%9$NI|85$rCf;_$7$^$7$?(B"
		     "Stop restoring private jisyo database from dump file"))
    (skk-message "$B%@%s%W%U%!%$%k$+$i8D?M<-=q%G!<%?%Y!<%9$rI|85$7$F$$$^$9(B..."
		 "Restoring private jisyo database from dump file...")
    (condition-case nil
	(skk-rdbms-run-SQL-command
	 (format "DROP TABLE %s;" skk-rdbms-private-jisyo-table))
      ;; error will occur if the table does not exist.
      (error nil))
    (if (= (call-process
	    shell-file-name
	    nil (list nil skk-rdbms-dump-error) t
	    "-c" (format "%s %s < %s" skk-rdbms-shell ; PostgreSQL only command.
			 skk-rdbms-shell-args
			 (expand-file-name skk-rdbms-private-jisyo-dump)))
	   0)
	(skk-message "$B%@%s%W%U%!%$%k$+$i8D?M<-=q%G!<%?%Y!<%9$rI|85$7$F$$$^$9(B...$B40N;(B!"
		     "Restoring private jisyo database from dump file...done!")
      (skk-error "$B%@%s%W%U%!%$%k$+$i$N8D?M<-=q%G!<%?%Y!<%9$NI|85$K<:GT$7$^$7$?(B"
		 "Failed to restore private jisyo database from dump file"))))

(defun skk-rdbms-count-jisyo-candidates (table)
  (if (interactive-p)
      (message "Counting jisyo candidates..."))
  (skk-rdbms-run-SQL-command (format "SELECT COUNT(*) FROM %s;" table)
			     skk-rdbms-cutoff-output-function-3))

(defun skk-rdbms-public-jisyo-to-be-searched ()
  (list 'skk-rdbms-search-jisyo-table 'skk-rdbms-public-jisyo-table))

(defun skk-rdbms-completion (first)
  (require 'skk-comp)
  (let (c-word)
    (skk-kana-cleanup 'force)
    (if (or first skk-dabbrev-like-completion)
	(setq skk-completion-word
	      (buffer-substring-no-properties skk-henkan-start-point (point))
	      ;; 0th $B$K$OJd40$7$h$&$H$9$k85$N8l$rF~$l$k$N$G!"%9%-%C%W!#(B
	      skk-rdbms-completion-index 1))
    (if (string= skk-completion-word "")
        (skk-error "$B6uJ8;z$+$iJd40$9$k$3$H$O$G$-$^$;$s!*(B"
                   "Cannot complete an empty string!"))
    (if (or first skk-dabbrev-like-completion)
	(setq skk-rdbms-completion-list
	      (skk-rdbms-run-SQL-command
	       (skk-rdbms-SQL-search-completion-word-command)
	       skk-rdbms-cutoff-output-function)))
    (setq skk-rdbms-completion-list
	  (cons skk-completion-word
		(delete skk-completion-word skk-rdbms-completion-list))
	  c-word (nth skk-rdbms-completion-index skk-rdbms-completion-list))
    (if (null c-word)
        (if skk-japanese-message-and-error
            (error "\"%s\" $B$GJd40$9$Y$-8+=P$78l$O(B%s$B$"$j$^$;$s(B"
                   skk-completion-word (if first "" "$BB>$K(B"))
          (error "No %scompletions for \"%s\""
                 (if first "" "more ") skk-completion-word))
      (setq skk-rdbms-completion-index (1+ skk-rdbms-completion-index))
      (delete-region skk-henkan-start-point (point))
      (insert c-word))))

(defun skk-rdbms-previous-completion ()
  (require 'skk-comp)
  (let ((inhibit-quit t)
        c-word)
    (catch 'exit
      (while (not c-word)
	(setq skk-rdbms-completion-index (1- skk-rdbms-completion-index))
	;; (nth -1 '(A B C D)) $B$O(B A $B$rJV$9(B...$B!#(B
	(if (> 0 skk-rdbms-completion-index)
	    (throw 'exit nil)
	  (setq c-word (nth skk-rdbms-completion-index
			    skk-rdbms-completion-list)))
	(if (string= c-word (buffer-substring-no-properties skk-henkan-start-point (point)))
	    ;; $B%]%C%W$7$?8l$,%P%C%U%!$N%]%$%s%HD>A0$K$"$kJ8;zNs$HF1$8(B
	    ;; $B$@$C$?$i(B 1 $B$D<N$F$k!#(B
	    (setq c-word nil))))
    ;; $B%(%i!<$r=P$9A0$K$3$N%3%^%s%IL>$r=$@0$7$F$*$/!#(B
    (setq this-command 'skk-completion)
    (if (not c-word)
	(skk-error "\"%s\"$B$GJd40$9$Y$-8+=P$78l$OB>$K$"$j$^$;$s(B"
		   "No more previous completions for \"%s\""
		   skk-completion-word)
      (delete-region skk-henkan-start-point (point))
      (insert c-word))))

(defun skk-rdbms-okuri-search ()
  (require 'skk-auto)
  (let* ((inhibit-quit t)
	 ;; "$B$?$A$"$2$k(B" -> (53825 53794 53810 53867) == (?$B$A(B ?$B$"(B ?$B$2(B ?$B$k(B)
	 (okurigana-list (cdr (append skk-henkan-key nil)))
	 (henkan-key-length (length skk-henkan-key))
	 (length skk-kanji-len)
	 (original-henkan-key skk-henkan-key)
	 var full-okurigana kanji)
    (catch 'exit
      (while (> henkan-key-length length)
	(let ((skk-henkan-okurigana (char-to-string (car okurigana-list))) ; "$B$A(B"
	      skk-okuri-char skk-henkan-key next-okurigana)
	  (if (string= skk-henkan-okurigana "$B$C(B")
	      (progn
		;; $BB%2;!V$C!W$@$C$?$i<!$N$+$J$r4^$a$F(B skk-henkan-okurigana $B$KF~(B
		;; $B$l$kI,MW$"$j!#(B
		(setq next-okurigana (nth 1 okurigana-list))
		(if (not next-okurigana)
		    (throw 'exit nil)
		  (setq skk-henkan-okurigana
			(concat skk-henkan-okurigana
				(char-to-string next-okurigana))))))
	  (setq skk-okuri-char (skk-auto-okurigana-prefix
				(char-to-string (car okurigana-list))) ; "t"
		skk-henkan-key (concat (substring original-henkan-key 0 length) ; "$B$?(Bt"
				       skk-okuri-char))
	  (if (not skk-okuri-char)
	      nil
	    ;; ("$B7P(B" "$B7z(B" "$BCG(B" "$BN)(B")
	    (setq kanji (skk-rdbms-search-jisyo-table skk-rdbms-private-jisyo-table))
	    (if (null kanji)
		nil
	      (setq full-okurigana (concat okurigana-list) ; "$B$A$"$2$k(B"
		    var (nconc
			 ;; ("$B7P$A$"$2$k(B" "$B7z$A$"$2$k(B" "$BCG$A$"$2$k(B" "$BN)$A$"$2$k(B")
			 (mapcar
			  (function (lambda (x) (concat x full-okurigana)))
			  kanji) var))))
	  (if next-okurigana
	      (setq okurigana-list (nthcdr 2 okurigana-list)
		    length (+ length (* 2 skk-kanji-len)))
	    ;; $BAw$j2>L>$rC;$/$7$F!"(B
	    (setq okurigana-list (cdr okurigana-list)
		  ;; skk-henkan-key $B$rD9$/$9$k!#(B
		  length (+ length skk-kanji-len))))))
    ;; ("$BN)$A>e$2$k(B" "$B7P$A$"$2$k(B" "$B7z$A$"$2$k(B" "$BCG$A$"$2$k(B" "$BN)$A$"$2$k(B")
    var))

(defun skk-rdbms-sahen-search ()
  (save-match-data
    (if (not
	 (or (and skk-okuri-char (memq (string-to-char skk-okuri-char) '(?s ?z)))
	     (and (not skk-okuri-char) (string-match "[$B$9$:(B]$B$k(B$" skk-henkan-key))))
	nil
      (let (l)
	(setq skk-okuri-index-min (if (= skk-okuri-index-min -1)
				      (length skk-henkan-list)
				    (min (length skk-henkan-list) skk-okuri-index-min))
	      l (skk-rdbms-sahen-search-1)
	      skk-okuri-index-max (max skk-okuri-index-min
				       (+ skk-okuri-index-min (length l))))
	l))))

(defun skk-rdbms-sahen-search-1 ()
  (let* ((okurigana
	  (and (not skk-okuri-char)
	       (string-match "[$B$9$:(B]$B$k(B$" skk-henkan-key)
	       (substring skk-henkan-key (match-beginning 0))))
	 (dakuten
	  (cond
	   ((and
	     skk-okuri-char
	     (eq ?z (car (memq (string-to-char skk-okuri-char) '(?s ?z))))))
	   ((and okurigana
		 (string= "$B$:(B" (char-to-string (aref okurigana 0)))))))
	 (skk-henkan-key
	  (cond
	   (skk-okuri-char
	    (substring skk-henkan-key 0 (1- (length skk-henkan-key))))
	   (t (substring skk-henkan-key 0 (match-beginning 0)))))
	 v)
    (setq v (skk-rdbms-run-SQL-command
	     (skk-rdbms-search-sahen-command dakuten)
	     skk-rdbms-cutoff-output-function))
    (if (not okurigana)
	v
      (mapcar (function (lambda (x) (concat x okurigana))) v))))

(defun skk-rdbms-busyu-henkan (&optional busyu)
  "$BIt<s$+$iJ8;z$r8!:w$9$k!#(B"
  (interactive)
  ;; $BD>@\4X?t0z?t$H$7$F(B busyu $B$rM?$($i$l$?$iJ86g$r8@$o$J$$!#(B
  (and (not busyu) (not skk-henkan-on)
       (skk-error "$B"&%b!<%I$KF~$C$F$$$^$;$s(B" "Not in $B"&(B mode"))
  (or busyu
      (setq busyu (buffer-substring-no-properties (point) skk-henkan-start-point)))
  (and (string= busyu "")
       (skk-error "$B8!:w$9$Y$-It<s$,$"$j$^$;$s(B" "No BUSYU to search"))
  (setq skk-henkan-key busyu)
  (let ((skk-search-prog-list
	 '((skk-rdbms-run-SQL-command (skk-rdbms-SQL-search-busyu-command busyu)
				      skk-rdbms-cutoff-output-function))))
    ;; $B8D?M<-=q%F!<%V%k$K<h$j9~$^$J$$!#(B
    (setq skk-rdbms-no-update-command t)
    (skk-start-henkan nil)))

(defun skk-rdbms-stroke-henkan (&optional stroke)
  "$B2h?t$+$iJ8;z$r8!:w$9$k!#(B"
  (interactive)
  ;; $BD>@\4X?t0z?t$H$7$F(B stroke $B$rM?$($i$l$?$iJ86g$r8@$o$J$$!#(B
  (if (and (not stroke) (not skk-henkan-on))
      (skk-error "$B"&%b!<%I$KF~$C$F$$$^$;$s(B" "Not in $B"&(B mode"))
  (save-match-data
    (or stroke
	(setq stroke (buffer-substring-no-properties (point) skk-henkan-start-point)))
    (if (or (string= stroke "") (string-match "[^0-9]" stroke))
	(skk-error "$B8!:w$9$Y$-2h?t$,$"$j$^$;$s(B" "No stroke to search"))
    (setq skk-henkan-key stroke)
    (let ((skk-search-prog-list
	   '((skk-rdbms-run-SQL-command
	      (skk-rdbms-SQL-search-stroke-command stroke)
	      skk-rdbms-cutoff-output-function))))
      ;; $B8D?M<-=q%F!<%V%k$K<h$j9~$^$J$$!#(B
      (setq skk-rdbms-no-update-command t)
      (skk-start-henkan nil))))

(defun skk-rdbms-stroke (kanji)
  "$B4A;z$N2h?t$rI=<($9$k!#(B"
  (interactive (list (read-from-minibuffer
		      (if skk-japanese-message-and-error "$B4A;z(B: " "Kanji: "))))
  (if (interactive-p)
      (message (skk-rdbms-kanji-to-stroke kanji))
   (let (v)
     (add-hook 'minibuffer-setup-hook 'skk-j-mode-on)
     (add-hook
      'minibuffer-setup-hook
      (function (lambda () (add-local-hook 'pre-command-hook 'skk-pre-command))))
     (setq v (car (skk-rdbms-run-SQL-command
		   (format "SELECT stroke FROM %s WHERE kanji = '%s';"
			   skk-rdbms-stroke-table kanji)
		   skk-rdbms-cutoff-output-function)))
     (if (not (string-match "[0-9]+" v))
	 nil
       (setq v (substring v (match-beginning 0) (match-end 0)))
       (if skk-japanese-message-and-error
	   (concat v " $B2h(B")
	 (concat v " stroke" (if (not (string= v "1")) "s")))))))

;; skk-rdbms-save-jisyo $B$NCf$G(B VACUUM $B$7$?8e$K%W%m%;%9$r;&$9!#(B
(add-hook 'skk-before-kill-emacs-hook 'skk-rdbms-kill 'append)

(run-hooks 'skk-rdbms-load-hook)

(provide 'skk-rdbms)
;;; skk-rdbms.el ends here
