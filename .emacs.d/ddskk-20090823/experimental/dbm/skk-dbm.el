;;; skk-dbm.el --- SKK dbm interfaces. -*- coding: iso-2022-jp -*-
;; Copyright (C) 1999, 2000 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Version: $Id: skk-dbm.el,v 1.8 2007/04/22 02:38:28 skk-cvs Exp $
;; Keywords: japanese, dbm, gdbm
;; Created: Jan. 1, 1999
;; Last Modified: $Date: 2007/04/22 02:38:28 $

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

;; $B$3$N%W%m%0%i%`$O!"(Bskkserv $B$r2p$9$k%5!<%A$NBeBX$N<-=q%"%/%;%9J}K!$rDs6!$7$^$9!#(B
;; $B%5!<%P!<$r2p$5$:!"(BXEmacs $B$N5!G=$r;HMQ$7!"D>@\(B dbm $B%G!<%?%Y!<%9%U%!%$%k(B ($B0J2<(B
;; $BC1$K!V%G!<%?%Y!<%9!W$H8@$$$^$9(B) $B$r%*!<%W%s$7$F8!:w$7$^$9!#(B
;; --with-database $B%*%W%7%g%s$N;XDj$5$l$?(B XEmacs $B$G(B *$B$N$_(B* $B;HMQ$9$k$3$H$,$G$-$^(B
;; $B$9!#F0:n3NG'$O!"(BXEmacs 21.2b8 $B$G9T$J$$$^$7$?!#(B
;;
;; $B%G!<%?%Y!<%9%i%$%V%i%j$H$7$F(B gdbm $B$r;H$&>l9g$G!"%G!<%?%Y!<%9$r(B gdbm $B7A<0$H$7(B
;; $B$?$$$H$-$O!"(BXEmacs 21.2b8 $B$,(B gdbm $B%U%!%$%k$KD>@\%"%/%;%9$G$-$J$$(B (libgdbm $B$r(B
;; $B%j%s%/$7$F$$$J$,$i(B libndbm $B8_494X?t$7$+;HMQ$7$J$$$N$G(B .dir, .pag $B$r3HD%;R$H$9(B
;; $B$k%U%!%$%k$7$+3+$1$J$$(B) $B$3$H$+$i!"JLES:n6H$,I,MW$H$J$j$^$9!#(B
;; $BJLE:$N(B diff-to-xemacs-21.2.8 $B$r(B xemacs-21.2.8 $B$N%=!<%9$KEv$F!"(B
;; --with-database=gnudbm $B%*%W%7%g%s$r;XDj$7$F(B XEmacs $B$r:F(B configure $B$7!":F%3%s(B
;; $B%Q%$%k$9$kI,MW$,$"$j$^$9!#$3$N%Q%C%A$rEv$F$?(B XEmacs $B$G$O!"(Bgdbm $B%U%!%$%k$N(B
;; synchronize $B$d(B reorganize $B$,2DG=$H$J$kB>!"(Bcashsize $B$d(B fastmode $B$N%*%W%7%g%s$b(B
;; $B;XDj2DG=$H$J$j$^$9!#(B
;;
;; ** berkeley-db $B$b$7$/$O(B ndbm $B%U%!%$%k$@$1$G$"$l$P%Q%C%A$rEv$F$kI,MW$O$"$j$^$;$s!#(B***
;;
;; skk-search-prog-list $B$rNc$($P!"2<5-$N$h$&$K@_Dj$9$k$3$H$G!"(Bautoload $B$5$l$^$9!#(B
;;
;;    (setq skk-search-prog-list
;;         '((skk-search-jisyo-file skk-jisyo 0 t)
;;   	     (skk-dbm-search-jisyo-database skk-dbm-large-jisyo 'nomsg)))
;;
;; $B2<5-$N$h$&$K@_Dj$9$k$H!"8D?M<-=q$b%G!<%?%Y!<%92=$7$F8!:w$r9T$J$&$3$H$,$G$-$^$9!#(B
;;
;;    (setq skk-search-prog-list
;;         '((skk-dbm-search-jisyo-database skk-dbm-jisyo)
;;   	     (skk-dbm-search-jisyo-database skk-dbm-large-jisyo 'nomsg)))
;;
;; pskkserv $BE:IU$N(B makedbmdic $B$G:n$C$?<-=q$G$O2?8N$+8!:w$G$-$^$;$s(B (XEmacs $B$N%G!<(B
;; $B%?%Y!<%95!G=$,%3!<%G%#%s%0%7%9%F%`$rL5;k$7$F$$$k$+$i!)(B)$B!#$3$N%U%!%$%k$N(B
;; skk-dbm-make-jisyo $B4X?t$r;H$C$F%G!<%?%Y!<%9$r:n$C$F2<$5$$!#(B

;; TODO
;; $BJd40!&<+F0Aw$j=hM}BP1~!#(B

;;; Code:

(eval-when-compile
  (require 'skk-macs)
  (require 'skk-vars))

(unless (or (featurep 'gdbm)
	    (featurep 'dbm)
	    (featurep 'berkeley-db))
  (error "%s" "You need XEmacs built with --with-database option"))

(defgroup skk-dbm nil "SKK dbm related customization."
  :prefix "skk-dbm-"
  :group 'skk)

;; User variables.
;;;###autoload
(defcustom skk-dbm-jisyo "~/.skk-jisyo.db"
  "*dbm $B%G!<%?%Y!<%92=$5$l$?8D?M<-=q$N%U%!%$%kL>!#(B"
  :type 'file
  :group 'skk-dbm)

;;;###autoload
(defcustom skk-dbm-large-jisyo "/usr/local/share/skk/SKK-JISYO.L.db"
  "*dbm $B%G!<%?%Y!<%92=$5$l$?(B SKK-JISYO.L $B$N%U%!%$%kL>!#(B"
  :type 'file
  :group 'skk-dbm)

;;;###autoload
(defcustom skk-dbm-subtype
  (save-match-data
    (and (or (string-match "\\.db$" skk-dbm-jisyo)
	     (string-match "\\.db$" skk-dbm-large-jisyo))
	 'hash))
  "*database $B<-=q$N(B subtype$B!#(B hash, btree, recno $B$N$$$:$l$+!#(B
berkeley-db $B$r;HMQ$9$k>l9g$N$_;XDj$9$k$3$H!#(B"
  :type '(choice (const hash) (const btree) (const recno) (const nil))
  :group 'skk-dbm)

(and (member '(skk-dbm-search-jisyo-database skk-dbm-jisyo) skk-search-prog-list)
     (setq skk-update-jisyo-function 'skk-dbm-update-jisyo)
     (setq skk-save-jisyo-function
	   (function
	    (lambda (quiet)
	      (if (not quiet)
		  (progn
		    (skk-message "SKK $B%G!<%?%Y!<%9<-=q$rJD$8$F$$$^$9(B..."
				 "Closing SKK database jisyo...")
		    (sit-for 1)))
	      (skk-dbm-close-all-database)
	      (if (not quiet)
		  (progn
		    (skk-message "SKK $B%G!<%?%Y!<%9<-=q$rJD$8$F$$$^$9(B...$B40N;(B!"
				 "Closing SKK database jisyo...done")
		    (sit-for 1)))))))
	
;; System constants and variables.
(defvar skk-dbm-alist nil)
(defvar skk-dbm-type nil)
(defconst skk-dbm-working-buffer " *skk-dbm*")
(defconst skk-dbm-coding-system (cdr (assoc "euc" skk-coding-system-alist)))

;; Functions.
;;;###autoload
(defun skk-dbm-search-jisyo-database (dbfile &optional nomsg)
  (setq dbfile (expand-file-name dbfile))
  (let (
	;; I want `get-file-database'...
	(database (cdr (assoc dbfile skk-dbm-alist)))
	(okurigana (or skk-henkan-okurigana skk-okuri-char))
	(midasi
	 (if skk-use-numeric-conversion
	     (skk-num-compute-henkan-key skk-henkan-key)
	   skk-henkan-key))
	(henkan-buffer (current-buffer))
	string entry-list entry)
    (or (and database (databasep database) (database-live-p database))
	(setq database (skk-dbm-get-jisyo-database dbfile nomsg)
	      ;; this should do in Emacs internal like Vbuffer_alist of
	      ;; buffer.c.
	      skk-dbm-alist (cons (cons dbfile database) skk-dbm-alist)))
    (skk-dbm-init-working-buffer)
    (with-current-buffer skk-dbm-working-buffer
      ;;(and okurigana
      ;;     (setq okurigana (encode-coding-string
      ;;                    okurigana skk-dbm-coding-system)))
      ;;(setq midasi (encode-coding-string midasi skk-dbm-coding-system))
      (setq string (get-database midasi database))
      (if (not string)
	  nil
	(erase-buffer)
	(insert string)
	;; skip first character '/'.
	(goto-char (1+ (point-min)))
	(setq entry-list (skk-compute-henkan-lists okurigana))
	(setq entry
	      (cond ((and okurigana skk-henkan-okuri-strictly)
		     (nth 2 entry-list))
		    ((and okurigana skk-henkan-strict-okuri-precedence)
		     (skk-nunion (nth 2 entry-list) (car entry-list)))
		    (t (car entry-list))))
	(and skk-search-end-function
	     (setq entry (funcall skk-search-end-function henkan-buffer
				  midasi okurigana entry)))
	entry))))

(defun skk-dbm-get-jisyo-database (dbfile &optional nomsg)
  ;; Return database object.
  (save-match-data
    (setq dbfile (expand-file-name dbfile))
    (let (database access modes)
      (if (string= (expand-file-name skk-dbm-jisyo) dbfile)
	  ;; $B8D?M<-=q(B
	  (progn
	    (setq modes 0600)
	    (or (file-exists-p dbfile)
		;; $B$J$1$l$P(B plain text $B$N<-=q$+$i%G!<%?%Y!<%9<-=q$r:n@.$9$k!#(B
		(skk-dbm-make-private-jisyo)))
	;; $B6&M-<-=q(B
	(setq modes 0444)
	(or (file-exists-p dbfile)
	    ;; $B$J$+$C$?$i%(%i!<$K$9$k!#:n@.$9$k$N$K;~4V$,$+$+$k$b$N$M!#(B
	    (skk-error "$B%G!<%?%Y!<%9<-=q(B %s $B$,8+$D$+$j$^$;$s(B"
		       "Cannot find out database jisyo %s" dbfile))
	(or (file-readable-p dbfile)
	    (skk-error "$B%G!<%?%Y!<%9<-=q(B %s $B$,FI$a$^$;$s(B"
		       "Cannot read database jisyo %s" dbfile)))
      (or nomsg
	  (skk-message "SKK $B<-=q%G!<%?%Y!<%9(B %s $B$r3+$$$F$$$^$9(B..."
		       "Opening SKK dictionary database %s ..."
		       (file-name-nondirectory dbfile)))
      (setq skk-dbm-type
	    (cond
	     ((and (featurep 'berkeley-db)
		   (string-match "\\.db$" dbfile))
	      'berkeley-db)
	     ((and (featurep 'gdbm)
		   (string-match "\\.gdbm$" dbfile))
	      'gdbm)
	     ((and (featurep 'dbm)
		   (not (string-match "\\.db$" dbfile))
		   (not (string-match "\\.gdbm$" dbfile)))
	      'dbm)
	     (t
	      (skk-error
	       "$B%G!<%?%Y!<%9<-=q$r3+$/$?$a$NE,Ev$J%?%$%W$r7h$a$k$3$H$,$G$-$^$;$s(B"
	       "Cannot find out proper type for opening database jisyo")))
	    access (if (string= (file-name-nondirectory dbfile)
				(file-name-nondirectory skk-dbm-jisyo))
		       "+rw" "r"))
      (and skk-dbm-subtype (not (eq skk-dbm-type 'berkeley-db))
	   (skk-error "$B%G!<%?%Y!<%9%?%$%W$H%5%V%?%$%W$,L7=b$7$F$$$^$9(B"
		      "Database type and subtype conflicts"))
      (and (string= access "+rw")
	   (not (file-writable-p (file-name-directory dbfile)))
	   (skk-error "%s $B$K=q$-9~$_8"8B$,$"$j$^$;$s(B"
		      "You don't have write permission to %s"
		      (file-name-directory dbfile)))
      (setq database (open-database dbfile skk-dbm-type skk-dbm-subtype access
				    modes))
      (or (databasep database)
	  (skk-error "SKK $B<-=q%G!<%?%Y!<%9(B %s $B$r3+$/$3$H$,$G$-$^$;$s(B"
		     "Cannot open SKK dictionary database %s"
		     (file-name-nondirectory dbfile)))
      (or nomsg
	  (skk-message "SKK $B<-=q%G!<%?%Y!<%9(B %s $B$r3+$$$F$$$^$9(B...$B40N;!*(B"
		       "Opening SKK dictionary database %s ...done"
		       (file-name-nondirectory dbfile)))
      database)))

(defun skk-dbm-init-working-buffer ()
  (or (get-buffer skk-dbm-working-buffer)
      (with-current-buffer (get-buffer-create skk-dbm-working-buffer)
	(set-buffer-file-coding-system skk-dbm-coding-system)
	(buffer-disable-undo)
	(auto-save-mode -1)
	(setq buffer-read-only nil
	      case-fold-search nil
	      major-mode 'skk-jisyo-mode
	      mode-name "SKK dbmdic"))))

(defun skk-dbm-update-jisyo (word &optional purge)
  (let* ((database (cdr (assoc (expand-file-name skk-dbm-jisyo) skk-dbm-alist)))
	 (midasi (if skk-use-numeric-conversion
		     (skk-num-compute-henkan-key skk-henkan-key)
		   skk-henkan-key))
	 (old-str (get-database midasi database))
	 (inhibit-quit t)
	 (henkan-buffer (current-buffer))
	 old-entry okurigana)
    (if (> skk-okuri-index-min -1)
	(setq word (skk-remove-common word)
	      midasi skk-henkan-key))
    (setq okurigana (or skk-henkan-okurigana skk-okuri-char))
    (with-current-buffer skk-dbm-working-buffer
      (let ((skk-okuri-ari-min (point-min)) ; dymmy
	    (skk-okuri-nasi-min (point-min)) ; dymmy
	    buffer-read-only)
	(setq skk-henkan-key midasi)
	(erase-buffer)
	(if (not old-str)
	    nil
	  (goto-char (point-min))
	  (insert old-str)
	  (goto-char (1+ (point-min)))
	  ;; skk-compute-henkan-lists $B$H(B skk-update-jisyo-1 $B$O!"%]%$%s%H$K0MB8$;(B
	  ;; $B$:!"J8;zNs(B ($B$"$k$$$O%j%9%H(B) $B$r0z?t$K<h$C$FJ8;zNs(B ($B$"$k$$$O%j%9%H(B)
	  ;; $B$rJV$9$h$&$J!"$b$C$H0lHLE*$J%i%$%V%i%j$K$9$k$Y$-$+$J!#(B
	  (setq old-entry (skk-compute-henkan-lists okurigana))
	  (erase-buffer))
	(skk-update-jisyo-1 okurigana word old-entry purge)
	(if (> 1 (buffer-size))
	    nil
	  (goto-char (point-min))
	  (search-forward " /" nil)
	  (put-database
	   skk-henkan-key
	   (buffer-substring-no-properties (1- (point)) (point-max))
	   database 'replace))
	(and skk-update-end-function
	     (funcall skk-update-end-function
		      henkan-buffer midasi okurigana word purge))))))

;;;###autoload
(defun skk-dbm-make-jisyo (file dbm &optional type subtype nomsg)
  (save-match-data
    (let ((start (current-time)))
      (or nomsg
	  (skk-message "SKK $B%G!<%?%Y!<%9<-=q$r:n@.$7$F$$$^$9(B..."
		       "Making SKK database jisyo..."))
      (or type (setq type (cond ((featurep 'berkeley-db) 'berkeley-db)
				((featurep 'gdbm) 'gdbm)
				(t 'dbm))))
      (and subtype (not (eq type 'berkeley-db))
	   (skk-error
	    "berkeley-db $B$G$J$$%G!<%?%Y!<%9$K(B subtype $B$r;XDj$9$k$3$H$O$G$-$^$;$s(B"
	    "Cannot specify subtype for a non berkeley-db database"))
      (save-excursion
	(set-buffer (get-buffer-create " *skk-work*"))
	(let ((dbase (or (open-database (expand-file-name dbm) type subtype "+")
			 ;; $B%b!<%I$r;XDj$9$k$H2?8N$+(B database $B$r(B open $B$G$-$J$$!#(B
			 ;;0600)
			 (skk-error "$B%G!<%?%Y!<%9(B %s $B$r3+$/$3$H$,$G$-$^$;$s(B"
				    "Cannot open database %s" dbm)))
	      enable-character-translation enable-character-unification
	      midasi cand)
	  (and (eq type 'gdbm) (fboundp 'set-database-property)
	       (set-database-property dbase 'fastmode t))
	  (buffer-disable-undo)
	  (erase-buffer)
	  ;; coding-system-for-read $B$,(B undecided $B$d(B automatic-conversion $B$8$cJ8;z%3!<(B
	  ;; $B%I8mH=Dj$K$J$C$F$7$^$&(B... (on XEmacs 21.2b7)$B!#(B
	  (insert-file-contents-as-coding-system 
	   (cond ((and skk-jisyo-code (coding-system-p skk-jisyo-code))
		  skk-jisyo-code)
		 ((and skk-jisyo-code (stringp skk-jisyo-code))
		  (cdr (assoc skk-jisyo-code skk-coding-system-alist)))
		 (t skk-dbm-coding-system))
	   (expand-file-name file))
	  (goto-char (point-min))
	  (while (= (forward-line 1) 0)
	    (beginning-of-line)
	    (if (or (looking-at ";") (eobp))
		nil
	      (setq midasi (buffer-substring-no-properties
			    (point) (search-forward " ")))
	      (and (string-match " $" midasi)
		   (setq midasi (substring midasi 0 (match-beginning 0))))
	      (setq cand (buffer-substring-no-properties
			  (point) (progn (end-of-line) (point))))
	      (put-database midasi cand dbase 'replace)))
	  (close-database dbase)
	  (or nomsg
	      (skk-message "SKK $B%G!<%?%Y!<%9<-=q$r:n@.$7$F$$$^$9(B...$B40N;!*(B"
			   "Making SKK database jisyo...done"))
	  (sit-for 1)
	  (or nomsg
	      (skk-message "$B%G!<%?%Y!<%9<-=q$r:n@.$9$k$N$K(B %s $BIC$+$+$j$^$7$?(B"
			   "It took %s minutes to make database jisyo"
			   (skk-time-difference start (current-time))))
	  (sit-for 2))))))

(defun skk-dbm-make-private-jisyo ()
  (save-match-data
    (let* ((type
	    (cond
	     ((and (featurep 'berkeley-db)
		   (string-match "\\.db$" skk-dbm-jisyo))
	      'berkeley-db)
	     ((and (featurep 'gdbm)
		   (string-match "\\.gdbm$" skk-dbm-jisyo))
	      'gdbm)
	     ((and (featurep 'dbm)
		   (not (string-match "\\.db$" skk-dbm-jisyo))
		   (not (string-match "\\.gdbm$" skk-dbm-jisyo)))
	      'dbm)
	     (t
	      (skk-error
	       "$B%G!<%?%Y!<%9<-=q$r:n@.$9$k$?$a$NE,Ev$J%?%$%W$r7h$a$k$3$H$,$G$-$^$;$s(B"
	       "Cannot find out proper type for making database jisyo"))))
	   (subtype (and (eq type 'berkeley-db) 'hash)))
      (skk-dbm-make-jisyo skk-jisyo skk-dbm-jisyo type subtype))))

(defun skk-dbm-close-all-database ()
  (let ((alist skk-dbm-alist)
	e)
    (condition-case nil
	(progn
	  (while alist
	    (and (setq e (car alist))
		 (database-live-p (cdr e))
		 (close-database  (cdr e)))
	    (setq alist (cdr alist)))
	  ;; set global alist to nil if successfully finished.
	  (setq skk-dbm-alist nil))
      (error
       ;; if error occurred, delete such element from skk-dbm-alist.
       (setq skk-dbm-alist (delq e skk-dbm-alist))))))

;; advices.
(defadvice close-database (around skk-ad activate)
  (let ((file (database-file-name (ad-get-arg 0))))
    (prog1
	ad-do-it
      ;; this should do in Emacs internal.
      (setq skk-dbm-alist (delq (assoc file skk-dbm-alist) skk-dbm-alist)))))
	
;;(add-hook 'kill-emacs-hook 'skk-dbm-close-all-database)

(run-hooks 'skk-dbm-load-hook)
(require 'product)
(product-provide (provide 'skk-dbm) (require 'skk-version))
;; Local Variables:
;; mode: auto-fill
;; fill-column: 78
;; End:

;;; skk-dbm.el ends here
