;;; skk-mkmgk.el --- Make Mazegaki dictionary from SKK-JISYO.* -*- coding: euc-jp -*-

;; Copyright (C) 2001 NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>

;; Author: NAKAJIMA Mikio <minakaji@osaka.email.ne.jp>
;; Maintainer: SKK Development Team <skk@ring.gr.jp>
;; Version: $Id: skk-mkmgk.el,v 1.16 2010/08/02 15:30:56 skk-cvs Exp $
;; Keywords: japanese, mule, input method
;; Last Modified: $Date: 2010/08/02 15:30:56 $

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

;; M-x skk-make-mazegaki-dic ���ƽ��Ϥ��줿���� (��������Ǥ� TMP) ��
;;
;;   $ skkdic-expr TMP | skkdic-sort > NEWDICT
;;
;; �Ȳù�����Ⱥ����� SKK ���� (NEWDICT) ������夬��ޤ���
;;
;; <TODO>
;; - search okuri-ari entries, too.
;; - fix a know bug below.
;;
;; <KNOWN BUG>
;; + ��ͷ�� /��ͷ��/                                fixed.
;; + ���ꤨ /�ڤ골/�ڳ�/ �� ����골 /�ڤ골/      �Ȥꤢ������̾��ޤ����ϼΤƤ뤳�Ȥˤ��ޤ�����
;; + ���� /��/��/��/��/��/��/��/��/��/��/��/��/��/ �� �� /��/   fixed.
;; - �Ϥ󲾤��������� /Ⱦ���Ҳ�̾/                  �����ɤ����褦��ʤ��� (^^;;
;; - ������ /��/ �� ������ /��/                      (���ɤߤΰ�����ʸ�������ɤߤ�ʸ����Ʊ��) ditto...

;;; Code:

(eval-when-compile (require 'skk-macs))
(require 'skk)

(defvar skk-mkmgk-region-limit 10000
  "*�����꡼�����Υ����������ο���꾮�������ˡ��Х��ʥꥵ������ߤ��˥��������˰ܹԤ��롣
�Ŀͼ����ù�������ϡ�0 �����ꤹ�롣")

;;;###autoload
(defun skk-make-mazegaki-dic (dic &optional nomsg)
  "SKK �����μ��񤫤� TUT-Code �ʤɤǻȤ���뺮���񤭼�����롣
���Ϥ��줿���� (��������Ǥ� TMP) ��

  $ skkdic-expr TMP | skkdic-sort > NEWDICT

�Ȥ��Ʋù�����ɬ�פ��ꡣ"
  (interactive "f����ե�����:\nP ")
  (let ((cont t)
	(workbuf (get-buffer-create " *skkmkmgk working*"))
	max output ret)
    (unwind-protect
	(progn
	  (while cont
	    (setq output (read-file-name
			  "�ɤ��˽��Ϥ��ޤ���? "
			  nil
			  (convert-standard-filename "~/.skk-jisyo.tmp")))
	    (if (or (not (file-exists-p output))
		    (yes-or-no-p
		     (format "%s �˾�񤭤��Ƥ�����Ǥ���? " output)))
		(setq cont nil)))
	  (with-current-buffer workbuf (erase-buffer))
	  (with-temp-buffer
	    (insert-file-contents-as-coding-system
	     (cdr (assoc "euc" skk-coding-system-alist))
	     (expand-file-name dic))
	    (re-search-forward "^;; okuri-nasi entries\\.$")
	    ;; abbrev ����� skip
	    (delete-region (point-min) (progn (re-search-forward "^��")
					      (beginning-of-line)
					      (point)))
	    (setq ret (skk-make-mazegaki-dic-1 workbuf nomsg)))
	  (if ret
	      (with-current-buffer workbuf
		(write-region-as-coding-system
		 (cdr (assoc "euc" skk-coding-system-alist))
		;; only Emacs 21's write-region has 6th arg MUSTBENEW.
		 1 (point-max) output nil t nil)
		(or nomsg
		    (message "Making Mazegaki dictionary...100%% done")))))
      (kill-buffer workbuf))))

;;;###autoload
(defun skk-make-mazegaki-dic-region (min max &optional reference)
  "SKK �����μ��񤫤� TUT-Code �ʤɤǻȤ���뺮���񤭼�����롣
�ù�����������ȥХåե��˽񤭽Ф� `pop-to-buffer' ���롣
���Ϥ��줿�Хåե���ե����� (��������Ǥ� TMP) ����¸��

  $ skkdic-expr TMP | skkdic-sort > NEWDICT

�Ȥ��Ʋù�����ɬ�פ��ꡣ
���ץ���ʥ������ reference ����ꤹ��ȡ��ù��κݤ˸������뼭���
�̤˻��ꤹ�뤳�Ȥ��Ǥ��롣reference �λ��꤬�ʤ����ϡ��꡼�����
��Ǹ����Ǥ����Τߤ����Ѥ��Ƹ��Ф����ù����롣"
  (interactive "r\nP")
  (unwind-protect
      (let ((outbuf (get-buffer-create " *skkmkmgk working*"))
	    ret)
	(with-current-buffer outbuf (erase-buffer))
	(if reference
	    (progn
	      (setq reference (read-file-name "�ɤμ���򻲾Ȥ��ޤ���? "
					      nil nil 'mustmatch))
	      (with-current-buffer (get-buffer-create " *skkmkmgk working1*")
		(erase-buffer)
		(insert-file-contents-as-coding-system
		 (cdr (assoc "euc" skk-coding-system-alist))
		 (expand-file-name reference))
		(goto-char (point-min))
		(re-search-forward "^;; okuri-nasi entries\\.$")
		;; abbrev ����� skip
		(delete-region (point-min) (progn (re-search-forward "^��")
						  (beginning-of-line)
						  (point)))
		(setq reference (current-buffer)))))
	(save-excursion
	  (save-restriction
	    (narrow-to-region min max)
	    (goto-char min)
	    (setq ret (skk-make-mazegaki-dic-1 outbuf nil reference))))
	(if ret
	    (progn
	      (pop-to-buffer outbuf)
	      (goto-char (point-min)))))
    (kill-buffer reference)))

(defun skk-make-mazegaki-dic-1 (outbuf nomsg &optional reference)
  (let ((max (point-max))
	(cont t)
	header0 header-list candidates0 candidates1)
    (or reference (setq reference (current-buffer)))
    (while (and cont (not (eobp)))
      (or nomsg
	  (message "Making Mazegaki dictionary...%d%% done"
		   (* (/ (* (point) 100.00) (* max 100.00))
		      100.0)))
      (beginning-of-line)
      ;; �Ҥ餬�ʸ��Ф� e.x. "��������"
      (setq header0 (buffer-substring-no-properties
		     (point) (save-excursion (search-forward " ")
					     (backward-char 1) (point))))
      (if (= (skk-str-length header0) 1)
	  nil
	(search-forward " /")
	;; �Ҥ餬�ʸ��Ф��򥭡��ˤ�������ꥹ�� (2 ʸ���ʾ�) e.x. "����"
	(setq candidates0 (skk-mkmgk-filter
			   (car (skk-compute-henkan-lists nil))))
	(if (null candidates0)
	    nil
	  ;; �Ҥ餬�ʸ��Ф���ʬ��
	  (setq header-list (string-to-list header0))
	  (save-excursion ; have to restore point after searching
	    (set-buffer reference)
	    (let ((max (point-max)) (min (point-min))
		  (header-list1 header-list)
		  header1 n)
	      (while header-list1
		(setq header1 (char-to-string (car header-list1))
		      n 1)
		(while (and header1 (> (length header0) (length header1)))
		  (goto-char min)
		  ;; ʬ�򤷤��Ҥ餬�ʸ��Ф��� 1 ʸ���Ťĸ��Ф��ˤ��ƺƸ��� e.x. "����"
		  (if (not (setq candidates1 (skk-mkmgk-binary-search
					      header1 min max skk-mkmgk-region-limit)))
		      nil
		    (let ((can0 candidates0) key0 key1)
		      (while (and candidates1 can0)
			(if (not (string-match (car candidates1) (car can0)))
			    ;;(string-match header1 header0 (* skk-kanji-len (1- n)))))
			    (or (setq candidates1 (cdr candidates1))
				(setq can0 (cdr can0)))
			  ;; ʬ�򥭡� (e.x. ����) �ˤ����� e.x. "��" ��
			  ;; ���ꥸ�ʥ륭�� (e.x. ��������) �θ��� e.x.
			  ;; "����" �� match ������
			  (setq key0 (concat
				      (substring (car can0)
						 0 (match-beginning 0))
				      header1
				      (substring (car can0) (match-end 0))))
			  (if (string-match header1 header0)
			      (setq key1 (concat
					  (substring header0 0
						     (match-beginning 0))
					  (car candidates1)
					  (substring header0 (match-end 0)))))
			  (or (string= key0 (car can0))
			      (with-current-buffer outbuf
				(goto-char (point-max))
				;; ������ /����/
				(insert key0 " /" (car can0) "/\n")))
			  (or (string= key1 (car can0))
			      (with-current-buffer outbuf
				(goto-char (point-max))
				;; �꤫�� /����/
				(insert key1 " /" (car can0) "/\n")))
			  (setq candidates1 nil)))))
		  (if (> (skk-str-length
			  (mapconcat 'char-to-string header-list1 nil)) n)
		      ;; HEADER1 �򿭤Ф��ƺƸ���
		      ;; e.x. "��" �� "����" �� "������" �� "��������"
		      ;;      "��" �� "����" �� "������"
		      ;;      "��" �� "����"
		      ;;      "��"
		      (setq header1 (concat
				     header1
				     (char-to-string (nth n header-list1)))
			    n (1+ n))
		    (setq header1 nil)))
		;; ���� char ��Ƭ�ˤ�����������
		;; e.x. "��" �� "��" �� "��" �� "��"
		(setq header-list1 (cdr header-list1)))))))
      (setq cont (= (forward-line 1) 0)))
    (let ((ret (with-current-buffer outbuf (> (buffer-size) 0))))
      (prog1
	  ret
	(or nomsg
	    (if ret
		(message "Making Mazegaki dictionary...100%% done")
	      (message "No entries of Mazegaki dictionary")))))))

(defun skk-mkmgk-filter (list &optional onecharacter)
  ;; Optional arg ONECHARACTER means put out one character.
  (delete "" ; there was a bug in SKK-JISYO.L...
	  (delq nil
		(mapcar
		 (function
		  (lambda (word)
		    (setq word (if (and
				    (not (string-match "[��-��-��]" word))
				    ;; �Ѹ�� skip
				    (not (string-match "^[a-zA-Z]+$" word)))
				   (if (string-match ";" word)
				       (substring word 0 (match-beginning 0))
				     word)))
		    (if (and (not onecharacter) (= (skk-str-length word) 1))
			(setq word nil))
		    word))
		 list))))

(defun skk-mkmgk-binary-search (key min max limit)
  (let ((case-fold-search nil)
        size p)
    (if (> limit 0)
	(while (progn (setq size (- max min)) (> size limit))
	  (goto-char (+ min (/ size 2)))
	  (beginning-of-line)
	  (setq p (point))
	  (if (string< key (buffer-substring-no-properties
			    p (1- (search-forward " "))))
	      (setq max p)
	    (setq min p))))
    (goto-char min)
    (beginning-of-line)
    (if (re-search-forward (concat "^" key " /") max 'noerror)
	(skk-mkmgk-filter (car (skk-compute-henkan-lists nil))))))

(provide 'skk-mkmgk)

;;; skk-mkmgk.el ends here
