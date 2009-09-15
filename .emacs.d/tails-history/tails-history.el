;;���Ф� tails-comint-histrory.el ��Ʊ���褦�ʤ��Ȥ�ߥ˥Хåե��Ǥ�äƤ�����ΤǤ��� 
;;�㤨�С� 
;;      M-x forward-line
;;      M-x backward-line
;;      M-x f
;;�����Ϥ����Ȥ��ޤ������λ����� M-p �����Ϥ���ȡ�ɸ��Ǥϥߥ˥Хåե��� 
;;      M-x backward-line
;;��ɽ������Ƥ��ޤ��ޤ�����tails-histrory.el ��ȤäƤ���� 
;;      M-x forward-line
;;�ȤʤäƤ���ޤ����Ĥޤ�ǽ�ΰ�ʸ���� f �Ǥ���褦�ʥҥ��ȥ�򸡺�����ɽ�����Ƥ����櫓�Ǥ��� 
;;�Ĥ����ˤϡ�~/.emacs �� (load-library "tails-histrory") �Ƚ񤤤Ƥ����ޤ��� 


(define-key minibuffer-local-map [?\M-p] 'tails-previous-history-element)
(define-key minibuffer-local-completion-map [?\M-p] 'tails-previous-history-element)
(define-key minibuffer-local-must-match-map [?\M-p] 'tails-previous-history-element)
(define-key minibuffer-local-ns-map [?\M-p] 'tails-previous-history-element)

(define-key minibuffer-local-map [?\M-n] 'tails-next-history-element)
(define-key minibuffer-local-completion-map [?\M-n] 'tails-next-history-element)
(define-key minibuffer-local-must-match-map [?\M-n] 'tails-next-history-element)
(define-key minibuffer-local-ns-map [?\M-n] 'tails-next-history-element)


(defun tails-previous-history-element ()
  (interactive)
  (tails-history-element -1))

(defun tails-next-history-element ()
  (interactive)
  (tails-history-element 1))

(defun tails-history-element (direction)
  (let ((prefix (buffer-substring (point-at-bol) (point)))
	(pt (point))
	(history (symbol-value minibuffer-history-variable))
	(position minibuffer-history-position))
    (while (not (or (progn
		      (setq position (- position direction))
		      (and (or (< position 1)
			       (> position (length history)))
			   (progn (ding)
				  t)))
		    (let ((element (nth (1- position) history)))
		      (setq minibuffer-history-position position)
		      (and minibuffer-history-sexp-flag
			   (setq element (prin1-to-string element)))
		      (and (<= (length prefix) (length element))
			   (string= prefix (substring element 0 (length prefix)))
			   (progn
			     (delete-region (line-beginning-position) (line-end-position))
			     (insert element)
			     (goto-char pt)
			     t))))))))


;; EOF
