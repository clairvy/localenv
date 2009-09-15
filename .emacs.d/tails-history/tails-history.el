;;前出の tails-comint-histrory.el と同じようなことをミニバッファでやってくれるものです。 
;;例えば、 
;;      M-x forward-line
;;      M-x backward-line
;;      M-x f
;;と入力したとします。この時点で M-p を入力すると、標準ではミニバッファに 
;;      M-x backward-line
;;と表示されてしまいますが、tails-histrory.el を使っていれば 
;;      M-x forward-line
;;となってくれます。つまり最初の一文字が f であるようなヒストリを検索して表示してくれるわけです。 
;;つかうには、~/.emacs に (load-library "tails-histrory") と書いておきます。 


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
