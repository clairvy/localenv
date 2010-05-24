;;; -*- mode: emacs-lisp; coding: utf-8-unix; -*-

;;; より一般度の高いものを上にする方針．
;;; 途中でコケて，C-h が効かなかったらアレでしょ？

;;; キーバインド
(define-key global-map "\C-h" 'delete-backward-char) ; 削除
(define-key global-map "\M-?" 'help-for-help)        ; ヘルプ
(define-key global-map "\M-g" 'goto-line)

(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;;; コマンドキィをMeta キィとして利用
(setq mac-commandkey-is-meta t)

(defvar home (getenv "HOME"))
(defvar hostname nil)
(let ((local-config-file (expand-file-name ".emacs.local" home)))
  (load local-config-file t))
(setenv "PATH" (concat (expand-file-name "local/bin" home) ":"
                       (getenv "PATH")))
(defvar os-type nil)
(cond ((string-match "apple-darwin" system-configuration)
       (setq os-type 'mac))
      (t 'unknown))

;;; apel
(if (or (string-equal hostname "mbp1")
	(= (floor (string-to-int emacs-version)) 23))
    (setq load-path (cons (concat home "/.emacs.d/apel")
			  load-path)))

;;; dired HACK
(setq load-path (cons (concat home "/.emacs.d/dired")
                      load-path))
(require 'sorter)
(defface my-face-f-2 '((t (:foreground "GreenYellow"))) nil)
(defvar my-face-f-2 'my-face-f-2)
(defun my-dired-today-search (arg)
  "Fontlock search function for dired."
  (search-forward-regexp
;   (concat (format-time-string "%Y-%m-%d" (current-time)) " [0-9]....") arg t))
   (concat (format-time-string "%b %e" (current-time)) " [0-9]....") arg t))
(add-hook 'dired-mode-hook
          '(lambda ()
             (font-lock-add-keywords
              major-mode
              (list
               '(my-dired-today-search . my-face-f-2)
               ))))
(defun ls-lisp-handle-switches (file-alist switches)
  ;; FILE-ALIST's elements are (FILE . FILE-ATTRIBUTES).
  ;; Return new alist sorted according to SWITCHES which is a list of
  ;; characters.  Default sorting is alphabetically.
  (let (index)
    (setq file-alist
          (sort file-alist
                (cond
                 ((memq ?S switches)    ; sorted on size
                  (function
                   (lambda (x y)
                     ;; 7th file attribute is file size
                     ;; Make largest file come first
                     (if (equal (nth 0 (cdr y))
                                (nth 0 (cdr x)))
                         (< (nth 7 (cdr y))
                            (nth 7 (cdr x)))
                       (nth 0 (cdr x))))))
                 ((memq ?t switches)    ; sorted on time
                  (setq index (ls-lisp-time-index switches))
                  (function
                   (lambda (x y)
                     (if (equal (nth 0 (cdr y))
                                (nth 0 (cdr x)))
                         (ls-lisp-time-lessp (nth index (cdr y))
                                             (nth index (cdr x)))
                       (nth 0 (cdr x))
                       ))))
                 ((memq ?X switches)    ; sorted on ext
                  (function
                   (lambda (x y)
                     (if (equal (nth 0 (cdr y))
                                (nth 0 (cdr x)))
                         (string-lessp (file-name-extension (upcase (car x)))
                                       (file-name-extension (upcase (car y))))
                       (nth 0 (cdr x))))))
                 (t                     ; sorted alphabetically
                  (if ls-lisp-dired-ignore-case
                      (function
                       (lambda (x y)
                         (if (equal (nth 0 (cdr y))
                                    (nth 0 (cdr x)))
                             (string-lessp (upcase (car x))
                                           (upcase (car y)))
                           (nth 0 (cdr x)))))
                    (function
                     (lambda (x y)
                       (if (equal (nth 0 (cdr y))
                                  (nth 0 (cdr x)))
                           (string-lessp (car x)
                                         (car y))
                         (nth 0 (cdr x)))))
                    )))))
    )

  (if (memq ?r switches)                ; reverse sort order
      (setq file-alist (nreverse file-alist)))
  file-alist)

;;; elscreen
(cond ((= (string-to-int emacs-version) 23) ; carbon だと同梱
       (setq load-path (cons (concat home "/.emacs.d/elscreen")
                             load-path))
       (load "elscreen" "ElScreen" t)))

;;; anything (yasnippet / auto-install)
(when (>= emacs-major-version 22)
  (add-to-list 'load-path (expand-file-name ".emacs.d/auto-install-mode" home))
  (add-to-list 'load-path (expand-file-name ".emacs.d/yasnippet" home))
  (add-to-list 'load-path (expand-file-name ".emacs.d/anything" home))
  (add-to-list 'load-path (expand-file-name ".emacs.d/auto-install" home))
  (require 'auto-install)
                                        ;(auto-install-update-emacswiki-package-name t)
  (require 'anything-auto-install)
;;; yasnippet
  (setq yas/trigger-key "TAB")
  (require 'yasnippet-config)
  (yas/setup (expand-file-name ".emacs.d/yasnippet" home))
;;; anything
  (require 'anything-config)
  (require 'anything)
  ;; (install-elisp "http://svn.coderepos.org/share/lang/elisp/anything-c-yasnippet/anything-c-yasnippet.el")
  (require 'anything-c-yasnippet)         ;[2008/03/25]
  (setq anything-c-yas-space-match-any-greedy t) ;[default: nil]
  (global-set-key (kbd "C-c y") 'anything-c-yas-complete)
  (yas/initialize)

  (add-to-list 'anything-sources 'anything-c-source-emacs-commands)
  (define-key global-map (kbd "C-;") 'anything)
  (require 'anything-show-completion)
  (require 'anything-kyr-config)
  (require 'anything-match-plugin)
  (define-key anything-map "\C-k" (lambda () (interactive) (delete-minibuffer-contents)))
  (setq anything-map-C-j-binding 'anything-select-3rd-action)
  ;; [2008/04/02]
  (define-key anything-map [end] 'anything-scroll-other-window)
  (define-key anything-map [home] 'anything-scroll-other-window-down)
  (define-key anything-map [next] 'anything-next-page)
  (define-key anything-map [prior] 'anything-previous-page)
  (define-key anything-map [delete] 'anything-execute-persistent-action)
  ;; [2008/08/22]
  (define-key anything-map (kbd "C-:") 'anything-for-create-from-anything)
  ;; (@> " frequently used commands - keymap")
  (define-key anything-isearch-map "\C-m"  'anything-isearch-default-action)
  (setq anything-enable-digit-shortcuts nil)
  (define-key anything-map (kbd "M-1") 'anything-select-with-digit-shortcut)
  (define-key anything-map (kbd "M-2") 'anything-select-with-digit-shortcut)
  (define-key anything-map (kbd "M-3") 'anything-select-with-digit-shortcut)
  (define-key anything-map (kbd "M-4") 'anything-select-with-digit-shortcut)
  (define-key anything-map (kbd "M-5") 'anything-select-with-digit-shortcut)
  (define-key anything-map (kbd "M-6") 'anything-select-with-digit-shortcut)
  (define-key anything-map (kbd "M-7") 'anything-select-with-digit-shortcut)
  (define-key anything-map (kbd "M-8") 'anything-select-with-digit-shortcut)
  (define-key anything-map (kbd "M-9") 'anything-select-with-digit-shortcut)
  (define-key anything-map (kbd "C-SPC") 'anything-toggle-visible-mark)
  (define-key anything-map "\M-[" 'anything-prev-visible-mark)
  (define-key anything-map "\M-]" 'anything-next-visible-mark)
  (define-key anything-map "\C-a" 'beginning-of-line)
  ;; (install-elisp-from-emacswiki "anything-dabbrev-expand.el")
  (require 'anything-dabbrev-expand)
  (define-key anything-dabbrev-map [(control ?@)] 'anything-dabbrev-find-all-buffers)
  (setq anything-dabbrev-input-idle-delay 0.0)
  (setq anything-dabbrev-idle-delay 1.0)
  (setq anything-dabbrev-expand-candidate-number-limit 20)
  (setq anything-dabbrev-expand-strategies
        '(;; anything-dabbrev-expand--first-partial-dabbrev
          anything-dabbrev-expand--anything))
  (setq anything-dabbrev-sources
        '(anything-dabbrev-partial-source
          anything-c-source-complete-emacs-commands
          anything-c-source-complete-emacs-functions
          anything-c-source-complete-emacs-variables
          anything-c-source-complete-emacs-other-symbols
          anything-dabbrev-all-source))
  (require 'anything-complete nil t)
  (anything-read-string-mode 1))

;;; session
(setq load-path (cons (concat home "/.emacs.d/session/lisp")
                      load-path))
(require 'session)
(add-hook 'after-init-hook 'session-initialize)
;;; tails-history
(setq load-path (cons (concat home "/.emacs.d/tails-history")
                      load-path))
(load-library "tails-history")

;;; color-moccur
(setq load-path (cons (concat home "/.emacs.d/occur")
                      load-path))
(require 'color-moccur)
(require 'moccur-edit)
(setq *moccur-buffer-name-exclusion-list*
      '(".+TAGS.+" "*Completions*" "*Messages*"
        "newsrc.eld" ".bbdb"))
(setq moccur-split-word t)
(setq dmoccur-use-list t)
(setq dmoccur-use-project t)
(setq dmoccur-list
      '(
        ("dir" default-directory (".*") dir)
        ))
(global-set-key "\C-x\C-o" 'occur-by-moccur)
(define-key Buffer-menu-mode-map "O" 'Buffer-menu-moccur)
;(define-key dired-mode-map "O" 'dired-do-moccur)
(global-set-key "\C-c\C-x\C-o" 'moccur)
(global-set-key "\M-f" 'grep-buffers)
(global-set-key "\C-c\C-o" 'search-buffers)

;;; skk
(setq load-path (cons (concat home "/.emacs.d/ddskk")
                      load-path))
(setq skk-use-azik t)
(setq skk-dcomp-activate t)
(setq skk-dcomp-multiple-activate t)
(setq skk-dcomp-multiple-rows 10)
(setq skk-show-annotation t)
(setq skk-annotation-show-wikipedia-url t)
(setq skk-use-look t)
(setq skk-auto-insert-paren nil)
(setq skk-henkan-strict-okuri-precedence t)
(cond ((or (string-equal hostname "canaan")
           (string-equal hostname "vh001"))
       (setq skk-azik-keyboard-type 'jp106))
      (t
       (setq skk-azik-keyboard-type 'en)))
(cond ((string-equal hostname "vh001")
       (setq skk-large-jisyo (expand-file-name ".emacs.d/ddskk/dic/SKK-JISYO.L" home)))
      (t
       (add-hook 'skk-mode-hook
                 (lambda ()
                   (setq skk-server-host "localhost")
                   (setq skk-server-portnum 1178)
                   (setq skk-auto-insert-paren t)
                   (setq skk-kutouten-type 'en)))))
(require 'skk-setup)
(require 'skk-study)

;;; コントロールキーをシステムにとられないようにする
(setq mac-pass-control-to-system nil)

;;; Localeに合わせた環境の設定
(set-locale-environment nil)
(set-default-coding-systems 'utf-8-unix)

;;; 対応する括弧を光らせる。
(show-paren-mode t)

;;; ツールバーを消す
(tool-bar-mode 0)

;;; バックアップファイルを作らない
(setq backup-inhibited t)

;;; 終了時にオートセーブファイルを消す
(setq delete-auto-save-files t)

;;; カーソルの位置が何文字目かを表示する
(column-number-mode t)

;;; カーソルの位置が何行目かを表示する
(line-number-mode t)

;;; スクロールを一行ずつにする
(setq scroll-step 1)

;;; スクロールバーを右側に表示する
(set-scroll-bar-mode nil)

;;; モードラインに時間を表示する
(setq display-time-day-and-date 't)
(display-time)

;;; 同名のファイルを開いたときなどでファイル名がわかりやすくなる設定
(require 'uniquify)
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;;; elscreen
(load "elscreen" "Elscreen" t)

;;; git(egg)
(setq egg-git-command "/opt/local/bin/git")
(setq load-path (cons (concat home "/.emacs.d/egg")
                      load-path))
(require 'egg)

;;; gist
(setq load-path (cons (concat home "/.emacs.d/gist")
                      load-path))
(require 'gist)

;;; auto-complete
(add-to-list 'load-path (expand-file-name ".emacs.d/auto-complete" home))
(require 'auto-complete nil t)
(global-auto-complete-mode t)
(define-key ac-complete-mode-map "\C-n" 'ac-next)
(define-key ac-complete-mode-map "\C-p" 'ac-previous)
;;; - http://d.hatena.ne.jp/rubikitch/20081109/autocomplete
(defvar ac-chars "0-9a-zA-Z¥¥?!_-")
(defun ac-candidate-substrings-in-buffer ()
  "partial dabbrev for auto-complete."
  (when (> (length ac-target) 1)
    (let ((inhibit-quit nil)            ;for debug
          (i 0)
          candidate
          candidates
          (regexp (concat (regexp-quote ac-target) "[" ac-chars "]*¥¥b")))
      (save-excursion
        ;; search backward
        (goto-char ac-point)
        (while (and (< i ac-candidate-max)
                    (re-search-backward regexp nil t))
          (skip-chars-backward ac-chars)
          (setq candidate (buffer-substring-no-properties
                           (point)
                           (match-end 0)))
          (when (and (>= (length candidate) 3)
                     (not (member candidate candidates)))
            (setq candidates (cons candidate candidates))
            (setq i (1+ i))))
        ;; search backward
        (goto-char (+ ac-point (length ac-target)))
        (while (and (< i ac-candidate-max)
                    (re-search-forward regexp nil t))
          (goto-char (match-beginning 0))
          (skip-chars-backward ac-chars)
          (setq candidate (buffer-substring-no-properties
                           (point)
                           (match-end 0)))
          (goto-char (match-end 0))
          (when (and (>= (length candidate) 3)
                     (not (member candidate candidates)))
            (setq candidates (cons candidate candidates))
            (setq i (1+ i))))
        (nreverse candidates)))))

(setq ac-source-substrings-in-buffer
      '((candidates  . ac-candidate-substrings-in-buffer)
        (limit . 4)))

;; pit
(add-to-list 'load-path (expand-file-name ".emacs.d/pit" home))
(require 'pit)

;; autoinsert
(require 'autoinsert)
(auto-insert-mode)

;; junk buffer and file
;; - http://d.hatena.ne.jp/rubikitch/20080923/1222104034
(defun open-junk-file ()
  (interactive)
  (let* ((file (expand-file-name
                (format-time-string
                 "%Y/%m/%Y-%m-%d-%H%M%S." (current-time))
                "~/memo/junk/"))
         (dir (file-name-directory file)))
    (make-directory dir t)
    (find-file-other-window (read-string "Junk Code: " file))))

;; *scratch*バッファを kill できないように
;; - http://www.bookshelf.jp/soft/meadow_29.html#SEC388
(defun my-make-scratch (&optional arg)
  (interactive)
  (progn
    ;; "*scratch*" を作成して buffer-list に放り込む
    (set-buffer (get-buffer-create "*scratch*"))
    (funcall initial-major-mode)
    (erase-buffer)
    (when (and initial-scratch-message (not inhibit-startup-message))
      (insert initial-scratch-message))
    (or arg (progn (setq arg 0)
                   (switch-to-buffer "*scratch*")))
    (cond ((= arg 0) (message "*scratch* is cleared up."))
          ((= arg 1) (message "another *scratch* is created")))))
(defun my-buffer-name-list ()
  (mapcar (function buffer-name) (buffer-list)))
(add-hook 'kill-buffer-query-functions
          ;; *scratch* バッファで kill-buffer したら内容を消去するだけにする
          (function (lambda ()
                      (if (string= "*scratch*" (buffer-name))
                          (progn (my-make-scratch 0) nil)
                        t))))
(add-hook 'after-save-hook
          ;; *scratch* バッファの内容を保存したら *scratch* バッファを新しく作る
          (function (lambda ()
                      (unless (member "*scratch*" (my-buffer-name-list))
                        (my-make-scratch 1)))))

;; windmove
(windmove-default-keybindings)
(global-set-key (kbd "C-s-h") 'windmove-left)
(global-set-key (kbd "C-s-j") 'windmove-down)
(global-set-key (kbd "C-s-k") 'windmove-up)
(global-set-key (kbd "C-s-l") 'windmove-right)

;;;
;;; Programming Languages
;;;

;;; OCaml
(setq load-path (cons (concat home "/.emacs.d/tuareg-mode")
                      load-path))
(setq auto-mode-alist (cons '("\\.ml\\w?" . tuareg-mode) auto-mode-alist))
(autoload 'tuareg-mode "tuareg" "Major mode for editing Caml code" t)
(autoload 'camldebug "camldebug" "Run the Caml debugger" t)

;;; Coq (ProofGeneral)
(load "/opt/local/share/ProofGeneral/generic/proof-site.el" t)

;;; JavaScript
(setq load-path (cons (concat home "/.emacs.d/js2-mode/build")
                      load-path))
(autoload 'js2-mode "js2-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;; org-mode
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)

;;; howm
(define-key global-map "\C-c,," 'howm-menu)
(when (> (string-to-int emacs-version) 22); in MacOS Emacs Cocoa
  (add-to-list 'load-path (expand-file-name ".emacs.d/howm" home))
  (autoload 'howm-menu "howm-mode" "Hitori Otegaru Wiki Modoki" t))
(cond ((string-equal hostname "canaan") ; for private pc
       (setq howm-directory (expand-file-name "Dropbox/howm" home)))
      (t
       (setq howm-directory "/Volumes/共有フォルダ/社員フォルダ/永谷/howm/")))
(add-to-list 'auto-mode-alist '("\\.howm$" . org-mode))

;;; rd-mode
(setq load-path (cons (concat home "/.emacs.d/rd-mode")
                      load-path))
(autoload 'rd-mode "rd-mode" "major mode for ruby document formatter RD" t)
(add-to-list 'auto-mode-alist '("\\.rd$" . rd-mode))
(require 'rd-mode-plus)

;;; perl
(setq load-path (cons (concat home "/.emacs.d/cperl-mode")
                      load-path))
(setq load-path (cons (concat home "/.emacs.d/cperl-calculate-indent")
                      load-path))
(autoload 'cperl-mode "cperl-mode" "alternate mode for editing Perl programs" t)
(fset 'perl-mode 'cperl-mode)
(add-hook 'cperl-mode-hook
          '(lambda ()
             (setq indent-tabs-mode nil)
             (setq cperl-font-lock t)
             (cperl-set-style "BSD")
             (setq tab-width 8)
             (c-set-offset 'arglist-intro '++)
             (c-set-offset 'arglist-close 0)
             ;; from Perl Best Practice
             '(cperl-indent-level 4)
             '(cperl-continued-statement-offset 4)
             '(cperl-close-paren-offset -4)
             '(cperl-indent-parens-as-block t)
             '(cperl-tab-always-indent t)
             ))
;;; perl flymake
;;; - http://d.hatena.ne.jp/sun-basix/20080705/1215278204
;;; - http://d.hatena.ne.jp/antipop/20080701/1214838633
(require 'flymake)
(push '("\\.pl$" flymake-perl-init) flymake-allowed-file-name-masks)
(push '("\\.pm$" flymake-perl-init) flymake-allowed-file-name-masks)
(push '("\\.t$" flymake-perl-init) flymake-allowed-file-name-masks)
(push '("\\(.*\\) at \\([^ \n]+\\) line \\([0-9]+\\)[,.\n]" 2 3 nil 1) flymake-err-line-patterns)
(add-hook 'cperl-mode-hook
 '(lambda ()
;    (set-perl5lib)
    (flymake-mode)))

;;; for flymake
;;; - http://www.credmp.org/index.php/2007/07/20/on-the-fly-syntax-checking-java-in-emacs/
;;; copy - http://d.hatena.ne.jp/khiker/20070720/emacs_flymake
(define-key global-map "\C-ce" 'credmp/flymake-display-err-minibuf)
(defun credmp/flymake-display-err-minibuf ()
  "Displays the error/warning for the current line in the minibuffer"
  (interactive)
  (let* ((line-no             (flymake-current-line-no))
         (line-err-info-list  (nth 0 (flymake-find-err-info flymake-err-info line-no)))
         (count               (length line-err-info-list))
         )
    (while (> count 0)
      (when line-err-info-list
        (let* ((file       (flymake-ler-file (nth (1- count) line-err-info-list)))
               (full-file  (flymake-ler-full-file (nth (1- count) line-err-info-list)))
               (text (flymake-ler-text (nth (1- count) line-err-info-list)))
               (line       (flymake-ler-line (nth (1- count) line-err-info-list))))
          (message "[%s] %s" line text)
          )
        )
      (setq count (1- count)))))

;; perl-completion
(setq load-path (cons (concat home "/.emacs.d/perl-completion")
                      load-path))
(add-hook 'cperl-mode-hook
          (lambda ()
            (require 'perl-completion)
            (perl-completion-mode t)))
(add-hook  'cperl-mode-hook
           (lambda ()
             (when (require 'auto-complete nil t) ; no error whatever auto-complete.el is not installed.
               (auto-complete-mode t)
               (make-variable-buffer-local 'ac-sources)
               (setq ac-sources
                     '(ac-source-perl-completion)))))
(add-hook 'cperl-mode-hook
          (lambda ()
            (require 'cperl-calculate-indent)))

;;; scala
(add-to-list 'load-path (expand-file-name ".emacs.d/scala-mode" home))
(require 'scala-mode-auto)
;;; with yasnippet
(setq yas/my-directory
      (expand-file-name ".emacs.d/scala-mode/contrib/yasnippet/snippets" home))
(yas/load-directory yas/my-directory)
(add-hook 'scala-mode-hook
          '(lambda ()
             (yas/minor-mode-on)
             ))
;;; flymake - http://d.hatena.ne.jp/kiris60/20091004/1254586627
(require 'scala-mode-flymake)
;;; scaladoc
(let ((ih (expand-file-name "s/app/InteractiveHelp/" home)))
  (setq scala-interpreter
        (concat "scala -cp "
                (concat ih "interactive-help-1.0.jar")
                " -i "
                (concat ih "import.scala"))))
;(setenv "SCALA_DOC_HELP" (expand-file-name "s/app/InteractiveHelp/scala-2.7.5-apidocs-fixed/" home))
(defun my-scala-doc ()
  "カーソル付近のScala doc を引く"
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (if bounds
        (let* ((beg (car bounds))
               (end (cdr bounds)))
          (my-scala-doc-1 (concat "\"" (buffer-substring beg end) "\""))))))
(defun my-scala-doc-1 (&optional arg)
  "引数指定をして直接scala doc を引く"
  (interactive "sscala help args(ex \"List\")(0: ")
  (scala-check-interpreter-running)
  (comint-send-string scala-inf-buffer-name (concat "h(" arg ")\n")))
(add-hook 'scala-mode-hook
          '(lambda ()
             (define-key scala-mode-map "\C-ch" 'my-scala-doc)
             (define-key scala-mode-map "\C-cH" 'my-scala-doc-1)
             ))

;;; tiarra-conf
(setq load-path (cons (concat home "/.emacs.d/tiarra-conf")
                      load-path))
(load-library "tiarra-conf")

;;; schema
(setq scheme-program-name "gosh -i")
(autoload 'scheme-mode "cmuscheme" "Major mode for Scheme." t)
(autoload 'run-scheme "cmuscheme" "Run an inferior Scheme process." t)

;;; haskell
(add-to-list 'load-path (expand-file-name ".emacs.d/haskell-mode" home))
(load-library "haskell-site-file")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
;;(add-hook 'haskell-mode-hook 'turn-on-haskell-simple-indent)

;;; erlang
(add-to-list 'load-path (expand-file-name ".emacs.d/distel/elisp" home))
(require 'erlang-start)
(require 'distel)
(distel-setup)

;;; php-mode
(add-to-list 'load-path (expand-file-name ".emacs.d/php-mode" home))
(require 'php-mode)
(add-hook 'php-mode-hook
          '(lambda () (define-abbrev php-mode-abbrev-table "ex" "extends")))

;;; markdown-mode
(add-to-list 'load-path (expand-file-name ".emacs.d/markdown-mode" home))
(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.mkdn" . markdown-mode))
             

;;; actionscript-mode
(add-to-list 'load-path (expand-file-name ".emacs.d/actionscript-mode" home))
(autoload 'actionscript-mode "actionscript-mode.el")
(add-to-list 'auto-mode-alist '("\\.as$" . actionscript-mode))

;;; clojure
(setq swank-clojure-jar-path "/opt/local/share/java/clojure/lib/clojure.jar")
(add-to-list 'load-path (expand-file-name ".emacs.d/swank-clojure/src/emacs" home))
(add-to-list 'load-path (expand-file-name ".emacs.d/clojure-mode" home))
(add-to-list 'load-path (expand-file-name ".emacs.d/slime" home))
(autoload 'clojure-mode "clojure-mode" "A major mode for Clojure" t)
(require 'swank-clojure-autoload)
(require 'slime)
(slime-setup)

;; android-mode
(setq android-mode-sdk-dir (expand-file-name "local/android" home))
(add-to-list 'load-path (expand-file-name ".emacs.d/android-mode" home))
(require 'android-mode)
(android-defun-ant-task "run-tests")
(add-to-list 'android-mode-keys '("t" . android-ant-run-tests))

;; R-lang
(add-to-list 'load-path (expand-file-name ".emacs.d/ess/lisp" home))
(require 'ess-site)

;;;
;;; Application
;;;

;;; el-expectations
;; M-x auto-install-from-emacswiki RET el-expectations.el
;(require 'el-expectations)

;;; twittering-mode
(setq load-path (cons (concat home "/.emacs.d/twittering-mode")
                      load-path))
(require 'twittering-mode)
(add-hook 'twittering-mode-hook
          (lambda ()
            (let ((alist (pit/get 'twitter.com
                                  '(require ((username . "Your twitter username")
                                             (password . "Your twitter password"))))))
              (setq twittering-username (cdr (assoc 'username alist)))
              (setq twittering-password (cdr (assoc 'password alist))))))


;;; typing-outputz
(add-to-list 'load-path (expand-file-name ".emacs.d/typing-outputz" home))
(require 'typing-outputz)
(global-typing-outputz-mode t)
(add-to-list 'typing-outputz-counted-commands
             'skk-insert)
(defun to-growl-record-buffer ()
  (let ((growl-command-template
         (cond
          ((eq 0 (shell-command "which notify-send"))
           "notify-send -i gnome-emacs \"Emacs notify\" \"%s\"")
          (t nil))))
    (when growl-command-template
      (shell-command
       (format growl-command-template
               (format "Outputz: %d chars"
                       typing-outputz-buffer-local-counter))))))
(add-hook 'typing-outputz-record-buffer-hook
          'to-growl-record-buffer nil)

;;; for simple-hatena-mode
(cond ((>= (string-to-int emacs-version) 23)
       (setq load-path (cons (expand-file-name ".emacs.d/html-helper-mode" home)
                             load-path))
       (autoload 'html-helper-mode "html-helper-mode" "Yay HTML" t)))

(setq load-path (cons (expand-file-name ".emacs.d/simple-hatena-mode" home)
                      load-path))
(require 'simple-hatena-mode)
(setq simple-hatena-root (expand-file-name ".hatena" home))
;(require 'hatenahelper-mode)
;(add-hook 'simple-hatena-mode-hook
;          '(lambda ()
;             (hatenahelper-mode 1)))

;;; transrate twitter-id notation to hatena-notation
;;; and transrate hashtag(#) to hatena-notation
(defun my-hatena-convert-twitter-notation-to-link ()
  "カーソル付近の@ 付きID をはてな記法のリンクに変換する．
又は，カーソル付近の# 付き文字列をはてな記法のリンクに変換する．
"
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (if bounds
        (let* ((beg (car bounds))
               (pre (- beg 1))
               (end (cdr bounds))
               (prestr (buffer-substring pre (+ 1 pre)))
               (idstr (buffer-substring beg end)))
          (cond ((string-equal "@" prestr)
                 (delete-region pre end)
                 (insert (my-hatena-convert-twitter-id-to-link idstr)))
                ((string-equal "#" prestr)
                 (delete-region pre end)
                 (insert (my-hatena-convert-twitter-hashtag-to-link idstr))))))))

(defun my-hatena-convert-twitter-id-to-link (id)
  "twitter の id をはてな記法のリンクに変換する"
  (if id
      (concat "[http://twitter.com/" id ":title=@" id "]")))

(defun my-hatena-convert-twitter-hashtag-to-link (tag)
  "twitter の hashtag をはてな記法のリンクに変換する"
  (if tag
      (concat "[http://search.twitter.com/search?q=%23" tag "&lang=ja:title=#" tag "]"))) ;; %23 = # だよ

;;; transrate URL to hatena-id notation
(defun my-hatena-convert-url-to-id ()
  "カーソル付近のURLをはてなID記法に変換する。"
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'url)))
    (if bounds
        (let* ((beg (car bounds))
               (end (cdr bounds))
               (idstr (my-hatena-convert-url-to-id-1
                       (buffer-substring beg end))))
          (if idstr
              (progn
                (delete-region beg end)
                (insert idstr)))))))

(defun my-hatena-convert-url-to-id-1 (url)
  "与えられた URL をはてなID記法に変換する"
  (let* ((delim ":")
         (urls (split-string url "/"))
         (authority (nth 2 urls))
         (user (nth 3 urls))
         (date (nth 4 urls))
         (item (nth 5 urls))
         (idstr (concat "id:" user)))
    (if (string-match "\\<\\(.\\)\\.hatena\\.ne\\.jp\\>" authority)
        (setq idstr (concat (match-string 1 authority) delim idstr)))
    (if (and date (not (string-equal date "")))
        (setq idstr (concat idstr delim date)))
    (if (and item (not (string-equal item "")))
        (setq idstr (concat idstr delim item)))
    idstr))

(defun my-hatena-convert-amazon ()
  "カーソル付近の値をはてなamazon記法に変換する。"
  (interactive)
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (if bounds
        (let* ((beg (car bounds))
               (end (cdr bounds))
               (amazon-str (my-hatena-convert-amazon-1
                       (buffer-substring beg end))))
          (if amazon-str
              (progn
                (delete-region beg end)
                (insert amazon-str)))))))

(defun my-hatena-convert-amazon-1 (id &optional title)
  "id でamazon へのリンクをつくる"
  (let ((url '("[http://www.amazon.co.jp/dp/")))
    (if (stringp id)
        (setq url (append url (list id "/search042-22/ref=nosim/")))
      (error "Invalid argument %s" x))
    (if (stringp title)
        (setq url (append url (list ":title=" title "]"))))
    (setq url (append url '("]")))
    (mapconcat (lambda (x) x) url "")))

(defun my-hatena-get-near-word (string-maker)
;  (let ((bounds (bounds-of-thing-at-point 'word)))
  (let ((bounds (bounds-of-thing-at-point 'filename))) ; :: のためにこっちに
    (if bounds
        (let* ((beg (car bounds))
               (end (cdr bounds))
               (new-str (funcall string-maker (buffer-substring beg end))))
          (if new-str
              (progn
                (delete-region beg end)
                (insert new-str)))))))

(defun my-hatena-convert-cpan ()
  "カーソル付近のモジュール名をcpan URLに変換する。"
  (interactive)
  (my-hatena-get-near-word 'my-hatena-convert-cpan-1))

(defun my-hatena-convert-cpan-1 (name)
  "search.cpan へのリンクを作る"
  (let ((url '("[http://search.cpan.org/dist/")))
    (if (stringp name)
        (let ((new-name name))
          (while (string-match "::" new-name)
            (setq new-name (replace-match "-" t t new-name)))
          (setcdr url (list new-name "/:title=" name "]")))
      (error "Invalid argument %s" x))
    (mapconcat (lambda (n) n) url "")))

;;; shell
(setq sh-basic-offset 2)
;; - http://dev.ariel-networks.com/Members/matsuyama/categories/emacs/cbcategory_view?b_start:int=8
(add-hook 'term-mode-hook
          '(lambda ()
             (define-key term-raw-map "\C-z" (lookup-key (current-global-map) "\C-z"))
             (define-key term-raw-map "\C-x" (lookup-key (current-global-map) "\C-x"))
             (setq term-scroll-to-bottom-on-output 'all)
             (defun term-send-raw ()
               "Send the last character typed through the terminal-emulator
without any interpretation."
               (interactive)
               ;; Convert `return' to C-m, etc.
               (when (and (symbolp last-command-event)
                          (get last-command-event 'ascii-character))
                 (setq last-command-event (get last-command-event 'ascii-character)))
               (term-send-raw-string (make-string 1 last-command-event)))
             ))

;;; color-theme
;;; - http://www.cs.cmu.edu/~maverick/GNUEmacsColorThemeTest/
(add-to-list 'load-path (expand-file-name ".emacs.d/color-theme" home))
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))


;;; eslide
(add-to-list 'load-path (expand-file-name ".emacs.d/eslide" home))
(require 'eslide)

;;; use command key as Meta
;;; http://cgi.NetLaputa.ne.jp/~kose/diary/?200908a&to=200908060#200908060
(when (and (eq window-system 'ns)
           (string-match " NS " (emacs-version)))
  (setq ns-command-modifier (quote meta))
  (setq ns-alternate-modifier (quote super)))

;;; text-translator
;;; http://d.hatena.ne.jp/khiker/20070503/emacs_text_translator
(add-to-list 'load-path (expand-file-name ".emacs.d/text-translator" home))
(require 'text-translator)
(global-set-key "\C-x\M-t" 'text-translator)
(global-set-key "\C-x\M-T" 'text-translator-translate-last-string)
;; 自動選択に使用する関数を設定
(setq text-translator-auto-selection-func
      'text-translator-translate-by-auto-selection-enja)
;; グローバルキーを設定
(global-set-key "\C-xt" 'text-translator-translate-by-auto-selection)
;;; use dictionary.app
;;; http://sakito.jp/mac/dictionary.html#emacs
(defun dictionary ()
  "dictionary.app"
  (interactive)

  (let ((editable (not buffer-read-only))
        (pt (save-excursion (mouse-set-point last-nonmenu-event)))
        beg end)

    (if (and mark-active
             (<= (region-beginning) pt) (<= pt (region-end)) )
        (setq beg (region-beginning)
              end (region-end))
      (save-excursion
        (goto-char pt)
        (setq end (progn (forward-word) (point)))
        (setq beg (progn (backward-word) (point)))
        ))

    (browse-url
     (concat "dict:///"
             (url-hexify-string (buffer-substring-no-properties beg end))))))
(define-key global-map "\C-cw" 'dictionary)


;;; insert \ instead of ¥
(when (and (eq window-system 'ns)
           (string-match " NS " (emacs-version))
           (string= hostname "canaan")) ; keyboard jp106
  (define-key global-map [165] nil)
  (define-key global-map [67109029] nil)
  (define-key global-map [134217893] nil)
  (define-key global-map [201326757] nil)
  (define-key function-key-map [165] [?\\])
  (define-key function-key-map [67109029] [?\C-\\])
  (define-key function-key-map [134217893] [?\M-\\])
  (define-key function-key-map [201326757] [?\C-\M-\\])
  )

(cond (;; fixd-width-font
       ;; http://d.hatena.ne.jp/kazu-yamamoto/20090122/1232589385
       (and (eq window-system 'ns)
            (string-match " NS " (emacs-version)))
       (setq my-font "-*-*-medium-r-normal--12-*-*-*-*-*-fontset-hiramaru")
       (setq fixed-width-use-QuickDraw-for-ascii t)
       (setq mac-allow-anti-aliasing t)
       (if (= emacs-major-version 22)
           (require 'carbon-font))
       (set-default-font my-font)
       (add-to-list 'default-frame-alist `(font . ,my-font))
       (when (= emacs-major-version 23)
         (set-fontset-font
          (frame-parameter nil 'font)
          'japanese-jisx0208
          '("Hiragino Maru Gothic Pro" . "iso10646-1"))
         (setq face-font-rescale-alist
               '(("^-apple-hiragino.*" . 1.2)
                 (".*osaka-bold.*" . 1.2)
                 (".*osaka-medium.*" . 1.2)
                 (".*courier-bold-.*-mac-roman" . 1.0)
                 (".*monaco cy-bold-.*-mac-cyrillic" . 0.9)
                 (".*monaco-bold-.*-mac-roman" . 0.9)
                 ("-cdac$" . 1.3))))
       )
      (;; fixed-width-font for mac
       ;; http://macemacsjp.sourceforge.jp/matsuan/FontSettingJp.html
       (and (eq window-system 'mac)
            (string-match "Carbon" (emacs-version)))
       (setq load-path (cons (expand-file-name ".emacs.d/fixed-width-fontset"
                                               home)
                             load-path))
       (require 'carbon-font)
       (fixed-width-set-fontset "hiramaru" 12))
      (;; font for ubuntu
       (string-match "linux" system-configuration)
       (custom-set-faces
	;; custom-set-faces was added by Custom.
	;; If you edit it by hand, you could mess it up, so be careful.
	;; Your init file should contain only one such instance.
	;; If there is more than one, they won't work right.
	'(default ((t (:inherit nil
                       :stipple nil
                       :background "white"
                       :foreground "black"
                       :inverse-video nil
                       :box nil
                       :strike-through nil
                       :overline nil
                       :underline nil
                       :slant normal
                       :weight normal
                       :height 100
                       :width normal
                       :foundry "unknown"
                       :family "VL ゴシック"))))
	))
      )

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(display-time-mode t)
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(show-paren-mode t))

(put 'dired-find-alternate-file 'disabled nil)
