
(eval-when-compile (require 'cl))
(require 'flymake)

(defun string-join (sequence separator)
  (mapconcat #'identity sequence separator))

(defun flymake-scala-parent-dir (path)
  "return parent directory path of argument."
  (substring-no-properties (file-name-directory path) 0 -1))

(defun flymake-scala-find-target-file-dir (path target)
  (let* ((src (split-string (flymake-scala-parent-dir path) "/"))
         (paths (maplist #'(lambda (l) (string-join (reverse l) "/")) (nreverse src))))
    (loop for path in paths
          if (file-exists-p (concat path "/" target))
          return path)))

(defun flymake-scala-build-maven ()
  (list "mvn" (list "-fn" "-Dmaven.compiler.showWarnings=true" "scala:compile")))

(defun flymake-scala-build (target dist classpath)
  (list "fsc" (list "-classpath" classpath "-d" dist target)))

(defun flymake-scala-init ()
  (let ((dir (flymake-scala-find-target-file-dir buffer-file-name "pom.xml")))
    (remove-hook 'after-save-hook 'flymake-after-save-hook t)
    (save-buffer)
    (add-hook 'after-save-hook 'flymake-after-save-hook nil t)
;    (if (string-not-empty dir)
    (if (and dir (not (string= dir "")))
        (progn
          (cd dir)
          (flymake-scala-build-maven))
      (flymake-scala-build buffer-file-name distdir classpath))))

(defvar distdir "/tmp")
(defvar classpath "")
(add-hook 'scala-mode-hook (lambda () (flymake-mode-on)))
(push '(".+\\.scala$" flymake-scala-init) flymake-allowed-file-name-masks)
(push '("^\\(.*\\):\\([0-9]+\\): error: \\(.*\\)$" 1 2 nil 3) flymake-err-line-patterns)

(provide 'scala-mode-flymake)
