; This is an emacs major mode for Alloy3 that does keyword coloring
; and indentation.
; Copyright (C) 2002 Allison L Waingold (skippy AT mit DOT edu)

; This program is free software; you can redistribute it and/or
; modify it under the terms of the GNU General Public License
; as published by the Free Software Foundation; either version 2
; of the License, or (at your option) any later version.

; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.

; You should have received a copy of the GNU General Public License
; along with this program; if not, write to the Free Software
; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.


(defvar alloy-font-lock-keywords
  (let ((kw1 (mapconcat 'identity
            '("sig" "fun" "det" "let" "extends"
              "static" "disj" "option" "set" "all"
              "some" "sole" "open"
              "uses" "run" "check" "eval" "for" "but" "none"
              "univ" "iden" "in" "no"
              "with" "sum" "if" "then" "else"
              )
            "\\|"))
    (kw2 (mapconcat 'identity
            '("module" "fact" "assert")
            "\\|"))
    )
    (list
     ;; keywords
     (cons (concat "\\b\\(" kw1 "\\)\\b[ \n\t(]") 1)
     ;; block introducing keywords with immediately following colons.
     (cons (concat "\\b\\(" kw2 "\\)[ \n\t(]") 1)
     ;; classes
     '("\\bsig[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
       1 font-lock-type-face)
     '("\\bextends[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
       1 font-lock-type-face)
     ;; functions
     '("\\bfun[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
       1 font-lock-function-name-face)
     '("\\bmodule[ \t]+\\([a-zA-Z_]+[a-zA-Z0-9_]*\\)"
       1 font-lock-function-name-face)
     ))
  "Additional expressions to highlight in Alloy mode."
)

(defvar alloy-mode-map ()
  "Keymap used in `alloy-mode' buffers.")
(if alloy-mode-map
    nil
  (progn
    (setq alloy-mode-map (make-sparse-keymap))
    (define-key alloy-mode-map "\C-m" 'alloy-return)
))

(defun alloy-return (&optional arg)
  (interactive)
  (alloy-indent-line)
  (newline-and-indent)
)

(defvar alloy-mode-syntax-table nil
  "Syntax table used in `alloy-mode' buffers.")
(if alloy-mode-syntax-table
    nil
  (setq alloy-mode-syntax-table (make-syntax-table))
  (modify-syntax-entry ?\( "()" alloy-mode-syntax-table)
  (modify-syntax-entry ?\) ")(" alloy-mode-syntax-table)
  (modify-syntax-entry ?\[ "(]" alloy-mode-syntax-table)
  (modify-syntax-entry ?\] ")[" alloy-mode-syntax-table)
  (modify-syntax-entry ?\{ "(}" alloy-mode-syntax-table)
  (modify-syntax-entry ?\} "){" alloy-mode-syntax-table)
  ;; Add operator symbols misassigned in the std table
  (modify-syntax-entry ?\$ "."  alloy-mode-syntax-table)
  (modify-syntax-entry ?\% "."  alloy-mode-syntax-table)
  (modify-syntax-entry ?\& "."  alloy-mode-syntax-table)
  (modify-syntax-entry ?\* "."  alloy-mode-syntax-table)
  (modify-syntax-entry ?\+ "."  alloy-mode-syntax-table)
  (modify-syntax-entry ?\- "."  alloy-mode-syntax-table)
  (modify-syntax-entry ?\/ "."  alloy-mode-syntax-table)
  (modify-syntax-entry ?\< "."  alloy-mode-syntax-table)
  (modify-syntax-entry ?\= "."  alloy-mode-syntax-table)
  (modify-syntax-entry ?\> "."  alloy-mode-syntax-table)
  (modify-syntax-entry ?\| "."  alloy-mode-syntax-table)

  (modify-syntax-entry ?\_ "w"  alloy-mode-syntax-table)
  ;; backquote is open and close paren
  (modify-syntax-entry ?\` "$"  alloy-mode-syntax-table)
  ;; comment delimiters
  (modify-syntax-entry ?/  ". 124b" alloy-mode-syntax-table)
  (modify-syntax-entry ?*  ". 23"   alloy-mode-syntax-table)
  (modify-syntax-entry ?\n "> b    "  alloy-mode-syntax-table)
  (modify-syntax-entry ?  "    " alloy-mode-syntax-table)
  (modify-syntax-entry ?\t "    " alloy-mode-syntax-table)
  (modify-syntax-entry ?\r "    " alloy-mode-syntax-table)
  (modify-syntax-entry ?\f "    " alloy-mode-syntax-table))


(defvar alloy-indent-expr "[{|]")

(defvar same-indent-expr "&&\\|||")

(defun in-comment()
  (save-excursion
    (if (looking-at "//\\|/\\*") ;; if we are at the start of a comment, true
    t
      (let ((startpoint (point)))
    (re-search-backward "//" (- (point) (current-column)) t 1)
    (if (< (point) startpoint) ;; if there is a // before us in the line
        t ;; in comment
      (progn
        (re-search-backward "/\\*" nil t 1)
        (if (< (point) startpoint) ;; if there is a /* somewhere before us
        (let ((opencomm (point)))
          ;;(insert "before")
          (goto-char startpoint)
          (re-search-backward "\\*/" opencomm t 1)
          (if (< (point) startpoint) ;; and a */ between /* and us
              nil ;; not in comment
            t) ;; in comment
          )
          )
        )
      )
    )
      )
    )
  )


(defun alloy-indent-line (&optional whole-exp)
  "Indent current line as Lisp code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  (interactive "P")
  (let ((indentcol -1))
    (save-excursion
      (let ((oldpoint (point)) bolast last-indent last-end bol eol
      (obrace 0) (cbrace 0) (sameindent 0))
    (when (in-comment)
      (setq sameindent 1))
    (beginning-of-line)
    (setq bol (point))
    (condition-case nil
        (re-search-backward "[!-~]")
      (error (setq indentcol 0)))
    (when (< indentcol 0)
      (if (looking-at alloy-indent-expr)
          (setq obrace 1)
        (setq obrace 0))
      (when (looking-at ",")
          (setq sameindent 1))
      (when (> (current-column) 0)
        (backward-char))
      (when (looking-at "=>")
          (setq obrace 1))
      (when (looking-at same-indent-expr)
        (setq sameindent 1))
      (beginning-of-line)
      (setq bolast (current-column))
      (re-search-forward "[!-~]")
      (backward-char)
      (setq last-indent (- (current-column) bolast))
      (goto-char oldpoint)
      (end-of-line)
      (setq eol (point))
      (beginning-of-line)
      (re-search-forward "[!-~]" eol t 1)
      (if (> (point) bol) (backward-char) nil)
      (if (looking-at "}")
          (setq cbrace 1)
        (setq cbrace 0))
      (when (looking-at "{")
        (setq sameindent 1))
      (if (= sameindent 1)
          (setq indentcol last-indent)
        (if (= obrace 1)
        (if (= cbrace 1)
            (setq indentcol last-indent)
          (setq indentcol (+ last-indent 3)))
          ;; if there isn't an opening brace at the end of the last row,
          ;; use the nearest enclosing sexp to determine indentation
          ;; if the enclosing sexp starts with ( or [
          (save-excursion
        (condition-case nil
            (progn
              (up-list 1)
              (backward-sexp 1)
              (if (looking-at "[\\(]")
              (setq indentcol (+ 1 (current-column)))
            ;; if enclosing sexp starts with {, indent three from the line
            ;; with the {
            (progn
              (beginning-of-line)
              (re-search-forward "[!-~]")
              (backward-char)
              (if (= cbrace 1)
                  (setq indentcol (current-column))
                (setq indentcol (+ 3 (current-column)))))))
          (error (setq indentcol last-indent)))
        )))

      )
    (if (> (current-column) indentcol)
        (delete-region bol (point))
      ())
    (indent-to indentcol)
    ))
    (if (< (current-column) indentcol)
    (move-to-column indentcol)
      nil
      )
    )
  )

(defun alloy-mode ()
  (interactive)
  (kill-all-local-variables)
  (make-local-variable 'font-lock-defaults)
  (make-local-variable 'comment-start)
  (make-local-variable 'comment-end)
  (make-local-variable 'comment-start-skip)
  (make-local-variable 'comment-column)
  (make-local-variable 'comment-indent-function)
  (make-local-variable 'indent-line-function)

  (set-syntax-table alloy-mode-syntax-table)

  (setq major-mode          'alloy-mode
    mode-name               "Alloy"
    font-lock-defaults      '(alloy-font-lock-keywords)
    comment-start           "/* "
    comment-end             " */"
    comment-start-skip      "/\\*+ *"
    comment-column          40
    comment-indent-function 'java-comment-indent
    indent-line-function    'alloy-indent-line
    indent-tabs-mode        t
    )
  (use-local-map alloy-mode-map)
)

(provide 'alloy-mode)
