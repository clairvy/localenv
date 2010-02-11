(require 'as-profiler)

(defvar *working-directory* (concat (getenv "PWD") "/")
	"String. The directory where we launched emacs and from which all of our files will be located.")
(defvar *as-debug-buffer* "*as-tracer*"
	"String. The name of the buffer that will receive our trace output messages.")

;; These variables should be overridden via the .project-config file
;; for each project.
(defvar *as-source-directory* (concat *working-directory* "src")
	"String. A complete path to our source files for the current project. The default is a dir
named 'src' inside the dir where emacs was started.")
(defvar *as-build-directory* (concat *working-directory* "build")
	"String. A complete path to where we will output the build files for the current project. The default is a dir
named 'build' inside the dir where emacs was started.")
(defvar *swf-original-name* nil
	"String. If we are injecting code into an existing swf, this would be it's name.")
(defvar *swf-new-name* nil
	"String. The name for our output swf.")
(defvar *swf-width* nil
	"String.")
(defvar *swf-height* nil
	"String.")
(defvar *swf-frame-rate* nil
	"String")
(defvar *as-main-file* nil
	"String. The target for the compiler.")
(defvar *use-existing-swf* nil
	"Boolean. Are we injecting code into an existing swf?")
(defvar *flash-version* nil
	"An integer. Which player are we targeting? Currently, if we choose
9, we will use the mxmlc compiler, otherwise we won't do anything.")
(defvar *server-path* nil
	"String. Path to a remote location to copy our new files to.")
(defvar *copy-command* nil
	"String. The command used to copy the new files.")
(defvar *cpp-args* nil
	"String. Args passed to the cpp preprocessor.")
(defvar *as-launch-browser* nil
	"Boolean. Should we launch a browser after compiling?")
(defvar *as-browser-target* nil
	"String. The path that we will pass the browser to find our new files.")
(defvar *as-frame* 1
	"Integer. On which frame of the swf should we inject our code?")

;; Automatically use hideshow with actionscript files.
(add-hook 'actionscript-mode-hook 'hs-minor-mode)

;; *** This note is here b/c we use perform-replace several times in this file. ***
;; The documentation for perform-replace is wrong. Here is the correct interface:
;; (defun perform-replace (from-string replacements query-flag regexp-flag delimited-flag &optional repeat-count map start end)
;; Note that the regexps are tried in the order they were added to the list,
;; so you should add the more specific ones first.

;;;; Pet Tomato specific stuff ----------------------------------------------------

(defun pt-output-sfx-data (external?)
	"Grab all the marked files in dired and output a string that
we can cut-n-paste into our AS code for how we use sound files."
	(interactive 
	 (list
		(y-or-n-p "External? ")))
	(let ((marked-files (dired-get-marked-files))
				(external (if external? "true" "false")))
		(setq marked-files (map 'list 'file-name-nondirectory marked-files))
		(princ "[\n")
		(loop for f in marked-files do
					(princ (format "{id: \"%s\", copies: 1, path: \"%s\", external: %s},\n" (file-name-sans-extension f) f external)))
		(princ "];\n")))

;;;; Useful functions -------------------------------------------------------------

(defun string-char-replace(string oldchar newchar)
	"Return a new string where all occurrences of oldchar
have been replaced with newchar."
	(let ((new-string (copy-sequence string)))
		(do ((i 0 (1+ i)))
				((= i (length new-string)) nil)
			(let ((x (aref new-string i)))
				(when (char-equal x oldchar)
					(setf (aref new-string i) newchar))))
		new-string))

(defun as-get-package()
	"Based on the file path to the current buffer, returns the package string."
	(let* ((wholePath buffer-file-name)
				 (filename (file-name-nondirectory wholePath))
				 (className (file-name-sans-extension filename))
				 (package-regexp (concat "^.*/as/\\(.*?\\)/" filename)))
			;; We use a regexp to grab everything in the path between the as (root) directory and this file.
			;; We'll break this apart to get a list of package names.
			(when (string-match package-regexp wholePath)
				(let ((modified-path (match-string 1 wholePath)))
					(string-char-replace modified-path ?/ ?.)))))

(defun as-get-classname()
	"Based on the buffer's filename, return the classname for this buffer."
	(file-name-sans-extension (file-name-nondirectory buffer-file-name)))

(defun insert-flash-boilerplate()
	"When we open a new AS file, automatically insert some boilerplate
code. This function expects that your AS root starts
with a directory named 'as' from which it builds package names."
	(when (string= (file-name-extension filename) "as")
		(let ((className (as-get-classname))
					(package-string (as-get-package)))
			(insert (concat "package " package-string "{\n\n\tpublic class " className "{\n\n\t\tpublic function " className "(){\n\n\t\t}\n\n\t\tpublic function toString():String{\n\t\t\treturn \"" className "()\";\n\t\t}\n\t}\n}")))))

(add-hook 'find-file-not-found-hooks 'insert-flash-boilerplate)

(defun as-print-func-info()
	"Insert a print statement immediately after the nearest function definition before point."
	(interactive)
	(save-excursion
		(re-search-backward as-function-re)
		(goto-char (match-end 0))
		(let ((modifier1 (match-string 1))
					(modifier2 (match-string 2))
					(function (match-string 3))
					(args (match-string 4))
					(return-type (match-string 5))
					(debug-msg "")
					(first-part "\"")
					(arg-trace "\""))

			;; Check if we should add "this."
			(unless (or (string= modifier1 "static")
									(string= modifier2 "static"))
					(setf first-part "this + \"."))

			;; Parse arguments.
			(when (> (length args) 0)

				(let ((arg-string (mapconcat (function (lambda (x)
																			 ;; chop off any type info
																			 (car (split-string x ":"))))
																		 (split-string args ",")
																		 " +\", \"+ ")))

					(setf arg-trace (concat " : \" + " arg-string))))

			;;now, print out our debug statement after the function start
			(setf debug-msg (concat "trace(" first-part function "(" args ")" arg-trace ");"))
			(insert (concat "\n" debug-msg))
			(indent-according-to-mode)
			(message debug-msg))))

(defun as-insert-trace ()
	"Insert an empty trace call at point. If we are over a word, then trace that word on the next line"
	(interactive)
	(let ((trace-cmd "trace")
				(cw (current-word)))
		(cond
			((string= cw "")
			 ;; point is not over a word.
			 (indent-according-to-mode)
			 (insert (format "%s(\"\");" trace-cmd))
			 (backward-char 3))
			(t
			 ;; point is over a word.
			 (end-of-line)
			 (insert (format "\n%s(\"%s: \" + %s);" trace-cmd cw cw))
			 (indent-according-to-mode)))))

(defun update-etags()
	(interactive)
	(save-some-buffers)
	(shell-command (concat "find " 
												 (concat *working-directory* *as-source-directory*) 
												 " -name '*.as' -print | sed '/_darcs/d' | etags --language=none --regex="
												 "'/" as-function-re "/\\3/' -o " 
												 *working-directory* "TAGS -"))
	(visit-tags-table (concat *working-directory* "TAGS")))

;;;; .project-config stuff --------------------------------------------------------

(defun as-make-cfg-func(props)
	"Returns a function that sets all the given props in actionscript-mode."
	(lexical-let ((props props))
		(lambda ()
			(interactive)
			(dolist (p props)
				(let ((prop (first p))
							(val (second p)))
					(setf (symbol-value prop) val))))))

(defun as-collect-props (id cfg-lst)
	"Gather all the properties associated with id, including
any inherited props."
	(let* ((cur (assoc id cfg-lst))
				 (section-props (second cur))
				 (my-props (second (assoc :props section-props)))
				 (my-parents (second (assoc :super section-props)))
				 (all-props '()))
		;; Iterate through each of id's parents.
		(dolist (parent my-parents)
			(setf all-props (append all-props (as-collect-props parent cfg-lst))))
		(append all-props my-props)))

(defun as-set-env (env)
	(interactive (list
								(read-from-minibuffer "Which env? " nil nil t)))
	(let* ((section (assoc env *PROJECT-CONFIG*))
				 (all-props (as-collect-props (car section) *PROJECT-CONFIG*)))
		(cond 
			(section
			 (funcall (as-make-cfg-func all-props))
			 (message "The current environment is now: %s." env))
			(t
			 (message "\"%s\" is not a valid environment" env)))))

;;;; Compilation related functions ------------------------------------------------

(defun as-print(msg)
	(with-current-buffer (get-buffer "*as-build*")
		(princ msg)
		(terpri)))

(defun collect-files (dir-root file-regexp exclude-dir-list)
	"Travel recursively through directories, starting with dir-root.
Collect any files matching file-regexp into a list. Do not recurse into 
any dirs that match an item of exclude-dir-list."
	(let ((master-list '())
				(files (directory-files dir-root t nil t)))
		(dolist (f files)
			(let ((basename (file-name-nondirectory f)))
				(if (file-directory-p f)
						(when (not (member basename exclude-dir-list))
							(setf master-list (append (collect-files f file-regexp exclude-dir-list) master-list)))
						(when (string-match file-regexp basename)
							(push f master-list)))))
		master-list))

;;(time
;; (let ((root "/home/astro/projects/client/as/spacegame/app/"))
;;	 (collect-files (concat root "src/") "^.*\.as$" '("." ".." "CVS" "_darcs"))))

;; (defun my-file-newer-than-file-p(file1 file2)
;; 	"Return t if file1 is newer than file2."
;; 	(string= (shell-command-to-string (concat "[ " file1 " -nt " file2 " ] && echo 1")) "1\n"))

(defvar *last-cpp-args* nil
	"We need to know if the cpp-args have changed since the last invocation to determine if
we need to reprocess files.")

(defun as-preprocess(src-directory build-directory file-regexp cpp-args)
	"Recursively walk through src-directory and call cpp with cpp-args on all files
matching file-regexp, copying the results to build-directory."
	;; Make sure that src-directory exists.
	;; If it doesn't, exit.

	;; BUG -- This needs to recompile the file if it has any #includes that have changed.
	;;        To fix this in the crudest way possible, we collect all the *.h files, find 
	;;        the most recently modified one, and test every file against that.

	;;   TODO: Consider using GNU make or Ant. Alternatively, to improve this script, we need
	;;         to know which files include which.

	(if (file-directory-p src-directory)
			(progn
				(let ((as-files (collect-files src-directory "^.*\.as$" '("." ".." "CVS" "_darcs")))
							(h-files (collect-files src-directory "^.*\.h$" '("." ".." "CVS" "_darcs")))
							(must-recompile? (not (string= cpp-args *last-cpp-args*))))
					(let ((newest-h-file (reduce (lambda (x y) 
																				 (if (file-newer-than-file-p x y)
																						 x
																						 y))
																			 h-files)))
						(dolist (f as-files)
							(let* ((basename (file-name-nondirectory f))
										 (build-location (concat build-directory (file-relative-name f src-directory)))
										 (new-file-directory (file-name-directory build-location))
										 (build-file (concat new-file-directory "/" basename)))

								(as-print (format "Processing: %s" basename))
								(when (or (not (file-exists-p build-file))
													must-recompile?
													(file-newer-than-file-p f build-file)
													(file-newer-than-file-p newest-h-file build-file)) ;; We've updated a header file since this one.
									(unless (file-directory-p new-file-directory)
										(as-print (format "Build directory does not exist. Creating %s" new-file-directory))
										(make-directory new-file-directory t))

									(as-print (shell-command-to-string (format "cpp %s -P %s %s" cpp-args f build-file)))))))))

			(let ((msg (format "as-preprocess: Source directory: %s does not exist!!!" src-directory)))
				(as-print msg)
				(message msg)))

	(setf *last-cpp-args* cpp-args))


;; (let ((root "/home/astro/projects/client/as/spacegame/app/")
;; 			(*cpp-args* "-I \"/home/astro/projects/client/as/spacegame/app/src/as/standard/\" -include \"PT_global_include.h\" -D PT_DEBUG -D TRACE -D AS3 -ftabstop=2"))
;; 	(message (time
;; 						(as-preprocess (concat root "src/") (concat root "build/") "^.*\.as$" *cpp-args*))))


(defun ordinary-insertion-filter (proc string)
	(with-current-buffer (process-buffer proc)
		(let ((moving (= (point) (process-mark proc))))
			(save-excursion
				;; Insert the text, advancing the process marker.
				(goto-char (process-mark proc))
				(insert string)
				(set-marker (process-mark proc) (point)))
			(if moving (goto-char (process-mark proc))))))

(defun launch-flash-tracer ()
	(interactive)
	(when (get-buffer *as-debug-buffer*)
		(save-current-buffer
			(set-buffer *as-debug-buffer*)
			;; Clear the buffer.
			(erase-buffer)))
	(unless (get-process "as-tracer")
		(message "Starting as-tracer...")
		(let ((p (start-process "as-tracer" *as-debug-buffer* "sbcl" "--core" "/home/astro/projects/web/flash-tracer/flash-tracer.core" "--noinform")))
			(set-process-filter p 'ordinary-insertion-filter))
;;		(start-process "as-tracer" *as-debug-buffer* "sbcl" "--core" "/home/astro/projects/web/flash-tracer/flash-tracer.core" "--noinform")
		;; Highlight odd numbered frames
		(with-current-buffer *as-debug-buffer*
			(hi-lock-mode t)
			(highlight-lines-matching-regexp "^\[[ 0-9]*?[13579]\]" 'actionscript-global-props-face))
		(message "Done.")))

(defun show-dev-screens ()
	(let ((cb *as-debug-buffer*))
		(switch-to-buffer-other-window "*as-build*")
		(goto-char (point-max))
		(switch-to-buffer-other-window cb)))

(defun mxmlc-compile(main-file build-directory swf-name as-frame swf-width swf-height swf-frame-rate use-existing-swf)
	"Compile a swf using mxmlc. Return the output."
	(let ((compile-string (format "cd %s && mxmlc --optimize=true --default-frame-rate %s --default-size %s %s --incremental=true --output %s%s --show-actionscript-warnings=true --allow-source-path-overlap=true --source-path %sas/ --strict=true -file-specs %s" *working-directory* swf-frame-rate swf-width swf-height build-directory swf-name build-directory main-file)))
		(as-print (format "Compile command: %s" compile-string))
		(shell-command-to-string compile-string)))

(defun as-ediff-list (file-A file-B)
	(require 'ediff)
	(let (buffer-A buffer-B)
		(message "Reading file %s ... " file-A)
		(ediff-find-file 'file-A 'buffer-A 'ediff-last-dir-A nil)
		(message "Reading file %s ... " file-B)
		(ediff-find-file 'file-B 'buffer-B 'ediff-last-dir-B nil)

		;; ediff-convert-standard-filename puts file names in the form appropriate
		;; for the OS at hand.
		(setq file-A (ediff-convert-standard-filename (expand-file-name file-A)))
		(setq file-B (ediff-convert-standard-filename (expand-file-name file-B)))

		(setq ediff-buffer-A buffer-A
					ediff-buffer-B buffer-B
					ediff-buffer-C nil)

		(setq ediff-diff-buffer
					(get-buffer-create (ediff-unique-buffer-name "*ediff-diff" "*")))

		(ediff-make-diff2-buffer ediff-diff-buffer file-A file-B)
		(ediff-prepare-error-list ediff-diff-ok-lines-regexp ediff-diff-buffer)
		(setq ediff-diff-options "-E --tabsize=1")
		(let ((result (cdr (ediff-extract-diffs	ediff-diff-buffer nil))))
			(kill-buffer ediff-diff-buffer)
			(kill-buffer "*ediff-errors*")
			result)))

(defun parse-mxmlc-output (output)
	;; If there's an error, it will start with a path to a file
	;; in our build-dir.
	(let ((error-regexp (concat "^" *working-directory* *as-build-directory* "\\([\\/_[:alnum:]]+\.as\\)" "\\(?:(\\([0-9]+\\))\\)?:" "\\(?: col: \\([0-9]+\\)\\)?" " Error: \\(.*\\)$")))
		(if (string-match error-regexp output)
				;; Not all error messages include a line and col number.
				(let ((error-path (match-string 1 output))
							(error-line (if (match-string 2 output) 
															(string-to-number (match-string 2 output))
															0))
							(error-start-char (if (match-string 3 output)
																		(string-to-number (match-string 3 output))
																		0))
							(error-msg (match-string 4 output)))
					;; mxmlc doesn't give us an end char for the error, so we just return the start char twice.
					(values error-path error-line error-start-char error-start-char error-msg))
				nil)))

;; Test
;; (let ((bad-output "Loading configuration file /home/astro/software/flex/frameworks/flex-config.xml
;; Recompile: /home/astro/projects/experiments/flex_tests/build/as/game/G_GameBoard.as
;; Reason: The source file or one of the included files has been updated.
;; Recompile: /home/astro/projects/experiments/flex_tests/build/as/standard/debug/Tracer.as
;; Reason: The source file or one of the included files has been updated.
;; Files changed: 2 Files affected: 0
;; /home/astro/projects/experiments/flex_tests/build/as/game/G_GameBoard.as(7): col: 19 Error: Syntax error: expecting rightbrace before Tracer.

;;    _tracer = newt Tracer(Start);
;;                   ^
;; ")
;; 			(bad-output2 "Compile command: cd /home/astro/projects/experiments/flex_tests/ && mxmlc --default-frame-rate 24 --default-size 558 360 --incremental=true --output flex.swf --show-actionscript-warnings=true --allow-source-path-overlap=true --source-path build/as/ --strict=true -file-specs build/as/game/G_GameBoard.as
;; Loading configuration file /home/astro/software/flex/frameworks/flex-config.xml
;; /home/astro/projects/experiments/flex_tests/build/as/game/G_GameBoard_306942.cache (No such file or directory)
;; /home/astro/projects/experiments/flex_tests/build/as/game/G_GameBoard.as: Error: A file found in a source-path must have the same package structure '', as the definition's package, 'game'.")
;; 			(good-output "Loading configuration file /home/astro/software/flex/frameworks/flex-config.xml
;; Recompile: /home/astro/projects/experiments/flex_tests/build/as/game/G_GameBoard.as
;; Reason: The source file or one of the included files has been updated.
;; Recompile: /home/astro/projects/experiments/flex_tests/build/as/standard/debug/Tracer.as
;; Reason: The source file or one of the included files has been updated.
;; Files changed: 2 Files affected: 0
;; flex.swf (963 bytes)"))
;; 	(parse-mxmlc-output bad-output))


(defun find-compiler-error(error-list src-dir build-dir)
		
	;;; The biggest difficulty with finding the correct match is that
	;;; preprocessor directives can appear anywhere in the source, and
	;;; cause discrepencies between the src and build. Fortunately,
	;;; these discrepencies are isolated, so we should be able to work
	;;; around them. In the worst case, the offending text occurs within
	;;; a cpp directive. In that case we should still be able to identify
	;;; that position as the source of the error.

	;;; To overcome this problem, we use ediff to find all the regions of 
	;;; similar text. 
	(as-print (format "error-list: %s" error-list))
	(multiple-value-bind (error-path error-line error-start-char error-end-char error-msg) error-list
		(as-print (format "error-path: %s" error-path))
		(let (source-buffer
					build-buffer
					offending-text
					offending-start-pos
					offending-end-pos)
			(let ((diff-list (as-ediff-list (concat build-dir error-path) (concat src-dir error-path))))

				;; 1. Find the error in the build file.
				(setq build-buffer (find-file-read-only (concat build-dir error-path)))
				(goto-line error-line)
				(forward-char error-start-char)

				;; 2. Record the offending text listed in the error.
				(setq offending-start-pos (point))
				(setq offending-end-pos (+ (point) (- error-end-char error-start-char)))
				(setq offending-text (buffer-substring-no-properties offending-start-pos offending-end-pos))

				(as-print (format "Offending text: %s" offending-text))
				(as-print (format "line: %s" error-line))
				;;(as-print (format "start: %s" offending-start-pos))

				;; 3. Open the source file
				(setq source-buffer (find-file (concat src-dir error-path)))
				(set-mark (point))
				(beginning-of-buffer)

				;; 4. Search for matching text in the source file.
				;; Iterate through diff-list until we find a max value for a build region that is greater than
				;; offending-start-pos.
				;; Compute offending-start-pos - build-region-min and add that value to src-region-min.
;; 				(as-print (format "offending-start-pos: %s" offending-start-pos))		
;; 				(as-print (format "diff-list: %s" diff-list))
;; 				(as-print (format "length of diff-list: %s" (length diff-list)))

				(let ((error-src-start-pos
							 (if diff-list
									 ;; Iterate through the diffs. 
									 (do* ((i 0 (1+ i))
												 (diff-region (nth i diff-list) (nth i diff-list)))
												;; Test. Stop when the starting position of the offending text has been
												;; found in the current region.
												((or (<= offending-start-pos (aref diff-region 1))
														 (= i (1- (length diff-list))))
												 ;; Result. 
												 ;; NOTE: This only works if we pass ftabstop=2 to cpp. 
												 ;; TODO: Fix this!
												 (+ (- offending-start-pos (aref diff-region 0)) (aref diff-region 2))))
									 offending-start-pos)))

					;; 5a. Highlight the text.
					(let ((best-match-start-pos error-src-start-pos)
								(best-match-end-pos (+ error-src-start-pos (length offending-text))))

;; 						(as-print (format "start pos: %s" best-match-start-pos))
;; 						(as-print (format "end pos: %s" best-match-end-pos))
						(as-print "\n")
						(as-print error-msg)
						(message error-msg)

						(setq as-error-overlay (make-overlay best-match-start-pos best-match-end-pos))
						(overlay-put as-error-overlay 'face '(background-color . "#6a0000"))

						;; put point at the start of the error
						(goto-char best-match-start-pos)

						;; Show block if hidden by hideshow.
						(save-excursion
							(hs-show-block))

						;; close the build file
						(kill-buffer build-buffer)))))
		(let ((cb (current-buffer)))
			(switch-to-buffer-other-window "*as-build*")
			(goto-char (point-max))
			(switch-to-buffer-other-window cb))))

(defun as-create-flash-html ()
	(let ((html-page (concat *working-directory* *as-build-directory* "index.html")))
		(unless (file-exists-p html-page)
			(with-temp-file html-page
				(insert-file-contents "/home/astro/.emacs.d/site-lisp/swf_html_template")
				(perform-replace "#TITLE#" *swf-new-name* nil nil nil nil nil (point-min) (point-max))
				(perform-replace "#WIDTH#" *swf-width* nil nil nil nil nil (point-min) (point-max))
				(perform-replace "#HEIGHT#" *swf-height* nil nil nil nil nil (point-min) (point-max))
				(perform-replace "#SWFNAME#" *swf-new-name* nil nil nil nil nil (point-min) (point-max))))))

(defun as-finalize-compile (src-dir build-dir)
	;; Catch debug messages
	(launch-flash-tracer)
	(launch-flash-profiler)
	(unless (string= *server-path* "local")
		;; copy this file over to our staging
		(as-print "\nCopying files to staging server...")
		(as-print "----------------------------------")
		(as-print (format "[%s]" *server-path*))
		(let ((copy-output (shell-command-to-string (concat *copy-command* build-dir "* " *server-path*))))
			(as-print (format "%s" copy-output))))
	;; Launch browser (if required).
	(when *as-launch-browser*
		(let ((browse-url-browser-function #'browse-url-netscape))
;;			(if (string= *server-path* "local")
;;					(browse-url (concat "file://" build-dir "index.html"))
			(browse-url *as-browser-target*)))
	(as-print (current-time-string))
	(as-print "Success!")
	(message "Success!")
	(show-dev-screens))
;; 				 (when (memq 'on-finish-hook (symbol-plist 'actionscript-mode))
;; 					 (let ((ofh (get 'actionscript-mode 'on-finish-hook)))
;; 						 ;; Make sure it isn't nil.
;; 						 (when ofh
;; 							 (funcall ofh)))))


(defun as-flash-compile ()
  "Compile Flash movie. Reload in browser. Check for errors."
  (interactive)
  (save-some-buffers)
 	(let ((standard-output (get-buffer-create "*as-build*"))
				(src-dir (concat *working-directory* *as-source-directory*))
				(build-dir (concat *working-directory* *as-build-directory*)))
		(with-current-buffer (get-buffer "*as-build*")
			(goto-char (point-max)))
		(as-print "\n---Starting Build---\n")
		;; run the preprocessor
		(as-print "Running preprocessor...")
		(as-print "-----------------------")
		(as-preprocess src-dir build-dir "^.*\.as$" *cpp-args*)
		;; Copy a clean swf over, if necessary.
		(when *use-existing-swf*
			(as-print (format "\nCopying swf: %s" *swf-new-name*))
			(as-print "--------------")
			(copy-file (concat src-dir *swf-original-name*) (concat build-dir *swf-new-name*) t))
		;; copy any additional files over, if newer than build versions.
		(as-print "\nCopying additional files...")
		(as-print "---------------------------")
		(as-print (shell-command-to-string (format "rsync --update --cvs-exclude --exclude \"embed\" --exclude \"*.as\" --exclude \"_darcs\" --exclude \"*.fla\" --exclude \"*.psd\" --archive --verbose %s %s" src-dir build-dir)))
		;; create the html page, if necessary.
		;;(as-create-flash-html)
		;; delete any error overlays
		(when (boundp 'as-error-overlay)
			(delete-overlay as-error-overlay))
		;; Compile.
		(let (compiler-output compiler-error)
			(case *flash-version*
				(9
				 ;; Use mxmlc.
				 (as-print "\nRunning mxmlc...")
				 (as-print "----------------")
				 (setf compiler-output (mxmlc-compile (concat "./" *as-build-directory* *as-main-file*) *as-build-directory* *swf-new-name* *as-frame* *swf-width* *swf-height* *swf-frame-rate* *use-existing-swf*))
				 (setf compiler-error (parse-mxmlc-output compiler-output)))
				(t
				 (as-print "\n Sorry, we only support Flash 9 at this time.")))
			(as-print compiler-output)
			(if compiler-error
					(find-compiler-error compiler-error src-dir build-dir)
					(as-finalize-compile src-dir build-dir)))))

(defun as-path-to-current-file ()
  (let* ((wholePath buffer-file-name)
				 (filename (file-name-nondirectory wholePath))
				 (className (file-name-sans-extension filename)))
		;; We use a regexp to grab everything in the path between the as (root) directory and this file.
		(let ((package-regexp (concat "^.*/as/\\(.*?\\)" filename)))
			;; We'll break this apart to get a list of package names.
			(if (string-match package-regexp wholePath)
					(let ((package-path (match-string 1 wholePath)))
						;; Parse the path into package names.
						(let ((package-list (split-string package-path "/"))
									(package-string ""))
							;; combine the package names into a string
							(dolist (elt package-list)
								(setf package-string (concat package-string elt ".")))
							(concat package-string className)))
					nil))))

(defun as-quick-compile ()
	"Compile the current buffer into a temporary swf and launch the browser. This function
is intended for running quick tests of code."
  (interactive)
	(save-buffer)
 	(let ((standard-output (get-buffer-create "*as-build*")))
		(with-current-buffer (get-buffer "*as-build*")
			(goto-char (point-max)))
		(as-print "\n---Starting Build---\n")
		;; Setup environment for Quick Compile
		(let* ((*as-main-file* "Quick_Compile_Main.as")
					 (*as-build-directory* "/tmp/as-quick-compile/")
					 (*swf-new-name* "temp.swf")
					 (*as-frame* 1)
					 (*swf-width* 100)
					 (*swf-height* 100)
					 (*swf-frame-rate* 24)
					 (*use-existing-swf* nil)
					 (*as-launch-browser* t)
					 (*as-browser-target* (concat "file://" (expand-file-name *as-build-directory*) *swf-new-name*))
					 (*server-path* "local"))
			(as-print (concat "swf-new-name: " *swf-new-name*))
			(let ((src-dir (concat *working-directory* *as-source-directory*))
						(build-dir *as-build-directory*))

			;;;; Create the generic main class file.
				;; Are we compiling the whole class, or just some "free" code?
				;; We need to get the path to this file.
				(unless (file-directory-p build-dir)
					(as-print (format "Build directory does not exist. Creating %s" build-dir))
					(make-directory build-dir t))
			
				(let ((code-to-inject (concat (as-get-classname) ".test()"))
							(import-to-inject (concat "import " (as-path-to-current-file) ";"))
							(template-path (concat build-dir "Quick_Compile_Main_template.as")))
					(with-temp-file template-path
						(insert-file-contents "/home/astro/.emacs.d/site-lisp/Quick_Compile_Template")
						(perform-replace "%IMPORT%" import-to-inject nil nil nil nil nil (point-min) (point-max))
						(perform-replace "%CODE%" code-to-inject nil nil nil nil nil (point-min) (point-max)))
					(as-print (shell-command-to-string (format "cpp %s -P %s %s" *cpp-args* template-path (concat build-dir "Quick_Compile_Main.as")))))

				;; run the preprocessor
				(as-print "Running preprocessor...")
				(as-print "-----------------------")
				(as-preprocess src-dir build-dir "^.*\.as$" *cpp-args*)
				(as-print "\n")

				(when (boundp 'as-error-overlay)
					(delete-overlay as-error-overlay))
				;; Compile.
				(let (compiler-output compiler-error)
					(case *flash-version*
						(9
						 ;; Use mxmlc.
						 (as-print "\nRunning mxmlc...")
						 (as-print "----------------")
						 (setf compiler-output (mxmlc-compile (concat *as-build-directory* *as-main-file*) *as-build-directory* *swf-new-name* *as-frame* *swf-width* *swf-height* *swf-frame-rate* *use-existing-swf*))
						 (setf compiler-error (parse-mxmlc-output compiler-output)))
						(t
						 (as-print "\nSorry, we only support Flash 9 at this time.")))
					(as-print compiler-output)
					(if compiler-error
							(find-compiler-error compiler-error src-dir build-dir)
							(progn
								(as-finalize-compile src-dir build-dir))))))))

(defun toggle-build-source ()
	"If we are visiting a file in the source directory, open the corresponding file
in the build directory, and vice-versa."
	(interactive)
	;; get the path to the current buffer
  (let ((wholePath buffer-file-name)
				(build-regexp (concat "^" (concat *working-directory* *as-build-directory*) "\\(.*\\)"))
				(source-regexp (concat "^" (concat *working-directory* *as-source-directory*) "\\(.*\\)")))
		;; check if it is within the build or the source directory
		(if (string-match source-regexp wholePath)
				;; We are in the source directory.
				(let ((my-file (concat *working-directory* *as-build-directory* (match-string 1 wholePath))))
					(if (file-exists-p my-file)
							(find-file my-file)
							(message "File not found in build directory.")))
				(if (string-match build-regexp wholePath)
						;; We are in the build directory.
						(let ((my-file (concat *working-directory* *as-source-directory* (match-string 1 wholePath))))
							(if (file-exists-p my-file)
									(find-file my-file)
									(message "File not found in src directory.")))
						(message "Not in build or source directory.")))))

(defun as3-convert ()
	"Do some simple things to convert an AS2 buffer to closer to AS3."
	(interactive)
	;;; Convert package/class declaration
	(let ((package-string (as-get-package))
				(classname (as-get-classname))
				(imports '()))
		;; First check that it's still there in AS2 format.
		(goto-char (point-min))
		(when (re-search-forward (concat "^class " package-string "." classname) nil t)
		  ;;; Grab any import statments
			(goto-char (point-min))
			(while (re-search-forward "^import .*\n" nil t)
				(push (match-string 0) imports)
				(replace-match "" nil t))
		  ;;; replace the old declaration
			(re-search-forward (concat "^class " package-string "." classname))
			(replace-match (concat "package " package-string "{\n"))
			(when imports
				(insert "\n")
				(dolist (x imports)
					(insert "\t")
					(insert x))
				(insert "\n"))
			(insert "\tpublic class " classname)
		  ;;; add the trailing }
			(goto-char (point-max))
			(insert "\n}")
			(indent-region (point-min) (point-max) nil)))
	;; :Void -> :void
	(goto-char (point-min))
	(while (search-forward ":Void" nil t)
		(replace-match ":void" nil t))
	;; (Void) -> ()
	(goto-char (point-min))
	(while (search-forward "(Void)" nil t)
		(replace-match "()" nil t))
	(goto-char (point-min)))

;; Keybindings
(define-key actionscript-mode-map [f5] 'as-print-func-info)
(define-key actionscript-mode-map "\C-c\C-t" 'as-insert-trace)
(define-key actionscript-mode-map [f4] 'as-flash-compile)
(define-key actionscript-mode-map [f6] 'as-quick-compile)

(when running-on-x
	(pushnew 
	 '("*as-build*" 
		 (background-color . "#000018")
		 (width . 120) 
		 (height . 30)
		 (auto-raise))
	 special-display-buffer-names)
	(pushnew 
	 '("*as-profiler*"
		 (background-color . "#000018")
		 (width . 120) 
		 (height . 30)
		 (auto-raise))
	 special-display-buffer-names))
						 
;; Load our project specific options, if available.
(load (concat *working-directory* ".project-config.el") t)

;; ------------------------------------------
;; Temporary stuff
(defun erase-profiler-text()
	(interactive)
  (while (re-search-forward "\[[ 0-9]+\] {[.0-9]+}" nil t)
    (replace-match "" nil nil)))

(defun print-class-info-for-cogre()
	(interactive)
	(let ((classname (as-get-classname))
				(attributes '())
				(methods '()))
		(goto-char (point-min))
		;; Find the constructor.
		(re-search-forward (concat "function " classname))
		(let ((endpos (match-beginning 0)))
			(goto-char (point-min))
			(while (re-search-forward as-attribute-re endpos t)
				(push (list (match-string-no-properties 4) 'variable (or (match-string-no-properties 5) "void")) attributes))
			(while (re-search-forward as-function-re nil t)
				(push (list (match-string-no-properties 3) 'method (or (match-string-no-properties 5) "void")) methods))
			(print 'ATTRIBUTES)
			(print (reverse attributes))
			(print 'METHODS)
			(print (reverse methods)))))
