;; --------------------------------------------------
;; Various utility functions for verilog mode:
;; Navigate verilog files, and annotate `defines, and
;; show/hide instantiation port lists.
;; --------------------------------------------------
;; Load:
;; (nconc load-path '("/path/to/this/dir"))
;; (require 'verilogUtils)
;;
;; Set the following variables in your .emacs (replace
;; myproj with your project):
;; 
;; (setq proj "myproj")
;; (setq myproj-verilog-extensions   '(".v" "_b.v" ".icm.v"))
;; (setq myproj-verilog-dirs         '("." "list" "of" "search" "dirs"))
;; (setq myproj-verilog-define-files '("list" "of" "include" "files" "for" 
;;                                     "define" "annotations"))
;; Key bindings:
;;  Control-c p -- Call within an instantation to go to the
;;                 source file corresponding to the
;;                 instantiation.
;;  Control-c o -- Grep for word around cursor in current file
;; 
;;  Control-c t -- Toggle define annotations on and off
;;  Control-c d -- Print value of `define around cursor.
;;  Control-c H -- Toggle display of all instantiation port lists.
;;  Control-c h -- Toggle display of the port list around cursor
;;  Control-Meta-n -- Goto next instantiation
;;  Control-Meta-p -- Goto previous instantiation
;;

(autoload 'verilog-mode "verilog-mode")

;; Default values of some project specific variables
(defvar verilog-extensions-default  '(".v" "_b.v" ".icm.v"))
(defvar verilog-dirs-default        '("."))
(defvar verilog-define-files-default '()) ;; e.g. '("file1" "file2"))

;; The defines are stored in a hash '<proj>-verilog-hash-table'
(defvar proj "none")
(defvar hash-table-suffix "verilog-hash-table")

(defface verilog-define-annotation-face
  '((t (:background "red4" :foreground "white")))
  "Face for verilog define annotations.")

;; --------------------------------------------------
;; Keybinding for the navigation function
;; --------------------------------------------------
(add-hook 'verilog-mode-hook
  (function
    (lambda ()
      ;; C-c p can be used to push into a module.
      (define-key verilog-mode-map [?\C-c ?\p] 'verilog-push-hier)
      ;; C-c o runs 'occur' on the word around point
      (define-key verilog-mode-map "\C-co" #'(lambda ()
                                               (interactive)
                                               (isearch-update-ring (current-word))
                                               (occur (current-word))))
      (define-key verilog-mode-map [?\C-\M-n] #'(lambda ()
                                               (interactive)
                                               (verilog-prev-nxt-module t)))
      (define-key verilog-mode-map [?\C-\M-p] #'(lambda ()
                                               (interactive)
                                               (verilog-prev-nxt-module nil)))
      )))

;; --------------------------------------------------
;; Resize occur window to make it smaller if possible. Min height is 10.
;; Max height is 'frame-height/num-windows'
;; --------------------------------------------------
(add-hook 'occur-hook
  (function
    (lambda ()
      (let* ((curr-window (get-buffer-window (current-buffer)))
             (num-windows  (length (window-list)))
             ;; Divide frame height with number of windows in the frame to get the max height.
             (max-height (/ (frame-parameter (window-frame curr-window) 'height) num-windows)))
             ;(max-height (window-height curr-window)))
        (fit-window-to-buffer curr-window max-height 10)))))

;; --------------------------------------------------
;; Key bindings for occur mode
;; --------------------------------------------------
(add-hook 'occur-mode-hook
  (function
    (lambda ()
      (define-key occur-mode-map "\C-ct" 'verilog-toggle-annotations))))

;; --------------------------------------------------
;; Goto the prev or next module instantiation. (next if
;; nxt=t, prev if nxt=nil) We call
;; verilog-find-prev-module-name repeatedly until the module
;; name does not match a known verilog keyword.
;; --------------------------------------------------
(defun verilog-prev-nxt-module (&optional nxt)
  (interactive)
  (let (done match modName st-pos en-pos pos kws kws-found)
    ;; Look for a non keyword match
    (while (not done)
      (setq match     (verilog-find-prev-module-name (point) nil nxt))
      (setq modName   (nth 0 match))
      (setq st-pos    (nth 3 match))
      (setq en-pos    (nth 4 match))
    ;; Choose st-pos if not nxt, choose en-pos if nxt
      (setq pos       (or (and nxt en-pos) (and (not nxt) st-pos)))
      (setq kws       verilog-keywords)
      (setq kws-found nil)
      ;; Now see of modName is a keyword
      (while (and kws (not kws-found))
        (if (string= (car kws) modName)
          (setq kws-found t))
        (setq kws (cdr kws)))
      (if pos (goto-char pos))
      ;; If kws-found is false, we are done.
      (setq done (not kws-found)))))
      
;; --------------------------------------------------
;; Search backward from curr-pos till end-pos (or start of
;; file) for the module and instance name. If nxt is true, then
;; find the next module name (not previous).
;; NOTE: DOES NOT MATCH COMMENTS WITHIN PARAMETERS
;; --------------------------------------------------
(defun verilog-find-prev-module-name (curr-pos &optional end-pos nxt)
  (save-excursion
    (let* ((wordRE     "\\([[:alnum:]_]+\\)")  ;; We save only words
            (noWordRE  "[^[:alnum:]_]")
            (spaceRE   "[ \t\n]")
            (begLineRE "^[ \t]*")
            (limit     (if nxt (point-max) (point-min)))
            (end-pos   (or end-pos limit))
            (optParamRE (concat "\\(?:#" spaceRE "*(" ;; Parameters start
                            "\\(" spaceRE "*" ".*?,?\\)*" ;; Match .*, (one non-greedy argument)
                            ;;"\\(" spaceRE "*" ".*(.*),?\\)*" ;; Match .AAA (BBB),
                          spaceRE "*)"                ;; Parameters end
                          spaceRE "*\\)?"))
            (moduleRE (concat begLineRE wordRE spaceRE "+" optParamRE
                        wordRE spaceRE "*("))
            (portRE   (concat "\\." spaceRE "*" wordRE))
           moduleName instName portName modStartPos modEndPos)
      ;; Go to the start point
      (goto-char curr-pos)
      ;; First we find the port around or before the point.
      (re-search-forward noWordRE nil t)         ;; Go to non word char
      (if (re-search-backward portRE 0 t)
          (setq portName (match-string 1)));; Look back for port name
      ;; Now we look for the module and inst name
      (goto-char curr-pos)
      (if (or 
            (and (not nxt) (re-search-backward moduleRE end-pos t))
            (and      nxt  (re-search-forward moduleRE end-pos t)))
      ;(if (re-search-backward moduleRE end-pos t)
        (progn
          (setq moduleName (match-string 1))
          (setq instName (match-string 3))
          (setq modStartPos (match-beginning 0))
          (setq modEndPos   (match-end 0))
          (list moduleName instName portName modStartPos modEndPos))))))

;; --------------------------------------------------
;; Given a module name, search verilog-dirs and get the
;; appropriate file name.
;; --------------------------------------------------
(defun verilog-get-file-name (moduleName)
  (let* ((extns-list (verilog-get-proj-var "verilog-extensions"))
          (dirs      (verilog-get-proj-var "verilog-dirs"))
          ;; (dirs      (nconc (verilog-get-proj-var "verilog-dirs")
          ;;                     verilog-library-directories))
          (extns extns-list)
            dir extn fileName retVal)
    ;(message "%s" dirs)
    (while dirs
      (setq dir  (car dirs))
      (while extns
        (setq extn  (car extns))
        (setq fileName (concat dir "/" moduleName extn))
        (if (file-readable-p fileName)
          (progn
            (setq retVal fileName)
            (setq dirs '() extns '())))
        (setq extns  (cdr extns)))
      (setq dirs (cdr dirs))
      (setq extns extns-list))
    retVal))

;; --------------------------------------------------
;; The main function
;; --------------------------------------------------
(defun verilog-push-hier()
  (interactive)
  (let* ((modInfo (verilog-find-prev-module-name (point)))
          (moduleName (nth 0 modInfo))
          (instName   (nth 1 modInfo))
          (portName   (nth 2 modInfo))
          (fileName   (verilog-get-file-name moduleName))
          (max-height (/ (frame-parameter (window-frame (selected-window)) 'height) 2))
          )
    (if fileName
      (progn
        ;; Save curr buffer and position in stack here.
        ;; (push (current-buffer) v)
        ;(view-file fileName)
        (find-file fileName)
        (if portName
          (progn
            (isearch-update-ring portName) ;; Add portname to the search ring
            (search-forward portName (point-max) t 2)
            (occur portName)
            ;; This gets done in occur-hook now.
            ;(fit-window-to-buffer (get-buffer-window "*Occur*") max-height 10)
            ))
        )
      (message "Could not find file name for module %s, inst %s"
        moduleName instName))))

;; (defun verilog-pop-hier ()
;;   (switch-to-buffer (pop v)))

;; --------------------------------------------------
;; Return <proj>-<var> if defined, else return <var>-default
;; <var>-default should be defined in the top of this file.
;; --------------------------------------------------
(defun verilog-get-proj-var (var)
  (let* ((var-sym (intern-soft (concat proj "-" var)))
         (var-value (and var-sym (eval var-sym)))
         (def-var-sym (intern-soft (concat var "-default")))
         (def-var-value (and (boundp def-var-sym) (eval def-var-sym)))) ;; Should be defined
    (if (not (boundp def-var-sym))
      (error "Variable %s not found. Needs to be defined." (concat var "-default"))
      ; else
      (or var-value def-var-value))))


;; --------------------------------------------------
;; Read files with define macros, if not already read.
;; Consults the <proj>-verilog-define-files variable. Stores them 
;; in a hash dependent on the current project.
;; --------------------------------------------------
(defun verilog-read-define-files ()
  (let* ((files (verilog-get-proj-var "verilog-define-files"))
         (hash-table-name (concat proj "-" hash-table-suffix)) ;; Hash table name
         (hash-table (intern-soft hash-table-name))
         (hash-found (and hash-table (hash-table-p (eval hash-table)))))
    (if (not hash-found)
      (progn
        (if (not hash-table) (setq hash-table (intern hash-table-name)))
        (set hash-table (make-hash-table :test 'equal :size 5000 :rehash-size 2.0))
        (while files           ;; Read other define files
          (if (file-readable-p (car files))
            (verilog-read-defines-hash-lc (eval hash-table)
              (substitute-in-file-name (car files))))
          (setq files (cdr files)))))
    ;; Here we have a valid hash table
    ;; We always read the defines in the current file
    (verilog-read-defines-hash-lc (eval hash-table) nil nil)))

;; --------------------------------------------------
;; Read a single include file and put its defines in the
;; project hash. Meant to be called interactively.
;; --------------------------------------------------
(defun verilog-read-define-file (file)
  (interactive "fFile: ")
  (let* ((hash-table-name (concat proj "-" hash-table-suffix)) ;; Hash table name
         (hash-table (intern-soft hash-table-name))
         (hash-found (and hash-table (hash-table-p (eval hash-table)))))
    (if hash-found
        (verilog-read-defines-hash-lc (eval hash-table)
          (substitute-in-file-name file)))))

;; --------------------------------------------------
;; Copied from verilog-mode to store the defines in a hash (faster).
;; --------------------------------------------------
(defun verilog-read-defines-hash-lc (hash-var &optional filename recurse subcall)
  "Read `defines and parameters for the current file, or optional FILENAME.
If the filename is provided, `verilog-library-flags' will be used to
resolve it.  If optional RECURSE is non-nil, recurse through `includes.

Parameters must be simple assignments to constants, or have their own
\"parameter\" label rather than a list of parameters.  Thus:

    parameter X = 5, Y = 10;	// Ok
    parameter X = {1'b1, 2'h2};	// Ok
    parameter X = {1'b1, 2'h2}, Y = 10;	// Bad, make into 2 parameter lines

Defines must be simple text substitutions, one on a line, starting
at the beginning of the line.  Any ifdefs or multiline comments around the
define are ignored.

Defines are stored inside Emacs variables using the name vh-{definename}.

This function is useful for setting vh-* variables.  The file variables
feature can be used to set defines that `verilog-mode' can see; put at the
*END* of your file something like:

    // Local Variables:
    // vh-macro:\"macro_definition\"
    // End:

If macros are defined earlier in the same file and you want their values,
you can read them automatically (provided `enable-local-eval' is on):

    // Local Variables:
    // eval:(verilog-read-defines)
    // eval:(verilog-read-defines \"group_standard_includes.v\")
    // End:

Note these are only read when the file is first visited, you must use
\\[find-alternate-file] RET  to have these take effect after editing them!

If you want to disable the \"Process `eval' or hook local variables\"
warning message, you need to add to your .emacs file:

    (setq enable-local-eval t)"
  (let ((origbuf (current-buffer)))
    (save-excursion
      (unless subcall (verilog-getopt-flags))
      (when filename
	(let ((fns (verilog-library-filenames filename (buffer-file-name))))
	  (if fns
	      (set-buffer (find-file-noselect (car fns)))
	    (error (concat (verilog-point-text)
			   ": Can't find verilog-read-defines-hash-lc file: " filename)))))
      (when recurse
	(goto-char (point-min))
	(while (re-search-forward "^\\s-*`include\\s-+\\([^ \t\n\f]+\\)" nil t)
	  (let ((inc (verilog-string-replace-matches "\"" "" nil nil (match-string-no-properties 1))))
	    (unless (verilog-inside-comment-p)
	      (verilog-read-defines-hash-lc inc recurse t)))))
      ;; Read `defines
      ;; note we don't use verilog-re... it's faster this way, and that
      ;; function has problems when comments are at the end of the define
      (goto-char (point-min))
      (while (re-search-forward "^\\s-*`define\\s-+\\([a-zA-Z0-9_$]+\\)\\s-+\\(.*\\)$" nil t)
	(let ((defname (match-string-no-properties 1))
	      (defvalue (match-string-no-properties 2)))
	  (setq defvalue (verilog-string-replace-matches "\\s-*/[/*].*$" "" nil nil defvalue))
;; -- Modified -- LC 10Aug2011
          (puthash defname defvalue hash-var)))
;; -- End Modified -- LC 10Aug2011          
      ;; Hack: Read parameters
      (goto-char (point-min))
      (while (re-search-forward
	      "^\\s-*\\(parameter\\|localparam\\)\\(\\s-*\\[[^]]*\\]\\)?\\s-+" nil t)
	(let (enumname)
	  ;; The primary way of getting defines is verilog-read-decls
	  ;; However, that isn't called yet for included files, so we'll add another scheme
	  (if (looking-at "[^\n]*synopsys\\s +enum\\s +\\([a-zA-Z0-9_]+\\)")
	      (setq enumname (match-string-no-properties 1)))
	  (forward-comment 999)
	  (while (looking-at "\\s-*,?\\s-*\\([a-zA-Z0-9_$]+\\)\\s-*=\\s-*\\([^;,]*\\),?\\s-*")
	    (verilog-set-define (match-string-no-properties 1) (match-string-no-properties 2) origbuf enumname)
	    (goto-char (match-end 0))
	    (forward-comment 999)))))))

;; --------------------------------------------------
;; Code to display `define-d values for variable around cursor.
;; --------------------------------------------------
(defun verilog-display-define-val ()
  (interactive)
  (save-excursion
    (let* ((wordRE  "\\([[:alnum:]_]+\\)")  ;; We save only words
            (noWordRE  "[^[:alnum:]_]")
            (spaceRE "[ \t\n]")
            (defRE    (concat noWordRE wordRE))
            (endPos   0)
            tempVar defName)
      (verilog-read-define-files)
      ;; First we find the define word around or before the point.
      (re-search-forward noWordRE)         ;; Go to non word char
      (if (re-search-backward defRE endPos t)
          (setq defName (match-string 1)));; Look back for define
      (message "%s=%s" defName (verilog-get-define-value defName)))))

;; --------------------------------------------------
;; Does the region (st end) contain a verilog-def overlay?
;; --------------------------------------------------
(defun verilog-define-overlay-p (st en)
  "Does (st en) contain a verilog-def overlay?"
  (let* ((ovlist (overlays-in st en))
          (retVal nil))
    (while ovlist
      (let ((overlay (car ovlist)))
        (if (overlay-get overlay 'verilog-def)
          (progn 
            (setq retVal t)
            (setq ovlist nil)))
        (setq ovlist (cdr ovlist))))
    retVal))

;; --------------------------------------------------
;; Active-annotation like display for all defines.
;; --------------------------------------------------
(defun verilog-annotate-defines ()
  "Add value annotation to defines in a file."
  (interactive)
  (save-excursion
    (let* ((wordRE  "\\([[:alnum:]_]+\\)")  ;; We save only words
           (kwRE   "^\\(define\\|ifdef\\|ifndef\\|endif\\|include\\)$")
           (files (verilog-get-proj-var "verilog-define-files"))
           start end ov defName defVal)
      (verilog-read-define-files)
      (goto-char (point-min))
      (while (re-search-forward (concat "`" wordRE) (point-max) t)
        (progn
          (setq defName (match-string 1))   ;; Macro name
          (setq start (nth 0 (match-data))) ;; Start of match
          (setq end   (nth 1 (match-data))) ;; End   of match
          (if (and (not (string-match kwRE defName))
                   (not (verilog-define-overlay-p start end)))
            (progn
              (setq defVal (verilog-get-define-value defName))
              (if defVal
                (progn
                  (setq defVal (concat "(" defVal ")"))
                  (setq ov    (make-overlay start end))
                  (overlay-put ov 'verilog-def t)
                  (overlay-put ov 'after-string 
                    (propertize defVal 'face 
                      'verilog-define-annotation-face)))))))))))

;; --------------------------------------------------
;; Get the definition of a macro string, recursively. Get it
;; from a project specific hash variable.
;; --------------------------------------------------
(defun verilog-get-define-value (str)
  "Get the value of the define defined by string"
  (interactive)
  (let* ((wordRE  "\\([[:alnum:]_]+\\)")
         (hash-table-name (concat proj "-" hash-table-suffix)) ;; Hash table name
         (hash-table (intern-soft hash-table-name))
         (hash-found (and hash-table (hash-table-p (eval hash-table))))
         (val     (and (stringp str) hash-found (gethash str (eval hash-table)))))
    (if val
      ;; Look for any other defines in val, if present,
      ;; substitute them recursively. Return the
      ;; substituted string.
      (let ((idx  0)
             def1 val1 beg)
        ;; Remove trailing blanks
        (setq val (replace-regexp-in-string " *$" "" val))
        ;; Go through the string and replace all child macros.
        (while (string-match (concat "`" wordRE) val idx)
          ;; Call the match functions before recursively
          ;; calling this function, otherwise the match data gets
          ;; overwritten
          (setq def1 (match-string 1 val))
          (setq beg  (match-beginning 0))
          (setq idx  (match-end 0))
          (setq val1 (verilog-get-define-value def1))
          ;; I cannot use replace-match because the match
          ;; data is not valid any more, because we may have
          ;; recursively called this function again, so I do
          ;; the replacement manually.
          (setq val (concat (substring val 0 beg) val1
                            (substring val idx)))
          ;; Update idx
          (setq idx (+ beg (length val1))))
        val)
      ;; Return nil if there is no definition for this macro
      ;; in the define files that we have read.
      nil)))

;; --------------------------------------------------
;; Remove defines annotation
;; --------------------------------------------------
(defun verilog-unannotate-defines ()
  "Remove annotations put by verilog-annotate-defines."
  (interactive)
  (save-excursion
      (let* ((ovlist (overlays-in (point-min) (point-max))))
        (while ovlist
          (let ((overlay (car ovlist)))
            (if (overlay-get overlay 'verilog-def)
              (delete-overlay overlay))
            (setq ovlist (cdr ovlist)))))))

;; --------------------------------------------------
;; Annonate/Unannotate verilog defines
;; --------------------------------------------------
(defun verilog-toggle-annotations ()
  "Toggle annotations"
  (interactive)
  (if (not (local-variable-p 'verDefineAnnotActive))
    (progn
      (make-local-variable 'verDefineAnnotActive)
      (verilog-annotate-defines))
    (progn
      (kill-local-variable `verDefineAnnotActive)
      (verilog-unannotate-defines))
    ))

;; --------------------------------------------------
;; Add key binding and menu item
;; --------------------------------------------------
(add-hook 'verilog-mode-hook
  (function
    (lambda ()
      (easy-menu-add-item nil '("Verilog") 
        ["(Un)Annotate `defines" verilog-toggle-annotations t]
        "Line up declarations around point")
      ;; Note that this key binding is also defined separately in occur mode
      (define-key verilog-mode-map "\C-ct" 'verilog-toggle-annotations)
      ;; Keybinding for the display defines function
      (define-key verilog-mode-map "\C-cd" 'verilog-display-define-val)
      )))

;; =======================================================================================
;; --------------------------------------------------
;; Fold all port lists
;; --------------------------------------------------
(defvar verilog-inst-end-re     ")[ 	]*;")

(defun verilog-hide-all-port-lists ()
  "Hide ports in all instantiations in the current buffer"
  (interactive)
    (save-excursion
      (let* ((limit (point-max))
             (currPoint (point-max))
             modInfo)
        (setq modInfo (verilog-find-prev-module-name currPoint))
        (while (nth 4 modInfo)
          ;; Move point to end of instantiation
          (goto-char (nth 4 modInfo))
          (setq currPoint (verilog-hide-port-list))
          (setq modInfo (verilog-find-prev-module-name currPoint))
          ))))

;; --------------------------------------------------
;; Show all port lists
;; --------------------------------------------------
(defun verilog-show-all-port-lists ()
  "Show all instantiation port lists that were hidden."
  (interactive)
  (verilog-show-port-list (point-min) (point-max)))

;; --------------------------------------------------
;; Hide 
;; Overlay is called 'ver-port-list
;; --------------------------------------------------
(defun verilog-hide-port-list ()
  "Hide the list of ports in the instantiation around point. Point has to be within the instantiation."
  (interactive)
  (save-excursion
    (let* ((limit 1000000)       ;; Begin and end should be between these many chars of point
            (cursor (point))
            (modInfo (verilog-find-prev-module-name (point)))
            (max-limit (+ cursor limit))
            ;; Make sure min and max limits are within the buffers
            (max-limit (if (> max-limit (point-max)) (point-max) max-limit))
            (end-re    verilog-inst-end-re)
            (modName   (nth 0 modInfo))
            (retVal    (nth 3 modInfo))  ;; Start of match
            (start     (nth 4 modInfo))
            (kws       verilog-keywords)
            kw kws-found
            end ov)
      (if (not (verilog-hidden-port-list)) ;; Only if it is not hidden
        (progn
          (while (and kws (not kws-found))
            (if (string= (car kws) modName) 
              (setq kws-found t))
            (setq kws (cdr kws)))
          (if (not kws-found)
            (progn
              (goto-char start) ;; Dont really need this
              ;; (if (re-search-backward start-re min-limit t)
              ;;     (setq start (nth 1 (match-data)))) ;; End of match
              ;; From the start-re position, look for end-re
              (if (re-search-forward end-re max-limit t)
                (setq end (nth 0 (match-data)))) ;; Start of match
              ;; If cursor is between start and end, do the overlay
              (if (and start end (>= cursor start) (<= cursor end))
                (progn 
                  (setq ov    (make-overlay start end))
                  (overlay-put ov 'ver-port-list t)
                  (overlay-put ov 'invisible t)
                  (overlay-put ov 'intangible t)
                  (overlay-put ov 'before-string "<...>")))))))
        retVal)))

;; --------------------------------------------------
;; Show verilog copyright that was hidden
;; --------------------------------------------------
(defun verilog-show-port-list (&optional start end)
  "Unhide the currently hidden port list. The hidden text has to be in the current line."
  (interactive)
  (save-excursion
    (let* ((start  (if start start (line-beginning-position)))
           (end    (if end end (line-end-position)))
           (ovlist (overlays-in start end)))
      (while ovlist
        (let ((overlay (car ovlist)))
          (if (overlay-get overlay 'ver-port-list)
            (delete-overlay overlay))
          (setq ovlist (cdr ovlist)))))))

;; --------------------------------------------------
;; See if there is a hidden port list overlay in the current line.
;; Overlay is called 'ver-port-list
;; --------------------------------------------------
(defun verilog-hidden-port-list (&optional start end)
  "See if we have a hidden port list overlay in the current line."
  (save-excursion
    (let* ((start  (if start start (line-beginning-position)))
           (end    (if end end (line-end-position)))
           (ovlist (overlays-in start end))
           (retVal nil))
      (while (and ovlist (not retVal))
        (let ((overlay (car ovlist)))
          (if (overlay-get overlay 'ver-port-list)
            (setq retVal t))
          (setq ovlist (cdr ovlist))))
      retVal)))


;; --------------------------------------------------
;; Toggle hide/unhide of port lists
;; --------------------------------------------------
(defun verilog-toggle-port-list-display ()
  "Toggle hide/unhide of current instantiation port list"
  (interactive)
  (let* ((end-re    verilog-inst-end-re)
         (start  (line-beginning-position))
         (cursor (point))
         (max-limit (point-max))
          ovlist end)
    (save-excursion
      (goto-char start)
      (if (re-search-forward end-re max-limit t)
        (setq end (nth 0 (match-data))))  ;; Start of match
      (if end
        (progn
          (goto-char end)
          (if (verilog-hidden-port-list start end)
            (verilog-show-port-list start end)
            (verilog-hide-port-list)))))))

;; --------------------------------------------------
;; Toggle hide/unhide of all port lists in the current buffer
;; --------------------------------------------------
(defun verilog-toggle-all-port-list-display ()
  "Toggle hide/unhide of all instantiation port lists"
  (interactive)
  (if (not (local-variable-p 'verHiddenAllPortLists))
    (progn
      (make-local-variable 'verHiddenAllPortLists)
      (verilog-hide-all-port-lists)
      (message "Hide all port lists"))
    (progn
      (kill-local-variable `verHiddenAllPortLists)
      (verilog-show-all-port-lists)
      (message "Show all port lists"))
    ))


;; --------------------------------------------------
;; Add key bindings for port list folding functions
;; --------------------------------------------------
(add-hook 'verilog-mode-hook
  (function
    (lambda ()
      (define-key verilog-mode-map "\C-ch" 'verilog-toggle-port-list-display)
      (define-key verilog-mode-map "\C-cH" 'verilog-toggle-all-port-list-display)
      )))


(provide 'verilogUtils)
