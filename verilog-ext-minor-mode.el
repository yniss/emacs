;;;;;;;;;;;;;;;;;
;;  Features   ;;
;;;;;;;;;;;;;;;;;
;;
;;   The package includes the following features:
;;    Feature                                               |  Keybind
;;    -------                                               |  -------
;;   - Templates Auto-insert                                |  by writing one of the template keywords (e.g. always, for, etc.)
;;   - Transform module ports into instance ports           |  C-c p
;;   - Copy module and                                      |  C-c m (copy module)
;;     Auto instantiate copied module                       |  C-c n (instantiate copied module)
;;   - Auto Comment line (by 3x'/') or header (by 4x'/')    |  /// (comment line) //// (comment header)

(require 'verilog-mode)
(require 'function-pool)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  VERILOG EXTENSION USER CUSTOMIZATION  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup verilog-ext nil
  "Provide extension functions for verilog.")
  
;; In order to customize verilog-ext attributes - use custom-set-variables in .emacs
(defcustom verilog-ext-comments t
  "Support auto insertion of comment line (by 3x'/') or header (by 4x'/')"
  :group 'verilog-ext
  :type 'boolean)
(defcustom verilog-ext-auto-templates t
  "Support auto templates insertion for verilog keywords."
  :group 'verilog-ext
  :type 'boolean)

;;;;;;;;;;;;;;;
;; Templates ;;
;;;;;;;;;;;;;;;
(defun verilog-ext-template-check () 
  (let (cur-point bol-point)
    (save-excursion
      (setq cur-point (point))
      (beginning-of-line)
      (setq bol-point (point))
      (goto-char cur-point)
      (re-search-backward "^[ \t]*\\(\\w+\\)" bol-point t))
    (when (and
           (hash-table-p verilog-electric-words)
           (setq electric-func (gethash (match-string 1) verilog-electric-words))
           ; check if template word is inserted, but not in a comment
           (not (inside-comment))
           (not (null electric-func)))
      (funcall electric-func))))


(defun verilog-ext-template-query (query &optional optional)
  "Query a decision from the user. Next char user enters is the answer, unless enters 'return' char."
  (let ((start (point)))
    (message query)
    (let ((char (read-char)))
      (delete-region start (point))
      (if (and optional (eq char ?\r))
      (progn (insert " ")
         (unexpand-abbrev)
         (throw 'abort "Error:  template aborted")) ;TODO: this doesn't exit correctly from error, there is no catch 'abort
      char))))


(defun remove-template-word (word)
  "Remove original template word"
  (re-search-backward "\\(word\\)" nil t)
  (replace-match "" 1))
 
(defun electric-always ()
  "Insert verilog 'always' template"
  (let (always-pos end-pos)
  (save-excursion
    (setq always-type (verilog-ext-template-query "(c)omb or (s)eq?" t))
    (remove-template-word "always")
    ; always template
    (verilog-sk-always)
    ; indent
    (re-search-backward "always" nil t)
    (set-mark (point))
    (re-search-forward "end" nil t)
    (electric-verilog-tab)
    (deactivate-mark)
    ; remove "/*AUTOSENSE*/"
    (re-search-backward " /\\*AUTOSENSE\\*/ " nil t)
    (replace-match "")
    ; check if seq or comb
    (cond ((eq always-type ?s) (setq pos (always-seq)))
          ((eq always-type ?c) (setq pos (always-comb)))
          (t "default"))))
  (goto-char pos))

(defun always-seq ()
  "Sequential always"
  (insert "posedge ")
  (setq clk   (prompt-user-arg "clock name" t))
  (insert " or negedge ")
  (setq reset (prompt-user-arg "reset name" t))
  (forward-line 1)
  (insert (concat "\tif (~" reset ") begin" "\n\n\tend\n\telse begin\n\n\tend"))
  (forward-line -4)
  (insert "\t\t")
  (setq pos (point))
  pos)

(defun always-comb ()
  "Combinationational always"
  (insert "\*")
  (forward-line 1)
  (insert "\t")
  (setq pos (point))
  pos)

(defun electric-case ()
  "Insert verilog 'case' template"
  (save-excursion
    (remove-template-word "case")
    (verilog-sk-case)))

(defun electric-for ()
  "Insert verilog 'for' template"
  (save-excursion
    (remove-template-word "for")
    (verilog-sk-for)))

(defun electric-generate ()
  "Insert verilog 'generate' template"
  (save-excursion
    (remove-template-word "generate")
    (verilog-sk-generate)))

(defun electric-module () ;;TODO: add support for prompt parameters, ports, etc.
  "Insert verilog 'module' template"
  (save-excursion
    (remove-template-word "module")
    (verilog-sk-module)
    ; remove "/*AUTOARG*/"
    (forward-line -1)
    (re-search-forward "AUTOARG" nil t)
    (replace-match "")
    (delete-backward-char 2)
    (delete-char 2)))

(defun electric-casex ()
  "Insert verilog 'casex' template"
  (save-excursion
    (remove-template-word "casex")
    (verilog-sk-casex)))

(defun electric-casez ()
  "Insert verilog 'casez' template"
  (save-excursion
    (remove-template-word "casez")
    (verilog-sk-casez)))

(defvar verilog-electric-words (make-hash-table :test 'equal) ;;TODO: this causes issues in copy paste
  "Words for auto-generating verilog structures")
(setf (gethash "always"   verilog-electric-words) 'electric-always)
(setf (gethash "case"     verilog-electric-words) 'electric-case)
;(setf (gethash "for"      verilog-electric-words) 'electric-for) 
(setf (gethash "generate" verilog-electric-words) 'electric-generate)
(setf (gethash "module"   verilog-electric-words) 'electric-module)
(setf (gethash "casex"    verilog-electric-words) 'electric-casex)
(setf (gethash "casez"    verilog-electric-words) 'electric-casez)


;;;;;;;;;;;;;;;;;;;;;;
;;  Instance Ports  ;;
;;;;;;;;;;;;;;;;;;;;;;
(defun verilog-ext-indent-instance-ports ()
  "Transforms selected region to format of verilog instantiation ports, e.g. '.port_name (),'.
   Function also indent region according to the port prior to the region, if there is such.
   If port line doesn't contains only '.' or '(),' then function fills only the missing part.
   In addition, function removes common keywords that may have been copied from module ports, such as input, output and bus widths."
  (interactive)
  (save-excursion
    (when (> (point) (mark)) (exchange-point-and-mark))
    (let ((prev-point (point)))
      (while (< (point) (region-end))
        (when (and (re-search-forward "^\\([ \t]*\\(input\\|output\\)*[ \t]*\\(wire\\|reg\\)*[ \t]*\\(\\[.*\\]\\)*[ \t]*\\(\\.+\\)*\\)\\w+\\((*[ \t]*.*)*,*\\)" (region-end) t) ;;TODO: is it possible for any other words? for example signed
                 (not (inside-comment)))
              (when (or (not (match-string 5)) (not (match-string 6))) ;; check that line is not exactly in port format (otherwise leave it)
                    (replace-match "." nil nil nil 1)
                    (replace-match " ()," nil nil nil 6)))
              (forward-line 1))
      (setq point-instance-start (re-search-backward "^[ \t]*\\w+" (point-min) t)) ;; find module instance name as alignment search border
      (goto-char prev-point)
      (if (re-search-backward "^[ \t]*\\.+\\w+[ \t]*\\(.*\\)," point-instance-start t) ;; search for previous port format line, but not before instance beginning
          (progn
            (align-regexp-first-line (point) (region-end) "\\.")
            (align-regexp-first-line (point) (region-end) "("))
        (progn
          (electric-verilog-tab)
          (align-regexp-simple prev-point (region-end) "(")))
      (setq point-instance-end (re-search-forward ");" (point-max) t)) ;; search module instance end ");" and set border for following search 
      (when point-instance-end ;; search and replace last ")," by ")" if there was such inside region 
        (when (and (re-search-backward "\\(),\\)" point-instance-start t) (< (point) (region-end)) (> (point) prev-point))
          (replace-match ")" nil nil nil 1)))
      )))
;(global-set-key "\M-i" 'verilog-ext-indent-instance-ports)    ;;TODO: replace with keymap / add to verilog-keymap


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Copy Module & Make Instance  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun verilog-ext-init-module ()
  "Initialize a general purpose module struct with default nil values. 
   This module will be overriden whenever a module is copied."
  (defvar gp-module-name nil)
  (defvar gp-module-parameters nil)
  (defvar gp-module-ports nil))


(defun verilog-ext-copy-module ()
  "Copy module name, ports and optionally paramteres to the general purpose module ('gp-module').
   The copied module can be used to create a module instance using make-instance."
  (interactive)
  (let (error-not-in-module module-name (module-parameters nil) (module-ports nil) prev-point module-end (loop-break nil))
  (save-excursion
    ; check that function is called from within a module declaration, otherwise abort function and return error
    (setq
     error-not-in-module
     (catch 'module-error
       (end-of-line)
       (when (or (not (re-search-backward "^[ *\\t*]*\\(module\\|);\\)" nil t))
                 (equal ");" (match-string 1)))
         (throw 'module-error "Error: Called verilog-ext-copy-module not from within a module declaration"))       
       ; extract module name
       (re-search-forward "module *\\(\\w+\\) *" nil t) ;;TODO: does not support TABs instead of spaces
       (setq module-name (match-string-no-properties 1))
       ; save current location for later
       (setq prev-point (point))
       (when (not (re-search-forward ");" nil t))
         (throw 'module-error "Error: module has no ');' ending symbol"))
       (setq module-end (point))
       (goto-char prev-point) 
       ; check if module have parameters, either in '#(parameter <names>)' format or inside declarations, preceding ports 
       (re-search-forward "\\(\\(# *( *parameter\\)\\|\\((.*\\(\n\\)+.*\\(\n\\)*.*parameter\\)\\)*" nil t)
       ; module consists parameters
       (when (match-string-no-properties 1)
         (while (not loop-break)
           (or 
            (and (re-search-forward "\\(\\w+\\) *=" module-end t)
                 (not (inside-comment))
                 (setq module-parameters (append module-parameters (list (match-string-no-properties 1)))))
            (and (re-search-forward "\\()\\)" module-end t)
                 (setq loop-break (match-string-no-properties 1)))
            (setq loop-break t))))
       (goto-char prev-point) 
       (while (< (point) module-end)
         (when (and (re-search-forward "^ *\\(\\(input\\|output\\|inout\\) +.*$\\)" module-end t)
                  (not (inside-comment)))
           (setq module-ports (append module-ports (list (match-string-no-properties 1)))))
         (forward-line))
       (message "Copying module %s...done" module-name)
       nil))
    (if error-not-in-module (error error-not-in-module))
    (setf gp-module-name module-name
          gp-module-ports module-ports
          gp-module-parameters module-parameters))))


(defun verilog-ext-make-instance ()
  "Create an instance from 'gp-module' which was created by calling verilog-ext-copy-module."
  (interactive)
  (let (error-no-module prev-point)
    (setq
     error-no-module
     (catch 'no-module
       (unless gp-module-name
         (throw 'no-module  "Error: no module was copied"))
       (electric-verilog-tab)
       (insert gp-module-name " ")
       ; if there are parameters - insert them and indent
       (when gp-module-parameters
         (insert "#(")
         (newline)
         (setq prev-point (point))       
         (dolist (param gp-module-parameters)          
           (insert "." param " (), ")
           (newline))
         (set-mark (point))
         (goto-char prev-point)
         (verilog-ext-indent-instance-ports)
         (goto-char (region-end))
         (re-search-backward ")" nil t)
         (replace-match "))")
         (newline))
       ; insert instance name and indent
       (insert gp-module-name "_i (")
       (beginning-of-line)
       (electric-verilog-tab)
       (end-of-line)
       (newline)
       ; insert ports and indent
       (setq prev-point (point))
       (dolist (port gp-module-ports)
         (insert port)
         (newline))
       (set-mark (point))
       (insert ");")
       (goto-char prev-point)
       (verilog-ext-indent-instance-ports)
       (deactivate-mark)
       (re-search-forward ").*\\(\n\\)+.*);" nil t)
       (replace-match "));")
       (forward-line)
       nil))
    (message "Instantiating %s...done" gp-module-name)
    (if error-no-module (error error-no-module))
  ))

;;;;;;;;;;;;;;;
;;  Comment  ;;
;;;;;;;;;;;;;;;
(defvar *comment-count* 0)

(defun incr-comment-count ()
  (setf *comment-count* (+ 1 *comment-count*)))
  
(defun init-comment-count (&optional init-val)
  (or init-val (setq init-val 0))
  (setf *comment-count* (+ init-val (- *comment-count* *comment-count*))))

(defun verilog-ext-comment-check () 
  "Check if 3 or 4 comment symbols entered"
  (cond ((= *comment-count* 3) (if (re-search-backward "/\\{3\\}" (- (point) 3) t) (comment-line 4) (init-comment-count 1)))
        ((= *comment-count* 4) (if (re-search-backward "/\\{35\\}" (- (point) 35) t) (comment-header) (init-comment-count 1)))))

(defun comment-line (start-idx)
  "Enter a comment line"
  (setq repeat (- 35 start-idx))
  (dotimes (i repeat)
    (insert "/"))
  (end-of-line))

(defun comment-header ()
  "Enter a comment header"
  (end-of-line)
  (newline)
  (insert "// ")
  (newline)
  (comment-line 0)
  (forward-line -1)
  (forward-char 3)
  (init-comment-count))

(defun verilog-ext-comment-region-inline ()
  "Comment out a region inside a line using /**/ comment format"
  (interactive)
 ;(let
  (save-excursion
    (insert "*/")
    (goto-char (region-beginning))
    (insert "/*")))
 
  
;;;;;;;;;;;;;;;
;;  General  ;;
;;;;;;;;;;;;;;;
(defun verilog-ext-check ()
  "Check if we should add verilog structure or comment"
  ; last character is space or /
  (cond ((and verilog-ext-auto-templates (= last-command-event ?\s)) (progn (verilog-ext-template-check) (init-comment-count)))
        ((and verilog-ext-comments (= last-command-event ?/)) (progn (incr-comment-count) (verilog-ext-comment-check )))
        (t (init-comment-count))))
        

(define-minor-mode verilog-ext-minor-mode
  "Verilog extensions minor mode"
  :lighter " Verilog-ext"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "\C-c p") 'verilog-ext-indent-instance-ports)
;           (define-key map [remap tab-to-tab-stop] 'verilog-ext-indent-instance-ports)
            (define-key map (kbd "\C-c m") 'verilog-ext-copy-module)
            (define-key map (kbd "\C-c n") 'verilog-ext-make-instance)
            (define-key map (kbd "\C-c \C-v") 'verilog-ext-comment-region-inline)
            (define-key map [remap save-buffer] 'save-buffer-without-tabs)
            map)
  (if verilog-ext-minor-mode
      (progn
        (add-hook 'post-self-insert-hook
                  'verilog-ext-check nil t)
        (add-hook 'verilog-ext-minor-mode-hook
                  'verilog-ext-init-module))
    (remove-hook 'post-self-insert-hook
                 'verilog-ext-check t)))




