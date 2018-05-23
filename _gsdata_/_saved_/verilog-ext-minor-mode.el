;;;;;;;;;;;;;;;;;
;;  Features   ;;
;;;;;;;;;;;;;;;;;
;;
;;   The package includes the following features:
;;
;;   - Templates Auto-insert
;;   - Transform module ports into instance ports
;;   - Auto Comment line (by 3x'/') or header (by 4x'/')  

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
  (save-excursion
    (copy-region-as-kill (- (point) 1) (progn (backward-word) (point)))
    (hash-table-p verilog-electric-words)
    (setq electric-func (gethash (current-kill 0) verilog-electric-words)))
  (setq kill-ring (cdr kill-ring))
  ; check if template word is inserted, but not in a comment 
  (if (and (not (inside-comment)) (not (null electric-func)))
      (funcall electric-func)))


(defun verilog-ext-template-query (query &optional optional)
  "Query a decision from the user. Next char user enters is the answer, unless enters 'return' char."
  (let ((start (point)))
    (message query)
    (let ((char (read-char)))
      (delete-region start (point))
      (if (and optional (eq char ?\r))
      (progn (insert " ")
         (unexpand-abbrev)
         (throw 'abort "ERROR:  Template aborted")) ;TODO: this doesn't exit correctly from error
      char))))

(defun verilog-ext-template-arg (prompt)
  "Accept template argument from the user. After user enters 'return' char argument is accepted."
  (let ((start (point)) string)
    (setq string (read-from-minibuffer (concat prompt ": ")))
    (if (not (equal string ""))
        (insert string))
    string))

(defun remove-template-word ()
  "Remove original template word"
  (backward-char)
  (kill-word -1) 
  (setq kill-ring (cdr kill-ring)))
 
(defun electric-always ()
  "Insert verilog 'always' template"
  (save-excursion
    (setq always-type (verilog-ext-template-query "(c)omb or (s)eq?" t))
    (remove-template-word)
    ; always template
    (verilog-sk-always)
    ; remove "/*AUTOSENSE*/"
    (forward-line -1)
    (re-search-forward "AUTOSENSE" nil t)
    (replace-match "")
    (delete-backward-char 3)
    (delete-char 3)
    ; check if seq or comb
    (cond ((eq always-type ?s) (setq pos (always-seq)))
          ((eq always-type ?c) (setq pos (always-comb)))
          (t "default")))
  (goto-char pos))

(defun always-seq ()
  "Sequential always"
  (insert "posedge ")
  (setq clk   (verilog-ext-template-arg "clock name"))
  (insert " or negedge ")
  (setq reset (verilog-ext-template-arg "reset name"))
  (forward-line 1)
  (insert (concat "\tif (~" reset ")" "\n\n\telse\n"))
  (forward-line -2)
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
    (remove-template-word)
    (verilog-sk-case)))

(defun electric-for ()
  "Insert verilog 'for' template"
  (save-excursion
    (remove-template-word)
    (verilog-sk-for)))

(defun electric-generate ()
  "Insert verilog 'generate' template"
  (save-excursion
    (remove-template-word)
    (verilog-sk-generate)))

(defun electric-module () ;;TODO: add support for prompt parameters, ports, etc.
  "Insert verilog 'module' template"
  (save-excursion
    (remove-template-word)
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
    (remove-template-word)
    (verilog-sk-casex)))

(defun electric-casez ()
  "Insert verilog 'casez' template"
  (save-excursion
    (remove-template-word)
    (verilog-sk-casez)))

(defvar verilog-electric-words (make-hash-table :test 'equal)
  "Words for auto-generating verilog structures")
(setf (gethash "always"   verilog-electric-words) 'electric-always)
(setf (gethash "case"     verilog-electric-words) 'electric-case)
(setf (gethash "for"      verilog-electric-words) 'electric-for) 
(setf (gethash "generate" verilog-electric-words) 'electric-generate)
(setf (gethash "module"   verilog-electric-words) 'electric-module)
(setf (gethash "casex"    verilog-electric-words) 'electric-casex)
(setf (gethash "casez"    verilog-electric-words) 'electric-casez)


;;;;;;;;;;;;;;;;;;;;;;
;;  Instance Ports  ;;
;;;;;;;;;;;;;;;;;;;;;;
(defun make-instance-ports ()
  "Transforms selected region to format of verilog instantiation ports, e.g. '.<port_name> (),'.
   Function also indent region according to the port prior to the region, if there is such.
   If port line doesn't contains only '.' or '(),' then function fills only the missing part.
   In addition, function removes common keywords that may have been copied from module ports, such as input, output and bus widths."
  (interactive)
  (save-excursion
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((prev_point (point)) (prev_mark (mark)))
      (while (< (point) (region-end))
        (if (and (re-search-forward "^\\([ \t]*\\(input\\|output\\)*[ \t]*\\(wire\\|reg\\)*[ \t]*\\(\\[.*\\]\\)*[ \t]*\\(\\.+\\)*\\)\\w+\\((*[ \t]*.*)*,*\\)" (region-end) t)
                 (not (inside-comment)))
            (progn
              (if (or (not (match-string 5)) (not (match-string 6))) ;; check that line is not exactly in port format (otherwise leave it)
                  (progn
                    (replace-match "." nil nil nil 1)
                    (replace-match " ()," nil nil nil 6)))))
        (forward-line 1))
      (goto-char prev_point)
      (re-search-backward "^[ \t]*\\.+\\w+[ \t]*\\(.*\\)," (point-min) t);; search for previous port format line ;;TODO: WHAT IF THERE ARE NO PORTS ALREADY?
      (align-regexp-col-first (point) prev_mark "\\.")
      (align-regexp-col-first (point) prev_mark "("))))      
;(global-set-key "\M-i" 'make-instance-ports)    ;;TODO: replace with keymap / add to verilog-keymap


;;;;;;;;;;;;;;;
;;  Comment  ;;
;;;;;;;;;;;;;;;
(defvar *comment-count* 0)

(defun incr-comment-count ()
  (setf *comment-count* (+ 1 *comment-count*)))
  
(defun zero-comment-count ()
  (setf *comment-count* (- *comment-count* *comment-count*)))

(defun verilog-ext-comment-check () 
  "Check if 3 or 4 comment symbols entered"
  (cond ((= *comment-count* 3) (comment-line 4))
        ((= *comment-count* 4) (comment-header))))

(defun comment-line (start-idx)
  "Enter a comment line"
  (setq repeat (- 35 start-idx))
  (dotimes (i repeat)
        (insert "/")))

(defun comment-header ()
  "Enter a comment header"
  (newline)
  (insert "// ")
  (newline)
  (comment-line 0)
  (forward-line -1)
  (forward-char 3))
 
  
;;;;;;;;;;;;;;;
;;  General  ;;
;;;;;;;;;;;;;;;
(defun verilog-ext-check ()
  "Check if we should add verilog structure or comment"
  ; last character is space or /
  (cond ((and verilog-ext-auto-templates (= last-command-event ?\s)) (progn (verilog-ext-template-check) (zero-comment-count)))
        ((and verilog-ext-comments (= last-command-event ?/)) (progn (incr-comment-count) (verilog-ext-comment-check )))
        (t (zero-comment-count))))
        

(define-minor-mode verilog-ext-minor-mode
  "Verilog extensions minor mode"
  :lighter " Verilog-ext"
  :keymap (let ((map (make-sparse-keymap)))
;			(define-key map (kbd "\M-i") 'make-instance-ports)
			(define-key map [remap tab-to-tab-stop] 'make-instance-ports)
			map)
  (if verilog-ext-minor-mode
      (add-hook 'post-self-insert-hook
                'verilog-ext-check nil t)
    (remove-hook 'post-self-insert-hook
                 'verilog-ext-check t)))




