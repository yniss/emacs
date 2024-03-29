;;;;;;;;;;;;;;;;;
;;  Features   ;;
;;;;;;;;;;;;;;;;;
;;
;;   The package includes the following features:
;;    Feature                                               |  Keybind
;;    --------                                              |  -------
;;   - Clear-Case commands                                  |
;;       . co                                               |   C-c o
;;       . ci                                               |   C-c i
;;       . unco                                             |   C-c u
;;   - Auto-Untabify                                        |
;;   - TAG functions                                        |
;;       . find file in TAGs                                |
;;       . easy open/close instance in other window         |  C-c . or C-c ,
;;   - Indentation functions                                |
;;       . Align regexp to column of 1st row                |  C-x C-a
;;   - Rectangle - incremental numbers                      |  C-x r N
;;   - Copy line above                                      |  M-[up]
;;   - Face at point                                        |


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  FUNCTION POOL USER CUSTOMIZATION  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup function-pool nil
  "Provide common emacs functions to be used by other packages or interactively.")
  
;; In order to customize function-pool attributes - use custom-set-variables in .emacs
(defcustom function-pool-clear-case t
  "Support for clear case commands within emacs"
  :group 'function-pool
  :type 'boolean)
(defcustom function-pool-tags t
  "Support for using tags for easy files navigation"
  :group 'function-pool
  :type 'boolean)
(defcustom function-pool-untab t
  "Support for indentation with space instead of tab"
  :group 'function-pool
  :type 'boolean)
(defcustom function-pool-align t
  "Support for alignment functions"
  :group 'function-pool
  :type 'boolean)

;; key-bindings
(defvar function-pool-map nil
  "Keymap for function-pool")

;; TODO: Currently defcustom has no effect, should check how to define keymap with condition of defcustom values
;  (setq function-pool-map (make-sparse-keymap))
;  (if function-pool-clear-case
;      (progn
;        (define-key function-pool-map "\C-co"  'clear-case-co)  
;        (define-key function-pool-map "\C-cu" 'clear-case-unco)
;        (define-key function-pool-map "\C-ci" 'clear-case-ci)))
;  (if function-pool-tags
;      (define-key function-pool-map "\C-xf" 'ido-find-file-in-tag-files))
;  (if function-pool-untab
;      (define-key function-pool-map "\C-x\C-s" 'save-buffer-without-tabs)))

;;;;;;;;;;;;;;;;;;;;
;;  CLEAR CASE    ;;
;;;;;;;;;;;;;;;;;;;;
(defun clear-case-base (cc-cmd)
  "Clear-Case base function - run cc command from shell on current file"
  (shell-command (format "%s %s" cc-cmd buffer-file-name))
  (kill-buffer "*Shell Command Output*")
  (revert-buffer t t))

(defun clear-case-co ()         
  "Clear-Case checkout current file."
  (interactive)
;  (clear-case-base "ct co -nc"))
  (execute-shell-command "ct co -nc"))
;(if function-pool-clear-case
;    (global-set-key "\C-co" 'clear-case-co))

(defun clear-case-unco ()
  "Clear-Case uncheckout current file."
  (interactive)
;  (clear-case-base "ct unco -rm"))
  (execute-shell-command "ct unco -rm"))
;(global-set-key "\C-cu" 'clear-case-unco)

(defun clear-case-ci ()
  "Clear-Case checkin current file."
  (interactive)
;  (clear-case-base "ct ci -nc"))
  (execute-shell-command "ct ci -nc"))
;(global-set-key "\C-ci" 'clear-case-ci)


;; TODO: temp
;; TODO: temp
;; TODO: temp
;; TODO: temp
;; TODO: temp
;;;;;;;;;;;;;;;
;;   UNTAB   ;;
;;;;;;;;;;;;;;;
(defun untabify-buffer ()
  "Untabify whole buffer text."
  (interactive)
  (untabify (point-min) (point-max)))
  

(defun untabify-line ()
  "Untabify current line."
  (interactive)
  (untabify (point-at-bol) (point-at-eol)))


(defun verilog-tab-indent-with-space ()
  "Indent line using space." 
  (interactive)
  (electric-verilog-tab)
  (untabify-line))


(defun save-buffer-without-tabs ()
  "Untabify buffer, then save it."
  (interactive)
  (untabify-buffer)
  (save-buffer))
;(global-set-key "\C-x\C-s" 'save-buffer-without-tabs)


;;;;;;;;;;;;;;;;;
;;    TAGS     ;;
;;;;;;;;;;;;;;;;;
(require 'ido)
;(ido-mode t)
(setq ido-enable-flex-matching t)
(defun ido-find-file-in-tag-files ()
  (interactive)
  (save-excursion
    (let ((enable-recursive-minibuffers t))
      (visit-tags-table-buffer))
    (find-file
     (expand-file-name
      (ido-completing-read
       "Project file: " (tags-table-files) nil t)))))
;(global-set-key "\C-xf" 'ido-find-file-in-tag-files)

(defun xref-pop-marker-stack-close-other-window ()
  (interactive)
  (xref-pop-marker-stack)
  (delete-window))


;;;;;;;;;;;;;;;;;;;;;;
;;   INDENT/ALIGN   ;;
;;;;;;;;;;;;;;;;;;;;;;
(defun align-regexp-first-line (start end regexp)
  "Align specified regexp to the column of the first line in which it is matched."
  (interactive "r\nsAlign first regexp:")
  (save-excursion 
    (goto-char start)
    (when (search-forward-regexp regexp end t) 
      (search-backward-regexp regexp start t)
      (setq col (current-column))
      (forward-char)
      (align-regexp-to-col start end regexp col))))


(defun align-regexp-to-col (start end regexp col)
  "Align regular expression to a specified column."
  (align-regexp start end 
                (concat "\\(\\s-*\\)" regexp) 1 (- col) nil))


(defun align-regexp-simple (start end regexp &optional repeat)
  "A simplified align-regexp."
  (align-regexp start end 
                (concat "\\(\\s-*\\)" regexp) 1 align-default-spacing repeat))


;;;;;;;;;;;;;;;;;;;;
;;     FACE       ;;
;;;;;;;;;;;;;;;;;;;;
(defun what-face ()
  "Prints face of point"
  (interactive)
  (setq pos (point))
  (let ((face (or (get-char-property pos 'read-face-name)
                  (get-char-property pos 'face))))
    (if face (message "Face: %s" face) (message "No face at %d" pos))))


;;;;;;;;;;;;;;;;;;;;
;;    GENERAl     ;;
;;;;;;;;;;;;;;;;;;;;
(defun execute-shell-command (command) ;;TODO: add interactive - with shortcut
  "Execute shell-command asynchronously, followed by revert-buffer."
  (message "%s %s" command buffer-file-name)
  (set-process-sentinel 
   (start-process-shell-command  "execute-process" "execute-process-buffer" command buffer-file-name)
   (lambda (p e) (when (= 0 (process-exit-status p))
                                   (revert-buffer t t)))))


(defun inside-comment ()
  "Check if point is inside a comment"
  (nth 4 (syntax-ppss)))


; copy-above-line
(autoload 'copy-from-above-command "misc"
    "Copy characters from previous nonblank line, starting just above point.
  \(fn &optional arg)"
    'interactive)
(defun copy-above-line ()
  "Uses copy-from-above-command to copy line above from beginning of line."
  (interactive)
  (beginning-of-line)
  (copy-from-above-command)
  (newline))

; rectangle numbers
(defun rectangle-incremental-numbers ()
  "Uses rectangle-number-lines to create rectangle of incremental numbers starting from a user given number."
  (interactive)
  (let (inc-start clean-end-pos)
  (setq inc-start (prompt-user-arg "Incremental number rectangle - start (default 0)"))
  (rectangle-number-lines (region-beginning) (region-end) (string-to-number inc-start))
  (forward-word)
  (forward-char)
  (setq clean-end-pos (point))
  (goto-char (region-beginning))
  (forward-word)
  (kill-rectangle (point) clean-end-pos)
  (deactivate-mark)))

(defun increment-number-hexadecimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer hex-format)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789abcdefABCDEF")
        (when (re-search-forward "[0-9a-fA-F]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 16) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 16 field-width) answer)))
          (if (equal (match-string 0) (upcase (match-string 0)))
              (setq hex-format "X")
            (setq hex-format "x"))
          (replace-match (format (concat "%0" (int-to-string field-width)
                                         hex-format)
                                 answer)))))))

(defun increment-number-hexadecimal-by-x ()
  (interactive)
  (let ((num (string-to-number (read-string "Increment by how much? (default 4):" nil nil "4") 16)))
    (increment-number-hexadecimal num)))

(defun increment-number-decimal (&optional arg)
  "Increment the number forward from point by 'arg'."
  (interactive "p*")
  (save-excursion
    (save-match-data
      (let (inc-by field-width answer)
        (setq inc-by (if arg arg 1))
        (skip-chars-backward "0123456789")
        (when (re-search-forward "[0-9]+" nil t)
          (setq field-width (- (match-end 0) (match-beginning 0)))
          (setq answer (+ (string-to-number (match-string 0) 10) inc-by))
          (when (< answer 0)
            (setq answer (+ (expt 10 field-width) answer)))
          (replace-match (format (concat "%0" (int-to-string field-width) "d")
                                 answer)))))))

(defun increment-number-decimal-by-x ()
  (interactive)
  (let ((num (string-to-number (read-string "Increment by how much? (default 1):" nil nil "1") 10)))
    (increment-number-decimal num)))

(defun prompt-user-arg (prompt &optional print-arg)
  "Accept argument from the user. After user enters 'return' char argument is accepted."
  (let ((start (point)) string)
    (setq string (read-from-minibuffer (concat prompt ": ")))
    (if (and (not (equal string ""))
             print-arg)
        (insert string))
    string))

;;;###autoload
(define-minor-mode function-pool-minor-mode
  "Global functions pool minor mode"
  :lighter ""
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "\C-co") 'clear-case-co)
            (define-key map (kbd "\C-cu") 'clear-case-unco)
            (define-key map (kbd "\C-ci") 'clear-case-ci)
;           (define-key map (kbd "\C-xf") 'ido-find-file-in-tag-files)
            (define-key map [remap set-fill-column] 'ido-find-file-in-tag-files)
            (define-key map (kbd "\C-c.") 'xref-find-definitions-other-window)
;           (define-key map [remap end-of-buffer] 'xref-find-definitions-other-window)
            (define-key map (kbd "\C-c,") 'xref-pop-marker-stack-close-other-window)
;           (define-key map [remap beginning-of-buffer] 'xref-pop-marker-stack-close-other-window)
;           (define-key map (kbd "\C-x\C-s") 'save-buffer-without-tabs)
            (define-key map [remap save-buffer] 'save-buffer-without-tabs)
            (define-key map (kbd "\C-x\C-a") 'align-regexp-first-line)
            (define-key map [M-up]  'copy-above-line)
            (define-key map [remap rectangle-number-lines] 'rectangle-incremental-numbers)
            (define-key map (kbd "\C-c+") 'increment-number-hexadecimal-by-x)
            map))
;;;  (if function-pool-minor-mode
;;;      (add-hook 'function-pool-minor-mode-hook  'function-pool-map-init nil t)
;;;    (remove-hook 'function-pool-minor-mode-hook 'function-pool-map-init t)))

              
(provide 'function-pool)
