;;;;;;;;;;;;;;;;;
;;  Features   ;;
;;;;;;;;;;;;;;;;;
;;
;;   The package includes the following features:
;;
;;   - Clear-Case commands
;;       . co
;;       . ci
;;       . unco
;;   - Auto-Untabify
;;   - TAG functions
;;       . find file in TAGs
;;       . easy open/close instance in other window 
;;   - Indentation functions
;;       . Align regexp to column of 1st row
;;   - Face at point


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

(defun function-pool-map-init ()
  (if function-pool-clear-case
      (progn
        (global-set-key "\C-co" 'clear-case-co)
        (global-set-key "\C-cu" 'clear-case-unco)
        (global-set-key "\C-ci" 'clear-case-ci)))
  (if function-pool-tags
      (progn
        (global-set-key "\C-xf" 'ido-find-file-in-tag-files)
        (global-set-key "\M->" 'xref-find-definitions-other-window)
        (global-set-key "\M-<" 'xref-pop-marker-stack-close-other-window)))
  (if function-pool-untab
      (global-set-key "\C-x\C-s" 'save-buffer-without-tabs))
  (if function-pool-align
      (global-set-key "\M-A" 'align-regexp-col-first))
  )
;; TODO: SHOULD REPLACE global-set-key
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
(defun align-regexp-col-first (start end regexp)
  "Align to the column of the first line with specified regular expression."
  (interactive "r\nsAlign first regexp:")
  (save-excursion 
    (unwind-protect
        (goto-char start)
      (if (search-forward-regexp regexp end t) 
          ;; found regexp
          (progn
            (end-of-line)
            (search-backward-regexp regexp start t)
            (setq col (current-column))
;            (message "Column is: %d" col) ;; FOR DEBUG
            (align-regexp-to-col start end regexp col))
        ;; didn't find
        (message "Can't find regexp: %s" regexp)))))
;(global-set-key "\M-A" 'align-regexp-col-first)


(defun align-regexp-to-col (start end regexp col)
  "Align regular expression to a specified column."
  (align-regexp start end 
                (concat "\\(\\s-*\\)" regexp) 1 (- col) nil))


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


;;;###autoload
(define-minor-mode function-pool-minor-mode
  "Global functions pool minor mode"
  :lighter ""  
  (if function-pool-minor-mode
      (add-hook 'function-pool-minor-mode-hook  'function-pool-map-init nil t)
    (remove-hook 'function-pool-minor-mode-hook 'function-pool-map-init t)))

      
(provide 'function-pool)
