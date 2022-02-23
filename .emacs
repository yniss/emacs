;;-------
;; MELPA
;;-------
;; Doesn't work if no access to web!
(require 'package)
;(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
;                    (not (gnutls-available-p))))
;       (proto (if no-ssl "http" "https")))
;  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
;  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
;  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
;  (when (< emacs-major-version 24)
;    ;; For important compatibility libraries like cl-lib
;    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; Comment/uncomment this line to enable MELPA Stable if desired.  See `package-archive-priorities`
;; and `package-pinned-packages`. Most users will not need or want to do this.
;;(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


;; disable splash screen and startup message
(setq inhibit-startup-message t) 
(setq initial-scratch-message nil)

(add-to-list 'load-path "~/emacs/")

;;;;;;; FROM XEMACS init.el ; TODO: go over and check
(defun prepend-path ( my-path )
  (setq load-path (cons (expand-file-name my-path) load-path)))
(defun append-path ( my-path )
(setq load-path (append load-path (list (expand-file-name my-path)))))

;(prepend-path "~/elisp/ecb-2.32")
;(prepend-path "~/elisp")
;;; Turn on autofill mode automatically in text mode
(setq text-mode-hook
   '(lambda ()   
      (setq fill-column 70)
      (auto-fill-mode 1)))

;; Use tabs for indentation, not spaces
(setq-default indent-tabs-mode t)
(setq-default tab-width 4)
(setq-default tab-stop-list '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60 64 68 72 76 80 84 88 92 96 100 104 108 112 116 120))
;;;;;;; END FROM XEMACS init.el

;;---------------
;;; Verilog
;;---------------
;; Load verilog mode only when needed
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t )
;; Any files that end in .v should be in verilog mode
(autoload 'vera-mode "vera-mode" "Vera Mode" t)
(setq auto-mode-alist (append '(("\\.vr$"   . vera-mode)
                ("\\.vrh$"  . vera-mode)
                ("\\.vri$"  . vera-mode)
                ("\\.vpp$"  . vera-mode)
                ("\\.ova$"  . vera-mode)
                ("\\.scr"   . dcsh-mode)
                ("\\.tst$"  . qsim-mode) 
                ("\\.v$"    . verilog-mode)
                ("\\.vh$"   . verilog-mode)
                ("\\.sv$"   . verilog-mode)
                ("\\.svh$"  . verilog-mode)
                ("\\.vhdl$" . vhdl-mode)
                ("\\.vhd$"  . vhdl-mode))
                auto-mode-alist))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-custom-commands
   (quote
    (("c" "Unscheduled TODOs for scheduling next month's tasks (excluding LowPriority and FutureFeature tagged)"
      ((tags-todo "-LowPriority-FutureFeature"
                  ((org-agenda-skip-function
                    (quote
                     (org-agenda-skip-entry-if
                      (quote timestamp))))
                   (org-agenda-overriding-header "Unscheduled TODOs for scheduling next month's tasks (excluding LowPriority and FutureFeature tagged)")))
       (agenda ""
               ((org-agenda-span 30))))
      nil)
     ("f" "FutureFeature tagged TODOs (excluding LowPriority tagged)"
      ((tags-todo "FutureFeature-LowPriority"
                  ((org-agenda-overriding-header "FeutureFeature tagged TODOs (excluding LowPriority tagged)"))))
      nil)
     ("x" "Unscheduled TODO"
      ((todo ""
             ((org-agenda-overriding-header "Unscheduled TODO")
              (org-agenda-skip-function
               (quote
                (org-agenda-skip-entry-if
                 (quote timestamp))))))
       (agenda ""
               ((org-agenda-span 30))))
      nil))))
 '(org-agenda-weekend-days (quote (5 6)))
 '(package-selected-packages (quote (use-package exec-path-from-shell jedi smartparens)))
 '(vc-follow-symlinks t)
 '(verilog-auto-indent-on-newline nil))
; '(verilog-indent-level-declaration 0)
; '(verilog-indent-level-module 0))
; '(verilog-tab-always-indent nil))

; disable auto-indent after semicolon
(add-hook 'verilog-mode-hook
          '(lambda ()
           (local-set-key ";" 'self-insert-command)))



(delete-selection-mode 1)

(setq frame-title-format "%b")

;;-------------
;; Key bindings
;;-------------
(global-set-key "\M-r" 'revert-buffer)
(global-set-key "\M-a" 'align-regexp)
(global-set-key "\M-2" 'switch-to-next-buffer)
(global-set-key "\M-@" 'switch-to-previous-buffer)
(global-set-key "\C-t" 'untabify-buffer)
(global-set-key "\C-a" 'mark-whole-buffer)
(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-e" 'eval-buffer)
(global-set-key "\C-xx" 'copy-to-register)
(global-set-key "\C-xg" 'insert-register)
(global-set-key [C-tab] 'other-window)
(global-set-key [M-S-up]  'tabbar-backward)
(global-set-key [M-S-down] 'tabbar-forward)

;------------------------------------
; TABBED BUFFERS
;------------------------------------

(load "tabbar.el")
(require 'tabbar)
(tabbar-mode)

 (set-face-attribute
   'tabbar-default-face nil
   :background "gray60")
  (set-face-attribute
   'tabbar-unselected-face nil
   :background "gray85"
   :foreground "gray30"
   :box nil)
  (set-face-attribute
   'tabbar-selected-face nil
   :background "#f2f2f6"
   :foreground "black"
   :box nil)
  (set-face-attribute
   'tabbar-button-face nil
   :box '(:line-width 1 :color "gray72" :style released-button))
  (set-face-attribute
   'tabbar-separator-face nil
   :height 0.7)

  (tabbar-mode 1)


;;---------------
;;; Load Packages
;;---------------
; function-pool.el - should be loaded before other packages which requires it
(load "function-pool")
(define-globalized-minor-mode my-global-function-pool-minor-mode function-pool-minor-mode
  (lambda () (function-pool-minor-mode 1)))
(my-global-function-pool-minor-mode 1)
 

; ficme-mode - TODO/FIXME Highlight
(load "~/emacs/fic-mode")
(add-hook 'verilog-mode-hook 'my-fic-mode-hook)
(add-hook 'elisp-mode-hook 'my-fic-mode-hook)
(add-hook 'python-mode-hook 'my-fic-mode-hook)
(add-hook 'c-mode-hook 'my-fic-mode-hook)
(defun my-fic-mode-hook ()
  (fic-mode 1))

; my verilog-ext-mode
(load "~/emacs/verilog-ext-minor-mode")
(add-hook 'verilog-mode-hook (lambda ()
                           "Turn on `verilog-ext-minor-mode' mode."
                           (verilog-ext-minor-mode 1)))
 (setq verilog-ext-auto-templates nil) ; disable electric verilog

; company-mode - autocomplete
(add-to-list 'load-path "~/emacs/company-mode/")
;(load "company")
;(add-hook 'after-init-hook 'global-company-mode)

;; neotree
;(add-to-list 'load-path "~/emacs/emacs-neotree/")
;(require 'neotree)
;(global-set-key "\C-c\C-f" 'neotree-toggle)

; smartparens
(add-to-list 'load-path "~/emacs/dash/")
(add-to-list 'load-path "~/emacs/smartparens/")
(require 'smartparens-config)
(define-globalized-minor-mode my-global-smartparens-mode smartparens-mode
  (lambda () (smartparens-mode 1)))
(my-global-smartparens-mode 1)

; yaml-mode
(require 'yaml-mode)
    (add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))

;;-------
;; Tags
;;-------
;(setq cc-view-name (clear-case-root));;TODO: for tags depending on the curr project, complete/remove
(setq tags-file-name (concat (getenv "UNMANAGED_DIR") "/../tags_emacs"))

;;------------------------
;; Theme and modifications
;;------------------------
(add-to-list 'custom-theme-load-path "~/emacs/themes")
;(load-theme 'tango-dark)
(load-theme 'misterioso)
;(add-to-list 'custom-theme-load-path "~/emacs/moe-theme.el/")
;(add-to-list 'load-path "~/emacs/moe-theme.el/")
;(require 'moe-theme)
;(load-theme 'moe-dark t)


(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))
;(if (eq system-type 'windows-nt)
;;(add-to-list 'default-frame-alist
;;             '(font . "Monospace821 BT"))
(set-cursor-color "#ffff00")
;(set-face-attribute 'region nil :background "#483d8b")
(setq column-number-mode t)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(setq large-file-warning-threshold nil)

;; revert buffer without query
(setq revert-without-query '(".*"))

;; linum-mode
(global-linum-mode 1)

;;------------------------
;; Org-mode
;;------------------------
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
(if (eq system-type 'windows-nt)
    (setq org-agenda-files (list "C:/Users/ynissani/OneDrive - Qualcomm/Desktop/Tasks.org"))
)
(add-to-list 'org-tag-faces '("Verification" . (:foreground "tomato"  :weight bold)))
(add-to-list 'org-tag-faces '("FutureFeature" . (:foreground "sienna1"  :weight bold)))
(add-to-list 'org-tag-faces '("LowPriority" . (:foreground "DeepSkyBlue"  :weight bold)))
;(add-to-list 'org-tag-faces '("CANCELLED" . (:foreground "azure"  :weight bold)))
;(add-to-list 'org-tag-faces '("GOT_IT" . (:foreground "SeaGreen1"  :weight bold)))
(add-to-list 'org-emphasis-alist '("*" (:foreground "LawnGreen"  :weight bold)))
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-start-on-weekday 0)
(setq org-todo-keyword-faces
      '(("TODO" . org-warning) ("STARTED" . "yellow") ("WONT_DO" . "light gray")
        ("CANCELED" . (:foreground "azure"))))
(setq org-enforce-todo-dependencies t)
(setq org-enforce-todo-checkbox-dependencies t)


;;------------------------
;; Yasnippet
;;------------------------
(add-to-list 'load-path
              "~/emacs/yasnippet-master")
(require 'yasnippet)
(yas-global-mode 1)


;;------------------------
;; Polymode
;;------------------------
; without melpa
(setq load-path
      (append '("~/emacs/polymode-master" "~/emacs/poly-verilog-python")
              load-path))

(add-to-list 'auto-mode-alist '("\\.sv.epy$" . poly-verilog-python-mode))
;;(require 'poly-verilog-python)

; with melpa
;;(use-package polymode
;;  :ensure t
;;  :mode ("\.sv.epy$" . poly-verilog-python-mode)
;;  :config
;;  (define-hostmode poly-verilog-hostmode :mode 'verilog-mode)
;;
;;
;;  (define-innermode poly-python-expr-verilog-innermode
;;  :mode 'python-mode
;;  :head-matcher "^[ \t]*- \\|#{"
;;  :tail-matcher "$\\|}"
;;  :head-mode 'host 
;;  :tail-mode 'host   
;;  )
;;
;;  (define-polymode poly-verilog-python-mode
;;    :hostmode 'poly-verilog-hostmode
;;    :innermodes '(poly-python-expr-verilog-innermode)))
    


  
;;;;------------------------
;;;; Jedi
;;;;------------------------
;;(add-to-list 'load-path
;;              "~/emacs/popup")
;;(add-to-list 'load-path
;;              "~/emacs/auto-complete")
;;
;;;(add-to-list 'load-path
;;;             "~/emacs/jedi")
;;;(autoload 'jedi:setup "jedi" nil t)
;;;(setq jedi:server-command '("~/emacs/jedi/jediepcserver.py"))
;;;;;;;(require `jedi) ;;;;; TODO: temp removed as cause errors ;;;;;;;;;;;;;;
;;;(add-hook 'python-mode-hook 'jedi:setup)
;;;(setq jedi:complete-on-dot t)                 ; optional


;;;;------------------------
;;;; Elpy
;;;;------------------------
; python3
;(setq python-shell-interpreter "/pkg/qct/software/python/3.6.0/bin/python3.6")
;(setq python-shell-interpreter "~/venvironments/env1/Scripts/python.exe")
(setq python-shell-interpreter "C:/Users/ynissani/venvironments/env1/Scripts/python.exe")

(add-to-list 'load-path "~/emacs/s.el-master")
(add-to-list 'load-path "~/emacs/pyvenv-master")
(add-to-list 'load-path "~/emacs/Highlight-Indentation-for-Emacs-master")
(add-to-list 'load-path "~/emacs/elpy-master")
;(load "elpy")
;(load "elpy-rpc")
;;(setq elpy-rpc-python-command "~/venvironments/env1/Scripts/python.exe")
;(load "elpy-shell")
;(load "elpy-profile")
;(load "elpy-refactor")
;(load "elpy-django")

; enable elpy & activate venv for python mode only
(add-to-list 'auto-mode-alist '("\\.py\\'" . elpy-mode))
(defun my-python-mode-setup ()
  (require 'pyvenv)
  (pyvenv-activate "C:/Users/ynissani/venvironments/env1")
  (load "elpy")
  (load "elpy-rpc")
  (setq elpy-rpc-python-command "C:/Users/ynissani/venvironments/env1/Scripts/python.exe")
  (load "elpy-shell")
  (load "elpy-profile")
  (load "elpy-refactor")
  (load "elpy-djangp")
  (elpy-enable))
(add-hook 'python-mode-hook 'my-python-mode-setup)


;;;;------------------------
;;;; ESUP
;;;;------------------------
(add-to-list 'load-path "~/emacs/esup")
(autoload 'esup "esup" "Emacs Start Up Profiler." t)
