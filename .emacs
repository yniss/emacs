;;-------
;; MELPA
;;-------
(require 'package)
;; TODO: doesn't work, web required!
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
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
 '(verilog-auto-indent-on-newline nil)
 '(verilog-indent-level-declaration 0)
 '(verilog-indent-level-module 0))
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

;;---------------
;;; Load Packages
;;---------------
; function-pool.el - should be loaded before other packages which requires it
(load "function-pool")
(add-hook 'verilog-mode-hook 'my-function-pool-hook)
(defun my-function-pool-hook ()
  (function-pool-minor-mode 1))
(custom-set-variables
; '(function-pool-clear-case nil)
) 

; ficme-mode - TODO/FIXME Highlight
(load "~/emacs/fic-mode")
(add-hook 'verilog-mode-hook 'my-fic-mode-hook)
(add-hook 'elisp-mode-hook 'my-fic-mode-hook)
(defun my-fic-mode-hook ()
  (fic-mode 1))

; my verilog-ext-mode
(load "~/emacs/verilog-ext-minor-mode")
(add-hook 'verilog-mode-hook (lambda ()
                           "Turn on `verilog-ext-minor-mode' mode."
                           (verilog-ext-minor-mode 1)))
(custom-set-variables
; '(verilog-ext-comments nil)
) 

; company-mode - autocomplete
(add-to-list 'load-path "~/.emacs.d/company-mode/")
(load "company")
(add-hook 'after-init-hook 'global-company-mode)

;;-------
;; Tags
;;-------
(setq tags-file-name "~/tags")


;;------------------------
;; Theme and modifications
;;------------------------
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
;(load-theme 'tango-dark)
(load-theme 'misterioso)
;(add-to-list 'custom-theme-load-path "~/.emacs.d/moe-theme.el/")
;(add-to-list 'load-path "~/.emacs.d/moe-theme.el/")
;(require 'moe-theme)
;(load-theme 'moe-dark t)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(verilog-auto-indent-on-newline nil)
 '(verilog-indent-level-declaration 0)
 '(verilog-indent-level-module 0))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "DejaVu Sans Mono" :foundry "unknown" :slant normal :weight normal :height 98 :width normal)))))
(set-cursor-color "#ffff00")
;(set-face-attribute 'region nil :background "#483d8b")
(setq column-number-mode t)
