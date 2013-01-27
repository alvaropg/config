(setq user-full-name "Álvaro Peña")
(setq user-mail-address "alvaropg@gmail.com")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(c-basic-offset tab-width)
 '(case-fold-search t)
 '(column-number-mode t)
 '(css-indent-offset tab-width)
 '(current-language-environment "UTF-8")
 '(default-input-method "rfc1345")
 '(display-time-mode t)
 '(ede-project-directories (quote ("/home/alvaropg/work/git/almanah")))
 '(global-font-lock-mode t nil (font-lock))
 '(indent-tabs-mode nil)
 '(mumamo-submode-indent-offset tab-width)
 '(nxml-child-indent tab-width)
 '(org-agenda-files (quote ("~/Documents/Personales/GTD/gtd.org" "~/tmp/test.org")))
 '(php-manual-path "/usr/share/doc/php-doc/html/")
 '(safe-local-variable-values (quote ((indent-tabs-mode . s))))
 '(show-paren-mode t nil (paren))
 '(uniquify-buffer-name-style nil nil (uniquify)))

(setq locale-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system       'utf-8)
(setq default-buffer-file-coding-system 'utf-8)
(setq x-select-buffer-file-coding-system 'utf-8)

(define-key global-map [C-f1] 'eshell)
(define-key global-map [f2] 'ido-find-file)                      ;; F2 - OPEN
(define-key global-map [f3] 'ido-kill-buffer)                    ;; F3 - CLOSE
(define-key global-map [f5] 'goto-line)                          ;; F5 - GOTO LINE
(define-key global-map [f7] 'ispell)                             ;; F7 - ISPELL
(define-key global-map [f8] 'devhelp-word-at-point)              ;; F8 - DEVHELP
(define-key global-map [f9] 'undo)                               ;; F9 - UNDO
(define-key global-map [f10] 'compile)                           ;; F10 - COMPILE
(define-key global-map [f11] 'next-error)                        ;; F11 - NEXT ERROR
;;(define-key global-map [f12] 'add-change-log-entry-other-window) ;; F12 - CHANGE

;; Keys for Org
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

(add-to-list 'load-path "~/.lisp/")

(color-theme-initialize)

(require 'color-theme-subdued)
(require 'color-theme-less)
(require 'color-theme-gruber-darker)
(require 'color-theme-tango)

;;(color-theme-gtk-ide)
;;(color-theme-subdued)
(color-theme-gruber-darker)

(require 'php-mode)
(add-to-list 'auto-mode-alist '("\\.module$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.install$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.engine$" . php-mode))

;; Backups (This saved my life a bunch of times!)
(setq make-backup-files t)
(setq backup-by-copying t) ; Don't clobber symlinks
(setq backup-directory-alist '(("." . "~/.emacs.backups"))) ; Don't litter my fs tree
(setq delete-old-versions t) ; Clean up a little
(setq kept-new-versions 6) ; Keep 6 new
(setq kept-old-versions 2) ; Keep only 2 old
(setq version-control t) ; Use versioned backups

;; Filetypes & modes
(setq auto-mode-alist
      (append '(("\\.C$" . c-mode)
                ("\\.cc$" . c++-mode)
                ("\\.cpp$" . c++-mode)
                ("\\.cxx$" . c++-mode)
                ("\\.hxx$" . c++-mode)
                ("\\.hpp$" . c++-mode)
                ("\\.h$" . c-mode)
                ("\\.hh$" . c++-mode)
                ("\\.idl$" . c++-mode)
                ("\\.ipp$" . c++-mode)
                ("\\.c$" . c-mode)
                ("\\.pl$" . perl-mode)
                ("\\.pm$" . perl-mode)
                ("\\.java$" . java-mode)
                ("\\.inc$" . php-mode)
                ("\\.php$" . php-mode)
                ("\\.txt$" . text-mode)
                ("\\.ui$". xml-mode))
              auto-mode-alist))


(setq-default indent-tabs-mode nil)

;; (add-hook 'c-mode-common-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

(setq default-tab-width 8)
(setq tab-width 8)

;; Displays name of current function in modeline
(which-function-mode)

;; Replace "yes or no" prompt with "y or n" prompt
(defun yes-or-no-p (arg)
"Allows letters 'y' or 'n' in response to prompts asking for either 'yes' or 'no'."
(y-or-n-p arg))

;; Don't wrap long lines
(set-default 'truncate-lines t)

;; Make searches case insensitive
(setq case-fold-search t)

;; No sounds! visual beep for me please...
(setq visible-bell t)

;; Always end a file with a newline
(setq require-final-newline t)

;; No toolbar for X (if Xemacs of course!)
(tool-bar-mode -1)

;; No scrollbar
(set-scroll-bar-mode nil)

; Hide menu bar
(menu-bar-mode -1)

;; Put column number into modeline
(column-number-mode 1)

;; Display clock on global modeline
(display-time)

;; Line number
(global-linum-mode 1)

;; PC Selection
(pc-selection-mode 1)

;; disable welcome message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil) 

;; White spaces
(require 'whitespace)

;; Turn on font-lock mode 
(require 'font-lock) 
(global-font-lock-mode t)

;; Mousewheel
(defun sd-mousewheel-scroll-up (event)
  "Scroll window under mouse up by five lines."
  (interactive "e")
  (let ((current-window (selected-window)))
    (unwind-protect
        (progn 
          (select-window (posn-window (event-start event)))
          (scroll-up 5))
      (select-window current-window))))
(defun sd-mousewheel-scroll-down (event)
  "Scroll window under mouse down by five lines."
  (interactive "e")
  (let ((current-window (selected-window)))
    (unwind-protect
        (progn 
          (select-window (posn-window (event-start event)))
          (scroll-down 5))
      (select-window current-window))))

;; Line number column

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil background "#fff" :foreground "#000" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 80 :width normal :foundry "unknown" :family "Droid Sans Mono")))))

(put 'scroll-left 'disabled nil)

;; Edit remote files
(require 'tramp)

;; A better cscope script
(require 'ascope)

;; Devhelp
(defun devhelp-word-at-point ()
  "runs devhelp"
  (interactive)
  (setq w (current-word))
  (start-process-shell-command "devhelp" nil "devhelp" "-s" w))
