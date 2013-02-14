(setq user-full-name "Álvaro Peña")
(setq user-mail-address "alvaropg@gmail.com")

;; set up utf-8
(prefer-coding-system         'utf-8)
(setq coding-system-for-read  'utf-8)
(setq coding-system-for-write 'utf-8)

(define-key global-map [C-f1] 'eshell)
(define-key global-map [f2]   'ido-find-file)                      ;; F2 - OPEN
(define-key global-map [f3]   'ido-kill-buffer)                    ;; F3 - CLOSE
(define-key global-map [f5]   'goto-line)                          ;; F5 - GOTO LINE
(define-key global-map [f7]   'ispell)                             ;; F7 - ISPELL
(define-key global-map [f8]   'devhelp-word-at-point)              ;; F8 - DEVHELP
(define-key global-map [f9]   'undo)                               ;; F9 - UNDO
(define-key global-map [f10]  'compile)                            ;; F10 - COMPILE
(define-key global-map [f11]  'next-error)                         ;; F11 - NEXT ERROR
;;(define-key global-map [f12] 'add-change-log-entry-other-window) ;; F12 - CHANGE

;; Keys for Org
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)

;; Key C-c o to switch .c/.h
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(add-to-list 'load-path "~/.lisp/")

(color-theme-initialize)

;;(require 'color-theme-subdued)
;;(require 'color-theme-less)
(require 'color-theme-gruber-darker)
;;(require 'color-theme-tango)
(color-theme-gruber-darker)
;;(color-theme-gtk-ide)
;;(color-theme-subdued)

;; PHP
;; (require 'php-mode) Not in 24
(autoload 'php-mode "php-mode" "Major mode for editing php code." t) ;; For 24

(add-to-list 'auto-mode-alist '("\\.module$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.inc$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.install$" . php-mode))
(add-to-list 'auto-mode-alist '("\\.engine$" . php-mode))

;; PHPCS
(defun my-php-hook-function ()
  (set (make-local-variable 'compile-command) (format "phpcs --report=emacs --standard=PEAR %s" (buffer-file-name))))
(add-hook 'php-mode-hook 'my-php-hook-function)

;; PHP-DOC
(setq php-manual-path "/usr/share/doc/php-doc/html")

;; Completion
(setq php-completion-file  "~/.emacs.d/php/php-completion-file")
(define-key global-map [C-f2] 'php-complete-function)

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

;; No auto-indent
(setq-default indent-tabs-mode nil)

;; Highlight pairs of parentheses
(show-paren-mode 1)

;; (add-hook 'c-mode-common-hook '(lambda () (local-set-key (kbd "RET") 'newline-and-indent)))

(setq c-basic-offset 8)
(setq default-tab-width 8)
(setq tab-width 8)

;; Displays name of current function in modeline
(which-function-mode)

;; Scroll
(setq
 scroll-margin 5
 scroll-conservatively 100000
 scroll-preserve-screen-position t ;; Make pgup/dn remember current line
 scroll-step 1) ;; Scroll line by line

;; Replace "yes or no" prompt with "y or n" prompt
(defun yes-or-no-p (arg)
"Allows letters 'y' or 'n' in response to prompts asking for either 'yes' or 'no'."
(y-or-n-p arg))

;; Don't wrap long lines
(set-default 'truncate-lines t)

;; Make searches case insensitive
(setq case-fold-search t)

;;(standard-display-european +1)
(set-input-mode (car (current-input-mode))
                (nth 1 (current-input-mode))
                0)

;; No sounds! visual beep for me please...
(setq visible-bell t)

;; Always end a file with a newline
(setq require-final-newline t)

;; No toolbar
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

;; PC Selection (Not in 24)
;;(pc-selection-mode 1)

;; disable welcome message
(setq inhibit-startup-message t)
(setq inhibit-startup-echo-area-message t)
(setq initial-scratch-message nil) 

;; White spaces
(require 'whitespace)

;; Turn on font-lock mode 
(require 'font-lock) 
(global-font-lock-mode t)


;; Bookmarks
(setq
 bookmark-default-file "~/.emacs.d/bookmarks"
 bookmark-save-flag 1)


(set-face-attribute 'default nil :font "Droid Sans Mono-8")

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; ORG-MODE
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(custom-set-variables
 '(org-agenda-files (quote ("~/Documents/Private/GTD/gtd.org"))))
