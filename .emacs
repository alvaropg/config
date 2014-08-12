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
;; (define-key global-map [f6]   '?)                             ;; F6 - Reserved for debug
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

;;
;; Color theme
;;
(color-theme-initialize)

;;(require 'color-theme-subdued)
;;(require 'color-theme-less)
(require 'color-theme-gruber-darker)
;;(require 'color-theme-tango)
(color-theme-gruber-darker)
;;(color-theme-gtk-ide)
;;(color-theme-subdued)

;; Full ack
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)



;; syntax checking on-the-fly
(add-hook 'after-init-hook #'global-flycheck-mode)

;;
;; Python
;;
(require 'python)

(add-to-list 'auto-mode-alist '("\\.spy$" . python-mode))

;; auto-complete with jedi
(add-hook 'python-mode-hook 'auto-complete-mode)
(add-hook 'python-mode-hook 'jedi:ac-setup)

;; Debug & PDB breakpoint
(defun python--add-debug-highlight ()
  "Adds a highlighter for use by `python--pdb-breakpoint-string'"
  (highlight-lines-matching-regexp "## DEBUG ##\\s-*$" 'hi-red-b))

(add-hook 'python-mode-hook 'python--add-debug-highlight)

(defvar python--pdb-breakpoint-string "import pdb; pdb.set_trace() ## DEBUG ##"
  "Python breakpoint string used by `python-insert-breakpoint'")

(defun python-insert-breakpoint ()
  "Inserts a python breakpoint using `pdb'"
  (interactive)
  (back-to-indentation)
  ;; this preserves the correct indentation in case the line above
  ;; point is a nested block
  (split-line)
  (insert python--pdb-breakpoint-string))

;; Python debug
(define-key python-mode-map (kbd "<f6>") 'python-insert-breakpoint)

;;
;; PHP
;;

;;(require 'php-mode)
(autoload 'php-mode "php-mode" "Major mode for editing php code." t) ;; For 24

(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))
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

;;
;; Backups (This saved my life a bunch of times!)
;;
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
                ("\\.vala$" . vala-mode)
                ("\\.ui$". xml-mode))
              auto-mode-alist))

;; Use spaces instead of tabs
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

;; Overwrite selected region
(delete-selection-mode t)
(setq transient-mark-mode t)

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

;; Fonts
(setq-default line-spacing 1)
(set-face-attribute 'default nil :font "Liberation Mono-8")
;; Font for emacs-client
(setq default-frame-alist '((font . "Liberation Mono-8")))

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

;; ORG-MODE
(custom-set-variables
 '(org-agenda-files (quote ("~/Documents/Private/GTD/gtd.org"))))

;; Interactively do things
;(require 'ido)
;(ido-mode t)

;; Packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))

;; el-get
(add-to-list 'load-path "~/.emacs.d/el-get/el-get")
(unless (require 'el-get nil 'noerror)
  (with-current-buffer
      (url-retrieve-synchronously
       "https://raw.github.com/dimitri/el-get/master/el-get-install.el")
    (let (el-get-master-branch)
      (goto-char (point-max))
      (eval-print-last-sexp))))
(el-get 'sync)

;; ELPA Archive for gtk-look
;; http://user42.tuxfamily.org/gtk-look/index.html
(eval-after-load "package"
  '(add-to-list 'package-archives
    '("user42" . "http://download.tuxfamily.org/user42/elpa/packages/")))

;; Sync my packages
(package-initialize)
;; check if the packages is installed; if not, install it.
(mapc
 (lambda (package)
   (or (package-installed-p package)
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package))))
;; this list results from C-h v package-activated-list
 '(full-ack auto-complete auto-complete-c-headers evil-nerd-commenter expand-region flycheck f gh google-this google-translate gtk-look icicles java-snippets logito magit-svn magit git-rebase-mode git-commit-mode multiple-cursors pcache pkg-info epl dash popup cl-lib rainbow-mode s w3m yasnippet))

;; Speedbar
(require 'speedbar)

;; Speedbar font
(make-face 'speedbar-face)
(set-face-font 'speedbar-face "Droid Sans Mono-8")
(setq speedbar-mode-hook '(lambda () (buffer-face-set 'speedbar-face)))

;; Speedbar without separate frame
(defconst my-speedbar-buffer-name " SPEEDBAR")

(defun my-speedbar-no-separate-frame ()
  (interactive)
  (when (not (buffer-live-p speedbar-buffer))
    (setq speedbar-buffer (get-buffer-create my-speedbar-buffer-name)
          speedbar-frame (selected-frame)
          dframe-attached-frame (selected-frame)
          speedbar-select-frame-method 'attached
          speedbar-verbosity-level 0
          speedbar-last-selected-file nil)
    (set-buffer speedbar-buffer)
    (speedbar-mode)
    (speedbar-reconfigure-keymaps)
    (speedbar-update-contents)
    (speedbar-set-timer 1)
    (make-local-hook 'kill-buffer-hook)
    (add-hook 'kill-buffer-hook
              (lambda () (when (eq (current-buffer) speedbar-buffer)
                           (setq speedbar-frame nil
                                 dframe-attached-frame nil
                                 speedbar-buffer nil)
                           (speedbar-set-timer nil)))))
  (set-window-buffer (selected-window) 
                     (get-buffer my-speedbar-buffer-name)))

;; Use the w3m text browser for urls
(setq browse-url-browser-function 'w3m-browse-url)
(autoload 'w3m-browse-url "w3m" "Ask a WWW browser to show a URL." t)
;; optional keyboard short-cut
(global-set-key "\C-xm" 'browse-url-at-point)

;; Google translate
(require 'google-translate)

;; Yasnippet
(require 'yasnippet) ;; not yasnippet-bundle
(yas-global-mode 1)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; Auto Complete
(require 'auto-complete-config)
(ac-config-default)
;; (setq ac-auto-start 1)
;; (defun my-ac-config ()
;;   (setq-default ac-sources '(ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
;;   (add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
;;   (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
;;   (add-hook 'ruby-mode-hook 'ac-ruby-mode-setup)
;;   (add-hook 'css-mode-hook 'ac-css-mode-setup)
;;   (add-hook 'auto-complete-mode-hook 'ac-common-setup)
;;   (global-auto-complete-mode t))
;; (my-ac-config)

;; Evil Nerd Commenter https://github.com/redguardtoo/evil-nerd-commenter
(evilnc-default-hotkeys)

;; Multiple Cursors
(require 'multiple-cursors)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)

;; Schematic unit selection
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)
