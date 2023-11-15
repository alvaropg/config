;; Set customization data in a specific file, without littering
;; my init files.

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; Initialize package system

(require 'package)

;; Update built in packages too
;; https://git.savannah.gnu.org/cgit/emacs.git/tree/etc/NEWS.29#n1714
(setq package-install-upgrade-built-in t)
;; https://github.com/magit/magit/issues/5011
;;(require 'seq-25)

(setq package-archives
      '(("org"     .       "https://orgmode.org/elpa/")
        ("gnu"     .       "https://elpa.gnu.org/packages/")
        ("melpa"   .       "https://melpa.org/packages/")))

(package-initialize)
(package-refresh-contents)

;; Use-package for civilized configuration

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)

;; Automatically install modules when they’re not in our system
(setq use-package-always-ensure t)

;; Auto upgrade packages
(use-package auto-package-update
  :defer 10
  :config
  ;; Delete residual old versions
  (setq auto-package-update-delete-old-versions t)
  ;; Do not bother me when updates have taken place.
  (setq auto-package-update-hide-results t)
  ;; Update installed packages at startup if there is an update pending.
  (auto-package-update-maybe))


(use-package ivy
  :ensure swiper
  :ensure counsel
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "C-x b") 'counsel-switch-buffer)
  (global-set-key (kbd "C-x r b") 'counsel-bookmark)
  (global-set-key (kbd "C-x C-r") 'counsel-recentf)
  )


(use-package multiple-cursors
  :config
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  )


(use-package evil-nerd-commenter)

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))


(use-package yasnippet-snippets
  :ensure t
  :init
  )


(use-package flycheck
  :ensure t
  :init
  ;; (add-hook 'python-mode-hook 'flycheck-mode)
  ;; (add-hook 'c-mode-hook 'flycheck-mode)
  ;; (add-hook 'c++-mode-hook 'flycheck-mode)
  )


;; Don't use formatter
;; (setq lsp-enable-on-type-formatting nil)
;; (setq lsp-enable-indentation nil)
(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c-mode . lsp)
	 (c++-mode . lsp))
  :commands lsp)
;; (use-package lsp-ui :commands lsp-ui-mode :after lsp-mode)
;; Because I'm an ivy user
(use-package lsp-ivy :commands lsp-ivy-workspace-symbol)


;; Autocompletion
(use-package company
  :ensure t
  :hook (prog-mode . company-mode))

;; Magit
(use-package magit
  :config (global-set-key (kbd "C-x g") 'magit-status))

;; NOTE: `diff-hl' depends on `vc'
(use-package vc
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  (vc-allow-async-revert t)
  (vc-handled-backends '(Git)))

;; Highlight uncommitted changes using VC
(use-package diff-hl
  :ensure t
  :hook ((after-init         . global-diff-hl-mode)
         (dired-mode         . diff-hl-dired-mode-unless-remote)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh))
  :config
  ;; When Emacs runs in terminal, show the indicators in margin instead.
  (unless (display-graphic-p)
    (diff-hl-margin-mode)))


(use-package git-timemachine)

(use-package meson-mode)

;; Markdown
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Org-mode

;; Replace the content marker, “⋯”, with a nice unicode arrow.
(setq org-ellipsis " ⤵")
;; Using shift selection in org-mode buffers
(setq org-support-shift-select t)


;; Pretty colors
(set-face-attribute 'default nil  :font "Hack-10")

;; Treat all themes as safe; no query before use
(setf custom-safe-themes t)

(use-package dracula-theme
  :config (load-theme 'dracula t))
(use-package solarized-theme)
;;   :config (load-theme 'solarized-light t))
(use-package spacemacs-theme)

;; Nyan cat instead of scrollbar
;; scroll-bar-mode is turned off in custom.el
(use-package nyan-mode
  :config
  (nyan-mode 1))


;; YAML
(use-package yaml-mode
  :ensure t
  :mode (("\\.yml\\'" . yaml-mode)
	 ("\\.bst\\'" . yaml-mode))
  )

;; Backups
(setq make-backup-files t)
(setq backup-directory-alist '(("." . "~/.emacs.d/backups"))) ;; New location for backups.
(setq delete-old-versions t) ;; Silently delete execess backup versions
(setq kept-new-versions 6) ; Keep 6 new
(setq kept-old-versions 100) ;; Keep only 100 old
(setq vc-make-backup-files t) ;; Even version controlled files get to be backed up.
(setq version-control t) ;; Use version numbers for backup files.
(setq backup-by-copying t) ; Don't clobber symlinks


;; Devhelp
(defun devhelp-word-at-point ()
  "runs devhelp"
  (interactive)
  (setq w (current-word))
  (start-process-shell-command "devhelp" nil "devhelp" "-s"))

;; Shortcuts
(global-set-key [f3]  'ido-kill-buffer)
(global-set-key [f5]  'goto-line)
(global-set-key [f8]  'devhelp-word-at-point)
(global-set-key [f9]  'undo)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
;; C-x C-0 restores the default font size
