;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

;;(setq gc-cons-threshold 100000000)

(add-to-list 'initial-frame-alist '(fullscreen . maximized))
(setq kill-whole-line t)
(setq confirm-kill-emacs nil)

(setq evil-normal-state-cursor '(box "cyan")
      evil-insert-state-cursor '(bar "red")
      evil-visual-state-cursor '(hollow "yellow"))

(global-set-key (kbd "C-z") 'undo)
;; Basic Config

(setq auto-save-default t
      make-backup-files t)
;;(setq backup-directory-alist `(("." . "~/.emacs-tmp/")))
;;(setq auto-save-file-name-transforms `((".*" "~/.emacs-tmp/" t)))
(setq +ivy-buffer-icons t)

;; Spaces over tabs
(setq tab-width 2)
(setq-default indent-tabs-mode nil)

;;(setq exec-path
;;      (list "/usr/local/bin/"
;;            "/usr/bin/"
;;            "/bin/"
;;            "/usr/sbin/"
;;            "/sbin/"
;;            (concat (getenv "HOME") "/.cargo/bin")
;;            (concat (getenv "HOME") "/.local/bin")
;;            (concat (getenv "HOME") "/.asdf/shims")))

;;(setenv "PATH" (string-join exec-path ":"))

;;(global-auto-revert-mode t)

(setq doom-modeline-persp-icon t)
(setq doom-modeline-lsp t)
(setq doom-modeline-env-version t)
(setq doom-modeline-project-detection 'project)
;;(setq doom-modeline-buffer-file-name-style 'auto)
;; Whether display the icon for `major-mode'. It respects `doom-modeline-icon'.
(setq doom-modeline-major-mode-icon t)

;; Whether display the colorful icon for `major-mode'.
;; It respects `all-the-icons-color-icons'.
(setq doom-modeline-major-mode-color-icon t)

;; Whether display the icon for the buffer state. It respects `doom-modeline-icon'.
(setq doom-modeline-buffer-state-icon t)

;; Whether display the modification icon for the buffer.
;; It respects `doom-modeline-icon' and `doom-modeline-buffer-state-icon'.
(setq doom-modeline-buffer-modification-icon t)


;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

(setq whitespace-line-column 80)
(setq whitespace-style '(face trailing spaces tabs))
(setq show-trailing-whitespace t)
(global-whitespace-mode 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "gray40" :foreground nil))))
 '(whitespace-tab ((t (:background nil :foreground "gray30"))))
 '(whitespace-space ((t (:background nil :foreground "gray30")))))

;; Turn off line wrapping
(setq-default truncate-lines 1)

(setq confirm-kill-emacs nil)

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(setq doom-font (font-spec :family "JetBrains Mono" :size 15))
(setq which-key-idle-delay 0)

;;(setq 'solaire-hl-line-face "#fbffbf")
;;(setq 'solaire-hl-line-face nil)

(require 'doom-themes)
;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled

;;(setq doom-theme 'spacemacs-dark)

(load-theme 'doom-palenight t)
;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)
;; Enable custom neotree theme (all-the-icons must be installed!)
;;(doom-themes-neotree-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

(evil-commentary-mode)

(after! neotree
  (setq neo-theme 'icons))

(after! company
  (setq company-idle-delay 0))

(after! doom-themes
  (setq doom-neotree-file-icons t))

(after! ivy
  (setq ivy-re-builders-alist
      '((t . ivy--regex-ignore-order))))

(after! projectile
  (projectile-mode)
  (projectile-load-known-projects))


(dap-mode 1)
;; DAP
(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :commands dap-debug
  :config
  (require 'dap-go)
  (dap-go-setup)
  (require 'dap-hydra)
  ;;(require 'dap-gdb-lldb)
  ;;(dap-gdb-lldb-setup)

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger")))

;; Enabling only some features
(setq dap-auto-configure-features '(ui sessions locals controls tooltip))

(load! "elixir")

(load! "go")

(load! "rust")

(use-package! fci-mode
  :after-call doom-before-switch-buffer-hook
  :config
  (defun company-turn-off-fci (&rest ignore)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1)))

  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))

  (add-hook 'company-completion-started-hook #'company-turn-off-fci)
  (add-hook 'company-completion-finished-hook #'company-maybe-turn-on-fci)
  (add-hook 'company-completion-cancelled-hook #'company-maybe-turn-on-fci))

(use-package! dockerfile-mode
   :mode "Dockerfile$")

(use-package! erlang
  :mode "\\.erl$"
  :config
  (erlang-mode))

;;(require 'evil-multiedit)
(use-package evil-multiedit
  :defer 0
  :config
    (evil-multiedit-default-keybinds))

(setq +ivy-project-search-engines '(rg))

(after! ivy-rich
  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 30 :face bold))
            (ivy-rich-switch-buffer-size (:width 7 :face font-lock-doc-face))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 18 :face doom-modeline-buffer-major-mode))
            (ivy-rich-switch-buffer-path (:width 50)))
           :predicate
           (lambda (cand) (get-buffer cand)))
          +ivy/switch-workspace-buffer
          (:columns
           ((ivy-rich-candidate (:width 30 :face bold))
            (ivy-rich-switch-buffer-size (:width 7 :face font-lock-doc-face))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 18 :face doom-modeline-buffer-major-mode))
            (ivy-rich-switch-buffer-path (:width 50)))
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 40))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face :width 80))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 40))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face :width 80))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 40))
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face :width 80))))
          counsel-recentf
          (:columns
           ((ivy-rich-candidate (:width 100))
            (ivy-rich-file-last-modified-time (:face font-lock-doc-face)))))))

(after! counsel
  (setq counsel-evil-registers-height 20
        counsel-yank-pop-height 20
        counsel-org-goto-face-style 'org
        counsel-org-headline-display-style 'title
        counsel-org-headline-display-tags t
        counsel-org-headline-display-todo t))


(after! ivy
   (ivy-add-actions
    'counsel-M-x
    `(("h" +ivy/helpful-function "Helpful"))))

(use-package all-the-icons-ivy
  :after ivy
  :config
  (dolist (cmd '( counsel-find-file
                  counsel-file-jump
                  projectile-find-file
                  counsel-projectile-find-file
                  counsel-dired-jump counsel-projectile-find-dir
                  counsel-projectile-switch-project))
    (ivy-set-display-transformer cmd #'all-the-icons-ivy-file-transformer)))

(use-package ivy-posframe
  :after ivy
  :diminish
  :hook
  (ivy-mode . ivy-posframe-mode)
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center))
        ivy-posframe-height-alist '((t . 20))
        ivy-posframe-width 100
        ivy-posframe '((t (:background "#626266")))
        ivy-posframe-cursor '((t (:background "#00ff00"))))
  (ivy-posframe-mode 1))

(use-package lsp-mode
  :ensure t
  :commands (lsp lsp-deferred)
  :hook (go-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))


;; Optional - provides fancier overlays.
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

;; Company mode is a standard completion package that works well with lsp-mode.
(use-package company
  :config
  ;; Optionally enable completion-as-you-type behavior.
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 1))

;; Optional - provides snippet support.
(use-package yasnippet
  :commands yas-minor-mode
  :hook (go-mode . yas-minor-mode))

(use-package toml-mode
  :mode "\\.toml\\'")

(add-hook 'after-init-hook (lambda () (setq echo-keystrokes 5)))

(setq-default column-number-mode t
              line-number-mode t
              size-indication-mode nil
              mode-line-position nil
              mode-line-percent-position nil
              mode-line-in-non-selected-windows nil)
(unless (bound-and-true-p doom-modeline-mode)
  (set-face-attribute 'mode-line nil
                      :box (list :line-width 8
                                 :color (face-attribute 'mode-line :background))))

(use-package uniquify
  :custom (uniquify-buffer-name-style 'forward))

(use-package display-line-numbers
  :custom
  (display-line-numbers-grow-only t)
  (display-line-numbers-width-start t))

(use-package help
  :custom (help-window-select t))

(use-package editorconfig
  :commands editorconfig-mode
  :config (editorconfig-mode 1))

(load! "bindings")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(enable-recursive-minibuffers t)
 '(ivy-count-format "")
 '(ivy-display-style nil)
 '(ivy-ignore-buffers '("\\` " "\\`\\*"))
 '(ivy-minibuffer-faces '(default default default default))
 '(ivy-posframe-height-alist '((t . 16)))
 '(ivy-posframe-parameters '((internal-border-width . 6)))
 '(ivy-posframe-width 90)
 '(ivy-re-builders-alist '((t . ivy--regex-fuzzy)) t)
 '(ivy-use-virtual-buffers t)
 '(package-selected-packages
   '(go-imenu go-mode centaur-tabs uniquify-files solaire-mode separedit rustic rust-playground rust-auto-use ranger org-plus-contrib ob-rust minions mc-extras lsp-elixir format-all flymake-rust flycheck-rust evil-easymotion evil-commentary evil-collection dired-ranger company-org-roam cargo)))
