;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(def-package! direnv)
(direnv-mode)

(setq doom-font (font-spec :family "Fira Code Retina" :size 17))
(setq which-key-idle-delay 0)
(toggle-frame-maximized)

(ranger-override-dired-mode t)

;; Global settings (defaults)

(require 'doom-themes)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-dracula t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)
;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)

;; or for treemacs users
;; (doom-themes-treemacs-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)


(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))

(after! treemacs
  (advice-add '+treemacs--init :after #'balance-windows))

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

(setq projectile-completion-system 'ivy)

(add-hook! elixir-mode
  (flycheck-mode)
  (rainbow-delimiters-mode))

(setq counsel-grep-base-command
 "rg -i -M 120 --no-heading --line-number --color never '%s' %s")

(set-face-background 'solaire-hl-line-face "Cyan")
(set-face-foreground 'solaire-hl-line-face "dark slate grey")

(setq solaire-mode-remap-line-numbers t)

(add-hook 'emacs-lisp-mode-hook (lambda ()
    (fci-mode 1)))

(def-package! rust-mode
  :mode "\\.rs$"
  :config
  (flycheck-mode))

(def-package! lsp-rust
  :after (lsp-mode lsp-ui rust-mode)
  :config
  (setq lsp-rust-rls-command '("rustup" "run" "stable" "rls"))
  :hook
  (rust-mode . lsp-rust-enable))

(def-package! dockerfile-mode
   :mode "Dockerfile$")

(def-package! flycheck-mix
  :after elixir-mode
  :config
  (add-hook 'flycheck-mode-hook #'flycheck-mix-setup))

(def-package! erlang
  :mode "\\.erl$"
  :config
  (erlang-mode))

;;(add-to-list 'default-frame-alist '(fullscreen . maximized))
;;(add-to-list 'default-frame-alist '(height . 80))
;;(add-to-list 'default-frame-alist '(width . 200))

;; Fancy titlebar for MacOS
;;(add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
;;(add-to-list 'default-frame-alist '(ns-appearance . dark))
;;(setq ns-use-proxy-icon  nil)
;;(setq frame-title-format nil)

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)
(fci-mode 1)

(require 'evil-multiedit)
;; Highlights all matches of the selection in the buffer.
(define-key evil-visual-state-map "R" 'evil-multiedit-match-all)

;; Match the word under cursor (i.e. make it an edit region). Consecutive presses will
;; incrementally add the next unmatched match.
(define-key evil-normal-state-map (kbd "M-d") 'evil-multiedit-match-and-next)
;; Match selected region.
(define-key evil-visual-state-map (kbd "M-d") 'evil-multiedit-and-next)
;; Insert marker at point
(define-key evil-insert-state-map (kbd "M-d") 'evil-multiedit-toggle-marker-here)

;; Same as M-d but in reverse.
(define-key evil-normal-state-map (kbd "M-D") 'evil-multiedit-match-and-prev)
(define-key evil-visual-state-map (kbd "M-D") 'evil-multiedit-and-prev)

;; OPTIONAL: If you prefer to grab symbols rather than words, use
;; `evil-multiedit-match-symbol-and-next` (or prev).

;; Restore the last group of multiedit regions.
(define-key evil-visual-state-map (kbd "C-M-D") 'evil-multiedit-restore)

;; RET will toggle the region under the cursor
(define-key evil-multiedit-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

;; ...and in visual mode, RET will disable all fields outside the selected region
(define-key evil-motion-state-map (kbd "RET") 'evil-multiedit-toggle-or-restrict-region)

;; For moving between edit regions
(define-key evil-multiedit-state-map (kbd "C-n") 'evil-multiedit-next)
(define-key evil-multiedit-state-map (kbd "C-p") 'evil-multiedit-prev)
(define-key evil-multiedit-insert-state-map (kbd "C-n") 'evil-multiedit-next)
(define-key evil-multiedit-insert-state-map (kbd "C-p") 'evil-multiedit-prev)

(setq +ivy-buffer-icons t)

(load! "bindings")
