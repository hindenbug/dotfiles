;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(def-package! direnv)
(direnv-mode)

(setq doom-font (font-spec :family "Fira Code Retina" :size 18))
(setq which-key-idle-delay 0)
(toggle-frame-maximized)

;;(setq company-backends '(company-capf company-yasnippet))
;;(setq 'solaire-hl-line-face "#fbffbf")
;;(setq 'solaire-hl-line-face nil)

(def-package! fci-mode
  :after-call doom-before-switch-buffer-hook
  :config
  (defun company-turn-off-fci (&rest ignore)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1)))

(ranger-override-dired-mode t)

;; Global settings (defaults)

(require 'doom-themes)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-one t)

;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)
;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)

;; or for treemacs users
;; (doom-themes-treemacs-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

(add-to-list 'auto-mode-alist '("\\.ejs\\'" . web-mode))

;;(with-eval-after-load 'flycheck
;;  (flycheck-pos-tip-mode))

(after! treemacs
  (advice-add '+treemacs--init :after #'balance-windows))

(after! neotree
  (setq neo-theme 'icons))

(after! company
  (setq company-idle-delay 0))

(after! doom-themes
  (setq doom-neotree-file-icons t))

(use-package! counsel
    :hook
    (after-init . ivy-mode)
    (counsel-grep-post-action . better-jumper-set-jump)
    :diminish ivy-mode
    :config
    (setq counsel-find-file-ignore-regexp "\\(?:^[#.]\\)\\|\\(?:[#~]$\\)\\|\\(?:^Icon?\\)"
          counsel-describe-function-function #'helpful-callable
          ncounsel-describe-variable-function #'helpful-variable
          ;; Add smart-casing (-S) to default command arguments:
          counsel-rg-base-command "rg -S --no-heading --line-number --color never %s ."
          counsel-ag-base-command "ag -S --nocolor --nogroup %s"
          counsel-pt-base-command "pt -S --nocolor --nogroup -e %s"
          counsel-find-file-at-point t)
       )

     (use-package! ivy-rich
       :config
       (ivy-rich-mode 1)
       (setq ivy-format-function #'ivy-format-function-line))
     ;;[[https://github.com/gilbertw1/better-jumper][gilbertw1/better-jumper: A configurable jump list implementation for Emacs]]

(after! ivy
  (setq ivy-re-builders-alist
        '((t . ivy--regex-ignore-order))))

(after! projectile
  (projectile-mode)
  (projectile-load-known-projects))

(setq projectile-completion-system 'ivy)

;;(setq rust-format-on-save t)

(add-hook! elixir-mode
  (flycheck-mode)
  (rainbow-delimiters-mode))

(setq counsel-grep-base-command
 "rg -i -M 120 --no-heading --line-number --color never '%s' %s")

;;(set-face-background 'solaire-hl-line-face "Cyan")
;;(set-face-foreground 'solaire-hl-line-face "dark slate grey")

;;(setq solaire-mode-remap-line-numbers t)

(add-hook 'emacs-lisp-mode-hook (lambda ()
    (fci-mode 1)))

;; rust
;;(def-package! cargo)

(def-package! rust-mode
  :mode "\\.rs$"
  :config
  (flycheck-mode)
  ;;(add-hook 'rust-mode-hook #'cargo-minor-mode)
  )

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

(use-package! dimmer
  :config (dimmer-mode))

(setq visible-bell 1)

(use-package! golden-ratio
  :disabled
  :diminish golden-ratio-mode
  :init (golden-ratio-mode 1))

;; racer only works with nightly
;; (use-package racer
;;   :requires rust-mode

;;   :init (setq racer-rust-src-path
;;               (concat (string-trim
;;                        (shell-command-to-string "rustc --print sysroot"))
;;                       "/lib/rustlib/src/rust/src"))

;;   :config
;;   (add-hook 'rust-mode-hook #'racer-mode)
;;   (add-hook 'racer-mode-hook #'eldoc-mode)
;;   (add-hook 'racer-mode-hook #'company-mode))

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

(setq +ivy-buffer-icons t)

;; evil-multiedit
:v  "R"     #'evil-multiedit-match-all
:n  "M-d"   #'evil-multiedit-match-symbol-and-next
:n  "M-D"   #'evil-multiedit-match-symbol-and-prev
:v  "M-d"   #'evil-multiedit-match-and-next
:v  "M-D"   #'evil-multiedit-match-and-prev
:nv "C-M-d" #'evil-multiedit-restore
(:after evil-multiedit
  (:map evil-multiedit-state-map
    "M-d" #'evil-multiedit-match-and-next
    "M-D" #'evil-multiedit-match-and-prev
    "RET" #'evil-multiedit-toggle-or-restrict-region)
  (:map (evil-multiedit-state-map evil-multiedit-insert-state-map)
    "C-n" #'evil-multiedit-next
    "C-p" #'evil-multiedit-prev))

;; evil-mc
(:prefix "gz"
  :nv "m" #'evil-mc-make-all-cursors
  :nv "u" #'evil-mc-undo-all-cursors
  :nv "z" #'+evil/mc-toggle-cursors
  :nv "c" #'+evil/mc-make-cursor-here
  :nv "n" #'evil-mc-make-and-goto-next-cursor
  :nv "p" #'evil-mc-make-and-goto-prev-cursor
  :nv "N" #'evil-mc-make-and-goto-last-cursor
  :nv "P" #'evil-mc-make-and-goto-first-cursor)
(:after evil-mc
  :map evil-mc-key-map
  :nv "C-n" #'evil-mc-make-and-goto-next-cursor
  :nv "C-N" #'evil-mc-make-and-goto-last-cursor
  :nv "C-p" #'evil-mc-make-and-goto-prev-cursor
  :nv "C-P" #'evil-mc-make-and-goto-first-cursor)

(load! "bindings")
