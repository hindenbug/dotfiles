;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-

(add-to-list 'initial-frame-alist '(fullscreen . maximized))

(global-set-key (kbd "C-z") 'undo)
;; Basic Config
(setq backup-directory-alist `(("." . "~/.emacs-tmp/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs-tmp/" t)))
(setq +ivy-buffer-icons t)

;; Spaces over tabs
(setq tab-width 2)
(setq-default indent-tabs-mode nil)

(setq exec-path
      (list "/usr/local/bin/"
            "/usr/bin/"
            "/bin/"
            "/usr/sbin/"
            "/sbin/"
            (concat (getenv "HOME") "/.nix-profile/bin")
            (concat (getenv "HOME") "/.cargo/bin")
            (concat (getenv "HOME") "/.local/bin")
            (concat (getenv "HOME") "/.asdf/shims")))

(setenv "PATH" (string-join exec-path ":"))

;;(global-auto-revert-mode t)

(setq show-trailing-whitespace t)

;; Show matching parens
(setq show-paren-delay 0)
(show-paren-mode 1)

(setq
 whitespace-line-column 80
 whitespace-style
 '(face trailing lines-tail tabs))

(global-whitespace-mode)

(custom-set-faces
 '(whitespace-tab ((t (:background "red")))))

;; Turn off line wrapping
(setq-default truncate-lines 1)

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(require 'solaire-mode)
;; Enable solaire-mode anywhere it can be enabled
(solaire-global-mode +1)

(setq doom-font (font-spec :family "Fira Code Retina" :size 15))
(setq which-key-idle-delay 0)

;;(setq 'solaire-hl-line-face "#fbffbf")
;;(setq 'solaire-hl-line-face nil)

(require 'doom-themes)
;; Global settings (defaults)
(setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
      doom-themes-enable-italic t) ; if nil, italics is universally disabled
(load-theme 'doom-dracula t)
;; Enable flashing mode-line on errors
(doom-themes-visual-bell-config)
;; Enable custom neotree theme (all-the-icons must be installed!)
(doom-themes-neotree-config)

;; Corrects (and improves) org-mode's native fontification.
(doom-themes-org-config)

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

(load! "elixir")

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

(use-package! rust-mode
  :mode "\\.rs$"
  :config
  (flycheck-mode))

(use-package! dockerfile-mode
   :mode "Dockerfile$")

(use-package! erlang
  :mode "\\.erl$"
  :config
  (erlang-mode))

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

(load! "bindings")
