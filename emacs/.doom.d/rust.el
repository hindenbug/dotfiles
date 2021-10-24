;;; ~/.doom.d/rust.el -*- lexical-binding: t; -*-

;; Rust
(after! lsp-mode
  (add-hook! 'rust-mode-hook 'lsp-mode))

(setq lsp-rust-server 'rust-analyzer)

(use-package lsp-mode
  :hook  ((rust-mode . lsp-deffered)
          (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-defferred))

(after! rustic
  (setq rustic-format-on-save t)
  (setq rustic-lsp-server 'rust-analyzer))

(use-package! rust-mode
  :mode "\\.rs$"
  :config
  (flycheck-mode))

(use-package rust-mode
  :mode "\\.rs\\'"
  :commands (rust-format-buffer)
  :hook (rust-mode . #'eglot-ensure)
  :config
  (setq rust-format-on-save t)
  (progn
     ;; add flycheck support for rust (reads in cargo stuff)
     (use-package flycheck :ensure t)
     (use-package flycheck-rust :ensure t)

     ;; cargo-mode for all the cargo related operations
     (use-package cargo
       :ensure t
       :hook ((rust-mode toml-mode) . cargo-minor-mode)
       :bind
       ("C-c C-c C-n" . cargo-process-new))

     (add-hook 'after-init-hook #'global-flycheck-mode)
     (add-hook 'rust-mode-hook 'flycheck-mode)
     (add-hook 'flycheck-mode-hook 'flycheck-rust-setup)
     '(flycheck-check-syntax-automatically (quote (save idle-change mode-enabled)))

    ;; format rust buffers on save using rustfmt
     (add-hook 'before-save-hook
               (lambda ()
                 (when (eq major-mode 'rust-mode)
                   (rust-format-buffer))))))
