;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! flycheck)
(package! flycheck-mix)
(package! flycheck-credo)
(package! paredit)
(package! erlang)
(package! rust-mode)
(package! racer)
(package! flycheck-rust)
(package! org)
(package! lsp-mode)
(package! lsp-ui :recipe (:fetcher github :repo "emacs-lsp/lsp-ui"))
(package! lsp-rust)
(package! haskell-mode)
(package! flycheck-haskell)
(package! dockerfile-mode)
(package! paxedit)
(package! aggressive-indent)
(package! fill-column-indicator)
