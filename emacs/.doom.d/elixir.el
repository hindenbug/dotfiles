;;; ~/.doom.d/elixir.el -*- lexical-binding: t; -*-

;; Elixir
(setq alchemist-iex-program-name (concat (getenv "HOME") "/.asdf/shims/iex"))
;;default: iex

(add-hook! elixir-mode
  (flycheck-mode)
  (rainbow-delimiters-mode))

(use-package! alchemist
  :hook (elixir-mode . alchemist-mode)
  :config
  (set-lookup-handlers! 'elixir-mode
    :definition #'alchemist-goto-definition-at-point
    :documentation #'alchemist-help-search-at-point)
  (set-eval-handler! 'elixir-mode #'alchemist-eval-region)
  (set-repl-handler! 'elixir-mode #'alchemist-iex-project-run)
  (setq alchemist-mix-env "dev")
  (setq alchemist-hooks-compile-on-save t)
  (map! :map elixir-mode-map :nv "m" alchemist-mode-keymap))

;; Now configure LSP mode and set the client filepath.
(use-package! lsp-mode
  :commands lsp
  :config
  (setq lsp-enable-file-watchers nil)
  :hook
  (elixir-mode . lsp))

(after! lsp-clients
  (lsp-register-client
   (make-lsp-client :new-connection
    (lsp-stdio-connection
        (expand-file-name
          "~/elixir-ls/release/language_server.sh"))
        :major-modes '(elixir-mode)
        :priority -1
        :server-id 'elixir-ls
        :initialized-fn (lambda (workspace)
            (with-lsp-workspace workspace
             (let ((config `(:elixirLS
                             (:mixEnv "dev"
                                     :dialyzerEnabled
                                     :json-false))))
               (lsp--set-configuration config)))))))

;; Configure LSP-ui to define when and how to display informations.
(after! lsp-ui
  (setq lsp-ui-doc-max-height 20
        lsp-ui-doc-max-width 80
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'bottom
        lsp-ui-doc-use-webkit nil
        lsp-ui-flycheck-enable t
        lsp-ui-imenu-kind-position 'left
        lsp-ui-sideline-code-actions-prefix "💡"
        ;; fix for completing candidates not showing after “Enum.”:
        company-lsp-match-candidate-predicate #'company-lsp-match-candidate-prefix
        ))

(use-package! dap-mode)

;; Configure exunit
(use-package! exunit)

;; Enable credo checks on flycheck
(use-package! flycheck-credo
  :after flycheck
  :config
    (flycheck-credo-setup)
    (after! lsp-ui
      (flycheck-add-next-checker 'lsp-ui 'elixir-credo)))

;; Enable format and iex reload on save
(after! lsp
  (add-hook 'elixir-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'elixir-format nil t)
              (add-hook 'after-save-hook 'alchemist-iex-reload-module))))

(map! :mode elixir-mode
        :leader
        :desc "iMenu" :nve  "c/"    #'lsp-ui-imenu
        :desc "Run all tests"   :nve  "ctt"   #'exunit-verify-all
        :desc "Run all in umbrella"   :nve  "ctT"   #'exunit-verify-all-in-umbrella
        :desc "Re-run tests"   :nve  "ctx"   #'exunit-rerun
        :desc "Run single test"   :nve  "cts"   #'exunit-verify-single)
