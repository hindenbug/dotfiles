;; -*- mode: emacs-lisp -*-
;; This file is loaded by Spacemacs at startup.
;; It must be stored in your home directory.

(defun dotspacemacs/layers ()
  "Configuration Layers declaration.
You should not put any user code in this function besides modifying the variable
values."
  (setq-default
   ;; Base distribution to use. This is a layer contained in the directory
   ;; `+distribution'. For now available distributions are `spacemacs-base'
   ;; or `spacemacs'. (default 'spacemacs)
   dotspacemacs-distribution 'spacemacs
   ;; List of additional paths where to look for configuration layers.
   ;; Paths must have a trailing slash (i.e. `~/.mycontribs/')
   dotspacemacs-configuration-layer-path '()
   ;; List of configuration layers to load. If it is the symbol `all' instead
   ;; of a list then all discovered layers will be installed.
   dotspacemacs-configuration-layers
   '(
     erlang
     javascript
     python
     html
     yaml
     ;; ----------------------------------------------------------------
     ;; Example of useful layers you may want to use right away.
     ;; Uncomment some layer names and press <SPC f e R> (Vim style) or
     ;; <M-m f e R> (Emacs style) to install them.
     ;; ----------------------------------------------------------------
     auto-completion
     better-defaults
     ;;dash
     ;;clojure
     (ruby :variables ruby-version-manager 'chruby)
     ;;ruby-on-rails
     ;;django
     emacs-lisp
     ivy
     git
     go
     rust
     markdown
     org
     (shell :variables
            shell-default-shell 'multi-term
            shell-default-term-shell "/bin/zsh")
     syntax-checking
     ;;version-control
     ;;colors
     github
     ;;themes-megapack
     puppet
     )
   ;; List of additional packages that will be installed without being
   ;; wrapped in a layer. If you need some configuration for these
   ;; packages then consider to create a layer, you can also put the
   ;; configuration in `dotspacemacs/config'.
   dotspacemacs-additional-packages '()
   ;; A list of packages and/or extensions that will not be install and loaded.
   dotspacemacs-excluded-packages '()
   ;; If non-nil spacemacs will delete any orphan packages, i.e. packages that
   ;; are declared in a layer which is not a member of
   ;; the list `dotspacemacs-configuration-layers'. (default t)
   dotspacemacs-delete-orphan-packages t))

(defun dotspacemacs/init ()
  "Initialization function.
This function is called at the very startup of Spacemacs initialization
before layers configuration.
You should not put any user code in there besides modifying the variable
values."
  ;; This setq-default sexp is an exhaustive list of all the supported
  ;; spacemacs settings.
  (setq-default
   ;; One of `vim', `emacs' or `hybrid'. Evil is always enabled but if the
   ;; variable is `emacs' then the `holy-mode' is enabled at startup. `hybrid'
   ;; uses emacs key bindings for vim's insert mode, but otherwise leaves evil
   ;; unchanged. (default 'vim)
   dotspacemacs-elpa-https nil
   dotspacemacs-editing-style 'hybrid
   ;; If non nil output loading progress in `*Messages*' buffer. (default nil)
   dotspacemacs-verbose-loading nil
   ;; Specify the startup banner. Default value is `official', it displays
   ;; the official spacemacs logo. An integer value is the index of text
   ;; banner, `random' chooses a random text banner in `core/banners'
   ;; directory. A string value must be a path to an image format supported
   ;; by your Emacs build.
   ;; If the value is nil then no banner is displayed. (default 'official)
   dotspacemacs-startup-banner nil
   ;; List of items to show in the startup buffer. If nil it is disabled.
   ;; Possible values are: `recents' `bookmarks' `projects'.
   ;; (default '(recents projects))
   dotspacemacs-startup-lists '(recents projects)
   ;; List of themes, the first of the list is loaded when spacemacs starts.
   ;; Press <SPC> T n to cycle to the next theme in the list (works great
   ;; with 2 themes variants, one dark and one light)
   dotspacemacs-themes '(flatland
                         ;;afternoon
                         ;;atom-dark
                         ;;dracula
                         ;;atom-one-dark
                         ;;flatui
                         ;;spacemacs-dark
                         ;;leuven
                         )
   ;; If non nil the cursor color matches the state color.
   dotspacemacs-colorize-cursor-according-to-state t
   ;; Default font. `powerline-scale' allows to quickly tweak the mode-line
   ;; size to make separators look not too crappy.
   dotspacemacs-default-font '("Fira Code Retina"
                               :size 15
                               :powerline-scale 1.1)
   ;; The leader key
   dotspacemacs-leader-key "SPC"
   ;; The leader key accessible in `emacs state' and `insert state'
   ;; (default "M-m")
   dotspacemacs-emacs-leader-key "M-m"
   ;; Major mode leader key is a shortcut key which is the equivalent of
   ;; pressing `<leader> m`. Set it to `nil` to disable it. (default ",")
   dotspacemacs-major-mode-leader-key ","
   ;; Major mode leader key accessible in `emacs state' and `insert state'.
   ;; (default "C-M-m)
   dotspacemacs-major-mode-emacs-leader-key "C-M-m"
   ;; The command key used for Evil commands (ex-commands) and
   ;; Emacs commands (M-x).
   ;; By default the command key is `:' so ex-commands are executed like in Vim
   ;; with `:' and Emacs commands are executed with `<leader> :'.
   dotspacemacs-command-key ":"
   ;; If non nil `Y' is remapped to `y$'. (default t)
   dotspacemacs-remap-Y-to-y$ t
   ;; Location where to auto-save files. Possible values are `original' to
   ;; auto-save the file in-place, `cache' to auto-save the file to another
   ;; file stored in the cache directory and `nil' to disable auto-saving.
   ;; (default 'cache)
   dotspacemacs-auto-save-file-location 'cache
   ;; If non nil then `ido' replaces `helm' for some commands. For now only
   ;; `find-files' (SPC f f), `find-spacemacs-file' (SPC f e s), and
   ;; `find-contrib-file' (SPC f e c) are replaced. (default nil)
   dotspacemacs-use-ido nil
   ;; If non nil, `helm' will try to miminimize the space it uses. (default nil)
   dotspacemacs-helm-resize nil
   ;; if non nil, the helm header is hidden when there is only one source.
   ;; (default nil)
   dotspacemacs-helm-no-header nil
   ;; define the position to display `helm', options are `bottom', `top',
   ;; `left', or `right'. (default 'bottom)
   dotspacemacs-helm-position 'bottom
   ;; If non nil the paste micro-state is enabled. When enabled pressing `p`
   ;; several times cycle between the kill ring content. (default nil)
   dotspacemacs-enable-paste-micro-state nil
   ;; Which-key delay in seconds. The which-key buffer is the popup listing
   ;; the commands bound to the current keystroke sequence. (default 0.4)
   dotspacemacs-which-key-delay 0.1
   ;; Which-key frame position. Possible values are `right', `bottom' and
   ;; `right-then-bottom'. right-then-bottom tries to display the frame to the
   ;; right; if there is insufficient space it displays it at the bottom.
   ;; (default 'bottom)
   dotspacemacs-which-key-position 'bottom
   ;; If non nil a progress bar is displayed when spacemacs is loading. This
   ;; may increase the boot time on some systems and emacs builds, set it to
   ;; nil to boost the loading time. (default t)
   dotspacemacs-loading-progress-bar nil
   ;; If non nil the frame is fullscreen when Emacs starts up. (default nil)
   ;; (Emacs 24.4+ only)
   dotspacemacs-fullscreen-at-startup nil
   ;; If non nil `spacemacs/toggle-fullscreen' will not use native fullscreen.
   ;; Use to disable fullscreen animations in OSX. (default nil)
   dotspacemacs-fullscreen-use-non-native nil
   ;; If non nil the frame is maximized when Emacs starts up.
   ;; Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.
   ;; (default nil) (Emacs 24.4+ only)
   dotspacemacs-maximized-at-startup t
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's active or selected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-active-transparency 70
   ;; A value from the range (0..100), in increasing opacity, which describes
   ;; the transparency level of a frame when it's inactive or deselected.
   ;; Transparency can be toggled through `toggle-transparency'. (default 90)
   dotspacemacs-inactive-transparency 70
   ;; If non nil unicode symbols are displayed in the mode line. (default t)
   dotspacemacs-mode-line-unicode-symbols nil
   ;; If non nil smooth scrolling (native-scrolling) is enabled. Smooth
   ;; scrolling overrides the default behavior of Emacs which recenters the
   ;; point when it reaches the top or bottom of the screen. (default t)
   dotspacemacs-smooth-scrolling t
   ;; If non-nil smartparens-strict-mode will be enabled in programming modes.
   ;; (default nil)
   dotspacemacs-smartparens-strict-mode nil
   ;; Select a scope to highlight delimiters. Possible values are `any',
   ;; `current', `all' or `nil'. Default is `all' (highlight any scope and
   ;; emphasis the current one). (default 'all)
   dotspacemacs-highlight-delimiters 'all
   ;; If non nil advises quit functions to keep server open when quitting.
   ;; (default nil)
   dotspacemacs-persistent-server nil
   ;; List of search tool executable names. Spacemacs uses the first installed
   ;; tool of the list. Supported tools are `ag', `pt', `ack' and `grep'.
   ;; (default '("ag" "pt" "ack" "grep"))
   dotspacemacs-search-tools '("ag" "ack" "grep")
   ;; The default package repository used if no explicit repository has been
   ;; specified with an installed package.
   ;; Not used for now. (default nil)
   dotspacemacs-default-package-repository nil
   dotspacemacs-check-for-update t
   ))

(defun dotspacemacs/user-init ()
  "Initialization function for user code.
It is called immediately after `dotspacemacs/init'.  You are free to put any
user code."

  (setq exec-path-from-shell-check-startup-files nil)
  (setq tramp-ssh-controlmaster-options
        "-o ControlMaster=auto -o ControlPath='tramp.%%C' -o ControlPersist=no")

  (setq inhibit-splash-screen t)
  (setq use-package-verbose t)
  (setq gc-cons-threshold (* 100 1024 1024))
  ;; (setq-default
  ;;  js2-mode
  ;;  js2-basic-offset 2
  ;;  js-indent-level 2
  ;;  ;; web-mode
  ;;  css-indent-offset 2
  ;;  web-mode-markup-indent-offset 2
  ;;  web-mode-css-indent-offset 2
  ;;  web-mode-code-indent-offset 2
  ;;  web-mode-attr-indent-offset 2)
)

(defun dotspacemacs/user-config ()
  "Configuration function for user code.
 This function is called at the very end of Spacemacs initialization after
layers configuration. You are free to put any user code."

  (setq racer-cmd "~/.cargo/bin/racer")
  (setq racer-rust-src-path "~/.rustup/toolchains/stable-x86_64-apple-darwin/lib/rustlib/src/rust/src")

  (setq flycheck-check-syntax-automatically '(mode-enabled save))
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)

  (global-hl-line-mode 1)
  (set-face-background 'hl-line "#3e4446")
  (set-face-foreground 'highlight nil)
  (global-linum-mode)
  (setq linum-format "%d ")
  (setq initial-buffer-choice t)
  (setq ns-use-srgb-colorspace nil)
  (setq powerline-default-separator 'arrow)
  (setq neo-vc-integration nil)
  (setq clojure-enable-fancify-symbols t)
  (setq cider-show-error-buffer 'except-in-repl)
  (golden-ratio-mode 1)
  (setq diff-hl-side 'right)
  ;;(spacemacs/toggle-fill-column-indicator)
  ;;(setq-default rust-enable-racer t)

  ;; Indentation
  (indent-guide-global-mode 1)

  (setq-default truncate-lines t)
  (add-hook 'before-save-hook 'delete-trailing-whitespace)
  ;;(require 'helm-projectile)
  ;;(helm-projectile-on)

  (autoload 'markdown-mode "markdown-mode"
    "Major mode for editing Markdown files" t)
  (add-to-list 'auto-mode-alist '("\\.post\\'" . markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
  ;;(set-face-attribute 'default nil :height 120)

  (spaceline-compile)

  (defun spacemacs/rust-cargo-build ()
    (interactive)

    (if (locate-dominating-file (buffer-file-name) "Cargo.toml")
        (compile "cargo build")
      (rust-save-and-compile-this)))

  (defun spacemacs/rust-cargo-run ()
    (interactive)

    (if (locate-dominating-file (buffer-file-name) "Cargo.toml")
        (compile "cargo run")
      (rust-save-and-run-this)))

  (defun spacemacs/rust-cargo-test ()
    (interactive)

    (if (locate-dominating-file (buffer-file-name) "Cargo.toml")
        (compile "cargo test")
      (rust-save-and-run-this)))

  (defun spacemacs/rust-cargo-doc ()
    (interactive)
    (compile "cargo doc"))

  (defun spacemacs/rust-cargo-clean ()
    (interactive)
    (compile "cargo clean"))

  (defun rust-save-and-run-this ()
    (interactive)
    (compile
     (format "rustc %s && %s"
             (buffer-file-name)
             (file-name-sans-extension (buffer-file-name)))))

  (defun rust-save-and-compile-this ()
    (interactive)
    (compile
     (format "rustc %s"
             (buffer-file-name)
             (file-name-sans-extension (buffer-file-name)))))
  )

;; Do not write anything past this comment. This is where Emacs will
;; auto-generate custom variable definitions.
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(evil-want-Y-yank-to-eol t)
 '(fci-rule-color "#3E4451" t)
 '(hl-paren-background-colors (quote ("#2492db" "#95a5a6" nil)))
 '(hl-paren-colors (quote ("#ecf0f1" "#ecf0f1" "#c0392b")) t)
 '(package-selected-packages
   (quote
    (wgrep smex ivy-hydra counsel-projectile counsel swiper ivy ghub let-alist winum unfill fuzzy org-category-capture erlang web-beautify livid-mode skewer-mode simple-httpd json-mode json-snatcher json-reformat js2-refactor js2-mode js-doc company-tern dash-functional tern coffee-mode rust-playground highlight-current-line smart-mode-line color-theme-sanityinc-solarized color-theme-sanityinc-tomorrow flatland-theme nil-theme yaml-mode puppet-mode spacegray-theme afternoon-theme doom-themes racer clojure-snippets helm-themes goto-chg eshell-z dumb-jump diminish column-enforce-mode cargo atom-dark-theme dracula-theme yapfify uuidgen py-isort pug-mode org-projectile org org-download mwim minitest live-py-mode link-hint hide-comnt go-guru github-search eyebrowse evil-visual-mark-mode evil-unimpaired evil-ediff undo-tree company-web quelpa package-build use-package which-key bind-key bind-map evil xterm-color ws-butler window-numbering web-mode volatile-highlights vi-tilde-fringe toml-mode toc-org tagedit spacemacs-theme spaceline smooth-scrolling smeargle slim-mode shell-pop scss-mode sass-mode rvm ruby-tools ruby-test-mode rspec-mode robe restart-emacs rbenv rust-mode pyvenv pytest pyenv-mode py-yapf projectile-rails rake popwin pony-mode pip-requirements persp-mode pcre2el paradox page-break-lines orgit org-repo-todo org-present org-pomodoro alert log4e gntp org-plus-contrib org-bullets open-junk-file neotree multi-term move-text mmm-mode markdown-toc markdown-mode magit-gitflow magit-gh-pulls macrostep lorem-ipsum leuven-theme less-css-mode jade-mode info+ indent-guide ido-vertical-mode hy-mode hungry-delete htmlize hl-todo highlight-parentheses highlight-numbers parent-mode highlight-indentation help-fns+ helm-swoop helm-pydoc helm-projectile helm-mode-manager helm-make helm-gitignore request helm-flx helm-descbinds helm-dash helm-css-scss helm-company helm-c-yasnippet helm-ag haml-mode google-translate go-eldoc gnuplot gitignore-mode github-clone github-browse-file gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link git-gutter-fringe+ git-gutter-fringe fringe-helper git-gutter+ git-gutter gist gh marshal logito pcache ht gh-md flycheck-rust flycheck flx-ido flx fill-column-indicator feature-mode fancy-battery expand-region evil-visualstar evil-tutor evil-mc evil-magit magit-popup git-commit with-editor smartparens evil-indent-plus evil-iedit-state iedit evil-exchange evil-args evil-anzu anzu eshell-prompt-extras esh-help emmet-mode elisp-slime-nav define-word dash-at-point cython-mode web-completion-data company-racer deferred company-quickhelp pos-tip company-go go-mode company-anaconda hydra inflections edn multiple-cursors paredit peg clean-aindent-mode cider-eval-sexp-fu eval-sexp-fu highlight cider queue pkg-info clojure-mode epl chruby bundler inf-ruby buffer-move bracketed-paste auto-yasnippet yasnippet auto-highlight-symbol auto-compile packed anaconda-mode pythonic f dash s ace-link ace-jump-helm-line helm avy helm-core async ac-ispell auto-complete popup rainbow-mode diff-hl company-statistics company aggressive-indent adaptive-wrap ace-window spinner slamhound seq rainbow-identifiers rainbow-delimiters projectile powerline package-utils noflet magit linum-relative golden-ratio git-rebase-mode git-commit-mode flycheck-pos-tip flycheck-clojure evil-tabs evil-surround evil-smartparens evil-search-highlight-persist evil-paredit evil-numbers evil-nerd-commenter evil-matchit evil-lisp-state evil-jumper evil-escape evil-easymotion evil-commentary evil-cleverparens clojure-mode-extra-font-locking clojure-cheatsheet clj-refactor better-defaults ace-jump-mode ac-cider 4clojure)))
 '(paradox-github-token t)
 '(sml/active-background-color "#34495e")
 '(sml/active-foreground-color "#ecf0f1")
 '(sml/inactive-background-color "#dfe4ea")
 '(sml/inactive-foreground-color "#34495e")
 '(spacemacs-theme-custom-colors
   (quote
    ((act1 . "#ff0000")
     (act2 . "#0000ff")
     (base . "#ffffff")))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:foreground "#f8f8f8" :background "#26292c"))))
 '(company-tooltip-common ((t (:inherit company-tooltip :weight bold :underline nil))))
 '(company-tooltip-common-selection ((t (:inherit company-tooltip-selection :weight bold :underline nil)))))
