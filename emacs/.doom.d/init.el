;;; init.el -*- lexical-binding: t; -*-
;; Copy me to ~/.doom.d/init.el or ~/.config/doom/init.el, then edit me!

(doom! :feature
       ;;debugger          ; FIXME stepping through code, to help you add bugs

       :completion
       (company          ; the ultimate code completion backend
        +auto
        +childframe)     ; as-you-type code completion
       (ivy
         +counsel-fzf
         +childframe)    ; enable fuzzy search backend for ivy

       :ui
       ophints           ; display visual hints when editing in evil
       deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       modeline          ; a snazzy Atom-inspired mode-line
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       nav-flash         ; blink the current line after jumping
       ;;neotree           ; a project drawer, like NERDTree for vim
       treemacs          ; a project drawer, like neotree but cooler
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       pretty-code       ; replace bits of code with pretty symbols
       ;;tabbar            ; FIXME an (incomplete) tab bar for Emacs
       ;;unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       workspaces        ; tab emulation, persistence & separate workspaces

       :editor
       snippets          ; my elves. They type so I don't have to
       file-templates    ; auto-snippets for empty files
       (evil
        +everywhere); come to the dark side, we have cookies
       (format +onsave)  ; automated prettiness
       multiple-cursors  ; editing in many places at once
       ;;parinfer          ; turn lisp into python, sort of
       rotate-text       ; cycle region at point between text candidates
       file-templates    ; auto-snippets for empty files
       snippets          ; my elves. They type so I don't have to
       (evil
        +everywhere); come to the dark side, we have cookies

       :emacs
       (dired +ranger
              +icons)    ; making dired pretty [functional]
       ;;ediff             ; comparing files in Emacs
       electric          ; smarter, keyword-based electric-indent
       ;eshell            ; a consistent, cross-platform shell (WIP)
       vc                ; version-control and Emacs, sitting in a tree

       :term
       term              ; terminals in Emacs

       :tools
       eval              ; run code, run (also, repls)
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       flyspell
       ;;ansible
       docker
       lsp
       flyspell
       editorconfig      ; let someone else argue about tabs vs spaces
       ;;ein               ; tame Jupyter notebooks with emacs
       ;;gist              ; interacting with github gists
       ;;macos             ; MacOS-specific commands
       ;;make              ; run make tasks from Emacs
       magit             ; a git porcelain for Emacs
       ;;password-store    ; password manager for nerds
       ;;pdf               ; pdf enhancements
       ;;prodigy           ; FIXME managing external services & code builders
       rgb               ; creating color strings
       tmux              ; an API for interacting with tmux
       ;;upload            ; map local to remote projects via ssh/ftp
       ;;wakatime

       :lang
       ;;clojure           ; java with a lisp
       ;;coq               ; proofs-as-programs
       data              ; config/data formats
       erlang            ; an elegant language for a more civilized age
       elixir            ; erlang done right
       emacs-lisp        ; drown in parentheses
       ;;ess               ; emacs speaks statistics
       go                ; the hipster dialect
       (haskell +intero) ; a language that's lazier than I am
       ;;hy                ; readability of scheme w/ speed of python
       ;;idris             ;
       javascript        ; all(hope(abandon(ye(who(enter(here))))))
       ;;julia             ; a better, faster MATLAB
       ;;latex             ; writing papers in Emacs has never been so fun
       ;;ledger            ; an accounting system in Emacs
       ;;lua               ; one-based indices? one-based indices
       markdown          ; writing docs for people to ignore
       ;;nim               ; python + lisp at the speed of c
       ;;nix               ; I hereby declare "nix geht mehr!"
       (org              ; organize your plain life in plain text
        +attach          ; custom attachment system
        +babel           ; running code in org
        +capture         ; org-capture in and outside of Emacs
        +export          ; Exporting org to whatever you want
        +present
        +protocol)        ; Emacs for presentations
       ;;perl              ; write code no one else can comprehend
       ;;php               ; perl's insecure younger brother
       plantuml          ; diagrams for confusing people more
       ;;purescript        ; javascript, but functional
       ;;python            ; beautiful is better than ugly
       rest              ; Emacs as a REST client
       ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       rust              ; Fe2O3.unwrap().unwrap().unwrap().unwrap()
       (sh +zsh)        ; she sells (ba|z|fi)sh shells on the C xor
       ;;solidity          ; do you need a blockchain? No.
       ;;swift             ; who asked for emoji variables?
       web               ; the tubes

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
       ;;(email +gmail)    ; emacs as an email client
       irc               ; how neckbeards socialize
       ;;(rss +org)        ; emacs as an RSS reader
       ;;twitter           ; twitter client https://twitter.com/vnought
       ;;(write            ; emacs as a word processor (latex + org + markdown)
       ;; +wordnut         ; wordnet (wn) search
       ;; +langtool)       ; a proofreader (grammar/style check) for Emacs

       :collab
       ;;floobits          ; peer programming for a price
       ;;impatient-mode    ; show off code over HTTP

       :config
       ;; For literate config users. This will tangle+compile a config.org
       ;; literate config in your `doom-private-dir' whenever it changes.
       ;;literate

       ;; The default module sets reasonable defaults for Emacs. It also
       ;; provides a Spacemacs-inspired keybinding scheme, a custom yasnippet
       ;; library, and additional ex commands for evil-mode. Use it as a
       ;; reference for your own modules.
       (default +bindings +snippets +evil-commands +smartparens))

(global-set-key (kbd "C-z") 'undo)
;; Basic Config
(setq backup-directory-alist `(("." . "~/.emacs-tmp/")))
(setq auto-save-file-name-transforms `((".*" "~/.emacs-tmp/" t)))

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

(setq
 whitespace-line-column 80
 whitespace-style
 '(face trailing lines-tail tabs))

(global-whitespace-mode)

(custom-set-faces
 '(whitespace-tab ((t (:background "red")))))

;;(add-hook 'after-make-frame-functions
;;          (lambda (frame)
;;            (global-whitespace-mode)
;;            ))

;; Turn off line wrapping
(setq-default truncate-lines 1)

(add-hook 'before-save-hook 'whitespace-cleanup)
(add-hook 'before-save-hook #'delete-trailing-whitespace)

(require 'solaire-mode)
;; Enable solaire-mode anywhere it can be enabled
(solaire-global-mode +1)

;; To enable solaire-mode unconditionally for certain modes:
(add-hook 'ediff-prepare-buffer-hook #'solaire-mode)

;; ...if you use auto-revert-mode, this prevents solaire-mode from turning
;; itself off every time Emacs reverts the file
(add-hook 'after-revert-hook #'turn-on-solaire-mode)

;; highlight the minibuffer when it is activated:
(add-hook 'minibuffer-setup-hook #'solaire-mode-in-minibuffer)

;; if the bright and dark background colors are the wrong way around, use this
;; to switch the backgrounds of the `default` and `solaire-default-face` faces.
;; This should be used *after* you load the active theme!
;;
;; NOTE: This is necessary for themes in the doom-themes package!
(solaire-mode-swap-bg)

;; Elixir
(setq alchemist-iex-program-name (concat (getenv "HOME") "/.asdf/shims/iex")) ;;default: iex

;; Create a buffer-local hook to run elixir-format on save, only when we enable elixir-mode.
(add-hook 'elixir-mode-hook
          (lambda () (add-hook 'before-save-hook 'elixir-format nil t)))

;;(require 'quickrun)
;;(add-hook 'elixir-mode-hook #'lsp)
;;(add-hook 'lsp-mode-hook 'lsp-ui-mode)

(setq confirm-kill-processes nil)

(add-hook 'after-init-hook (lambda ()
                             (require 'server)
                             (unless (server-running-p)
                               (server-start))))

(after! which-key
  (setq which-key-idle-delay 0.1
        which-key-idle-secondary-delay 0.01
        which-key-sort-order 'which-key-key-order-alpha))
