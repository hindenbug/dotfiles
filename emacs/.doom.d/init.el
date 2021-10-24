;;; init.el -*- lexical-binding: t; -*-
;; Copy me to ~/.doom.d/init.el or ~/.config/doom/init.el, then edit me!

(doom! :feature

       :completion
       (company          ; the ultimate code completion backend
        +auto)           ; as-you-type code completion
       (ivy
         +prescient
         +fuzzy
         +icons
         +childframe)    ; enable fuzzy search backend for ivy

       :ui
       workspaces        ; tab emulation, persistence & separate workspaces
       ophints
       modeline
       deft              ; notational velocity for Emacs
       doom              ; what makes DOOM look the way it does
       doom-dashboard    ; a nifty splash screen for Emacs
       doom-quit         ; DOOM quit-message prompts when you quit Emacs
       fill-column       ; a `fill-column' indicator
       hl-todo           ; highlight TODO/FIXME/NOTE tags
       nav-flash         ; blink the current line after jumping
       treemacs          ; a project drawer, like neotree but cooler
       (popup            ; tame sudden yet inevitable temporary windows
        +all             ; catch all popups that start with an asterix
        +defaults)       ; default popup rules
       (ligatures
        +extra)     ; replace bits of code with pretty symbols
       ;;unicode           ; extended unicode support for various languages
       vc-gutter         ; vcs diff in the fringe
       vi-tilde-fringe   ; fringe tildes to mark beyond EOB
       window-select     ; visually switch windows
       zen

       :editor
       snippets          ; my elves. They type so I don't have to
       file-templates    ; auto-snippets for empty files
       (evil
        +everywhere); come to the dark side, we have cookies
       (format +onsave)  ; automated prettiness
       multiple-cursors  ; editing in many places at once
       rotate-text       ; cycle region at point between text candidates

       :emacs
       (dired
        +ranger
        +icons)            ; making dired pretty [functional]
       ;;ediff             ; comparing files in Emacs
       electric          ; smarter, keyword-based electric-indent
       ;eshell            ; a consistent, cross-platform shell (WIP)
       ;hideshow          ; basic code-folding support
       vc                ; version-control and Emacs, sitting in a tree
       undo

       :term
       vterm

       :checkers
       (syntax
        +childframe)

       :os
       (:if IS-MAC macos)

       :tools
       debugger
       (lookup           ; helps you navigate your code and documentation
        +docsets)        ; ...or in Dash docsets locally
       (eval
        +overlay)              ; run code, run (also, repls)
       ;;ansible
       ;;docker
       ;;editorconfig      ; let someone else argue about tabs vs spaces
       ;;gist              ; interacting with github gists
       ;;macos             ; MacOS-specific commands
       ;;make              ; run make tasks from Emacs
       (magit -forge)             ; a git porcelain for Emacs
       ;;pdf               ; pdf enhancements
       tmux              ; an API for interacting with tmux
       ;;wakatime
       lsp

       :lang
       ;;clojure           ; java with a lisp
       ;;coq               ; proofs-as-programs
       data              ; config/data formats
       ;;erlang            ; an elegant language for a more civilized age
       ;;elixir            ; erlang done right
       emacs-lisp        ; drown in parentheses
       (go
        +lsp)                ; the hipster dialect
       markdown          ; writing docs for people to ignore
       (org              ; organize your plain life in plain text
        +attach          ; custom attachment system
        +babel           ; running code in org
        +capture         ; org-capture in and outside of Emacs
        +export          ; Exporting org to whatever you want
        +present
        +roam)           ; Emacs for presentations
       ;;plantuml          ; diagrams for confusing people more
       ;;python            ; beautiful is better than ugly
       ;; rest              ; Emacs as a REST client
       ;;ruby              ; 1.step do {|i| p "Ruby is #{i.even? ? 'love' : 'life'}"}
       (rust
        +lsp)           ;Fe2O3.unwrap().unwrap().unwrap().unwrap()
       (sh +zsh)        ; she sells (ba|z|fi)sh shells on the C xor
       zig

       ;; Applications are complex and opinionated modules that transform Emacs
       ;; toward a specific purpose. They may have additional dependencies and
       ;; should be loaded late.
       :app
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
       (default +bindings +snippets +evil-commands))
