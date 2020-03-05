;;; ~/.doom.d/bindings.el -*- lexical-binding: t; -*-

(map!
  :nv  "gc"   #'evil-commentary
  :nmvo "C-p"    #'counsel-projectile-find-file
)

;; Elixir Mode
;; (after! elixir-mode
;;         (:leader
;;           :desc "Toggle between file and tests"   :n "t" (λ! (alchemist-project-toggle-file-and-tests))
;;           :desc "Jump to definition at point"     :n "l" #'alchemist-goto-definition-at-point))

 ;; Rust Mode
;; (after! rust-mode
;;         (:leader
;;           :desc "Lookup documentation at point"   :n "d" #'racer-describe
;;           :desc "Jump to definition at point"     :n "l" #'racer-find-definition))
