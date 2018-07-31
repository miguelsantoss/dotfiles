;;; private/ms/config.el -*- lexical-binding: t; -*-

(def-package! feature-mode
  :mode "\\.feature$")

(map! "C-s" #'counsel-grep-or-swiper
      (:leader  :prefix "p" :n "s" #'+ivy/project-search)
      (:leader  :prefix "f" :n "d" #'dired-jump))

(def-package! apropospriate-theme
  :init (load-theme 'apropospriate-light t))
