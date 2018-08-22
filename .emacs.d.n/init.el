;;; init.el --- pull in the world, mold it to my liking.
;;; commentary:
;;; code:

(eval-and-compile (load "~/.emacs.d/lisp/theworld.el"))

(defmacro ms/load (&rest targets)
  `(mapc (lambda(target)
           (funcall (intern (concat "ms/" (prin1-to-string target)))))
	 ',targets))

(defmacro ms/compose (name &rest targets)
  `(defconfig ,name (ms/load ,@targets)))

(ms/compose
 core

 ;; use-package
 straight
 bedrock
 sanity
 evil
 interface
 editing
 ;; shell
 git
 ;; org
 util
 )

(ms/compose
 extra

 company
 flycheck
 jump
 ;; dashdocs

 ;; terminal
 ;; zoom
 ;; dimmer
 projectile
 treemacs
 restclient
 deadgrep
 ;; latex
 ;; search-engines

 ;; target-process
 ;; music
 ;; pdf
 ledger
 emoji
 ;; filehooks
 ;; writing
 )

(ms/compose
 development

 ;; clojure
 ;; csharp
 elisp
 ;; javascript
 ;; typescript
 ;; lsp
 ;; sql
 )

;; (ms/compose
 ;; communication
;;
 ;; irc slack twitter email
 ;; reddit stackexchange elfeed
 ;; )

;; liftoff

;; add communication
;; check staging

(ms/load core extra development)
(ms/check-for-orphans)

;; Emacs is terribly slow on windows
(ms/toggle-bloat-global t)

(ms/style) ;; also gets spaceline

(provide 'init)
;;; init.el ends here
