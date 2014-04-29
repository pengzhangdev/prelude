;; my custom code style
;; (add-to-list 'load-path prelude-core-dir)

(prelude-require-packages '(yasnippet highlight-symbol dropdown-list auto-complete
                                      autopair slime ac-slime ac-c-headers smart-compile
                                      emamux))

(mapc 'load (directory-files
             (expand-file-name "custom" prelude-personal-dir) 't "^[^#].*el$"))

(setq-default indent-tabs-mode nil)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; O_RDONLY
(defun make-some-files-read-only ()
  "when file opened is of a certain mode, make it read only"
  (when (memq major-mode '(c++-mode tcl-mode text-mode python-mode c-mode java-mode))
    (toggle-read-only 1)))

(add-hook 'find-file-hooks 'make-some-files-read-only)

;;;speedbar
(global-set-key [(f4)] 'speedbar-get-focus)

;; highligth
(require 'highlight-symbol)
(global-set-key [f5] 'highlight-symbol-at-point)
(global-set-key [(control f5)] 'highlight-symbol-next)
(global-set-key [(shift f5)] 'highlight-symbol-prev)
(global-set-key [(meta f5)] 'highlight-symbol-prev)
(global-set-key [(control meta f5)] 'highlight-symbol-query-replace)

;; (global-set-key [(control left)] 'windmove-left)
;; (global-set-key [(control down)] 'windmove-down)
;; (global-set-key [(control up)] 'windmove-up)
;; (global-set-key [(control right)] 'windmove-right)


;;(setq semanticdb-default-save-directory "~/.emacs.d/semanticdb/")


;;;;;;yasnippet
(require 'yasnippet)
(require 'dropdown-list)
(setq yas/prompt-functions '(yas/dropdown-prompt
                             yas/ido-prompt
                             yas/completing-prompt))
(yas/global-mode 1)

(global-set-key (kbd "<f6>") 'goto-line)

;;;;smart compile
;;(load "smart-compile.el")
(global-set-key (kbd "<f7>") 'smart-compile)

;;;; auto save to ~/.emacs.d/autobakcup
;;(setq kept-old-versions 2)
;;(setq kept-new-versions 5)
;;(setq delete-old-versions t)
;;(setq backup-directory-alist '(("." . "~/.emacs.d/autobakcup")))
;;(setq backup-by-copying t)

;;;; show errors in C/Cpp
(global-cwarn-mode 1)

;;;;;;cscope
(require 'xcscope)

;;;; semantic
;; (require 'semantic)
;;(semantic-load-enable-minimum-features)
;;(semantic-load-enable-code-helpers)
;;(global-semantic-idle-summary-mode)
;;(global-semantic-decoration-mode nil)
;;(require 'semantic)
;;(semantic-load-enable-code-helpers)

(semantic-mode)
(global-semantic-idle-scheduler-mode)
(global-semantic-idle-summary-mode)
(global-semantic-highlight-func-mode)
(global-semantic-decoration-mode)

;;;;gtags
(autoload 'gtags-mode "gtags" "" t)
(gtags-mode 1)
(setq gtags-auto-update 1)
(global-set-key [f3] 'gtags-find-tag-from-here)
(global-set-key [f2] 'gtags-pop-stack)
(global-set-key (kbd "C-c g f") 'gtags-find-tag)
(global-set-key (kbd "C-c g r") 'gtags-find-rtag)
(global-set-key (kbd "C-c g u") 'gtags-pop-stack)
(global-set-key (kbd "C-c g s") 'gtags-find-symbols)
(global-set-key (kbd "C-c g g") 'gtags-find-with-grep)
(global-set-key (kbd "C-c g o") 'gtags-select-tag)
(add-hook 'gtags-select-mode-hook
          '(lambda ()
             (setq hl-line-face 'underline)
             (hl-line-mode 1)
             ))

;; diable withspace and use C-n n instead.
(setq prelude-whitespace nil)

;; flyspell conflict with auto-complete
(setq prelude-flyspell nil)

;; auto-complete
(define-key ac-completing-map "\r" nil)
(define-key ac-completing-map "\t" nil)
(define-key ac-completing-map "\M-j" 'ac-complete)

;; set highlight color
;; both the higlight current line color and enclose parens color
(set-face-background 'hl-line "#3e4446")
(set-face-foreground 'highlight nil)

;; set off guru-mode, because of smartparen
(setq prelude-guru nil)

;; confirm befor exit emacs
(setq confirm-kill-emacs 'yes-or-no-p)

;; remove smartparen pair '{}'
(sp-pair "\{" nil :actions :rem)

;; emacs tmux
(require 'emamux)

