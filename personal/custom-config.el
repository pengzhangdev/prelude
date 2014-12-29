;; my custom code style
;; (add-to-list 'load-path prelude-core-dir)

(prelude-require-packages '(yasnippet highlight-symbol dropdown-list auto-complete
                                      autopair slime ac-slime ac-c-headers smart-compile
                                      emamux pomodoro htmlize xcscope jedi auto-compile
                                      ggtags auto-complete-clang multi-eshell))
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (ggtags-mode 1)
              (define-key ggtags-mode-map "\M-," 'pop-tag-mark))
            (when (derived-mode-p 'c-mode 'c++-mode 'objc-mode)
              (setq ac-source (append '(ac-source-clang ac-source-yasnippet) ac-source))
              )))

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
(setq kept-old-versions 2)
(setq kept-new-versions 5)
(setq delete-old-versions t)
(setq backup-directory-alist `((".*" . ,(expand-file-name "autobackup" prelude-savefile-dir))))
(setq auto-save-file-name-transforms `((".*" ,(expand-file-name "autobackup" prelude-savefile-dir) t)))

;;;; show errors in C/Cpp
(global-cwarn-mode 1)

;;;;;;cscope
(require 'xcscope)
(add-hook 'c-mode-hook (function cscope-minor-mode))
(add-hook 'c++-mode-hook (function cscope-minor-mode))
(add-hook 'dired-mode-hook (function cscope-minor-mode))


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
;;(autoload 'gtags-mode "gtags" "" t)
;;(gtags-mode 1)
;;(setq gtags-auto-update 1)
;;(global-set-key [f3] 'gtags-find-tag-from-here)
;;(global-set-key [f2] 'gtags-pop-stack)
;;(global-set-key (kbd "C-c g f") 'gtags-find-tag)
;;(global-set-key (kbd "C-c g r") 'gtags-find-rtag)
;;(global-set-key (kbd "C-c g u") 'gtags-pop-stack)
;;(global-set-key (kbd "C-c g s") 'gtags-find-symbols)
;;(global-set-key (kbd "C-c g g") 'gtags-find-with-grep)
;;(global-set-key (kbd "C-c g o") 'gtags-select-tag)
;;(add-hook 'gtags-select-mode-hook
;;          '(lambda ()
;;;             (setq hl-line-face 'underline)
;;;            (hl-line-mode 1)
;;             ))

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

;; enable highlight when export to html
;; This require htmlize.el
(setq org-src-fontify-natively t)

;; disabel 2 window swap
(define-key prelude-mode-map (kbd "C-c s") nil)

;; python development env
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(setq jedi:use-shortcuts t) ;; using M-. and M-, to jump code
I
;;; jedi install guide
;;; Install Python server (jediepcserver.py) by running
;;;
;;; M-x jedi:install-server in Emacs
;;; (see also jedi:install-server).
;;;
;;;out of emacs, install python-virtualenv.
;;;apt-get install python-virtualenv
;; eshell prompt

(defmacro with-face (str &rest properties)
    `(propertize ,str 'face (list ,@properties)))

(defun shortened-path (path max-len)
  "Return a modified version of `path', replacing some components
      with single characters starting from the left to try and get
      the path down to `max-len'"
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str (if (= 0 (length (car components)))
                                "/"
                              (string (elt (car components) 0) ?/)))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (reduce (lambda (a b) (concat a "/" b)) components))))

(defun shk-eshell-prompt ()
  (let ((header-bg "#fff"))
    (concat
     (with-face user-login-name :foreground "blue")
     "@"
     (with-face "localhost" :foreground "green")
     (with-face (concat ":" (shortened-path  (eshell/pwd) 40)) :foreground "#689")
     ;; (with-face (format-time-string "(%Y-%m-%d %H:%M) " (current-time)) :background header-bg :foreground "#888") 
     (with-face
      (or (ignore-errors (format "(%s)" (vc-responsible-backend default-directory))) "")
      :foreground header-bg)
     ;; (with-face "\n" :background header-bg) 
     (if (= (user-uid) 0)
         (with-face " #" :foreground "red")
       " $")
     " ")))
(setq eshell-prompt-function 'shk-eshell-prompt)
(setq eshell-highlight-prompt nil)


