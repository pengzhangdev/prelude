;;(require 'cc-mode)

;;;; CodeStyle : C/CPP

(require 'google-c-style)

(defun how-many-region (begin end regexp &optional interactive)
  "Print number of non-trivial matches for REGEXP in region.
Non-interactive arguments are Begin End Regexp"
  (interactive "r\nsHow many matches for (regexp): \np")
  (let ((count 0) opoint)
    (save-excursion
      (setq end (or end (point-max)))
      (goto-char (or begin (point)))
      (while (and (< (setq opoint (point)) end)
                  (re-search-forward regexp end t))
        (if (= opoint (point))
            (forward-char 1)
          (setq count (1+ count))))
      (if interactive (message "%d occurrences" count))
      count)))

(defun infer-indentation-style ()
  ;; if our source file uses tabs, we use tabs, if spaces spaces, and if
  ;; neither, we use the current indent-tabs-mode
  (let ((space-count (how-many-region (point-min) (point-max) "^  "))
        (tab-count (how-many-region (point-min) (point-max) "^\t")))
    (if (> space-count tab-count) (setq indent-tabs-mode nil))
    (if (> tab-count space-count) (setq indent-tabs-mode t))))

(defun my-build-tab-stop-list (width)
  (let ((num-tab-stops (/ 80 width))
        (counter 1)
        (ls nil))
    (while (<= counter num-tab-stops)
      (setq ls (cons (* width counter) ls))
      (setq counter (1+ counter)))
    (set (make-local-variable 'tab-stop-list) (nreverse ls))))

(defun my-c-mode-common-hook ()
  (google-set-c-style)
  (c-set-style "google")
  (setq tab-width 4)
  ;; (my-build-tab-stop-list tab-width)
  (setq c-basic-offset tab-width)
  ;; (setq indent-tabs-mode nil) ;; force only spaces for indentation
  (infer-indentation-style)
  (c-set-offset 'substatement-open 0)
  ;;  (flymake-mode)
  ;;  (c-set-offset 'arglist-intro c-lineup-arglist-intro-after-paren)
  )
;; google sytle is defined in above function
;;(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c-mode-common-hook
          (function (lambda ()
                      (google-set-c-style)
                      (infer-indentation-style)
                      (setq tab-width 4)
                      (setq c-basic-offset tab-width))))
(add-hook 'c++-mode-common-hook
          (function (lambda ()
                      (google-set-c-style)
                      (infer-indentation-style)
                      (setq tab-width 4)
                      (setq c-basic-offset tab-width))))

;; (add-hook 'c-mode-common-hook
;;           (function (lambda () (flymake-mode))))
;; (add-hook 'c++-mode-common-hook
;;           (function (lambda () (flymake-mode))))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match ".*work.*"
                                       filename))
                (c-set-style "my-c-mode-common-hook")))))

(add-hook 'c++-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match ".*work.*"
                                       filename))
                (c-set-style "my-c-mode-common-hook")))))
;;(add-hook 'c++-mode-common-hook 'my-c-mode-common-hook)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)


;;; cpplint
(defun cpplint ()
  "check source code format according to Google Style Guide"
  (interactive)
  (compilation-start (concat "python ~/bin/cpplint.py " (buffer-file-name))))

;;;; cppcheck
(defun cppcheck ()
  "check source code foramt according to cppcheck"
  (interactive)
  (compilation-start (concat "cppcheck --enable=all " (buffer-file-name))))

;;;; CodeStyle : COMMON LISP

(defun my-lisp-mode-common-hook ()
  "lisp mode common hook abount the code style and config for plugins"
  (setq inferior-lisp-program "/usr/bin/sbcl")
  (require 'slime-autoloads)
  (setq slime-contribs '(slime-fancy))
  (require 'ac-slime)
  ;;  (add-hook 'slime-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-mode-hook
            #'(lambda ()
                (set-up-slime-ac)
                (autopair-mode)))
  ;; (add-hook 'slime-repl-mode-hook 'set-up-slime-ac)
  (add-hook 'slime-repl-mode-hook
            #'(lambda ()
                (set-up-slime-ac)
                (autopair-mode)))
  ;;  (slime-mode)
  ;;  (autopair-mode)
  ;;(auto-complete-mode)
  (eval-after-load "auto-complete"
    '(add-to-list 'ac-modes 'slime-repl-mode))
  )

(add-hook 'lisp-mode-hook 'my-lisp-mode-common-hook)
