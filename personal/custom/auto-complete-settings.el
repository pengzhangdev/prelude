;; -*- Emacs-Lisp -*-

;; Time-stamp: <2010-12-03 16:55:07 Friday by taoshanwen>

(require 'auto-complete-config)
(require 'auto-complete+)


;; After do this, isearch any string, M-: (match-data) always
;; return the list whose elements is integer
(global-auto-complete-mode 1)

;; 不让回车的时候执行`ac-complete', 因为当你输入完一个
;; 单词的时候, 很有可能补全菜单还在, 这时候你要回车的话,
;; 必须要干掉补全菜单, 很麻烦, 用M-j来执行`ac-complete'

(defun auto-complete-settings ()
  "Settings for `auto-complete'."
  (setq help-xref-following nil)
  
  (add-to-list 'ac-dictionary-directories "/usr/local/share/emacs/23.3/lisp/ac-dict")

  (setq ac-auto-show-menu t
        ac-auto-start t
        ac-dwim t
        ac-candidate-limit ac-menu-height
        ac-quick-help-delay .5
        ac-disable-faces nil)

  (set-default 'ac-sources
               '(ac-source-semantic-raw
                 ac-source-yasnippet
                 ac-source-dictionary
                 ac-source-abbrev
                 ac-source-words-in-buffer
                 ac-source-words-in-same-mode-buffers
                 ac-source-imenu
                 ac-source-files-in-current-dir
                 ac-source-filename))
  (setq ac-modes ac+-modes)

  (defun ac-start-use-sources (sources)
    (interactive)
    (let ((ac-sources sources))
      (call-interactively 'ac-start))))

(eval-after-load "auto-complete"
  '(auto-complete-settings))

(eval-after-load "cc-mode"
  '(ac-settings-4-cc))

(defun ac-settings-4-cc ()
  "`auto-complete' settings for `cc-mode'."
     (dolist (command `(c-electric-backspace
                        c-electric-backspace-kill))
       (add-to-list 'ac-trigger-commands-on-completing command)))

(defun ac-settings-4-autopair ()
  "`auto-complete' settings for `autopair'."
  (defun ac-trigger-command-p (command)
    "Return non-nil if `this-command' is a trigger command."
    (or
     (and
      (symbolp command)
      (or (memq command ac-trigger-commands)
          (string-match "self-insert-command" (symbol-name command))
          (string-match "electric" (symbol-name command))
          (let* ((autopair-emulation-alist nil)
                 (key (this-single-command-keys))
                 (beyond-autopair (or (key-binding key)
                                      (and (setq key (lookup-key local-function-key-map key))
                                           (key-binding key)))))
            (or
             (memq beyond-autopair ac-trigger-commands)
             (and ac-completing
                  (memq beyond-autopair ac-trigger-commands-on-completing)))))))))
      
(eval-after-load "autopair"
  '(ac-settings-4-autopair))

(defun ac-settings-4-lisp ()
  "Auto complete settings for lisp mode."
  (setq ac-omni-completion-sources '(("\\<featurep\s+'" ac+-source-elisp-features)
                                     ("\\<require\s+'"  ac+-source-elisp-features)
                                     ("\\<load\s+\""    ac-source-emacs-lisp-features)))
  (ac+-apply-source-elisp-faces)
  (setq ac-sources
        '(ac-source-features
          ac-source-functions
          ac-source-yasnippet
          ac-source-variables
          ac-source-symbols
          ac-source-dictionary
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-files-in-current-dir
          ac-source-filename
          ac-source-words-in-same-mode-buffers)))

(defun ac-settings-4-java ()
  (setq ac-omni-completion-sources (list (cons "\\." '(ac-source-semantic))
                                         (cons "->" '(ac-source-semantic))))
  (setq ac-sources
        '(ac-source-semantic
          ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-same-mode-buffers
          ac-source-files-in-current-dir
          ac-source-filename)))

(defun ac-settings-4-c ()
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-dictionary
          ac-source-semantic
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-same-mode-buffers
          ac-source-files-in-current-dir
          ac-source-filename)))

(defun ac-settings-4-cpp ()
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-dictionary
          ac-source-semantic
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-same-mode-buffers
          ac-source-files-in-current-dir
          ac-source-filename)))

(defun ac-settings-4-text ()
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-same-mode-buffers
          ac-source-imenu)))

(defun ac-settings-4-eshell ()
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-same-mode-buffers
          ac-source-files-in-current-dir
          ac-source-filename
          ac-source-symbols
          ac-source-imenu)))

(defun ac-settings-4-ruby ()
  (require 'rcodetools-settings)
  (setq ac-omni-completion-sources
        (list (cons "\\." '(ac-source-rcodetools))
              (cons "::" '(ac-source-rcodetools)))))

(defun ac-settings-4-html ()
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-same-mode-buffers
          ac-source-files-in-current-dir
          ac-source-filename)))

(defun ac-settings-4-tcl ()
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-same-mode-buffers
          ac-source-files-in-current-dir
          ac-source-filename)))

(defun ac-settings-4-awk ()
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-same-mode-buffers
          ac-source-files-in-current-dir
          ac-source-filename)))

(defun am-add-hooks (hooks function &optional append local)
  "Call `add-hook' on hook list HOOKS use arguments FUNCTION, APPEND, LOCAL.

HOOKS can be one list or just a hook."
  (if (listp hooks)
      (mapc
       `(lambda (hook)
          (add-hook hook ',function append local))
       hooks)
    (add-hook hooks function append local)))

(am-add-hooks
 `(lisp-mode-hook emacs-lisp-mode-hook lisp-interaction-mode-hook
                  svn-log-edit-mode-hook change-log-mode-hook)
 'ac-settings-4-lisp)


(defun apply-args-to-fun (fun args)
  "Apply args to function FUN."
  (if (listp args)
      (eval `(,fun ,@args))
    (eval `(,fun ,args))))

(defun eal-get-name-by-loadfile (file pos)
  "Get `symbol-name' by load file FILE and position POS."
  (concat file "-" (if (= pos 1) "mode" "mode-map")))

(defun am-intern (&rest strings)
  "`intern' use STRINGS."
  (intern
   (apply
    'concat
    (mapcar
     (lambda (element)
       (if (stringp element) element (symbol-name element)))
     strings))))

(defun eal-eval-by-modes (modes fun)
  "Run `eval-after-load' on function FUN by MODES.

FUN will be called by `eval' with argument mode of MODES.
Example:
\(eal-eval-by-modes
 ac-modes
 (lambda (mode)
   (let ((mode-name (symbol-name mode)))
     (when (and (intern-soft mode-name) (intern-soft (concat mode-name \"-map\")))
       (define-key (symbol-value (am-intern mode-name \"-map\")) (kbd \"C-c a\") 'ac-start)))))"
  (if (listp modes)
      (eal-eval-by-symbols modes 1 fun)
    (eal-eval-by-symbol modes 1 fun)))

(defcustom eal-loadfile-mode-maps
  `(("cc-mode"         nil                    c-mode-base-map)
    ("cc-mode"         c-mode                 c-mode-map)
    ("cc-mode"         c++-mode               c++-mode-map)
    ("cc-mode"         java-mode              java-mode-map)
    ("cc-mode"         awk-mode               awk-mode-map)
    "lisp-mode"
    ("lisp-mode"       emacs-lisp-mode        emacs-lisp-mode-map)
    "help-mode"
    ("man"             Man-mode               Man-mode-map)
    "log-view"
    ("compile"         compilation-mode       compilation-mode-map)
    ("gud")
    ("lisp-mode"       lisp-interaction-mode  lisp-interaction-mode-map)
    "browse-kill-ring"
    ("simple"          completion-list-mode   completion-list-mode-map)
    ("inf-ruby"        inferior-ruby-mode     inferior-ruby-mode-map)
    "ruby-mode"
    ("cus-edit"        custom-mode            custom-mode-map)
    ("info"            Info-mode              Info-mode-map)
    ("psvn"            svn-log-edit-mode      svn-log-edit-mode-map)
    ("psvn"            svn-status-mode        svn-status-mode-map)
    ("psvn"            svn-info-mode          svn-info-mode-map)
    ("package"         package-menu-mode      package-menu-mode-map)
    "dired"
    "apropos"
    "emaci"
    "cflow-mode"
    ("psvn"            svn-log-view-mode      svn-log-view-mode-map)
    ("vc-svn"          vc-svn-log-view-mode   vc-svn-log-view-mode-map)
    ("log-view"        log-view-mode          log-view-mode-map)
    "diff-mode"
    ("sgml-mode"       html-mode              html-mode-map)
    "sgml-mode"
    "w3m"
    ("data-debug"      data-debug-mode)
    ("debug"           debugger-mode          debugger-mode-map)
    "text-mode"
    "color-theme"
    "woman"
    "doxymacs"
    "grep"
    "view"
    ("hi-lock"         hi-lock-mode           hi-lock-map)
    "autoconf"
    "tcl"
    "sgml-mode"
    "image-mode"
    "shell"
    "sql"
    "rhtml-mode"
    "senator"
    "org"
    "org-agenda"
    "python"
    "groovy-mode"
    "nxml-mode"
    "perl-mode"
    "artist"
    "calendar"
    "outline"
    "google-maps-static"
    "flymake"
    ("speedbar"        speedbar-mode          speedbar-key-map)
    ("speedbar"        speedbar-mode          speedbar-file-key-map)
    ("yasnippet"       nil                    yas/keymap)
    ("yasnippet"       yas/minor-mode         yas/minor-mode-map)
    ("chart"           chart-mode             chart-map)
    ("recentf"         recentf-dialog-mode    recentf-dialog-mode-map)
    ("conf-mode"       conf-javaprop-mode     conf-javaprop-mode-map)
    ("conf-mode"       conf-space-mode        conf-space-mode-map)
    ("cua-base"        nil                    cua--rectangle-keymap)
    ("make-mode"       makefile-gmake-mode    makefile-gmake-mode-map)
    ("make-mode"       makefile-mode          makefile-mode-map)
    ("make-mode"       makefile-automake-mode makefile-automake-mode-map)
    ("sh-script"       sh-mode                sh-mode-map)
    ("auto-complete"   auto-complete-mode     ac-completing-map)
    ("auto-complete"   nil                    ac-mode-map)

    ("semantic-decoration-on-include" nil semantic-decoration-on-include-map)
    ("semantic-symref-list" semantic-symref-results-mode semantic-symref-results-mode-map))
  "*List used to find load file by mode or map.

Every element of list is or a list consisted by load file, mode and map,
or just one load file, or nil. If element is a list, and its last element is nil,
it will be ignored."
  :type 'alist
  :group 'eal)



(defun eal-find-loadfile-by-symbol (symbol pos)
  "Find load file by symbol SYMBOL, its position is POS."
  (let* ((symbol-name (symbol-name symbol))
         (first
          (find-if
           (lambda (pair)
             (if (stringp pair)
                 (if (string= symbol-name (eal-get-name-by-loadfile pair pos))
                     pair
                   (let ((file (and (string-match "^\\(.+\\)-mode$" pair)
                                    (match-string 1 pair))))
                     (if file
                         (if (string= symbol-name (eal-get-name-by-loadfile file pos))
                             pair))))
               (if pair
                   (if (eq (nth pos pair) symbol)
                       (car pair)))))
           eal-loadfile-mode-maps)))
    (if (listp first) (car first) first)))


(defun eal-eval-by-symbol (symbol pos fun)
  "Run `eval-after-load' on function FUN by SYMBOL."
  (let ((file (eal-find-loadfile-by-symbol symbol pos))
        (form `(,fun ',symbol)))
    (if file
        (eval-after-load file form)
      (eval form))))


(defun eal-eval-by-symbols (symbols pos fun)
  "Run `eval-after-load' on function FUN by SYMBOLS.

FUN will be call by `eval' with argument mode of SYMBOLS. "
  (mapc
   `(lambda (symbol)
      (eal-eval-by-symbol symbol ,pos ,fun))
   symbols))

(defun apply-args-list-to-fun (fun-list args-list)
  "Apply args list to function FUN-LIST.
FUN-LIST can be a symbol, also can be a list whose element is a symbol."
  (let ((is-list (and (listp fun-list) (not (functionp fun-list)))))
    (dolist (args args-list)
      (if is-list
          (dolist (fun fun-list)
            (apply-args-to-fun fun args))
        (apply-args-to-fun fun-list args)))))

(apply-args-list-to-fun
 (lambda (hook fun)
   (am-add-hooks hook fun))
 `(('java-mode-hook   'ac-settings-4-java)
   ('c-mode-hook      'ac-settings-4-c)
   ('c++-mode-hook    'ac-settings-4-cpp)
   ('text-mode-hook   'ac-settings-4-text)
   ('eshell-mode-hook 'ac-settings-4-eshell)
   ('ruby-mode-hook   'ac-settings-4-ruby)
   ('html-mode-hook   'ac-settings-4-html)
   ('awk-mode-hook    'ac-settings-4-awk)
   ('tcl-mode-hook    'ac-settings-4-tcl)))

(eal-eval-by-modes
 ac-modes
 (lambda (mode)
   (let ((mode-name (symbol-name mode)))
     (when (and (intern-soft mode-name) (intern-soft (concat mode-name "-map")))
       (define-key (symbol-value (am-intern mode-name "-map")) (kbd "C-c a") 'ac-start)))))

(provide 'auto-complete-settings)
