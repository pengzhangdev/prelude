;;; key binding and personal config for code jump such as cscope, gtags and so on.
;;; Code:

(defvar cj-buffer-stack nil
  "Stack for tag browsing.")

(defvar cj-point-stack nil
  "Stack for tag browsing")

(defvar cj-jump-sequence
  '(gtags-find-symbol
    cscope-find-global-definition))

(defun cj-do-code-jump (seq)
  "do code jump"
  (let ((context (cj-pop-context-del nil)))
    (if (null seq)
        (message "Done code jump!")
      (progn
        (funcall (car seq))
        (message "current-buffer %s context %s"
                 (current-buffer) (nth 0 context))
        ;;(message "pointer %s context %s"
        ;;       (point) (nth 1 context))
        (when (and (equal (current-buffer) (nth 0 context))
                   (equal (point) (nth 1 context)))
          ;;(message "failed")
          (cj-do-code-jump (cdr seq)))))))


;;; elisp does not support closure function,
;;; so context always is nil
;;; And the value in elisp is dynamic scoping defaultly,
;;; and it does not support lexical scoping
;; (defun build-code-jump-fun ()
;;   "build function with current context info"
;;   (let ((context (cj-pop-context-del nil)))
;;     `(lambda (seq)
;;       (if (null seq)
;;           (message "Done code jump")
;;         (progn
;;           (funcall (car seq))
;;           (when (and (equal (current-buffer) (nth 0 context))
;;                      (equal (point) (nth 1 context)))
;;             (cj-do-code-jump (cdr seq))))))))

(defun cj-push-context ()
  "push buffer and point into stack"
  (setq cj-buffer-stack (cons (current-buffer)
                              cj-buffer-stack))
  (setq cj-point-stack (cons (point)
                             cj-point-stack)))

(defun cj-pop-context-del (&optional delete)
  "pop buffer and point from stack"
  (let (buffer point)
    (setq buffer (car cj-buffer-stack))
    (setq point (car cj-point-stack))
    (when delete
      (setq cj-point-stack (cdr cj-point-stack))
      (setq cj-buffer-stack (cdr cj-buffer-stack)))
    (if (or (null buffer)
            (null point))
        nil
      (list buffer point))))

(defun cj-restore-context ()
  "restore context before code jump"
  (interactive)
  (let (context)
    (setq context (cj-pop-context-del t))
    (if (null context)
        (message "[code-jump] The tags stack is empty")
      (progn
        (switch-to-buffer (nth 0 context))
        (goto-char (nth 1 context))))))

(defun cj-find-symbol ()
  "find symbols"
  (interactive)
  (cj-push-context)
  (cj-do-code-jump cj-jump-sequence))

;;; code-jump.el ends here
