;;; key binding and personal config for code jump such as cscope, gtags and so on.
;;; Code:

(defvar cj-buffer-stack nil
  "Stack for tag browsing.")

(defvar cj-point-stack nil
  "Stack for tag browsing")

(defvar cj-jump-sequence
  '(gtags-find-symbol
    cscope-find-this-symbol))

(defun build-code-jump-fun ()
  "build function with current context info"
  (let ((context (cj-pop-context-del nil)))
    (defun cj-do-code-jump (seq)
      "Call code jump functions"
      (if (null seq)
          (message "Done code jump")
        (progn
          (funcall (car seq))
          (when (and (equal (current-buffer) (nth 0 context))
                     (equal (point) (nth 1 context)))
            (cj-do-code-jump (cdr seq))))))))

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
    (list buffer point)))

(defun cj-restore-context ()
  "restore context before code jump"
  (interactive)
  (let (context)
    (setq context (cj-pop-context-del t))
    (if (not context)
        (messge "[code-jump] The tags stack is empty")
      (progn
        (switch-to-buffer (nth 0 context))
        (goto-char (nth 1 context))))))

(defun cj-find-symbol ()
  "find symbols"
  (interactive)
  (cj-push-context)
  (funcall (build-code-jump-fun)
           cj-jump-sequence))



;;; code-jump.el ends here
