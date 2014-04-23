(defun filter-buffer-substring-add-line (func beg end delete)
  (concat
   (format "// line:%5d file: %s\n"
           (line-number-at-pos beg)
           (or (buffer-file-name) (buffer-name)))
   (funcall func beg end delete)
   (format "\n// line:%5d" (line-number-at-pos end))))

(defun yank-add-line-toggle ()
  (interactive)
  (if (memq 'filter-buffer-substring-add-line
            filter-buffer-substring-functions)
      (progn
        (setq filter-buffer-substring-functions
              (delq 'filter-buffer-substring-add-line
                    filter-buffer-substring-functions))
        (message "Add line is off!"))
    (push 'filter-buffer-substring-add-line
          filter-buffer-substring-functions)
    (message "Add line is on!")))
