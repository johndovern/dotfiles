;;; funcs.el -*- lexical-binding: t; -*-

(defun choose-buffer (&optional pattern)
  "Interactively choose an open buffer that matches the given PATTERN
   (or any buffer if PATTERN is nil) and return its name. If there is
   only one buffer in the list, return its name automatically."
  (interactive)
  (let ((buffer-names (mapcar #'buffer-name (buffer-list))))
    (if pattern
        (let ((matching-buffers ()))
          (dolist (buffer-name buffer-names)
            (when (string-match-p pattern buffer-name)
              (push buffer-name matching-buffers)))
          (cond
           ((null matching-buffers)
            (message "No buffers matching \"%s\" found." pattern)
            nil)
           ((= (length matching-buffers) 1)
            (car matching-buffers))
           (t
            (completing-read "Choose buffer: " matching-buffers nil t))))
      (cond
       ((null buffer-names)
        (message "No buffers open.")
        nil)
       ((= (length buffer-names) 1)
        (car buffer-names))
       (t
        (completing-read "Choose buffer: " buffer-names nil t))))))

(defun kill-selected-buffer (&optional pattern)
  "Kill the buffer selected by the user.
  If PATTERN is provided, use it to filter the list of
  buffers displayed to the user."
  (interactive)
  (let ((buffer-to-kill (choose-buffer pattern)))
    (when buffer-to-kill
      (delete-windows-on buffer-to-kill)
      (message "Killed buffer %s" buffer-to-kill))))

(defun execute-if-confirmed (func &optional arg prompt)
  "Execute FUNCTION with optional ARG if user confirms the action
   with a 'y' response to PROMPT."
  (when (y-or-n-p prompt)
    (if arg
        (if (listp arg)
            (apply func arg)
          (funcall func arg))
      (funcall func))))

(defun debug-cleanup-output (arg)
  "Kill output buffer if confirmed"
  (interactive)
  (let ((target-session (concat "\*" (dap--debug-session-name arg) "[^*]+-\scppdbg:.*\*")))
        (execute-if-confirmed #'kill-selected-buffer target-session "Kill output?")
        (message target-session))
  (message (dap--debug-session-name arg)))
