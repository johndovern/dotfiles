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
  (hydra-keyboard-quit)
  (let ((target-session (concat "\*" (dap--debug-session-name arg) "[^*]+-\scppdbg:.*\*")))
    (execute-if-confirmed #'kill-selected-buffer target-session "Kill output?")
    (message target-session))
  (message (dap--debug-session-name arg)))

(defun run-shell-command-split-window (&optional command)
  "Run a shell command in a horizontal split window.
   The user can interact with the command and the output is
   visible in the split window. Delete the split window after
   the command is finished if the user responds with y.
   If COMMAND is provided, use it as the shell command to run."
  (interactive)
  (unless command
    (setq command (read-shell-command "Shell command: ")))
  (let* ((split-buffer (split-window-below))
         (output-buffer (get-buffer-create "*Shell Command Output*")))
    (set-window-buffer split-buffer output-buffer)
    (async-shell-command command output-buffer)
    (with-selected-window split-buffer
      (setq-local quit-window-timer
                  (run-with-timer 0.1 nil
                                  (lambda ()
                                    (when (y-or-n-p "Delete output window?")
                                      (delete-window split-buffer))))))))

;; Get the current Unix time
(defun my-get-current-unix-time ()
  "Return the current Unix time."
  (car (time-convert (car (current-time)) (cadr (current-time)))))

(defun my-open-vterm ()
  "Open a unique vterm window and resize it to take up 35% of the bottom of the screen."
  (interactive)
  (setq vterm-buffer-name (format "*vterm<%s>*" (my-get-current-unix-time)))
  (let* ((height (round (* 0.35 (window-total-height))))
         (my-vterm-buffer (get-buffer-create vterm-buffer-name)))
    (with-current-buffer my-vterm-buffer
      (vterm)
      (fit-window-to-buffer nil nil height)
      (set-process-sentinel vterm--process
                            (lambda (process event)
                              (let ((window (get-buffer-window my-vterm-buffer)))
                                (when window
                                  (delete-window window))))))))

(defun org-add-id-link ()
  "Convert the word under the cursor to a user-defined reference."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (word (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (if (not (string= word ""))
        (progn
          (delete-region (car bounds) (cdr bounds))
          (insert (format "[[#%s][%s]]" word word)))
      (message "Using \"%s\" as reference" word))))

(defun org-add-id-link-desc ()
  "Convert the word under the cursor to a user-defined reference."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (word (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (user-ref (read-string (format "Enter a reference for \"%s\": " word) nil nil word)))
    (if (not (string= user-ref ""))
        (progn
          (delete-region (car bounds) (cdr bounds))
          (insert (format "[[#%s][%s]]" user-ref word)))
      (message "Using \"%s\" as reference" word))))

(defun org-add-header-link ()
  "Convert the word under the cursor to a user-defined reference."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (word (buffer-substring-no-properties (car bounds) (cdr bounds))))
    (if (not (string= word ""))
        (progn
          (delete-region (car bounds) (cdr bounds))
          (insert (format "[[%s][%s]]" word word)))
      (message "Using \"%s\" as reference" word))))

(defun org-add-header-link-desc ()
  "Convert the word under the cursor to a user-defined reference."
  (interactive)
  (let* ((bounds (bounds-of-thing-at-point 'word))
         (word (buffer-substring-no-properties (car bounds) (cdr bounds)))
         (user-ref (read-string (format "Enter a reference for \"%s\": " word) nil nil word)))
    (if (not (string= user-ref ""))
        (progn
          (delete-region (car bounds) (cdr bounds))
          (insert (format "[[%s][%s]]" user-ref word)))
      (message "Using \"%s\" as reference" word))))

(defun org-insert-link-from-clipboard ()
  "Replace the word at point with a link with the URL from the clipboard in an
   Org mode document.  If there is no word at point, prompt the user to enter a
   word."
  (interactive)
  (let ((url (gui-backend-get-selection 'CLIPBOARD 'TEXT)))
    (when url
      (let* ((bounds (bounds-of-thing-at-point 'word))
             (word (if bounds
                       (buffer-substring-no-properties (car bounds) (cdr bounds))
                     (read-string "Enter word: "))))
        (when bounds
          (delete-region (car bounds) (cdr bounds)))
        (org-insert-link nil url word)))))

(defun run-command-in-vterm (base-command)
  "Open a new vterm buffer and run a command with optional arguments.
   If BASE-COMMAND is not specified, prompt the user for a command.
   The command is run in the current directory of the file open in the
   current buffer, or $HOME if no directory is found."
  (interactive (list (read-string "Command: ")))
  (let ((default-directory (or (ignore-errors (file-name-directory buffer-file-name))
                               (getenv "HOME"))))
    (vterm-other-window)
    (vterm-send-string (concat base-command " " (read-string "Args: ") "\n"))))

;; (defun org-insert-link-from-clipboard ()
;;   "Replace the word at point with a link with the URL from the clipboard in an Org mode document."
;;   (interactive)
;;   (let ((url (gui-backend-get-selection 'CLIPBOARD 'TEXT)))
;;     (when url
;;       (let* ((bounds (bounds-of-thing-at-point 'word))
;;              (word (buffer-substring-no-properties (car bounds) (cdr bounds))))
;;         (delete-region (car bounds) (cdr bounds))
;;         (org-insert-link nil url word)))))

;; (defun org-insert-link-from-clipboard ()
;;   "Insert a link with the URL from the clipboard at point in an Org mode document."
;;   (interactive)
;;   (let ((url (gui-backend-get-selection 'CLIPBOARD 'TEXT)))
;;     (when url
;;       (org-insert-link nil url (thing-at-point 'word)))))

;; Does not work
(defun my/org-insert-last-link (description)
  "Insert the last stored link and prompt for a DESCRIPTION."
  (interactive "sDescription: ")
  (let* ((link-info (car org-stored-links))
         (link (org-link-make-string (nth 1 link-info))))
    (insert (format "[[%s][%s]]" link description))))



;; (defun my-open-vterm ()
;;   "Open a unique vterm window and resize it to take up 35% of the bottom of the screen."
;;   (interactive)
;;   (let ((vterm-buffer-name (format "*vterm<%s>*" (my-get-current-unix-time))))
;;     (switch-to-buffer (get-buffer-create vterm-buffer-name))
;;     (vterm)
;;     (fit-window-to-buffer nil nil (round (* 0.35 (window-total-height))))
;;     (let ((vterm-buffer (current-buffer)))
;;       (set-process-sentinel (get-buffer-process vterm-buffer)
;;                             (lambda (process event)
;;                               (let ((window (get-buffer-window vterm-buffer)))
;;                                 (when window
;;                                   (delete-window window))))))))

;; (defun open-vterm ()
;;   "Open a vterm window that takes up 35% of the screen, opens it below the current window, and kills the window when vterm is exited."
;;   (interactive)
;;   (let* ((vterm-buffer (generate-new-buffer "*vterm*"))
;;          (vterm-window-height (round (* 0.35 (window-height))))
;;          (vterm-window (split-window-below vterm-window-height)))
;;     (with-selected-window vterm-window
;;       (set-window-buffer vterm-window vterm-buffer)
;;       (vterm)))
;;   (set-process-sentinel
;;    (get-buffer-process (current-buffer))
;;    (lambda (process event)
;;      (when (string= event "finished\n")
;;        (kill-buffer-and-window)))))
