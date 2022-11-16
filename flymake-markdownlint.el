;;; flymake-markdownlint.el --- A flymake plugin for markdown files using markdownlint-cli2

;; Copyright © 2022 Erick Navarro
;; Author: Erick Navarro <erick@navarro.io>
;; URL: https://github.com/erickgnavar/flymake-markdownlint
;; Version: 0.1.0
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "27.1"))

;;; Commentary:

;; Usage:
;;   (require 'flymake-markdownlint)
;;   (add-hook 'markdown-mode-hook #'flymake-markdownlint-load)

;;; Code:

(defcustom flymake-markdownlint-program "markdown-cli2"
  "Path to program markdownlint."
  :group 'flymake-markdownlint
  :type 'string)

;;;###autoload
(defun flymake-markdownlint-load ()
  "Load hook for the current buffer to tell flymake to run checker."
  (interactive)
  (add-hook 'flymake-diagnostic-functions #'flymake-markdownlint--run-checker nil t))

(defun flymake-markdownlint--run-checker (report-fn &rest _args)
  "Run checker using REPORT-FN."
  (funcall report-fn (flymake-markdownlint--check-buffer)))

(defconst flymake-markdown--regex
  "^\\(.*\\):\\([0-9]+\\) \\([a-zA-Z0-9]+\\)\/\\([a-zA-Z0-9-\/]+\\) \\(.*\\)"
  "Regex to match elements in markdownlint-cli2 output.")

(defun flymake-markdownlint--check-buffer ()
  "Generate a list of diagnostics for the current buffer."
  (let* ((buffer (current-buffer))
         (temp-file (flymake-markdownlint--write-to-temp-file))
         (result (shell-command-to-string (format "%s %s" flymake-markdownlint-program temp-file)))
         ;;TODO: add support for buffer with not saved files
         (dxs '()))
    (with-temp-buffer
      (save-excursion
        (insert result)
        (goto-char (point-min))
        (while (search-forward-regexp flymake-markdown--regex (point-max) t)
          (when (match-string 1)
            (let* ((line (buffer-substring (match-beginning 2) (match-end 2)))
                   (error-point (flymake-markdownlint--get-position (string-to-number line) buffer (current-buffer)))
                   (description (buffer-substring (match-beginning 5) (match-end 5)))
                   (dx (flymake-make-diagnostic buffer error-point (1+ error-point) :error description)))
              (add-to-list 'dxs dx))))))
    dxs))

(defun flymake-markdownlint--write-to-temp-file ()
  "Create a temp file and write buffer's content on it."
  (let ((temp-file (make-temp-file "markdownlint")))
    (write-region (point-min) (point-max) temp-file nil 'quiet)
    temp-file))

(defun flymake-markdownlint--get-position (line content-buffer return-buffer)
  "Calculate position for the given LINE.
To accomplish this we need to switch to CONTENT-BUFFER and then
after position is calculated we have to return to RETURN-BUFFER."
  ;;TODO: find a better way to calculate position without switching buffers.
  (let ((temp-point nil))
    (switch-to-buffer content-buffer)
    (goto-char (point-min))
    (forward-line (1- line))
    (setq temp-point (point))
    (switch-to-buffer return-buffer)
    temp-point))

(provide 'flymake-markdownlint)
;;; flymake-markdownlint.el ends here
