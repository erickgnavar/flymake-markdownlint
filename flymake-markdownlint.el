;;; flymake-markdownlint.el --- A flymake plugin for markdown files using markdownlint-cli

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

(defcustom flymake-markdownlint-program "markdownlint-cli"
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
  (let* ((code-buffer (current-buffer))
         (code-content (buffer-substring-no-properties (point-min) (point-max)))
         (args '("--stdin"))
         (dxs '()))
    (with-temp-buffer
      (insert code-content)
      (apply #'call-process-region (point-min) (point-max) flymake-markdownlint-program t t nil args)
      (goto-char (point-min))
      (while (search-forward-regexp flymake-markdown--regex (point-max) t)
        (when (match-string 1)
          (let* ((line (match-string 2))
                 (code (match-string 3))
                 (description (match-string 5))
                 (message (format "[%s] %s" code description))
                 (region (flymake-diag-region code-buffer (string-to-number line)))
                 (dx (flymake-make-diagnostic code-buffer (car region) (cdr region) :error message)))
            (add-to-list 'dxs dx)))))
    dxs))

(provide 'flymake-markdownlint)
;;; flymake-markdownlint.el ends here
