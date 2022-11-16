# flymake-markdownlint

Flymake plugin to run a linter for markdown buffers using [markdownlint-cli2](https://github.com/DavidAnson/markdownlint-cli2)

## Installation

### Cloning the repo

Clone this repo somewhere, and add this to your config:

```elisp
(add-to-list 'load-path "path where the repo was cloned")

(require 'flymake-markdownlint)
(add-hook 'markdown-mode-hook #'flymake-markdownlint-load)
```

### Using use-package

```emacs-lisp
(use-package flymake-markdownlint
  :ensure t)
```

### Using straight.el

```emacs-lisp
(use-package flymake-markdownlint
  :straight (flymake-markdownlint
             :type git
             :host github
             :repo "erickgnavar/flymake-markdownlint"))
```
