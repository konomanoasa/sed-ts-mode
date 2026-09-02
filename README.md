# sed-ts-mode

[![CI](https://github.com/konomanoasa/sed-ts-mode/actions/workflows/ci.yml/badge.svg)](https://github.com/konomanoasa/sed-ts-mode/actions/workflows/ci.yml)

[Tree-sitter](https://tree-sitter.github.io/tree-sitter/)-based
[Emacs](https://www.gnu.org/software/emacs/) major mode for POSIX.1-2024 `sed`.

## Requirements

- Emacs 31.1 or later

## Installation

```elisp
(package-vc-install "https://github.com/konomanoasa/sed-ts-mode")
```

## Automatic Activation

Enabled for `.sed` files and scripts with a `sed` shebang.

## Features

- Comment Commands
- Font Lock
- Imenu: labels
- Indentation
- Syntax Table

## Font Lock

Supports `treesit-font-lock-level`.

| Level | Font Lock |
| --- | --- |
| 1 | Comments |
| 2 | Commands, labels, and strings |
| 3 | Numbers, constants, and escapes outside regular expressions |
| 4 | Operators, punctuation, brackets, and regular expressions |

## Regexp Syntax

BRE is the default. Set ERE as follows.

```elisp
(setopt sed-ts-mode-regexp-syntax 'ere)
```

Use `M-x sed-ts-toggle-regexp` to switch the current buffer.

## Grammar

[tree-sitter-sed](https://github.com/konomanoasa/tree-sitter-sed)

## License

[MIT](LICENSE)
