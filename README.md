# sed-ts-mode

[![CI](https://github.com/konomanoasa/sed-ts-mode/actions/workflows/ci.yaml/badge.svg)](https://github.com/konomanoasa/sed-ts-mode/actions/workflows/ci.yaml)

[Tree-sitter](https://tree-sitter.github.io/tree-sitter/)-based
[Emacs](https://www.gnu.org/software/emacs/) major mode for
POSIX.1-2024 sed.

## Requirement

Emacs 31.1 or later.

## Installation

```elisp
(package-vc-install "https://github.com/konomanoasa/sed-ts-mode")
```

## Automatic Activation

Enabled for `.sed` files and scripts with a `sed` shebang.

## Features

- Comment Commands
- Electric Pair
- Font Lock
- Imenu
- Indentation
- Navigation
- Syntax Table

## Font Lock

Supports `treesit-font-lock-level`.

| Level | Font Lock                                                   |
| ----- | ----------------------------------------------------------- |
| 1     | Comments                                                    |
| 2     | Commands, labels, and strings                               |
| 3     | Numbers, constants, and escapes outside regular expressions |
| 4     | Operators, punctuation, brackets, and regular expressions   |

## Regexp Syntax

Select ERE for matching files in `.editorconfig`.

```ini
[*.sed]
regex_dialect = ere
```

BRE is the default. Only `bre` and `ere` are accepted.
Restart the mode to apply changes.

## Grammar

[tree-sitter-sed](https://github.com/konomanoasa/tree-sitter-sed)

## License

[MIT](LICENSE)
