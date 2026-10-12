# sed-ts-mode

[![CI](https://github.com/konomanoasa/sed-ts-mode/actions/workflows/ci.yaml/badge.svg)](https://github.com/konomanoasa/sed-ts-mode/actions/workflows/ci.yaml)

[Tree-sitter](https://tree-sitter.github.io/tree-sitter/)-based
[Emacs](https://www.gnu.org/software/emacs/) major mode for
GNU sed 4.10 and POSIX.1-2024 sed.

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

## Parsing Conditions

Set parsing conditions for matching files in `.editorconfig`.

```ini
[*.sed]
posix = false
ere = false
```

Both properties accept `true` and `false`.
`posix` selects POSIX syntax and `ere` selects ERE.
Both properties default to `false`, selecting GNU BRE.
Restart the mode to apply changes.

## Grammar

[tree-sitter-gsed](https://github.com/konomanoasa/tree-sitter-gsed) and
[tree-sitter-sed](https://github.com/konomanoasa/tree-sitter-sed)

## License

[MIT](LICENSE)
