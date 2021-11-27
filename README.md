# LexicalCases

Extract substrings matching a lexical pattern.

## Install
After cloning this repository or downloading the package files, use `Get` to load definitions
```Mathematica
Get["path/to/LexicalCases.wl"]
Get["path/to/LexicalCasesTests.wl"]
```

## Usage

[LexicalCases](https://dishmint.github.io/LexicalCases/LexicalCases.html) takes a string as its first argument, and a lexical pattern as its second argument. A [LexicalSummary](https://dishmint.github.io/LexicalCases/LexicalSummary.html) object is returned with properties to access and manipulate the results.

```Mathematica
LexicalCases["This is a cool string", LexicalPattern["This is a ", TextType["Adjective"], " string"]]
```

Visit the [documentation](https://dishmint.github.io/LexicalCases/) for additional examples.

