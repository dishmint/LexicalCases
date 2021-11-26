# LexicalCases

## Syntax

#### Search over strings
```Mathematica
LexicalCases[<string>, LexicalPattern[...]]
```

---
#### Search Wikipedia articles
```Mathematica
LexicalCases["Content" -> <keywords>, LexicalPattern[...]]
```

#### Search Wikipedia by converting LexicalPattern to a search query
```Mathematica
LexicalCases[LexicalPattern[...]]
```

Use the `MaxItems` option to limit the number of articles to search over (the default is 50).

## Options

* "Service" select which service to search over. Use `$LexicalCasesSupportedServices` to see which are available.
