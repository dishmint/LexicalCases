# LexicalCases

## Syntax

#### Search over strings
```Mathematica
LexicalCases[<string>, LexicalPattern[...]]
```

---
#### Search Wikipedia articles
```Mathematica
LexicalCases["Content" -> "music", LexicalPattern[TextType["ProperNoun" | "Adjective"], " ", "music"], MaxItems -> 1000]
```

#### Search Wikipedia by converting LexicalPattern to a search query
```Mathematica
LexicalCases[LexicalPattern[TextType["ProperNoun" | "Adjective"], " ", "music"]]
```

## Options

* `"Service"` — Select which service to search over. Run `$LexicalCasesSupportedServices` for supported services.
* `"MaxItems"`— Limit the number of articles to search over (the default is 50).
