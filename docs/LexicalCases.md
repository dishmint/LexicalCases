# LexicalCases

## Overview

LexicalCases takes a [String](https://reference.wolfram.com/language/ref/String.html), [File](https://reference.wolfram.com/language/ref/File.html) or [WikipediaSearch](https://reference.wolfram.com/language/ref/WikipediaSearch.html) query as its first argument, and a [lexical pattern](./LexicalPattern.md) as its second argument. The [WikipediaSearch](https://reference.wolfram.com/language/ref/WikipediaSearch.html) query specifications that make sense for LexicalCases are the `"Content"` and `"Category"` rules. Each return a list of article names, the former returns articles containing a set of keywords, and the latter returns articles whose title contains keywords.

## Syntax

#### Search over strings
```Mathematica
LexicalCases["The best key lime pie is still up for debate.", TextType["Adjective"]~~"key lime pie"]
```

---
#### Search Wikipedia articles
```Mathematica
LexicalCases[
	"Content" -> "music",
	TextType["Adjective"]~~"music"~~TextType["Preposition"]~~TextType["Country"]
	];
```

Note that TextType gives you " " and WordBoundary for "free".

#### Search Wikipedia by converting LexicalPattern to a search query
```Mathematica
LexicalCases[LexicalPattern[TextType["ProperNoun" | "Adjective"], "music"]]
```

## Options

* `"Service"` — Select which service to search over. Run `$LexicalCasesSupportedServices` for supported services.
* `"MaxItems"`— Set the number of articles to search over (50 by default).
* `"StringTrim"`— True will trim whitespace from the beginning and end of matches (True by default).
