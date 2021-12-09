# LexicalSummary

A LexicalSummary object holds the match results and provides access to the data via properties.

![Image of a Lexical Summary object](./assets/images/LexicalSummary.png)

Properties:
* Data — Return results as a list of associations
* Dataset — Return results as a Dataset
* TotalMatchCount — The total number of matches found
* Counts — A Dataset of counts for each match
* CountGroups — Group matches by their count
* CountGroupPercentages — A Dataset of count groups and their percentage of occurrence
* LowercaseCountGroupPercentages — CountGroupPercentages with rows merged by lowercasing all matches
* PartOfSpeechGroups — A Dataset of unique words from matches grouped by their part-of-speech classes
* WordStemCountGroups — A Dataset of word stems from unique words in matches grouped by their count
* Source — A string indicating the source of the matches ("Text", "Wikipedia", etc...)
* LexicalPatternStructure — Visualize a LexicalPattern via TextElements
* Survey — A Dashboard showing the PercentDataset, PartOfSpeechGroups, and WordStemCountGroups

Use `CountSummaryLowercase` to lowercase the `Counts` or `CountGroups` datasets in post.
