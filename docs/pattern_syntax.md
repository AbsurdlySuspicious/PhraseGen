# Pattern syntax

Pattern consists of ordinary text and _tokens_ that will be replaced during generation
```
ordinary [token] text
```

Each token consists of two parts:

+ `wm` - Word mode _(optional)_
+ `ps` - Part of Speech

and looks like this: `[wm ps]`,
or this: `[ps]`

## Part of Speech

`ps` can be one of four supported PoS'es:

+ `verb`
+ `adverb`
+ `adj` or `adjective`
+ `noun`

By default, token will be replaced with random _word_ of selected PoS

`ps` can also be followed by parentheses that contains a query for word lookup, then token will be replaced with random lookup result

Token examples:
```
[verb]
[adj(query)]
```

## Word mode

_Word_ in terms of WordNet isn't just a single word, but possibly a group of words or a phrase. Word mode helps to handle situations, where you need exactly one word in place of token.

This is an example phrase, or a _WordNet word_ that will be used to provide examples in mode list below:

```
golden fairy lantern
```

So, the actual word modes (`wm`) list:

+ __First Word__ `[fw]`: `golden`
+ __Last Word__ `[lw]`: `lantern`
+ __Random Word__ `[rw]`: any of `golden`, `fairy` or `lantern`
+ __Separator__ `[sep(-)]`: `golden_fairy_lantern`

`fw` and `lw` selects first or last word of a phrase accordingly, `rw` just selects random word from a phrase, and `sep` replaces space with separator provided in parentheses followed by it

Token examples:
```
[rw noun]
[lw adverb]
[fw noun(grass)]
[sep(_) noun]
[sep(-) verb]
[sep(::) adj(ironic)]
```


