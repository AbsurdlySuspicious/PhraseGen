# PhraseGen

Pattern-based random phrase generation utility using WordNet dictionaries

It can be used in set of purposes such as name or password generation, inspiration for your new username or anything else you can think of

When using for password generation, make sure you will __not__ use generated phrases as-is. Add some numbers and case-variety to make it more strong before brute-force attacks

## Instructions

Options manual can be obtained with `--help`. It also supports interactive mode with flag `-I`

Pattern description can be found [here](docs/pattern_syntax.md)

Help pages (`--help` and interactive `:help`) also duplicated [here](docs/help.md)

## Examples of generated phrases

__Pattern__: `[adj] [rw noun] of [sep(-) noun]`
```
monochrome Vries of rift-valley
controlled tortilla of antifouling-paint
crisscross coif of Stenotomus-chrysops
medicinal amniote of element-114
occupational memory of denim
```

__Pattern__: `[adverb] [verb]ing [sep(-) noun]`
```
heroically betraying British-Commonwealth
pitty-pat summering Paliurus
foremost cementing Section-Eight
spotlessly waging sacred-mushroom
squarely spell outing smoky-quartz
```

__Pattern__: `[rw adj][rw noun]`
```
28chlamydia
primaAustralian
scornfulMenorah
teensy-weensyFungia
fullyChurch
```
