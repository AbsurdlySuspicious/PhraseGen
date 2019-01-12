# PhraseGen

Pattern-based random phrase generation utility using WordNet dictionaries

It can be used in set of purposes such as name or password generation, inspiration for your new username or anything else you can think of

When using for password generation, make sure you will __not__ use generated phrases as-is. Consider adding some numbers and case-variety to make it more strong before brute-force attacks

## Dependencies

+ Java JRE 8
+ sbt (build)

## Packages and builds

+ Prebuilt jars can be found in releases tab
+ Archlinux __AUR__ packages:
  + release (installs jar): https://aur.archlinux.org/packages/phgen/

## Build

Get sbt (Scala Build Tool) and run `sbt assembly` in project directory. Resulting jar will be in `target/scala-2.12/phgen.jar`. You can run it using `phgen` executable from the repo

## Instructions

Options manual can be obtained with `--help`. It also supports interactive mode with flag `-I`

__Pattern description__ can be found [here](docs/pattern_syntax.md)

__Help pages__ (`--help` and interactive `:help`) also duplicated [here](docs/help.md)

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

---

This project is based on WordNet and contains an embedded copy of WordNet 3.1 database licensed under [WordNet License](https://wordnet.princeton.edu/license-and-commercial-use). [WordNet](https://wordnet.princeton.edu/). Princeton University. 2010.

PhraseGen © AbsurdlySuspicious. Licensed under Apache License 2.0
