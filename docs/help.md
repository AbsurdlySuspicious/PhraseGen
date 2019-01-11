# Help pages

## Options help

```
PhraseGen 1.0
Usage: phgen [options] [PATTERNS]

  PATTERNS                 Patterns that will be used for generation
  -I, --interactive        Interactive mode (passed patterns will be ignored)
  -d, --dict <value>       Path to WordNet dictionary file (by default embedded wn3.1 will be used)
  -c, --count <value>      Count of names to generate (default 10)
  -y, --syncount <value>   Count of random synonyms for each phrase (default 1)
  -m, --pattern-mode <value>
                           Pattern selection mode: round, random (default: random)
  -s, --show-senses        Show WordNet (alternative) senses for each used word/synset
  --help                   Show this help
```

## Interactive help

```
Usage: PATTERN or command

Commands:
:exit - exit interactive mode
:count, :syncount - show or change corresponding counts
:senses - toggle showing senses
```
