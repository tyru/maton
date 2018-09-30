# maton

Convert ERE and Vim regexp bidirectionally.

# NOTE

* Tested only in SWI-Prolog
* Very slow!

# Example

## Conversion between ERE and Vim pattern

```
swipl main.pl -v -f ere -t vim {ERE pattern}
swipl main.pl -v -f vim -t ere {Vim pattern}
```

## Dump node

```
swipl main.pl -v -f ere -t node {ERE pattern}
```
