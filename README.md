# maton

Convert ERE and Vim regexp bidirectionally.

## NOTE

* Tested only in SWI-Prolog
* Little bit slow

```
$ make
$ time ./maton -f ere -t vim '^(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]\d|\d)(?:\.(?:25[0-5]|2[0-4]\d|1\d\d|[1-9]\d|\d)){3}$'
^\%(25[0-5]\|2[0-4]\d\|1\d\d\|[1-9]\d\|\d\)\%(\.\%(25[0-5]\|2[0-4]\d\|1\d\d\|[1-9]\d\|\d\)\)\{3}$

real    0m1.294s
user    0m1.250s
sys     0m0.016s
```

## Introduction

Make executable binary (`./maton` is output binary):

```
$ make
```

Run without making executable binary:

```
$ swipl main.pl ...
```

## Conversion between ERE and Vim pattern

```
swipl main.pl -f ere -t vim {ERE pattern}
swipl main.pl -f vim -t ere {Vim pattern}
```

## Dump node

```
swipl main.pl -f ere -t node {ERE pattern}
```
