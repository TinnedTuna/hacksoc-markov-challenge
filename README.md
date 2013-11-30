Haskell Markov Generator
========================

Written for the 2013 HackSoc randomly-generated speeches competition, see:
https://twitter.com/HackSoc/status/404302716106600448

Status
------

It's all written and working for small datasets (and reasonably quickly), 
however, it has serious memory issues.

Building
--------

Building is handled by cabal:

```
$ cabal configure
$ cabal build
```

Usage
-----

```
$ markov-generator --seed=SEED --length=LENGTH --inputDir=DIR --outputFile=OUTFILE --logFile=LOGFILE
```

This will generate a random set of text based on the SEED and files found in
DIR, of length LENGTH and written into OUTFILE. Logging of the seed, corpus 
files used will be written into LOGFILE

All arguments are optional, and if omitted, a reasonably sane default will be 
chosen:

  * seed = Current time in millis
  * length = 1500
  * inputDir = ./corpus
  * outputFile = stdout 
  * logFile = stdout

TODO
----

  * Generalise to n-gram analysis
  * Reduce memory footprint (We currently require +RTS -K<BIG>M -RTS for large-ish corpus)
