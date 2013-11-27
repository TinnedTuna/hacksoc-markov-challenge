Haskell Markov Generator
========================

Written for the 2013 HackSoc randomly-generated speeches competition, see:
https://twitter.com/HackSoc/status/404302716106600448

Status
======

NON-FUNCTIONAL AT THIS TIME.

The Markov chain generator is *written*, but it has not got a pretty driver,
there's nothing that reads files, and the command line option parsing is shot.

Usage
=====

```
$ runhaskell Main.hs --seed=SEED --length=LENGTH --inputDir=DIR --outputFile=FILE
```

This will generate a random set of text based on the SEED and files found in
DIR, of length LENGTH and written into FILE.

All arguments are optional, and if omitted, a reasonably sane default will be 
chosen:

  * seed = Current time in millis
  * length = 1500
  * inputDir = ./corpus
  * outputFile = stdout 

TODO
====

  * Opening files in the given directory that are files
  * Tokenising files into words
  * Parallel training of the chain



