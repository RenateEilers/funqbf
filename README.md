# Overview

This repository contains a 2QBF solver implemented in Haskell. It was created in fulfillment of the assignment given in the lecture on QBF as presented at the Doctorall College for Logical Methods in Computer Science's Ringvorlesung (see [here](http://fmv.jku.at/ringvorlesung/) for the assignment).

# Using funqbf
### Installation
```
cabal new-install
cabal new-build
``` 

### Usage
To solve a 2QBF file in Qdimacs format, run:
```
funqbf <path-to-file>
``` 

For help, run:
```
funqbf -h
``` 

To see what version of funqbf is installed, run:

```
funqbf -v
``` 