# Control Flow Analysis

Control flow analysis

## Installation

1.  Download and install the Haskell platform for your operating system.
    The Haskell platform can be found in https://www.haskell.org/platform/. 
    The three main platforms, Window, Mac OS X and Linux are supported.

2. Install the Stack tool from the hackage repository with the next steps.
   This need to be done from a terminal with the Haskell platform available in
   the path.

```bash
cabal update
cabal install stack
```

3. Clone this repository or extract the source code.

```bash
git clone https://github.com/carlostome/FunFlow.git
```

4. Change directory into the project and build the project with: .

```bash
stack build
```

5. To install it just type:

```bash
stack install
```
this will copy it to stack binaries folder.

## Usage

### As a library (prefered)

The easiest way to play with this library is to use it in a live ghci session.
In order to do so, just type `stack repl` in the project folder. The examples
are contained in the src/Examples.hs file

The function `runExpr` defined in src/FunFlow.hs performs parsing and analysis
on a string containing the program.

Also src/FunFlow.hs exports the function `runFile` which reads, parses and
executes the analysis of a given file.

### As an executable

Once installed, run funflow FILE. File examples can be found under examples/.
However is more recomendable the previous methods as all interesting examples
are coded as strings in the file src/Examples.hs.
