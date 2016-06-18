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

4. cd into the project and build the project with `stack build`.

## Usage

### As a library

The easiest way to play with this library is to use it in a live ghci session.
In order to do so, just type `stack ghci` in the project folder. The examples
are contained in the src/Examples.hs file


### As an executable

Once installed, run FunFlow FILE. File examples can be found under examples/.
