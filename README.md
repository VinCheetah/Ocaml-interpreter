# Project Report 2, Fouine

## Introduction

Here is our Fouine, coded in Ocaml, addressing most of the expected features, excluding polymorphism. Compilation instructions are standard—simply run `make` and execute `./fouine` followed by the options described below.

## Code Organization
### Code Structure
- `main.ml`: handles options and executes the appropriate code
- `expr.ml`: evaluates the code passed to Fouine
- `affichage.ml`: formats the output display of Fouine
- `options.ml`: contains references associated with available options
- `inference.ml`: type inference
- `unif.ml`: executes the unification algorithm on the type inference output
- `lexer.mll` and `parser.mll`: handle lexer and parser tasks, respectively

## Description of Options

The options available in our Fouine are:
- `-trace`: to trace the states of the automaton during expression parsing
- `-warnings`: to display warnings present in Ocaml
- `-output`: to return the output of the entered expression
- `-showsrc`: to display the Caml code corresponding to the Fouine expression
- `-debug`: to display the states of the environment during evaluation and inference
- `-slow`: step-by-step debugging of the evaluation
- `-showtypes`: to display the types of all variables created in the expression
- `-notypes`: behavior of Fouine without type inference
- `-showinf`: debug to follow the steps of type inference
- `-tree`: to display the instruction tree corresponding to the entered expression

## Tests Conducted

We tested our Fouine on the provided test suite, to which we added tests to ensure proper typing and output conformity with Ocaml.

## Possible Improvements

We realized late in the process that our understanding of adapting the unification algorithm for type inference differed from the guidance given in the course notes. This resulted in a loss of time that could have been used to implement polymorphism.

## Notable Points

- Handling of `Match .. With` expressions using a filter function to filter expressions corresponding to the pattern.
- Almost similar treatment for `Try .. With`, adding patterns corresponding to expressions in the `With` and possible exceptions.
- Implementation of references, lists, and expression sequences according to the instructions in the course notes.
- Differentiated handling of recursive functions from non-recursive functions using a boolean.
- Adapted expression typing process with variable renaming to avoid conflicts.
- Use of a recursive type for the unification algorithm with specific treatment for generic type variables.

## Mandatory Citation

Here is a reference [Landin66](https://dblp.org/rec/journals/cacm/Landin66) by Peter Landin.


