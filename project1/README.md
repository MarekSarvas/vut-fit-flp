# FLP project 1 - BKG-2-CNF
**Author**: Marek Sarvas \
**Login**: xsarva00
## Run
- *make* compiles the programme and runs it with '**2**' parameter on one test input.
- *make testi* runs tests with programme argument '**-i**'
- *make test1* runs tests with programme argument '**1**'
- *make test2* runs tests with programme argument '**2**'
- *make tests* runs all tests
## Implementation
The programme for removing simple rules and transforming BKG into CNF is implemented in 5 modules is **src/** folder.\
This programme supports only one argument [-i|1|2] at a time, the combination of these throws an error.\
Programme should stopped with an error if:
- grammar has illegal terminals or nonterminals
- starting symbol is not in nonterminals
- grammar has duplicate rules/terminals/nonterminals
- there are empty lines
- any rule contains a symbol that is not between terminals or nonterminals
- invalid combination of programme arguments appears
### Types.hs
Includes implementation of custom data type representing Grammar and its terminal, nonterminal, starting symbols and rules.
### Parser.hs
Implementation of functions for parsing Nonterminals, Terminals, Starting symbol and rules. Checks correct format of input and creates inner representation of BKG. Uses *Parser* functions from **Parsec** library. 
### RemoveSimpleRules.hs
Implementation of algorithm for removing simple rules from BKG (TIN - algorithm 4.5). Creates **N_A** sets for each nonterminal **A**, then creates new rules so that simple rules can be removed.
### CNF.hsn
Implementation of algorithm for transforming BKG to CNF (TIN - algorithm 4.7). While all rules are not in CNF, takes a rule that is not in CNF and create recursively all rules that are in CNF from it. Then performs union on grammar nonterminals and all nonterminals on the left side of rules to create new nonterminal set.
### Main.hs
Implements IO operations in *do* block, parses programme arguments and performs correct action. Also includes implementation of functions for checking semantic correctness of input grammar such as valid starting symbol or valid symbols in rules according to given nonterminals and terminals. \
! Programm supports only one argument [-i|1|2] **not** a combination of them.\
! Terminals, nonterminals and rules are alphabetically sorted before output to stdin.
## Testing
Folder **test/** containes all test files. Input grammars are in the files named **testXX.in** where XX is are numbers. Corresponding correct outputs are in files **testXX_OPTION.out** where XX are same number as for the input file, *OPTION* is [i|1|2] for outputs to match programme with argument **i,1,2** accordingly.\
Files named **testXX_err.in** have incorrect grammars and does not have corrsponding **.out** files. For these incorrect inputs only programme's return value is checked.\
Testing script is implemented in **test.sh** and can be run as follows:
```
bash test.sh [i|1|2|e]
```
Input test files that don't have corresponding output files are ignored.\
**!** Grammar in test02.in is taken from discord.
