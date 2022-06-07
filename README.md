## Overview

The exam is in the files:

- [Matryoshka.lc](./src/Matryoshka.lc)
- [FileSystem.hs](./src/FileSystem.hs)
- [Types.hs](./src/Types.hs)
- [Logging.hs](./src/Logging.hs)

### Testing and Evaluation

All the points, will be awarded automatically, by
evaluating your functions against a given test suite.

In this mock final, `Test.hs` contain the full test suite 
we would use to grade your solutions.
In a real final, instead, it will have a small number of public tests.

When you run

```shell
$ make test
```

Your last lines should have

```
All N tests passed (...)
```

**or**

```
K out of N tests failed
```

**If your output does not have one of the above your code will receive a zero**

If for some problem, you cannot get the code to compile,
leave it as is with the `error ...` with your partial
solution enclosed below as a comment.

The other lines will give you a readout for each test.
You are encouraged to try to understand the testing code,
but you will not be graded on this.

### Submission Instructions

*(This section is only valid for the real final, do not submit your mock final)*

Submit your code via the `final` assignment on Gradescope.
You must submit a single zip file containing a single directory with your repository inside.
A simple way to create this zip file is:

- Run `git push` to push your local changes to your private fork of the assignment repository
- Navigate to your private fork on github and download source code as a zip

Please *do not* include the `.stack-work` or the `_MACOSX` folder into the submission.

Upon submission to Gradescope, the auto-grader will only test your code on the public test suite,
so you can get no more than XX points.
After the deadline, we will re-test your code on the private test suite.



## Q1. Lambda Calculus: Matryoshka Numbers [25 points]

In this task we will implement an alternative representation of natural numbers in lambda calculus, 
which we are going to call [Matryoshka](https://en.wikipedia.org/wiki/Matryoshka_doll) numbers,
because they are reminiscent of nesting Russian dolls.

More precisely, Matryoshka numbers are nested pairs.
The number `0` is represented as a pair of `FALSE` and `FALSE`,
and a number `N + 1` is represented as a pair of `TRUE` and the number `N`.
In other words, we can define the zero value `MZERO`
and the successor function `MSUCC` as follows:

```haskell
let MZERO = PAIR FALSE FALSE     -- Zero
let MSUCC = \n -> PAIR TRUE n    -- Successor
```

Using `MZERO` and `MSUCC` we can easily define any Matryoshka numeral,
for example:

```haskell
let MONE   = MSUCC MZERO   --                             \b -> b TRUE (\b -> b FALSE FALSE)
let MTWO   = MSUCC MONE    --               \b -> b TRUE (\b -> b TRUE (\b -> b FALSE FALSE))
let MTHREE = MSUCC MTWO    -- \b -> b TRUE (\b -> b TRUE (\b -> b TRUE (\b -> b FALSE FALSE)))
```
(The comment next to each number is its normal form).

### 1.1. Is zero (5 points)

Implement the function `MISZ` that determines whether or not a Matryoshka number is zero.
Your implementation must satisfy the following test cases:

```haskell
eval isz_zero:
  MISZ MZERO
  =~> TRUE
  
eval isz_one:
  MISZ MONE
  =~> FALSE
```

In this and following tasks, you **may use** any function already defined in [Matryoshka.lc](./src/Matryoshka.lc).
You may also define your own helper functions if needed.

### 1.2. Decrement (5 points)

Implement the function `MDEC` that decrements a Matryoshka number by one
(and bottoms out at zero).
Your implementation must satisfy the following test cases:

```haskell
eval dec_two:
  MDEC MTWO
  =~> \b -> b TRUE (\b -> b FALSE FALSE)  -- MONE

eval dec_zero:
  MDEC MZERO
   =~> \b -> b FALSE FALSE                -- MZERO
```

### 1.3. Church to Matryoshka (5 points)

Implement the function `FROM_CHURCH` that converts a Church numeral
into a Matryoshka number.
(Recall that Church numerals are the traditional representation of natural numbers we studied in class.)
Your implementation must satisfy the following test cases:

```haskell
eval from_church_zero:
  FROM_CHURCH ZERO
  =~> \b -> b FALSE FALSE                 -- MZERO
  
eval from_church_one:
  FROM_CHURCH ONE
  =~> \b -> b TRUE (\b -> b FALSE FALSE)  -- MONE
```

### 1.4. Matryoshka to Church (10 points)

Implement the function `TO_CHURCH` that converts a Matryoshka number
into a Church numeral.
Your implementation must satisfy the following test cases:

```haskell
eval to_church_zero:
  TO_CHURCH MZERO
  =~> ZERO
  
eval to_church_one:
  TO_CHURCH MONE
  =~> ONE
```

## Q2. Datatypes and Recursion: Files and Directories [35 pts]

We can represent a directory structure using the following Haskell datatype:

```haskell
data Entry = 
    File String Int     -- file: name and size
  | Dir String [Entry]  -- directory: name and child entries 
```

For example, the value:

```haskell
homedir = Dir "home" 
            [ File "todo" 256
            , Dir  "HW0" [ File "Makefile" 575 ]
            , Dir  "HW1" [ File "Makefile" 845, File "HW1.hs" 3007]
            ]
```

represents the following directory structure:

```
home
 |---todo (256 bytes)
 |
 |---HW0
 |     |---Makefile (575  bytes)
 |
 |---HW1      
       |---Makefile (845  bytes)
       |---HW1.hs   (3007 bytes)
```

In your solutions you can use any library functions on integers
(e.g. arithmetic operators),
but **only the following** functions on lists:

```haskell
(==)   :: String -> String -> Bool       -- equality on strings
(++)   :: [a] -> [a] -> [a]              -- append on any lists
map    :: (a -> b) -> [a] -> [b]         -- map
filter :: (a -> Bool) -> [a] -> [a]      -- filter
foldr  :: (a -> b -> b) -> b -> [a] -> b -- fold right
foldl  :: (b -> a -> b) -> b -> [a] -> b -- fold left
```

### 2.1 Tail-Recursive Size [15 pts]

Implement a *tail-recursive* version of the function `size`,
which computes the total size of an entry in bytes,
using a helper function `loop` with the signature provided below.
*Hint:* the second argument of loop is a list of entries yet to be processed.

Your implementation must satisfy the following test cases

```haskell
size (File "todo" 256)  
  ==> 256
size (Dir "haskell-jokes" [])  
  ==> 0
size homedir
  ==> 4683    -- 256 + 575 + 845 + 3007
```

### 2.2 Remove [15 pts]

Implement the *higher-order* function `remove p e`,
which recursively traverses all the sub-entries inside `e`
and removes those that satisfy the predicate `p`.
The directory tree should be traveresed in a *post-order*,
i.e. the property `p` should be checked for a directory
*after* its sub-entries get removed.

Your implementation must satisfy the following test cases
(here `nameOf` is a function that returns the name of an entry):

```haskell
remove (\e -> nameOf e == "Makefile") homedir
  ==> Dir "home" 
            [ File "todo" 256
            , Dir  "HW0" []
            , Dir  "HW1" [File "HW1.hs" 3007]
            ]
            
remove (\e -> nameOf e == "HW1") homedir
  ==> Dir "home" 
            [ File "todo" 256
            , Dir  "HW0" [ File "Makefile" 575 ]
            ]            
            
remove (\e -> nameOf e == "home") homedir
  -- the current directory is never removed (only sub-entries),
  -- so return homedir unchanged:
  ==> Dir "home" 
            [ File "todo" 256
            , Dir  "HW0" [ File "Makefile" 575 ]
            , Dir  "HW1" [ File "Makefile" 845, File "HW1.hs" 3007]
            ]
```

### 2.3 Clean up [5 pts]

Using `remove` from 2.2, implement the function `cleanup e`
that removes all empty subdirectories of `e`.
A subdirectory is *not* considered empty if it has zero-size files in it.
Your implementation must satisfy the following test cases:

```haskell
cleanup (Dir "temp" [Dir "drafts" [], File "todo" 256])  
  ==> Dir "temp" [File "todo" 256]
cleanup (File "todo" 256)  
  ==> File "todo" 256  
cleanup (Dir "drafts" [])  
  -- the current directory is never removed (only sub-dirs):
  ==> Dir "drafts" []
```

## Q3: Monads and Interpreters: Logging and Lazy Evaluation [35 pts]

All the functions you must implement are in [Logging.hs](./src/Logging.hs).

In this task we will add two new features to our Nano interpreter
*logging* (debug output) and *lazy evaluation*.

### Logging

Our logging feature should work similar to the `trace` function in Haskell.
More specifically we want to be able to write a Nano program like this:

```haskell
(log 'I saw 2' 2)  +  (log 'I saw 3' 3)
```

and have the interpreter print out all the log messages in the order they have been evaluated,
followed by the result of evaluation, i.e.:

```
I saw 2
I saw 3
5
```

### Datatypes

For this task, we will used a stripped-down version of Nano that only has integers, arithmetic operations, 
variables, let bindings, and logging directives:

```haskell
data Expr = EInt Int                -- Constant
          | EVar Id                 -- Variable
          | EBin Binop  Expr Expr   -- Arithmetic expression
          | ELet Id     Expr  Expr  -- Let binding
          | ELog String Expr        -- Log a message and then evaluate the inner expression
```

For example, here are three versions of `2 + 3` in this representation with different amounts of logging:

```haskell
e1 = EBin Plus (EInt 2)                    -- No logging
               (EInt 3)
e2 = EBin Plus (ELog "I saw 2" (EInt 2))   -- Only log 2 
               (EInt 3)
e3 = EBin Plus (ELog "I saw 2" (EInt 2))   -- Log both 2 and 3 
               (ELog "I saw 3" (EInt 3))
```

To represent an evaluation result that contains both a value and a log,
we introduce the following type `Logging a`:

```haskell
-- | A logging computation contains a log (a list of messages) and a value
data Logging a = Logging [String] a
```

The overall idea is to make `Logging` a monad, 
so that we can write our `eval` function in a monadic way,
and not worry about concatenating logs produced by sub-expressions.

### 3.1 Monad Instance for Logging [10 pts]

Define an instance of `Monad` for `Logging`,
i.e. implement methods `return` and `(>>=)`
to achieve the following behavior:

- When two logging computations are sequenced, 
  their logs are concatenated together
  in the order in which they are executed.

Your implementation must satisfy the following tests cases:

```haskell
λ> do x <- return 3 :: Logging Int ; return (x + 5)
Logging [] 8

λ> do log "hello"; log "goodbye"
Logging ["hello", "goodbye"] ()
```

(Here `log` is a helper function we implemented that just logs a message and does not return anything;
feel free to inspect its implementation in `Logging.hs`).

### 3.2 Evaluating Logging Directives [10 pts]

Implement the function `eval :: Env -> Expr -> Logging Value`
that evaluates expressions with logging directives.

Your implementation must satisfy the following tests cases

```haskell
λ> execString "2 + 3"
5

λ> execString "(log 'I saw 2' 2) + 3"
I saw 2
5

λ> execString "let x = log 'computing x' 1 in let y = log 'computing y' 5 in 2 * y"
computing x
computing y
6

λ> execString "log 'one' (log 'two' 0)"
one
two
0
```

We encourage you to use the `do` notation 
and the monadic operations on `Logging` 
you have implemented in 3.1 to structure your code.
However, if you have not finished 3.1, 
you can also solve this task without monads, 
albeit more verbosely.

We also encourage you to use the following helper functions we have defined 
in `Types.hs` and `Logging.hs`:

```
-- | A computation that just logs `msg`
log :: String -> Logging ()

-- | Look up an identifier in the environment
lookupId :: Id -> Env -> Value

-- | Extend an environment with a new binding
extend :: Id -> Value -> Env -> Env

-- | Apply a binary operator to two values
evalOp :: Binop -> Value -> Value -> Value
```

### 3.3. Lazy evaluation [15 points]

Currently our interpreter evaluates `let x = def in body` expressions *eagerly*:
that is, `def` is always evaluated first,
whether or not `x` is actually used in `body`.

For example, consider the following test case:
```haskell
λ> execString "let x = log 'computing x' 1 in 1"
computing x
1
```
Here the definition for `x` is evaluated (which results in logging `"computing x"`) 
even if technically `x` is not needed to compute the final result.
We now want to implement a *lazy* interpreter for our language,
which evaluates the definition of a `let`-bound variable whenever this variable is *used*.

To this end, we extend our `Value` datatype
with a new constructor that represents *thunks*, 
i.e. values that have yet to be evaluated:
```haskell
data Value
  = VInt  Int       -- Integer value
  | VThunk Env Expr -- Unevaluated value
```

As you can see, a thunk stores an environment and an expression.
It is up to you to figure out what these parameters mean
and how to use them to implement lazy evaluation.

Your task is to implement the function `evalLazy :: Env -> Expr -> Logging Value`
that evaluates an expression in an environment *lazily*,
that is it only evaluates a let-definition when it is needed in the final result.
Your implementation should satisfy the following test cases:

```haskell
-- the definition for x is NOT evaluated:
λ> execStringLazy "let x = log 'computing x' 1 in 1"
1

-- the definition for x is evaluated (needed in the final result):
λ> execStringLazy "let x = log 'computing x' 1 in x + 1"
computing x
2

-- the definition for x is NOT evaluated (NOT needed in the final result):
λ> execStringLazy "let x = log 'computing x' 1 in let y = x + 1 in 1"
1
```

**Note:** although in a real lazy language like Haskell each thunk is evaluated *at most once*,
in your solution you don't have to implement this kind of caching;
instead you should evaluate a thunk as many times as it is used in the computation of the final value.
#   m o c k _ f i n a l  
 