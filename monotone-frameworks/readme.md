# Monotone Frameworks Practicum

    Rik van Toor    4239776
    Martijn Fleuren 5666163

# Strongly Live Variables

For *Strongly Live Variables*, we collect results of type `Set String`. The idea of this analysis is actually quite simple. For each block, there is a *gen* and a *kill* function. For SLV, assignments are the only interesting blocks. So, for each assignment, the *gen* function returns the set of all variables used in the right hand side of the assignment. The *kill* function returns the set of the variable that is assigned to on left hand side of the assignment. Other blocks simply return an empty set for both.

## Example

```
begin
  proc square(val u, res v) is // 1
    v := u * u; // 3
  end // 2
  v := 3; // 5
  call square(v,v); // 6 8
end
```

This program results in 

```
# Context:
1       {u}
2       {}
3       {}
5       {u}
6       {u}
8       {}

# Effect
1       {u}
2       {}
3       {u}
5       {u}
6       {u}
8       {}
```

# Constant Propagation

For *Constant Propagation*, we collect results of type `Map String D`, where `D` is an abstraction for $\{Top, Value, Bottom\}$. The transfer functions are generated by a function that takes a label $l$ and a call-string context $ctx$. At first, the function looks up the block that belongs to $l$. Next, the function pattern matches on the block, and ends up with the following options:

* **Assignments:** The transfer functions for assignments simply take the right hand side of the assignment, evaluate it to $D$ and add it to a given environment.
* **Procedures:** When entering a procedure, the function uses the context to lookup the last call statement that lead the program to this point, and copies the parameters passed by the call into the environment, but using the names as defined by the procedure. When exiting the procedure, the value of the output variable in the procedure is copied to the output variable as defined by the call in a similar fashion.
* Otherwise, do not change anything and leave the given environment as is.

## Example

```c
begin
  proc add1(val x, res y) is // 1
    y := 5; // 3
  end // 2
  p := 6; // 5
  call add1(p,q); // 6 8
end
```

The program gives us the following results:

```
# Effect
1       {p=6, x=6}
2       {p=6, q=5, x=6, y=5}
3       {p=6, x=6, y=5}
5       {p=6}
6       {p=6}
8       {p=6, q=5, x=6, y=5}
```

Or, in chronological order:

```
# Effect
5       {p=6}
6       {p=6}
1       {p=6, x=6}
3       {p=6, x=6, y=5}
2       {p=6, q=5, x=6, y=5}
8       {p=6, q=5, x=6, y=5}
```

Which is correct, because at first the value of $p$ is set to 6. It is then referenced in the call, so the value of $p$ is copied to $x$. $y$ is calculated and set to 5. Upon return, $y$ is copied to $q$.

# Compilation

In order to be able to easily toggle pretty printing on or off for our entire system, our `.ag` file is first passed through a C++ preprocessor before getting compiled. `AttributeGrammar_cpp.ag` contains `#define PRETTY_PRINT 1`. If this $1$ is changed to $0$ and the file is recompiled, pretty printing is turned off, which can be useful for debugging purposes.

Compilation is still done using *make*. In the root directory of the project, run `make clean && make dev` to build the project and launch GHCI. Once in GHCI, run `main` to test all analyses on all files in the *examples* directory with a $k$-value of 3. To test a specific file or a specific kind of analyses, please take a look at `src/Test.hs`. The MFP algorithm and monotone instances can all be found in `src/MFPAlgorithm.hs`.