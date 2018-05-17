---
title: Compiler Construction Practicum -  Attribute Grammars
author:
- Martijn Fleuren
- 5666163
- Rik van Toor
- 4239776
date: 2018
papersize: A4
lang: en-UK
fontsize: 12pt
mainfont: Ubuntu Light
geometry: margin=2cm
---

# Introduction

This document will briefly explain the implementation of the semantics and the
attributes defined in `src/StaticAnalysis/StaticChecks.ag`.


# Exercise B: letDepth

The attribute `letDepth` counts the maximal nesting depth of let bindings in the
program. Almost all nodes in the abstract syntax tree (AST) have been assigned
such an attribute to make it possible to move information from the leaves to
the root of the tree. It makes use of a default rule that assigns the default
value to be 0, and the default combinator to be +.

The maximum is simply chosen to be the value for lists of declarations or
expressions. For Let-expressions, the value is assigned to be $1 +
\max{\texttt{@declarations.letDepth}, \texttt{@expression.letDepth}}$


# Exercise D: EmptyClasses

There have been defined three attributes for computing the empty classes:
Firstly, the set of classes $C$ that is ever defined in the module. Secondly,
the set of classes $I$ for which instances have been defined. Finally, the set
of classes $E$ for which no instances exist. $E$ is trivially computed at the
top level when $I$ and $C$ are known with $E = C \ I$.

$C$ is a synthesized attribute that by default is $\emptyset$ and the combinator
is $\cup$. The attribute value is the name of the class in the case of a class
definition. $I$ is defined in a similar manner but with instance declarations
instead of class declarations.


# Exercise A: NrOfLeaves

This exercise was solved by adding SEM rules for `Declaration`, specifically for `Class` and `Instance`. The rules can be found in *StaticChecks.ag* on lines 250 and 254. The rules simply set the number of leaves for Instance and Class declarations to 0.

# Exercise C: ReservedWords

On lines 74 to 85, a synthesized attribute `reservedWords : [String]` has been defined. The idea is that this attribute will recursively build a list of all used keywords in the program. The default operator to combine two of those lists is simply `(++)` and the default value is `[]`. This way, we only have to override the semantics of this attribute in places where a keyword could have been used. This happens on lines 201 to 285.

# Exercise E: MultipleTypeDecls

Similar to exercise C, we introduce a synthesized `typeDecls : [(String, [Location])]` on lines 135 to 144. In the type signature, the String stands for the name of a type. `Location` is an alias for `(String, Int)`. In there, the String represents the file in which the type has been declared and the Int represents the line number on which this happens. To actually obtain this information, we introduce another attribute called `decls`, just for `SimpleType` on line 160. The semantics for this attribute can be found at lines 297 to 299. The `decls` function is called from `Type : Declaration` on line 243.

In the semantics of `Module.typeDecls` on line 202, we group all type declarations together by name and filter out any types that are not declared at least two times. Doing so results in the list of types that have been declared multiple types and therefore are a problem.