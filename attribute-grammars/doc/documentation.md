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


