---
title: Compiler Construction Practicum - Monotone Frameworks
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

# Compileren en draaien van de monotone frameworks

We gaan er van uit dat er geen dependency problemen zijn. Het draaien van ons
project zou zo gemakkelijk moeten zijn als

    monotone-frameworks $ make all

Je wordt dan in een GHCI instance geplaatst waarin je `main` aan moet roepen. In
de main krijg je een /hele simpele/ prompt. De eerste keer kun je `ls` typen voor
een lijstje van alle testprogrammas, daarna moet je ingeven welke programmas je
wilt testen. Als een filenaam `*` is, dan worden alle bestanden getest ongeacht
de rest van de input. Wanneer er slechts een paar namen ingegeven worden, worden
diegene uitgevoerd die niet beginnen met een `-`. Wanneer er alleen maar
bestandsnamen ingevuld worden die beginnen met een `-`, dan worden die weggelaten
uit de tests.

# Output
De output van elke test laat zien op welk programma de test werd gedraaid en wat
de naam van de test was. Als de test een analyse was (AExpr, SLV of CP), dan
worden er twee tabellen getoond. De context is $MFP_\circ$ en effect
correspondeert met $MFP_\bullet$.

# Bijzonderheden in de broncode
`Attributegrammar.hs` is vervangen door `Attributegrammar_cpp.hs`. Dit is omdat we het
handig vonden om te kunnen wisselen tussen afgeleide Show-instanties en zelf
gedefinieerde mooie Show instanties. De Makefile handelt dit automatisch af,
maar we raden aan om eerst Make te draaien alvorens `Attributegrammar.ag` te
inspecteren.

Het maximal fixed point algoritme staat in `MFPAlgorithm.hs` tezamen met de drie
analyses. In `Arc.hs` zijn nog wat hulpfuncties gedefinieerd voor graafoperaties.

# Het MFP algoritme
We hebben ernaar gestreefd om het algoritme zo veel mogelijk te laten lijken op
het algoritme uit het boek. Bij initialisatie wordt de flow in een for-loop in
de variabele W ge-const. Dat heeft hetzelfde effect als `reverse flow` wat wij
doen in de aanroep.

Omdat de waarde van $\bot$ soms afhangt van runtime waardes (zoals bij AExpr,
dan $\bot = AExpr_\star$. kunnen we niet op compile-time al aangeven welke
waarden dat moeten zijn, daarom moeten we de waarde injecteren in het MFP
algoritme tijdens de initialisatie.

Het iteratieproces maakt gebruik van een state monad omdat dat code geeft die
het meeste lijkt op wat er in het boek staat, om verwarring te voorkomen. Verder
zijn er alias-functies zoals `analysis :: Label -> L` die analoog staan aan de
Analysis[$\ell$] uit het boek.
