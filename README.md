# UNIDO Yearbook addendum / visual companion 

Create a reproducible addendum (mainly containing visualizations) for UNIDO Statistics data (mainly based on YB part I) using R and bookdown package for output rendering in PDF (compile via Latex) and HTML. The first priority is a proper pdf output.

This publication is also the first step in developing an own visual identity for UNIDO Statistics publications with R(markdown).


## Description

... add details here...

** list contributors with username and initials used (rather relevant before switch to gitlab)**

* Haitzmann Martin: HaitzmaM, MH, marti

### initial project (create MH::2021-02-03)

The project folder G:/STATISTICS/CS-YB-visual-addendum/YB-2021-addendum was updated regularly.
Due to problems sometimes with G Drive (writing blocked when drive almost full), development rather in personal H: drive.

To control versioning and syncing code changes for the time when  more users contribute to project, switch to UNIDO gitlab.

### switch project to gitlab (switch MH::2021-05-17)

Switch to https://gitlab.unido.org/internal/statistics-division/yb-addendum

The project path in G changes to G:/STATISTICS/CS-YB-visual-addendum/yb-addendum. 

No separate folder indication the publication year is needed an more, as yearly publication can be indicated via release versions (e.g.: tag v2021.0-pre, ...)

## Overview, install necessary packages, book rendering

all necessary steps in 
```
_start-prepare-render.Rmd
```

run lines in chunk:

```
## PDF:

params <- ...
bookdown::render_book("index.Rmd", output_format = "tufte::tufte_book", params = params,clean = TRUE)
```

render calls index.Rmd, reads common parameters, ..., and calls latex to compile pdf 

## Details

common parameters (visualID, libraries, functions, ...) are set in R files 
```
 _common*.R
```
every chapter of the book has its own Rmd file with code and text

different versions (VV) are indicated as well, e.g. -MH2.Rmd, allows different users to develop code (not necessary after switch to git, as branches can be used for that)
```
01-*-VV0.Rmd
02-*-VV0.Rmd
...
99-appendix.Rmd
```

in **_bookdown.yml** is defined which chapters are compiled

in **_output.yml** further parameters for different book types are defined

...

## Current Status and TODO 

* create MH::2021-05-17 until switch MH::2021-05-17:
  - create project/book structure
  - create proposal for template (especially latex)
  - proposal visual id, 
  - read (known) data sources
  - experiment and create proposals for visualizations according to TOC structure from Fernando. 
  
Last (output) status in old setting (without git) is V2021-05-17

* switch to git MH::2021-05-17:
  - create master branch with plain template (sample images as placeholder)
  - create MH2 branch with all latest figures (as of  V2021-05-17), ...
  - merge MH2 to master branch to have one common status to work on from time of switch to gitlab
  - ...

* develop visualisations and text for book until releas (tag V2021)


* ??at a final stage create R package with all functions, visual ID, tex and html templates ??


## Version History


