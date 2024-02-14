## _slimmr_: SLiM Models in R

### A minimalistic SLiM-R interface
The purpose of _slimmr_ is to provide a minimal interface between
R and SLiM. The motivation for its development was to facilitate a workflow
where models are developed in Eidos and the SLiM GUI, and then scaled up and
broadened from R. _slimmr_ simplifies the process of running and manipulating
SLiM models in R with simple functions to import Eidos scripts and modify them
on the fly between runs to adjust e.g. model parameters, functionality, and
events. So the purpose of _slimmr_ is not to remove the need for hands-on Eidos
scripting, but to provide a convenient way to run and re-program existing Eidos 
scripts in R. Much of its functionality is essentially possible within Eidos; in
such cases, _slimmr_ lets you write simpler Eidos scripts that you can
instead modify and branch from R. This could be simpler and more computationally
efficient than writing Eidos scripts with many conditional components.

This package was primarily developed for personal/in-house use. But if it seems
useful to you, you are more than welcome to apply it for your own needs.
I intend to provide a thorough demonstration of _slimmr_'s core functionality
below at a later time.

### _slimmr_ is a third-party package
_slimmr_ naturally requires a working installation of SLiM, which you can 
download from its creators at https://messerlab.org/slim/. Please note that
_slimmr_  and its contributor(s) are neither affiliated with nor endorsed by the
developers of SLiM. **So: if you find _slimmr_ useful, please remember to first
and foremost cite the developers of SLiM as per their preference!**

### Alternatives to _slimmr_
_slimmr_ is not the only R-SLiM interface out there, not the first, and most
certainly not the most comprehensive. Its scope is deliberately minimal, for
users who prefer that. But other packages provide considerably more support,
functionality, and integration between SLiM and R. If you consider using
_slimmr_, I would suggest also considering the R packages _slimr_
(https://github.com/rdinnager/slimr) and _slendr_ 
(https://github.com/bodkan/slendr) first!

### Installation
You can install _slimmr_ from GitHub with the _devtools_ package from CRAN:
```r
devtools::install_github("ErikSRoeed/slimmr")
```

### Publications
The following publication used _slimmr_ v. 0.2.0. Please note that later
releases of _slimmr_ are incompatible, so you should install that version if
you wish to run the code from the paper:

Røed, E. S. & Engelstädter, J. Cytoplasmic incompatibility in hybrid zones: infection dynamics and resistance evolution. J. Evol. Biol. 35, 240–253 (2022).
https://doi.org/10.1111/jeb.13974
