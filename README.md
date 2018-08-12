# bdDwC

Darwinazing biodiversity data in R.

"Darwin Core is a standard maintained by the Darwin Core maintenance group. It includes a glossary of terms intended to facilitate the sharing of information about biological diversity by providing identifiers, labels, and definitions." [Darwin Core](https://github.com/tdwg/dwc)

`bdDwC` is R package (soon to be submitted to CRAN) that user can Darwinize given data using `shiny` application interface.  This package is part of [`bd-R`](https://github.com/bd-R) and soon will be implemented into two other packages [`bdchecks`](https://github.com/bd-R/bdchecks) and [`bdclean`](https://github.com/bd-R/bdclean)


This package lets user to:

- Upload users dataset.  
- Upload users dictionary.  
- Run automatic $Darwinizer$.  
- Perform manual renaming.

All these actions are done within reactive `shiny` environment. 

---

Install `bdDwC` with: 

    devtools::install_github("bd-R/bdDwC")

Run `bdDwC` `shiny` app with:
    
    library(bdDwC)
    runDwC()