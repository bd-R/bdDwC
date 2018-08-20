[![Travis CI Build Status](https://api.travis-ci.org/bd-R/bdDwC.png)](https://api.travis-ci.org/bd-R/bdDwC)

# bdDwC

Darwinazing biodiversity data in R (this package is in preparation stage and soon will be submitted to CRAN).

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

---

To use `bdDwC` from the command line:

Load `bdDwC` package

    library(bdDwC)

`bdDwC` contains Indian Reptile dataset `bdDwC:::dataReptiles`.  
Darwinize data with `darwinizeNames` (replace `bdDwC:::dataReptiles` with wanted dataset):

    result <- darwinizeNames(dataUser = bdDwC:::dataReptiles,
                            dataDWC   = bdDwC:::dataDarwinCloud$data)


Rename your data using `bdDwC` with `renameUserData`:

    # Replace `bdDwC:::dataReptiles` with wanted dataset
    renameUserData(bdDwC:::dataReptiles, result)

To get newest version of Darwin Cloud Data run:

    downloadCloudData()

which will download data from the remote repository and extract field and standard names.


