# crop-yield

This project contains the source code of the R package **stcCropYield**,
which implements functionalities to perform crop yield prediction.

*   **101-package-stcCropYield**

    This folder contains a automated pipeline that generates the package source
    of **stcCropYield**.

*   **111-make-html-vignettes**

    This folder contains a automated pipeline that generates vignettes
    of the package **stcCropYield** outside of the package build process.

*   **112-package-test**

    This folder contains a automated pipeline that tests the package,
    assuming the package has been installed.

*   **901-dev**

    This folder contains a automated pipeline that can execute the source code
    of the package, assuming the package has NOT been installed.
    The pipeline does so by sourcing the code as R scripts.
    This pipeline was used primarily near the beginning of the development
    phase, before the package was in installable form.
