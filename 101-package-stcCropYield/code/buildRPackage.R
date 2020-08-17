
buildRPackage <- function(
    package.name       = NULL,
    copyright.holder   = "Kenneth Chu",
    description.fields = base::list(),
    packages.import    = base::c(),
    packages.depend    = base::c(),
    packages.suggest   = base::c(),
    packages.enhance   = base::c(),
    files.R            = base::c(),
    tests.R            = base::c(),
    vignettes.Rmd      = base::c(),
    images.png         = base::c(),
    log.threshold      = logger::DEBUG
    ) {

    this.function.name <- "buildRPackage";

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    log.file <- base::file.path(paste0(this.function.name,".log"));
    logger::log_threshold(level = log.threshold);
    logger::log_appender(logger::appender_tee(file = log.file));
    logger::log_info('{this.function.name}(): starts');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    base::require(usethis);
    base::require(devtools);
    base::require(roxygen2);
    base::require(rmarkdown);
    base::require(testthat);
    base::require(R6);
    base::require(dplyr);
    base::require(ggplot2);
    base::require(e1071);
    base::require(stats);

    # ~~~~~~~~~~ #
    initial.wd   <- base::normalizePath(base::getwd());
    path.package <- base::file.path(initial.wd,package.name);

    # ~~~~~~~~~~ #
    testthat::with_mock(
        usethis::create_package(
            path    = path.package,
            fields  = description.fields,
            rstudio = FALSE,
            open    = FALSE
            ),
        check_not_nested = function(path, name) return(),
        .env = "usethis"
        );

    base::setwd( base::normalizePath(path.package) );

    # ~~~~~~~~~~ #
    usethis::use_mit_license(name = copyright.holder);
    usethis::use_testthat();
    usethis::use_vignette("name-of-vignette");

    # ~~~~~~~~~~ #
    for ( temp.package in packages.import ) {
        usethis::use_package(package = temp.package, type = "Imports");
        }

    for ( temp.package in packages.depend ) {
        usethis::use_package(package = temp.package, type = "Depends");
        }

    for ( temp.package in packages.suggest ) {
        usethis::use_package(package = temp.package, type = "Suggests");
        }

    for ( temp.package in packages.enhance ) {
        usethis::use_package(package = temp.package, type = "Enhances");
        }

    # ~~~~~~~~~~ #
    for ( temp.file.R in files.R ) {
        base::file.copy(
            from = temp.file.R,
            to   = base::file.path(".","R")
            );
        }

    # ~~~~~~~~~~ #
    for ( temp.test.R in tests.R ) {
        base::file.copy(
            from = temp.test.R,
            to   = base::file.path(".","tests","testthat")
            );
        }

    # ~~~~~~~~~~ #
#    base::unlink(base::file.path(".","vignettes","name-of-vignette.rmd"));
#    for ( temp.vignette.Rmd in vignettes.Rmd ) {
#        base::file.copy(
#            from = temp.vignette.Rmd,
#            to   = base::file.path(".","vignettes")
#           );
#        }
#    for ( temp.image.png in images.png ) {
#        base::file.copy(
#            from = temp.image.png,
#            to   = base::file.path(".","vignettes")
#            );
#        }

    # ~~~~~~~~~~ #
    devtools::document();
#    devtools::build_vignettes();

    # ~~~~~~~~~~ #
    base::dir.create(base::file.path(".","inst"));
    base::dir.create(base::file.path(".","inst","doc"));
   
    doc.files <- list.files(path = base::file.path(".","doc"), all.files = TRUE)
    doc.files <- base::file.path(".","doc",doc.files)

    for ( temp.doc.file in doc.files ) {
        base::file.copy(
            from = temp.doc.file,
            to   = base::file.path(".","inst","doc")
           );
        }

    base::unlink(x = base::file.path(".","doc"), recurse = TRUE);

    # ~~~~~~~~~~ #
    base::setwd(initial.wd);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_info('{this.function.name}(): exits');
    return( NULL );

    }

