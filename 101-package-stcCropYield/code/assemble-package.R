
assemble.package <- function(
    package.name       = NULL,
    copyright.holder   = "Kenneth Chu",
    description.fields = base::list(),
    packages.import    = base::c(),
    packages.depend    = base::c(),
    packages.suggest   = base::c(),
    packages.enhance   = base::c(),
    files.R            = base::c(),
    tests.R            = base::c(),
    list.vignettes.Rmd = base::list(),
    images.png         = base::c(),
    log.threshold      = logger::DEBUG
    ) {

    this.function.name <- "assemble.package";

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    initial.wd <- base::normalizePath(base::getwd());

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    log.file <- file.path(initial.wd,paste0(this.function.name,".log"));
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
    path.package <- base::file.path(initial.wd,package.name);

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
    #usethis::use_vignette("template-vignette");
    #usethis::use_vignette("name-of-vignette");
    #base::unlink(base::file.path(".","vignettes","name-of-vignette.Rmd"));
    #usethis::use_vignette("rwFV-xgboost");

    vignettes.directory <- base::file.path(".","vignettes");
    if ( !dir.exists(vignettes.directory) ) {
        dir.create(
            path      = vignettes.directory,
            recursive = TRUE
            );
        }

    doc.directory <- base::file.path(".","inst","doc");
    if ( !dir.exists(doc.directory) ) {
        dir.create(
            path      = doc.directory,
            recursive = TRUE
            );
        }

    for ( temp.vignette in list.vignettes ) {
        logger::log_info('{this.function.name}(): processing vignette: title = {temp.vignette[["title"]]}, file = {temp.vignette[["file"]]}');
        usethis::use_vignette(
            name  = tools::file_path_sans_ext(base::basename(temp.vignette[['Rmd']])),
            title = temp.vignette[['title']]
            );
        base::file.copy(
            from      = temp.vignette[['Rmd']],
            to        = vignettes.directory,
            overwrite = TRUE
            );
        base::file.copy(
            from      = temp.vignette[['html']],
            to        = doc.directory,
            overwrite = TRUE
            );
        }

#    for ( temp.image.png in images.png ) {
#        base::file.copy(
#            from = temp.image.png,
#            to   = base::file.path(".","vignettes")
#            );
#        }

    # ~~~~~~~~~~ #
    # building vignette outside of, and prior to, R package build process,
    # since vignette building during package build doesn't seem to like
    # multicore workflows.
    # need to source the stcCropYield here since the package hasn't been
    # built yet.
    #for ( temp.file.R in setdiff(files.R,"package-init.R") ) {
    #    base::source(temp.file.R);
    #    }

    #devtools::build_vignettes(quiet = FALSE);

    # ~~~~~~~~~~ #
    devtools::document();
    #devtools::build_vignettes();

    # ~~~~~~~~~~ #
    doc.files <- list.files(path = base::file.path(".","doc"), all.files = TRUE);
    doc.files <- base::file.path(".","doc",doc.files)

    for ( temp.doc.file in doc.files ) {
        base::file.copy(
            from = temp.doc.file,
            to   = base::file.path(".","inst","doc")
           );
        }

    # base::unlink(x = base::file.path(".","doc"), recursive = TRUE);

    # ~~~~~~~~~~ #
    base::setwd(initial.wd);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_info('{this.function.name}(): exits');
    return( NULL );

    }
