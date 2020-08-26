
command.arguments <- base::commandArgs(trailingOnly = TRUE);
   code.directory <- base::normalizePath(command.arguments[1]);
 output.directory <- base::normalizePath(command.arguments[2]);
     package.name <- command.arguments[3];

base::cat(base::paste0("##### Sys.time(): ",base::Sys.time(),"\n"));
start.proc.time <- base::proc.time();

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
base::setwd(output.directory);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
base::require(R6);
base::require(logger);
base::source(base::file.path(code.directory,'buildRPackage.R'));

###################################################
###################################################

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
string.authors <- "base::c(
    person(
        given   = 'Kenneth',
        family  = 'Chu',
        email   = 'kenneth.chu@canada.ca',
        role    = 'cre',
        comment = c(ORCID = 'https://orcid.org/0000-0002-0270-4752')
        )
    )";
Encoding(string.authors) <- "UTF-8";

description.fields <- base::list(
    Version     = "0.0.1.0001",
    `Authors@R` = string.authors,
    Language    = "fr"
    );

packages.import <- base::c(
    "base",
    "doParallel",
    "dplyr",
    "foreach",
    "ggplot2",
    "jsonlite",
    "logger",
    "magrittr",
    "rlang",
    "R6",
    "stats",
    "stringi",
    "stringr",
    "utils",
    "xgboost"
    );

packages.suggest <- base::c(
    "testthat"
    );

files.R <- base::c(
    "crop-yield-train-model.R",
    "get-learner-metadata.R",
    "get-mock-production-errors.R",
    "get-performance-metrics.R",
    "getData-synthetic.R",
    "getLearner.R",
    "initializePlot.R",
    "learner-abstract.R",
    "learner-byGroup.R",
    "learner-multiphase.R",
    "learner-xgboost.R",
    "package-init.R",
    "preprocessor.R",
    "rollingWindowForwardValidation.R",
    "test-correctness.R",
    "validation-single-year.R",
    "weighted-statistics.R"
    );
files.R <- base::file.path( code.directory , files.R );

tests.R <- base::c(
    "test-correctness.R"
    );
tests.R <- base::file.path( code.directory , tests.R );

#images.png <- base::c("np-propensity-scatter-01.png", "np-propensity-scatter-03.png");
#images.png <- base::file.path( code.directory , images.png );

#vignettes.Rmd <- base::c("vignette-title.Rmd", "nppR-package-guide.Rmd");
#vignettes.Rmd <- base::file.path( code.directory , vignettes.Rmd );

assemble.package(
    package.name       = package.name,
    copyright.holder   = "Kenneth Chu",
    description.fields = description.fields,
    packages.import    = packages.import,
    packages.suggest   = packages.suggest,
    files.R            = files.R,
    tests.R            = tests.R
#   ,vignettes.Rmd     = vignettes.Rmd,
#   images.png         = images.png
    );

###################################################
###################################################
# print warning messages to log
base::cat("\n##### warnings()\n")
base::print(base::warnings());

# print session info to log
base::cat("\n##### sessionInfo()\n")
base::print( utils::sessionInfo() );

# print system time to log
base::cat(base::paste0("\n##### Sys.time(): ",base::Sys.time(),"\n"));

# print elapsed time to log
stop.proc.time <- base::proc.time();
base::cat("\n##### start.proc.time() - stop.proc.time()\n");
base::print( stop.proc.time - start.proc.time );

base::quit(save="no");

