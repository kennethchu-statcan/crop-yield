
build.package <- function(
    package.path  = NULL,
    log.threshold = logger::DEBUG
    ) {

    this.function.name <- "build.package";

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    initial.wd <- base::normalizePath(base::getwd());
    log.file   <- file.path(initial.wd,paste0(this.function.name,".log"));

    logger::log_threshold(level = log.threshold);
    logger::log_appender(logger::appender_tee(file = log.file));
    logger::log_info('{this.function.name}(): starts');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    package.path <- base::normalizePath(package.path);
    logger::log_info('{this.function.name}(): package.path = {package.path}');

    base::setwd( package.path );
    logger::log_info('{this.function.name}(): getwd() = {getwd()}');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    base::require(devtools);

    logger::log_info('{this.function.name}(): devtools:build(): starts');
    devtools::build(
        vignettes = TRUE,
        manual    = TRUE,
        quiet     = FALSE
        );
    logger::log_info('{this.function.name}(): devtools:build(): done');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    base::setwd(initial.wd);
    logger::log_info('{this.function.name}(): exits');
    return( NULL );

    }
