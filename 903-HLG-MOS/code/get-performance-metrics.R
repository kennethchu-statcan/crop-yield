
get.performance.metrics <- function(
    dir.performance.metrics = NULL,
    output.directory        = NULL
    ) {

    this.function.name <- "get.performance.metrics";
    logger::log_info('{this.function.name}(): starts');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.performance.metrics <- read.csv(
        file = file.path(dir.performance.metrics,"metrics-xgboost_multiphase-model-year.csv")
        );

    cat("\nstr(DF.performance.metrics)\n");
    print( str(DF.performance.metrics)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_info('{this.function.name}(): exits');
    base::return( DF.performance.metrics );

    }
