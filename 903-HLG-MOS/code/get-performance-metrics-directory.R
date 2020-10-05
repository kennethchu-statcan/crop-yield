
get.performance.metrics.directory <- function(
    dir.data = NULL
    ) {

    this.function.name <- "get.performance.metrics.directory";
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    cat(paste0("starting: ",this.function.name,"()\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    dir.temp <- dir.data;

    dir.temp <- gsub(
        x           = dir.temp,
        pattern     = "input",
        replacement = "output"
        );

    performance.metrics.directory <- file.path(
        dir.temp,
        "902-dev-snapshot-2019-05-19",
        "output.2020-10-01.02.Config196",
        "rwFV",
        "020-performance-metrics"
        );

    cat(paste0("\nperformance.metrics.directory: ",performance.metrics.directory,"\n"));

    cat("\nlist.files(performance.metrics.directory)\n");
    print( list.files(performance.metrics.directory)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\nexiting: ",this.function.name,"()"));
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    return( performance.metrics.directory );

    }

##################################################
