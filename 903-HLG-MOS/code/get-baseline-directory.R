
get.baseline.directory <- function(
    dir.data = NULL
    ) {

    this.function.name <- "get.baseline.directory";
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    cat(paste0("starting: ",this.function.name,"()\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    dir.temp <- dir.data;

    dir.temp <- gsub(
        x           = dir.temp,
        pattern     = "input",
        replacement = "output"
        );

    dir.temp <- gsub(
        x           = dir.temp,
        pattern     = "methodology2020",
        replacement = "methodology2019"
        );

    baseline.directory <- file.path(
        dir.temp,
        "804-adopt-baseline",
        "output.2019-09-18.01",
        "SASout"
        );

    cat(paste0("\nbaseline.directory: ",baseline.directory,"\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\nexiting: ",this.function.name,"()"));
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    return( baseline.directory );

    }

##################################################
