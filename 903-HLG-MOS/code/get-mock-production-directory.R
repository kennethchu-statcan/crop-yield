
get.mock.production.directory <- function(
    dir.data = NULL
    ) {

    this.function.name <- "get.mock.production.directory";
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    cat(paste0("starting: ",this.function.name,"()\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    dir.temp <- dir.data;

    dir.temp <- gsub(
        x           = dir.temp,
        pattern     = "input",
        replacement = "output"
        );

    mock.production.directory <- file.path(
        dir.temp,
        "902-dev-snapshot-2019-05-19",
        "output.2020-10-01.02.Config196",
        "rwFV",
        "030-mock-productions"
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\nmock.production.directory: ",mock.production.directory,"\n"));

    cat("\nlist.files(mock.production.directory)\n");
    print( list.files(mock.production.directory)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\nexiting: ",this.function.name,"()"));
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    return( mock.production.directory );

    }

##################################################
