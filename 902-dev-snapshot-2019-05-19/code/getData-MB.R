
getData.MB <- function(
    FILE.non.potato     = NULL,
    FILE.potato         = NULL,
    FILE.weekly         = NULL,
    FILE.avg.moving     = NULL,
    FILE.avg.cumulative = NULL,
    FILE.output.RData   = "raw-MB.RData",
    FILE.output.csv     = "raw-MB.csv"
    ) {

    this.function.name <- "getData.MB";
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    cat(paste0("starting: ",this.function.name,"()\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if (file.exists(FILE.output.RData)) {

        cat(paste0("\nloading file: ",FILE.output.RData,"\n"));
        DF.output <- readRDS(file = FILE.output.RData);

    } else {

        DF.MB.non.potato <- getData.MB.non.potato(
            FILE.non.potato = FILE.non.potato
            );

        DF.MB.potato <- getData.MB.potato(
            FILE.potato = FILE.potato
            );

        DF.output <- stackdata.MB(
            DF.non.potato = DF.MB.non.potato,
            DF.potato     = DF.MB.potato
            );

        DF.output <- recast.columns.MB(
            DF.input = DF.output
            );

        DF.output <- merge.by.CARUID(
            DF.input     = DF.output,
            FILE.weather = FILE.weekly
            );

        cat(paste0("\nsaving to file: ",FILE.output.RData,"\n"));
        saveRDS(object = DF.output, file = FILE.output.RData);

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # if ( !file.exists(FILE.output.csv) ) {

    #     cat(paste0("\nwriting file: ",FILE.output.csv,"\n"));
    #     write.csv(
    #         file      = FILE.output.csv,
    #         x         = DF.output,
    #         row.names = FALSE
    #         );

    #     }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\nexiting: ",this.function.name,"()"));
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    return( DF.output );

    }

##################################################
merge.by.CARUID <- function(
    DF.input     = NULL,
    FILE.weather = NULL
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\nreading file: ",FILE.weather,"\n"));
    DF.weather <- as.data.frame(readr::read_csv(
            file = FILE.weather
            ));

    factor.variables <- c("CARUID");
    for ( temp.variable in factor.variables) {
        DF.weather[,temp.variable] <- as.factor(DF.weather[,temp.variable]);
        }

    DF.weather[,"YEAR"] <- factor(
        x       = as.character(DF.weather[,"YEAR"]),
        levels  = sort(unique( DF.weather[,"YEAR"])),
        ordered = TRUE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- merge(
        x     = DF.input,
        y     = DF.weather,
        all.x = TRUE,
        by.x  = c("CAR16UID","YEAR"),
        by.y  = c(  "CARUID","YEAR")
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    primary.key <- c("YEAR","QSTRM"); 

    column.ordering <- c(
        primary.key,
        setdiff(colnames(DF.output),primary.key)
        );

    DF.output <- DF.output[,column.ordering];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    row.ordering <- order(
        DF.output[,    "YEAR"],
        DF.output[,   "QSTRM"],
        DF.output[,"CROPSURV"]
        );

    DF.output <- DF.output[row.ordering,];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return( DF.output );

    }

recast.columns.MB <- function(
    DF.input = NULL
    ) {

    factor.variables <- c(
        "QSTRM",    # Quarter section identifier (geographic parcel) - StatCan format
        "YMEcoReg", # Ecoregion identifier
        "CONTRACT", # Operator ID
        "CROPCODE", # Manitoba Insurance crop code
        "CROPDESC", # Manitoba Insurance crop description
        "CROPSURV", # Statcan Crop type - Crops survey
        "DA16UID",
        "CAR16UID"
        );

    DF.output <- DF.input;
    for ( temp.variable in factor.variables) {
        DF.output[,temp.variable] <- as.factor(DF.output[,temp.variable]);
        }

    DF.output[,"YEAR"] <- factor(
        x       = as.character(DF.output[,"YEAR"]),
        levels  = sort(unique( DF.output[,"YEAR"])),
        ordered = TRUE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return( DF.output );

    }

stackdata.MB <- function(
    DF.non.potato = NULL,
    DF.potato     = NULL
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.non.potato <- DF.non.potato[,setdiff(colnames(DF.non.potato),c(
        "QTRSEC","CEAGVAR","WeekOfMax"
        ))];

    DF.potato <- DF.potato[,setdiff(colnames(DF.potato),c(
        "BuPerMTon","SeededSharedAcres","HarvSharedAcres","PRODSHAREDTONS","PRODSHAREDBUSHELS","WeekOfMax7","WeekOfMax8"
        ))];

    colnames(DF.potato) <- gsub(
        x           = colnames(DF.potato),
        pattern     = "NDVIMAX7",
        replacement = "NDVIMax7"
        );

    colnames(DF.potato) <- gsub(
        x           = colnames(DF.potato),
        pattern     = "NDVIMAX8",
        replacement = "NDVIMax8"
        );

    DF.potato[,"CROPSURV"] <- "POTATO";

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\nsetdiff(colnames(DF.non.potato),colnames(DF.potato))\n");
    print( setdiff(colnames(DF.non.potato),colnames(DF.potato))   );

    cat("\nsetdiff(colnames(DF.potato),colnames(DF.non.potato))\n");
    print( setdiff(colnames(DF.potato),colnames(DF.non.potato))   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.output <- rbind(
        DF.non.potato,
        DF.potato
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return( DF.output );

    }

getData.MB.non.potato <- function(
    FILE.non.potato   = NULL,
    FILE.output.RData = "raw-MB-non-potato.RData",
    FILE.output.csv   = "raw-MB-non-potato.csv"
    ) {

    cat("\n### starting: getData.MB.non.potato()\n");

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if (file.exists(FILE.output.RData)) {

        cat(paste0("\nloading file: ",FILE.output.RData,"\n"));
        DF.output <- readRDS(file = FILE.output.RData);

    } else {

        cat(paste0("\nreading file: ",FILE.non.potato,"\n"));
        DF.output <- as.data.frame(readr::read_tsv(
            file = FILE.non.potato
            ));

        cat(paste0("\nsaving to file: ",FILE.output.RData,"\n"));
        saveRDS(object = DF.output, file = FILE.output.RData);

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # if ( !file.exists(FILE.output.csv) ) {

    #     cat(paste0("\nwriting file: ",FILE.output.csv,"\n"));
    #     write.csv(
    #         file      = FILE.output.csv,
    #         x         = DF.output,
    #         row.names = FALSE
    #         );

    #     }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\n### exiting: getData.MB.non.potato()\n");
    return( DF.output );

    }

getData.MB.potato <- function(
    FILE.potato       = NULL,
    FILE.output.RData = "raw-MB-potato.RData",
    FILE.output.csv   = "raw-MB-potato.csv"
    ) {

    cat("\n### starting: getData.MB.potato()\n");

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if (file.exists(FILE.output.RData)) {

        cat(paste0("\nloading file: ",FILE.output.RData,"\n"));
        DF.output <- readRDS(file = FILE.output.RData);

    } else {

        cat(paste0("\nreading file: ",FILE.potato,"\n"));
        DF.output <- as.data.frame(readr::read_csv(
            file = FILE.potato
            ));

        cat(paste0("\nsaving to file: ",FILE.output.RData,"\n"));
        saveRDS(object = DF.output, file = FILE.output.RData);

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    # if ( !file.exists(FILE.output.csv) ) {

    #     cat(paste0("\nwriting file: ",FILE.output.csv,"\n"));
    #     write.csv(
    #         file      = FILE.output.csv,
    #         x         = DF.output,
    #         row.names = FALSE
    #         );

    #     }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat("\n### exiting: getData.MB.potato()\n");
    return( DF.output );

    }
