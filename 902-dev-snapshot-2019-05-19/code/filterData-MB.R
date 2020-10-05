
filterData.MB <- function(
    DF.input          = NULL,
    FILE.output.RData = "filtered-MB.RData",
    FILE.output.csv   = "filtered-MB.csv"
    ) {

    this.function.name <- "filterData.MB";
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    cat(paste0("starting: ",this.function.name,"()\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if (file.exists(FILE.output.RData)) {

        cat(paste0("\nloading file: ",FILE.output.RData,"\n"));
        DF.output <- readRDS(file = FILE.output.RData);

    } else {

        DF.output <- DF.input %>%
            filter( YEAR     != "2018" )  %>%
            filter( CROPCODE !=  "902" )
            ; # remove parcels that were "TOO WET TO SEED"

        colnames(DF.output) <- tolower(colnames(DF.output));
        DF.output[,"year"]  <- as.numeric(as.character(DF.output[,"year"]));

        DF.output <- DF.output[,setdiff(get.retained.variables(),"weekofmax7")];

        DF.output <- DF.output[DF.output[,"cropsurv"] %in% get.retained.cropsurv(),];

        DF.output[,'weekofmax7'] <- apply(
            X      = DF.output[,paste0("ndvi",16:31)],
            MARGIN = 1,
            FUN    = function(x) {
                return( 15 + min(which(x == max(x))) );
                }
            );

        DF.output <- DF.output[,get.retained.variables()];

        cat(paste0("\nsaving to file: ",FILE.output.RData,"\n"));
        saveRDS(object = DF.output, file = FILE.output.RData);

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( !file.exists(FILE.output.csv) ) {

        #cat(paste0("\nwriting file: ",FILE.output.csv,"\n"));
        #write.csv(
        #    file      = FILE.output.csv,
        #    x         = DF.output,
        #    row.names = FALSE
        #    );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\nexiting: ",this.function.name,"()"));
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    return( DF.output );

    }

##################################################
get.retained.cropsurv <- function() {
    retained.cropsurv <- c(
        "CANOLA",
        "WHTSPG_HR",
        "SOYBNS",
        "OATS",
        "BARLEY",
        "WHTWIN",
        "CORNGR",
        "POTATO",
        "FLAXSD",
        "SUNFLS"
        );
    return( retained.cropsurv );
    }

get.retained.variables <- function() {
    retained.variables <- c(
        "year",
        "qstrm",
        "yield",
        "cropsurv",
        "car16uid",
        "ymecoreg",
        "latitude",
        "longitude",
        "harvacre",
        paste0("ndvi",16:31),
        "ndvimax7",
        "weekofmax7",
        "ndvitot7",
        "emergwk",
        "gppmax",
        "seeding_jday",
        paste0("sumpcpn",    18:31),
        paste0("sumheatd",   18:31),
        paste0("sumfrostd",  18:31),
        paste0("sumchu",     18:31),
        paste0("avgsi",      18:31),
        paste0("avgprcnawhc",18:31),
        paste0("sumpcpn_",    c( 5, 6, 7)),
        paste0("sumpcpn_",    c(56,57,67)),
        paste0("sumegdd_c_",  c( 5, 6, 7)),
        paste0("sumegdd_c_",  c(56,57,67)),
        paste0("sumchu_",     c( 5, 6, 7)),
        paste0("sumchu_",     c(56,57,67)),
        paste0("sumheatd_",   c( 5, 6, 7)),
        paste0("sumfrostd_",  c( 5, 6, 7)),
        paste0("sumfrostd_",  c(56,57,67)),
        paste0("avgsi_",      c( 5, 6, 7)),
        paste0("avgsi_",      c(56,   67)),  ## Why is avgsi_57 missing ???
        paste0("avgprcnawhc_",c( 5, 6, 7)),
        paste0("sdpcpn_",     c( 5, 6, 7)),
        paste0("sdsi_",       c( 5, 6, 7)),
        paste0("sdegdd_c_",   c( 5, 6, 7)),
        paste0("sdchu_",      c( 5, 6, 7))
        );
    return( retained.variables );
    }
