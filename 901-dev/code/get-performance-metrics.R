
get.performance.metrics <- function(
    list.prediction.directories = NULL,
    FILE.output                 = "list-performance-metrics.RData"
    ) {

    this.function.name <- "get.performance.metrics";
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    cat(paste0("starting: ",this.function.name,"()\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if (file.exists(FILE.output)) {

        list.performance.metrics <- base::readRDS(file = FILE.output);

    } else {

        list.performance.metrics <- list();
        for ( temp.name in names(list.prediction.directories) ) {
            cat(paste0("\n### technique: ",temp.name));
            temp.dir        <- list.prediction.directories[[ temp.name ]];
            temp.comparison <- get.performance.metrics_single.model(
                prefix      = temp.name,
                dir.results = temp.dir
                );
            list.performance.metrics[[ temp.name ]] <- temp.comparison;
            }

        base::saveRDS(
            file   = FILE.output,
            object = list.performance.metrics
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( prefix in names(list.prediction.directories) ) {
        temp.filename <- paste0("metrics-",prefix,"-model-year.csv");
        if ( !file.exists(temp.filename) ) {
            write.csv(
                x         = list.performance.metrics[[ prefix ]],
                file      = temp.filename,
                row.names = FALSE
                );
            }
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\nexiting: ",this.function.name,"()"));
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    return( list.performance.metrics );

    }

##################################################
get.performance.metrics_single.model <- function(
    prefix      = NULL,
    dir.results = NULL
    ) {

    require(dplyr);

    modelIDs <- list.files(dir.results);

    DF.errors.model.year <- data.frame();
    for ( modelID in modelIDs ) {

        folder.model     <- file.path(dir.results,modelID);
        validation.years <- list.files(folder.model);

        for ( validation.year in validation.years ) {

            cat(paste0("\n(modelID, validation.year): (",modelID,", ",validation.year,")"));

            folder.year <- file.path(folder.model,validation.year);
            errors.csv  <- list.files(path = folder.year, pattern = 'region-crop.csv');
            csvdata     <- as.data.frame(read.csv( file.path(folder.year,errors.csv) ));

            csvdata$weights <- (csvdata$actual_production) / sum(csvdata$actual_production);
            weighted_error  <- weighted.mean(x = csvdata$relative_error, weights = csvdata$weights);
            weighted_std    <- weighted.sd(  x = csvdata$relative_error, weights = csvdata$weights);

            newdata <- data.frame(
            	model          = modelID,
            	year           = as.numeric(validation.year),
            	weighted_error = weighted_error,
            	weighted_std   = weighted_std
            	);

            DF.errors.model.year <- rbind(DF.errors.model.year,newdata);

            }
        }

    DF.errors.model.year[,"composite_metric"] <- apply(
        X      = DF.errors.model.year[,c("weighted_error","weighted_std")],
        MARGIN = 1,
        FUN    = function(x) { return(sum(x)/sqrt(2)); }
        );

    cat("\n");
    return( DF.errors.model.year );

    }

