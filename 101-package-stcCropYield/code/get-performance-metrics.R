
get.performance.metrics <- function(
    list.prediction.directories = NULL,
    output.directory            = NULL
    ) {

    this.function.name <- "get.performance.metrics";
    logger::log_info('{this.function.name}(): starts');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( !base::dir.exists(output.directory) ) {
        base::dir.create(path = output.directory, recursive = TRUE);
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.performance.metrics <- base::list();
    for ( temp.name in base::names(list.prediction.directories) ) {
        temp.dir        <- list.prediction.directories[[ temp.name ]];
        temp.comparison <- get.performance.metrics_single.model(
            prefix      = temp.name,
            dir.results = temp.dir
            );
        list.performance.metrics[[ temp.name ]] <- temp.comparison;
        }

    FILE.output <- base::file.path(output.directory,"list-performance-metrics.RData");
    base::saveRDS(
        file   = FILE.output,
        object = list.performance.metrics
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( prefix in base::names(list.prediction.directories) ) {
        temp.filename <- base::file.path(output.directory,base::paste0("metrics-",prefix,"-model-year.csv"));
        if ( !base::file.exists(temp.filename) ) {
            utils::write.csv(
                x         = list.performance.metrics[[ prefix ]],
                file      = temp.filename,
                row.names = FALSE
                );
            }
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_info('{this.function.name}(): exits');
    base::return( list.performance.metrics );

    }

##################################################
get.performance.metrics_single.model <- function(
    prefix      = NULL,
    dir.results = NULL
    ) {

    modelIDs <- base::list.files(dir.results);

    DF.errors.model.year <- base::data.frame();
    for ( modelID in modelIDs ) {

        folder.model     <- base::file.path(dir.results,modelID);
        validation.years <- base::list.files(folder.model);

        for ( validation.year in validation.years ) {

            logger::log_info('get.performance.metrics_single.model(): ({modelID},{validation.year})');

            folder.year <- base::file.path(folder.model,validation.year);
            errors.csv  <- base::list.files(path = folder.year, pattern = 'region-crop.csv');
            csvdata     <- base::as.data.frame(utils::read.csv( base::file.path(folder.year,errors.csv) ));

            #csvdata$weights<- (csvdata$actual_production) / base::sum(csvdata$actual_production);
            csvdata$weights <- (csvdata$evaluation_weight) / base::sum(csvdata$evaluation_weight);
            weighted_error  <- weighted.mean(x = csvdata$relative_error, weights = csvdata$weights);
            weighted_std    <- weighted.sd(  x = csvdata$relative_error, weights = csvdata$weights);

            newdata <- base::data.frame(
                model          = modelID,
                year           = base::as.numeric(validation.year),
                weighted_error = weighted_error,
                weighted_std   = weighted_std
                );

            DF.errors.model.year <- base::rbind(DF.errors.model.year,newdata);

            }
        }

    #DF.errors.model.year[,"composite_metric"] <- base::apply(
    #    X      = DF.errors.model.year[,base::c("weighted_error","weighted_std")],
    #    MARGIN = 1,
    #    FUN    = function(x) { return(base::sum(x)/base::sqrt(2)); }
    #    );

    base::return( DF.errors.model.year );

    }
