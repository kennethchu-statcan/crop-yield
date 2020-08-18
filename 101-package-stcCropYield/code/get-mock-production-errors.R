
get.mock.production.errors <- function(
    list.performance.metrics = NULL,
    validation.window        = NULL,
    output.directory         = NULL
    ) {

    this.function.name <- "get.mock.production.errors";
    logger::log_info('{this.function.name}(): starts');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( !base::dir.exists(output.directory) ) {
        base::dir.create(path = output.directory, recursive = TRUE);
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.mock.production.errors <- base::list();
    for ( temp.name in base::names(list.performance.metrics) ) {
        temp.comparison <- get.mock.production.errors_single.model(
            prefix                 = temp.name,
            validation.window      = validation.window,
            DF.performance.metrics = list.performance.metrics[[ temp.name ]]
            );
        list.mock.production.errors[[ temp.name ]] <- temp.comparison;
        }

    FILE.output <- base::file.path(output.directory,"list-mock-production-errors.RData");
    base::saveRDS(
        file   = FILE.output,
        object = list.mock.production.errors
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( prefix in base::names(list.mock.production.errors) ) {

        temp.filename <- base::file.path(output.directory,base::paste0("mock-production-errors-",prefix,"-diagnostics.csv"));
        if ( !base::file.exists(temp.filename) ) {
            utils::write.csv(
                x         = list.mock.production.errors[[ prefix ]][[ "diagnostics" ]],
                file      = temp.filename,
                row.names = FALSE
                );
            }

        temp.filename <- base::file.path(output.directory,base::paste0("mock-production-errors-",prefix,".csv"));
        if ( !base::file.exists(temp.filename) ) {
            utils::write.csv(
                x         = list.mock.production.errors[[ prefix ]][[ "mock_production_errors" ]],
                file      = temp.filename,
                row.names = FALSE
                );
            }

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_info('{this.function.name}(): exits');
    base::return( list.mock.production.errors );

    }

##################################################
get.mock.production.errors_single.model <- function(
    prefix                 = NULL,
    validation.window      = NULL,
    DF.performance.metrics = NULL
    ) {

    this.function.name <- "get.mock.production.errors_single.model";
    logger::log_debug('{this.function.name}(): starts');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    production.years <- get.mock.production.errors_get.production.years(
        years             = base::sort(base::unique(DF.performance.metrics[,"year"])),
        validation.window = validation.window
        );

    logger::log_debug('{this.function.name}(): str(DF.performance.metrics):\n{paste(capture.output(str(DF.performance.metrics)),collapse="\n")}');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.diagnostics <- base::data.frame();
    for ( production.year in production.years ) {

        logger::log_debug('{this.function.name}(): production.year = {production.year}');

        temp.validation.years <- base::seq(production.year-validation.window,production.year-1,1);
        logger::log_debug('{this.function.name}(): production.year = {production.year}, validation.years = c({paste(temp.validation.years,collapse=",")})');

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        is.selected <- (DF.performance.metrics[,"year"] %in% temp.validation.years);
        DF.temp     <- DF.performance.metrics[is.selected,];
        DF.temp     <- DF.temp[,c("model","weighted_error","weighted_std")];

        DF.temp <- stats::aggregate(
            x   = DF.temp[,c("weighted_error","weighted_std")],
            by  = base::list(DF.temp$model),
            FUN = mean
            );

        base::colnames(DF.temp) <- base::gsub(
            x           = base::colnames(DF.temp),
            pattern     = "Group\\.1",
            replacement = "model"
            );

        base::colnames(DF.temp) <- base::gsub(
            x           = base::colnames(DF.temp),
            pattern     =      "weighted_",
            replacement = "mean_weighted_"
            );

        logger::log_debug('{this.function.name}(): production.year = {production.year}, str(DF.temp):\n{paste(capture.output(str(DF.temp)),collapse="\n")}');

        DF.temp[,"production_year"] <- production.year;
        DF.diagnostics <- base::rbind(DF.diagnostics,DF.temp);

        logger::log_debug('{this.function.name}(): production.year = {production.year}, computations of mean_weighted_error, mean_weighted_std complete');

        }

    DF.diagnostics[,"composite_metric"] <- base::apply(
        X      = DF.diagnostics[,base::c("mean_weighted_error","mean_weighted_std")],
        MARGIN = 1,
        FUN    = function(x) { return(base::sum(x)/base::sqrt(2)); }
        );

    logger::log_debug('{this.function.name}(): computation of composite_metric complete');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.diagnostics <- dplyr::left_join(
        x  = DF.diagnostics,
        y  = DF.performance.metrics[,base::c("model","year","weighted_error")],
        by = base::c("production_year" = "year", "model" = "model") 
        );
    DF.diagnostics <- base::as.data.frame(DF.diagnostics);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    reordered.colnames <- base::c("production_year",base::setdiff(base::colnames(DF.diagnostics),"production_year"));
    DF.diagnostics     <- DF.diagnostics[,reordered.colnames];

    base::colnames(DF.diagnostics) <- base::gsub(
        x           = base::colnames(DF.diagnostics),
        pattern     = "mean_weighted_error",
        replacement = "validation_error"
        );

    base::colnames(DF.diagnostics) <- base::gsub(
        x           = base::colnames(DF.diagnostics),
        pattern     = "mean_weighted_std",
        replacement = "validation_std"
        );

    base::colnames(DF.diagnostics) <- base::gsub(
        x           = base::colnames(DF.diagnostics),
        pattern     = "weighted_error",
        replacement = "mock_production_error"
        );

    logger::log_debug('{this.function.name}(): creation of DF.diagnostics complete');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.min.composite.metric <- stats::aggregate(
        x   = DF.diagnostics[,c("composite_metric","validation_error")],
        by  = base::list(DF.diagnostics$production_year),
        FUN = min
        );
    base::colnames(DF.min.composite.metric) <- base::gsub(
        x           = base::colnames(DF.min.composite.metric),
        pattern     = "Group\\.1",
        replacement = "production_year"
        );
    base::colnames(DF.min.composite.metric) <- base::gsub(
        x           = base::colnames(DF.min.composite.metric),
        pattern     =     "composite_metric",
        replacement = "min_composite_metric"
        );
    DF.min.composite.metric <- DF.min.composite.metric[,setdiff(colnames(DF.min.composite.metric),"validation_error")];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.mock.production.errors <- dplyr::left_join(
        x  = DF.diagnostics,
        y  = DF.min.composite.metric,
        by = "production_year"
        );
    DF.mock.production.errors <- base::as.data.frame(DF.mock.production.errors);

    is.selected <- (DF.mock.production.errors$composite_metric == DF.mock.production.errors$min_composite_metric);
    DF.mock.production.errors <- DF.mock.production.errors[is.selected,];

    logger::log_debug('{this.function.name}(): creation of DF.mock.production.errors complete');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_debug('{this.function.name}(): exits');
    base::return(
        base::list(
            diagnostics            = DF.diagnostics,
            mock_production_errors = DF.mock.production.errors
            )
        );

    }

get.mock.production.errors_get.production.years <- function(
    years             = NULL,
    validation.window = NULL
    ) {
    first.production.year <- base::min(years) + validation.window;
     last.production.year <- base::max(years) + 1;
    base::return( base::seq(first.production.year,last.production.year,1) );
    }

