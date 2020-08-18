
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

#' @importFrom magrittr %>%
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

#        DF.temp <- dplyr::filter(DF.performance.metrics, year %in% temp.validation.years);
#        DF.temp <- dplyr::select(DF.temp,model,weighted_error,weighted_std);
#        DF.temp <- dplyr::group_by(DF.temp,model);
#        DF.temp <- dplyr::summarize(
#            DF.temp,
#            mean_weighted_error = mean(weighted_error),
#            mean_weighted_std   = mean(weighted_std)
#            );
#        DF.temp <- base::as.data.frame(DF.temp);

        DF.temp <- DF.performance.metrics %>%
            dplyr::filter(rlang::.data$year %in% temp.validation.years) %>%
            dplyr::select(
                rlang::.data$model,
                rlang::.data$weighted_error,
                rlang::.data$weighted_std
                ) %>%
            dplyr::group_by(rlang::.data$model) %>%
            dplyr::summarize(
                mean_weighted_error = mean(rlang::.data$weighted_error),
                mean_weighted_std   = mean(rlang::.data$weighted_std)
                );
        DF.temp <- base::as.data.frame(DF.temp);

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
#    DF.mock.production.errors <- dplyr::group_by(DF.diagnostics, production_year);
#    DF.mock.production.errors <- dplyr::mutate(
#        DF.mock.production.errors,
#        min_composite_metric = min(composite_metric)
#        );
#    DF.mock.production.errors <- dplyr::filter(
#        DF.mock.production.errors,
#        composite_metric == min_composite_metric
#        );
    DF.mock.production.errors <- DF.diagnostics %>%
        dplyr::group_by(rlang::.data$production_year) %>%
        dplyr::mutate(         min_composite_metric =  min(rlang::.data$composite_metric)) %>%
        dplyr::filter(rlang::.data$composite_metric == rlang::.data$min_composite_metric );
        
    DF.mock.production.errors <- as.data.frame(DF.mock.production.errors);

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
    base::return( seq(first.production.year,last.production.year,1) );
    }

