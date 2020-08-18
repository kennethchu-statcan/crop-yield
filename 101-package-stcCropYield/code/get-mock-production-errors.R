
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

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    production.years <- get.mock.production.errors_get.production.years(
        years             = base::sort(base::unique(DF.performance.metrics[,"year"])),
        validation.window = validation.window
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.diagnostics <- base::data.frame();
    for ( production.year in production.years ) {
        temp.validation.years <- base::seq(production.year-validation.window,production.year-1,1);

#        DF.temp <- dplyr::filter(DF.performance.metrics, year %in% temp.validation.years);
#        DF.temp <- dplyr::select(DF.temp,model,weighted_error,weighted_std);
#        DF.temp <- dplyr::group_by(DF.temp,model);
#        DF.temp <- dplyr::summarize(
#            DF.temp,
#            mean_weighted_error = mean(weighted_error),
#            mean_weighted_std   = mean(weighted_std)
#            );
#        DF.temp <- base::as.data.frame(DF.temp);

        DF.temp <- base::suppressMessages(
            expr = DF.performance.metrics %>%
                dplyr::filter(.data[[year]] %in% temp.validation.years) %>%
                dplyr::select(.data[[model]],.data[[weighted_error]],.data[[weighted_std]]) %>%
                dplyr::group_by(.data[[model]]) %>%
                dplyr::summarize(
                    mean_weighted_error = mean(.data[[weighted_error]]),
                    mean_weighted_std   = mean(.data[[weighted_std]])
                    )
            ); 
        DF.temp <- base::as.data.frame(DF.temp);

        DF.temp[,"production_year"] <- production.year;
        DF.diagnostics <- base::rbind(DF.diagnostics,DF.temp);
        }

    DF.diagnostics[,"composite_metric"] <- base::apply(
        X      = DF.diagnostics[,base::c("mean_weighted_error","mean_weighted_std")],
        MARGIN = 1,
        FUN    = function(x) { return(base::sum(x)/base::sqrt(2)); }
        );

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
    DF.mock.production.errors <- base::suppressMessages(
        expr = DF.diagnostics %>% dplyr::group_by(.data[[production_year]]) %>%
            dplyr::mutate(min_composite_metric =  min(.data[[composite_metric]])) %>%
            dplyr::filter(    composite_metric == .data[[min_composite_metric]] )
        );
    DF.mock.production.errors <- as.data.frame(DF.mock.production.errors);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
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

