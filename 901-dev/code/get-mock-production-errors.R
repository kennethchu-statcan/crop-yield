
get.mock.production.errors <- function(
    list.performance.metrics = NULL,
    validation.window        = NULL,
    output.directory         = NULL
    ) {

    this.function.name <- "get.mock.production.errors";
    logger::log_info('{this.function.name}(): starts');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( !dir.exists(output.directory) ) {
        dir.create(path = output.directory, recursive = TRUE);
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.mock.production.errors <- list();
    for ( temp.name in names(list.performance.metrics) ) {
        temp.comparison <- get.mock.production.errors_single.model(
            prefix                 = temp.name,
            validation.window      = validation.window,
            DF.performance.metrics = list.performance.metrics[[ temp.name ]]
            );
        list.mock.production.errors[[ temp.name ]] <- temp.comparison;
        }

    FILE.output <- file.path(output.directory,"list-mock-production-errors.RData");
    base::saveRDS(
        file   = FILE.output,
        object = list.mock.production.errors
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( prefix in names(list.mock.production.errors) ) {

        temp.filename <- file.path(output.directory,paste0("mock-production-errors-",prefix,"-diagnostics.csv"));
        if ( !file.exists(temp.filename) ) {
            write.csv(
                x         = list.mock.production.errors[[ prefix ]][[ "diagnostics" ]],
                file      = temp.filename,
                row.names = FALSE
                );
            }

        temp.filename <- file.path(output.directory,paste0("mock-production-errors-",prefix,".csv"));
        if ( !file.exists(temp.filename) ) {
            write.csv(
                x         = list.mock.production.errors[[ prefix ]][[ "mock_production_errors" ]],
                file      = temp.filename,
                row.names = FALSE
                );
            }

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_info('{this.function.name}(): exits');
    return( list.mock.production.errors );

    }

##################################################
get.mock.production.errors_single.model <- function(
    prefix                 = NULL,
    validation.window      = NULL,
    DF.performance.metrics = NULL
    ) {

    require(dplyr);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    production.years <- get.mock.production.errors_get.production.years(
        years             = sort(unique(DF.performance.metrics[,"year"])),
        validation.window = validation.window
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.diagnostics <- data.frame();
    for ( production.year in production.years ) {
        temp.validation.years <- seq(production.year-validation.window,production.year-1,1);
        DF.temp <- DF.performance.metrics %>%
            dplyr::filter( year %in% temp.validation.years ) %>%
            dplyr::select( model, weighted_error, weighted_std ) %>%
            dplyr::group_by( model ) %>%
            dplyr::summarize(
                mean_weighted_error = mean(weighted_error), 
                mean_weighted_std   = mean(weighted_std)
                );
        DF.temp <- as.data.frame(DF.temp);
        DF.temp[,"production_year"] <- production.year;
        DF.diagnostics <- rbind(DF.diagnostics,DF.temp);
        }

    DF.diagnostics[,"composite_metric"] <- apply(
        X      = DF.diagnostics[,c("mean_weighted_error","mean_weighted_std")],
        MARGIN = 1,
        FUN    = function(x) { return(sum(x)/sqrt(2)); }
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.diagnostics <- dplyr::left_join(
        x  = DF.diagnostics,
        y  = DF.performance.metrics[,c("model","year","weighted_error")],
        by = c("production_year" = "year", "model" = "model") 
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    reordered.colnames <- c("production_year",setdiff(colnames(DF.diagnostics),"production_year"));
    DF.diagnostics     <- DF.diagnostics[,reordered.colnames];

    colnames(DF.diagnostics) <- base::gsub(
        x           = colnames(DF.diagnostics),
        pattern     = "mean_weighted_error",
        replacement = "validation_error"
        );

    colnames(DF.diagnostics) <- base::gsub(
        x           = colnames(DF.diagnostics),
        pattern     = "mean_weighted_std",
        replacement = "validation_std"
        );

    colnames(DF.diagnostics) <- base::gsub(
        x           = colnames(DF.diagnostics),
        pattern     = "weighted_error",
        replacement = "mock_production_error"
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.mock.production.errors = DF.diagnostics %>%
        dplyr::group_by( production_year ) %>%
        dplyr::mutate( min_composite_metric =  min(composite_metric) ) %>%
        dplyr::filter(     composite_metric == min_composite_metric  );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return(
        list(
            diagnostics            = DF.diagnostics,
            mock_production_errors = DF.mock.production.errors
            )
        );

    }

get.mock.production.errors_get.production.years <- function(
    years             = NULL,
    validation.window = NULL
    ) {
    first.production.year <- min(years) + validation.window;
     last.production.year <- max(years) + 1;
    return( seq(first.production.year,last.production.year,1) );
    }

