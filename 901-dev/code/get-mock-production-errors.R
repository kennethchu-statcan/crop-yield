
get.mock.production.errors <- function(
    list.performance.metrics = NULL,
    validation.window        = NULL,
    FILE.output              = "list-mock-production-errors.RData"
    ) {

    this.function.name <- "get.mock.production.errors";
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    cat(paste0("starting: ",this.function.name,"()\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if (file.exists(FILE.output)) {

        list.mock.production.errors <- base::readRDS(file = FILE.output);

    } else {

        list.mock.production.errors <- list();
        for ( temp.name in names(list.performance.metrics) ) {
            cat(paste0("\n### technique: ",temp.name));
            temp.comparison <- get.mock.production.errors_single.model(
                prefix                 = temp.name,
                validation.window      = validation.window,
                DF.performance.metrics = list.performance.metrics[[ temp.name ]]
                );
            list.mock.production.errors[[ temp.name ]] <- temp.comparison;
            }

        base::saveRDS(
            file   = FILE.output,
            object = list.mock.production.errors
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    for ( prefix in names(list.mock.production.errors) ) {

        #temp.filename <- paste0("mock-production-errors-",prefix,".csv");
        #if ( !file.exists(temp.filename) ) {
        #    write.csv(
        #        x         = list.mock.production.errors[[ prefix ]][[ "mock_production_errors" ]],
        #        file      = temp.filename,
        #        row.names = FALSE
        #        );
        #    }

        temp.filename <- paste0("mock-production-errors-",prefix,"-diagnostics.csv");
        if ( !file.exists(temp.filename) ) {
            write.csv(
                x         = list.mock.production.errors[[ prefix ]][[ "diagnostics" ]],
                file      = temp.filename,
                row.names = FALSE
                );
            }

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\nexiting: ",this.function.name,"()"));
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
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

    reordered.colnames <- c("production_year",setdiff(colnames(DF.diagnostics),"production_year"));
    DF.diagnostics     <- DF.diagnostics[,reordered.colnames];

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return(
        list(
            diagnostics            = DF.diagnostics,
            mock.production.errors = NULL
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

