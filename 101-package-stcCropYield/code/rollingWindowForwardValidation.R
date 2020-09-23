#' Rolling Window Forward Validation for Crop Yield Prediction
#'
#' This function produces a trained model for crop yield prediction for any
#' reference year, given appropriate training data from preceding reference
#' years. It implements the rolling window forward validation protocol
#' to execute a grid search in order to determine the optimal hyperparameter
#' configuration within the given hyperparameter grid for the chosen prediction
#' technique.
#'
#' @param training.window integer vector of length 1.
#' The number of years to use for each round of training. See Details below.
#'
#' @param validation.window integer vector of length 1.
#' The number of (validation) years to use for hyperparameter tuning for each
#' mock production. See Details below.
#'
#' @param DF.input data frame containing crop yield data. See Details below for more information.
#'
#' @param year character vector of length 1,
#' indicating the column name in \code{DF.input} for the calendar year variable.
#'
#' @param ecoregion character vector of length 1,
#' indicating column name in \code{DF.input} for the ecoregion variable.
#'
#' @param crop character vector of length 1,
#' indicating column name in \code{DF.input} for the crop type variable.
#'
#' @param response.variable character vector of length 1,
#' indicating column name in \code{DF.input} for the crop yield variable.
#'
#' @param harvested.area character vector of length 1,
#' indicating column name in \code{DF.input} for the harvested area variable.
#'
#' @param evaluation.weight character vector of length 1,
#' indicating column name in \code{DF.input} for the variable to be used
#' as evaluation weight.
#'
#' @param predictors character vector of arbitrary length,
#' indicating the column names in \code{DF.input} for the predictor variables
#' (such as NVDI measurements, weather measurements, etc.)
#'
#' @param min.num.parcels integer vector of length 1,
#' Must be positive.
#' During each round of training, if a subgroup of units
#' defined by the by-variable(s) has size strictly less than min.num.parcels,
#' then fitting is suppressed for that group of units.
#'
#' @param learner character vector of length 1,
#' must be one of c("xgboost_multiphase"), c("rlm_multiphase"), c("lm_multiphase")
#'
#' @param by.variables.phase01 character vector indicating the by-variables to use for Phase 1 prediction.
#' These must be column names in \code{DF.input} for categorical variables (character columns).
#' Default = c(ecoregion,crop).
#'
#' @param by.variables.phase02 character vector indicating the by-variables to use for Phase 2 prediction.
#' These must be column names in \code{DF.input} for categorical variables (character columns).
#' Default = c(crop)
#'
#' @param by.variables.phase03 character vector indicating the by-variables to use for Phase 3 prediction.
#' These must be column names in \code{DF.input} for categorical variables (character columns).
#' Default = c(ecoregion)
#'
#' @param search.grid list defining the search grid.
#' See Details and Examples below for more details.
#'
#' @param num.cores integer vector of length 1,
#' indicating the number of cores to be used. Must be positive.
#'
#' @param output.directory character vector of length 1,
#' indicating file path of the (output) directory to which all results will be written.
#'
#' @param log.threshold log threshold.
#' Must be one of the log levels supported by the \code{looger} package. Default: logger::INFO
#'
#' @param suppress.child.process.graphics logical.
#' If TRUE, generation of diagnostics graphics during child processes will be suppressed.
#' If FALSE, that will be enabled. Default is FALSE.
#'
#' @return NULL. This function writes all output to file within the given \code{output.directory}.
#'
#' @examples
#' \dontrun{
#' n.ecoregions <- 3;
#' n.crops      <- 5;
#' n.predictors <- 7;
#'
#' DF.synthetic <- getData.synthetic(
#'     years        = seq(2015,2020),
#'     n.ecoregions = n.ecoregions,
#'     n.crops      = n.crops,
#'     n.predictors = n.predictors,
#'     output.RData = NULL,
#'     output.csv   = NULL
#'     );
#'
#' rollingWindowForwardValidation(
#'     training.window      = 2,
#'     validation.window    = 3,
#'     DF.input             = DF.synthetic,
#'     year                 = "my_year",
#'     ecoregion            = "my_ecoregion",
#'     crop                 = "my_crop",
#'     response.variable    = "my_yield",
#'     harvested.area       = "my_harvested_area",
#'     evaluation.weight    = "my_evaluation_weight",
#'     predictors           = grep(x = colnames(DF.synthetic), pattern = "x[0-9]*", value = TRUE),
#'     by.variables.phase01 = c("my_ecoregion","my_crop"),
#'     by.variables.phase02 = c("my_crop"),
#'     by.variables.phase03 = c("my_ecoregion"),
#'     learner     = "xgboost_multiphase",
#'     search.grid = list(
#'         alpha       = seq(23,11,-8),
#'         lambda      = seq(23,11,-8),
#'         lambda_bias = c(23)
#'         ),
#'     output.directory = file.path(".","rwFV"),
#'     log.threshold  = logger::TRACE
#'     );
#' }
#' @export

rollingWindowForwardValidation <- function(
    training.window      = NULL,
    validation.window    = NULL,
    DF.input             = NULL,
    year                 = "year",
    ecoregion            = "ecoregion",
    crop                 = "crop",
    response.variable    = "yield",
    harvested.area       = "harvested_area",
    evaluation.weight    = "evaluation_weight",
    predictors           = NULL,
    min.num.parcels      = 50,
    learner              = "xgboost_multiphase",
    by.variables.phase01 = base::c(ecoregion,crop),
    by.variables.phase02 = base::c(crop),
    by.variables.phase03 = base::c(ecoregion),
    search.grid          = base::list(alpha = base::seq(23,11,-4), lambda = base::seq(23,11,-4), lambda_bias = base::seq(23,11,-4)),
    num.cores            = base::max(1,parallel::detectCores() - 1),
    output.directory     = base::paste0("rwFV.",base::gsub(x=base::Sys.time(),pattern="( |:)",replacement="-")),
    log.threshold        = logger::INFO,
    suppress.child.process.graphics = FALSE
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    this.function.name <- "rollingWindowForwardValidation";

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    log.threshold.original <- logger::log_threshold();
    logger::log_threshold(level = logger::INFO);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( !base::dir.exists(output.directory) ) {
        base::dir.create(path = output.directory, recursive = TRUE);
        }
    output.directory <- base::normalizePath(output.directory);

    logger::log_appender(logger::appender_console);
    logger::log_info('{this.function.name}(): All output and log files are written to: {output.directory}');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    log.file <- base::file.path(base::normalizePath(output.directory),paste0(this.function.name,".log"));
    logger::log_appender(logger::appender_file(file = log.file));
    logger::log_info('{this.function.name}(): starts');

    logger::log_threshold(level = log.threshold);
    logger::log_info('{this.function.name}(): logger::log_threshold(): {attr(x = logger::log_threshold(), which = "level")}');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    rollingWindowForwardValidation_input.validity.checks(
        training.window      = training.window,
        validation.window    = validation.window,
        DF.input             = DF.input,
        year                 = year,
        ecoregion            = ecoregion,
        crop                 = crop,
        response.variable    = response.variable,
        harvested.area       = harvested.area,
        evaluation.weight    = evaluation.weight,
        predictors           = predictors,
        min.num.parcels      = min.num.parcels,
        learner              = learner,
        by.variables.phase01 = by.variables.phase01,
        by.variables.phase02 = by.variables.phase02,
        by.variables.phase03 = by.variables.phase03,
        search.grid          = search.grid
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            predictions.directory <- base::file.path(output.directory,"010-predictions");
    performance.metrics.directory <- base::file.path(output.directory,"020-performance-metrics");
       mock.productions.directory <- base::file.path(output.directory,"030-mock-productions");

    metadata.json <- base::file.path(predictions.directory,"learner-metadata.json");

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    learner.metadata <- get.learner.metadata(
        year                 = year,
        ecoregion            = ecoregion,
        crop                 = crop,
        response.variable    = response.variable,
        harvested.area       = harvested.area,
        evaluation.weight    = evaluation.weight,
        predictors           = predictors,
        by.variables.phase01 = by.variables.phase01,
        by.variables.phase02 = by.variables.phase02,
        by.variables.phase03 = by.variables.phase03,
        min.num.parcels      = min.num.parcels,
        learner              = learner,
        search.grid          = search.grid,
        output.directory     = predictions.directory,
        metadata.json        = metadata.json
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    rollingWindowForwardValidation_generate.predictions(
        DF.input         = DF.input,
        year             = year,
        training.window  = training.window,
        learner.metadata = learner.metadata,
        num.cores        = num.cores,
        output.directory = predictions.directory,
        log.threshold    = log.threshold,
        suppress.child.process.graphics = suppress.child.process.graphics
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.performance.metrics <- rollingWindowForwardValidation_generate.performance.metrics(
        metadata.json         = metadata.json,
        validation.window     = validation.window,
        predictions.directory = predictions.directory,
        output.sub.directory  = performance.metrics.directory
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.mock.production.errors <- rollingWindowForwardValidation_generate.mock.production.errors(
        validation.window        = validation.window,
        list.performance.metrics = list.performance.metrics,
        output.sub.directory     = mock.productions.directory
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    rollingWindowForwardValidation_visualize.results(
        list.performance.metrics    = list.performance.metrics,
        list.mock.production.errors = list.mock.production.errors,
        output.sub.directory        = mock.productions.directory
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.optimal.final.models <- rollingWindowForwardValidation_save.optimal.final.models(
        DF.input                    = DF.input,
        year                        = year,
        training.window             = training.window,
        learner.metadata            = learner.metadata,
        list.mock.production.errors = list.mock.production.errors,
        output.sub.directory        = output.directory
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_threshold(level = logger::INFO);
    logger::log_info("{this.function.name}(): getOption('repos'):\n{getOption('repos')}");
    logger::log_info("{this.function.name}(): .libPaths():\n{base::paste(.libPaths(),collapse='\n')}");
    logger::log_info("{this.function.name}(): warnings():\n{base::paste(utils::capture.output(base::warnings()),collapse='\n')}");
    logger::log_info("{this.function.name}(): sessionInfo():\n{base::paste(utils::capture.output(utils::sessionInfo()),collapse='\n')}");
    logger::log_info('{this.function.name}(): exits');
    logger::log_threshold(level = log.threshold.original);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    base::return( NULL );

    }

##################################################
#' @importFrom rlang .data
rollingWindowForwardValidation_visualize.results <- function(
    list.performance.metrics    = NULL,
    list.mock.production.errors = NULL,
    plot.limits.y               = c(  0.0,0.5),
    plot.breaks.y               = seq(0.0,0.5,0.1),
    output.sub.directory        = NULL
    ) {

    for ( temp.model in names(list.mock.production.errors) ) {

        DF.performance.metrics <- list.performance.metrics[[temp.model]];
        colnames(DF.performance.metrics) <- gsub(
            x           = colnames(DF.performance.metrics),
            pattern     = "year",
            replacement = "production_year"
            );
        DF.performance.metrics[,"production_year"] <- as.numeric(as.character(DF.performance.metrics[,"production_year"]));
        DF.performance.metrics <- DF.performance.metrics[!is.na(DF.performance.metrics[,"weighted_error"]),];

        DF.mock.production.errors <- list.mock.production.errors[[temp.model]][["mock_production_errors"]];
        DF.mock.production.errors[,"production_year"] <- as.numeric(as.character(DF.mock.production.errors[,"production_year"]));
        DF.mock.production.errors <- DF.mock.production.errors[!is.na(DF.mock.production.errors[,"mock_production_error"]),];

        my.ggplot <- initializePlot();
        my.ggplot <- my.ggplot + ggplot2::ggtitle(label = NULL, subtitle = temp.model);
        my.ggplot <- my.ggplot + ggplot2::geom_line(
            data      = DF.performance.metrics,
            mapping   = ggplot2::aes(x = .data$production_year, y = .data$weighted_error, group = .data$model),
            colour    = "black",
            #linetype = 2,
            alpha     = 0.05
            );
        my.ggplot <- my.ggplot + ggplot2::geom_point(
            data      = DF.mock.production.errors,
            mapping   = ggplot2::aes(x = .data$production_year, y = .data$mock_production_error),
            shape     = 21,            # plot character that allows customizable border
            fill      = 'orange',      # fill colour
            colour    = 'transparent', # no border
            size      = 5,
            alpha     = 0.60
            );
        my.ggplot <- my.ggplot + ggplot2::geom_point(
            data      = DF.mock.production.errors,
            mapping   = ggplot2::aes(x = .data$production_year, y = .data$mock_production_error),
            colour    = "orange",
            size      = 1,
            alpha     = 0.99
            );
        my.ggplot <- my.ggplot + ggplot2::geom_line(
            data      = DF.mock.production.errors,
            mapping   = ggplot2::aes(x = .data$production_year, y = .data$mock_production_error),
            colour    = "orange",
            size      = 1,
            alpha     = 0.90
            );
        x.min <- min(
            min(DF.performance.metrics[,   "production_year"]),
            min(DF.mock.production.errors[,"production_year"])
            );
        x.max <- max(
            max(DF.performance.metrics[,   "production_year"]),
            max(DF.mock.production.errors[,"production_year"])
            );
        x.min <- ifelse(0 == (x.min %% 2),x.min,x.min-1);
        x.max <- ifelse(0 == (x.max %% 2),x.max,x.max+1);
        plot.limits.x <- c(  x.min,x.max);
        plot.breaks.x <- seq(x.min,x.max,2);
        my.ggplot <- my.ggplot + ggplot2::scale_x_continuous(limits = plot.limits.x, breaks = plot.breaks.x);
        my.ggplot <- my.ggplot + ggplot2::scale_y_continuous(limits = plot.limits.y, breaks = plot.breaks.y);
        my.ggplot <- my.ggplot + ggplot2::xlab("production year");
        my.ggplot <- my.ggplot + ggplot2::ylab("mock production error");
        temp.file <- base::file.path(output.sub.directory,base::paste0("plot-",temp.model,"-mockProdErr-vs-year.png"));
        ggplot2::ggsave(
            plot   = my.ggplot,
            file   = temp.file,
            dpi    = 300,
            height =   8,
            width  =  16,
            units  = 'in'
            );

        }

    }

rollingWindowForwardValidation_save.optimal.final.models <- function(
    DF.input                    = NULL,
    year                        = NULL,
    training.window             = NULL,
    learner.metadata            = NULL,
    list.mock.production.errors = NULL,
    output.sub.directory        = NULL
    ) {

    max.year       <- base::max(DF.input[,year]);
    training.years <- base::seq(max.year - training.window + 1, max.year);
    DF.training    <- DF.input[DF.input[,year] %in%   training.years,];

    list.optimal.final.models <- base::list();
    for ( temp.name in base::names(list.mock.production.errors) ) {

        DF.temp       <- base::as.data.frame(list.mock.production.errors[[temp.name]][["mock_production_errors"]]);
        temp.year     <- DF.temp[base::nrow(DF.temp),"production_year"];
        temp.model.ID <- DF.temp[base::nrow(DF.temp),"model"];
        temp.filename <- base::paste0("production-model-RY",temp.year,"-",temp.model.ID,".RData");
        temp.metadata <- learner.metadata[[temp.model.ID]];

        temp.trained.model    <- crop.yield.train.model(
            learner.metadata   = temp.metadata,
            DF.training        = DF.training,
            FILE.trained.model = base::file.path(output.sub.directory,base::paste0(temp.filename))
            );
        list.optimal.final.models[[ temp.name ]] <- temp.trained.model;

        }

    base::return( list.optimal.final.models );

    }

#' @importFrom foreach foreach %dopar%
rollingWindowForwardValidation_generate.predictions <- function(
    DF.input         = NULL,
    year             = NULL,
    training.window  = NULL,
    learner.metadata = NULL,
    num.cores        = NULL,
    output.directory = NULL,
    log.threshold    = NULL,
    suppress.child.process.graphics = NULL
    ) {

    this.function.name <- "rollingWindowForwardValidation_generate.predictions";

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_threshold(level = log.threshold);
    logger::log_info('{this.function.name}(): starts');
    logger::log_info('{this.function.name}(): log.threshold: {attr(x = log.threshold, which = "level")}');
    logger::log_info('{this.function.name}(): logger::log_threshold(): {attr(x = logger::log_threshold(), which = "level")}');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    num.cores <- base::max(num.cores,1);
    num.cores <- base::min(num.cores,parallel::detectCores());
    doParallel::registerDoParallel(cores = num.cores);
    logger::log_info('{this.function.name}(): number of cores to be used in parallel: {num.cores}');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    min.validation.year <- base::min(DF.input[,year]) + training.window;
    max.validation.year <- base::max(DF.input[,year]);
    validation.years    <- base::seq(min.validation.year,max.validation.year,1);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_info('{this.function.name}(): training window: {training.window}');
    logger::log_info('{this.function.name}(): validation years: c({paste(validation.years,collapse=",")})');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    global.objects <- NULL;
    if ( "windows" == base::.Platform[["OS.type"]] ) {
        if ( !( "stcCropYield" %in% utils::installed.packages()[,1]) ) {
            global.objects      <- base::list();
            global.object.names <- base::ls(name = base::.GlobalEnv);
            for ( temp.object.name in global.object.names ) {
                temp.object <- base::get(x = temp.object.name, envir = base::.GlobalEnv);
                if ( base::is.function(temp.object) | ("R6ClassGenerator" == base::class(temp.object)) | (identical(class(temp.object),c("loglevel","integer"))) ) {
                    logger::log_debug('{this.function.name}(): replicating the following object from Global Environment into current environment: {temp.object.name}');
                    base::assign(x = temp.object.name, value = temp.object, envir = base::environment());
                    global.objects[[temp.object.name]] <- temp.object;
                    }
                }
            }
        }
    logger::log_debug('{this.function.name}(): environment(): {capture.output(environment())}');
    logger::log_debug('{this.function.name}(): ls(environment()):\n{paste(ls(environment()),collapse="\n")}');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    foreach::foreach (
        temp.index = 1:base::length(learner.metadata),
        .export    = base::ls(name = base::environment())
        ) %dopar% {

        learner.name <- base::names(learner.metadata)[temp.index];

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        original.log.threshold <- logger::log_threshold();
        log.file <- base::file.path(output.directory,paste0(learner.name,".log"));
        logger::log_appender(logger::appender_file(file = log.file));
        logger::log_threshold(level = logger::INFO);
        logger::log_info('{this.function.name}(foreach, temp.index = {temp.index}): starts');
        logger::log_threshold(level = log.threshold);
        logger::log_info( '{this.function.name}(foreach, temp.index = {temp.index}): logger::log_threshold(): {attr(x = logger::log_threshold(), which = "level")}');

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        logger::log_debug('{this.function.name}(foreach, temp.index = {temp.index}): environment(): {capture.output(environment())}');
        logger::log_debug('{this.function.name}(foreach, temp.index = {temp.index}): ls(environment()):\n{paste(ls(environment()),collapse="\n")}');

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        for (validation.year in validation.years) {

            log.prefix <- '{this.function.name}(): ({learner.name},{validation.year})';

            training.years <- base::seq(validation.year - training.window, validation.year - 1);
            logger::log_info(base::paste0(log.prefix,', training.years = c({paste(training.years,collapse=",")})'));

            DF.training <- DF.input[DF.input[,year] %in%   training.years,];
            logger::log_info(base::paste0(log.prefix,', nrow(DF.training) = {nrow(DF.training)})'));

            DF.validation <- DF.input[DF.input[,year] ==   validation.year, ];
            logger::log_info(base::paste0(log.prefix,', nrow(DF.validation) = {nrow(DF.validation)})'));

            validation.single.year(
                learner.name       = learner.name,
                validation.year    = validation.year,
                learner.metadata   = learner.metadata[[learner.name]],
                DF.training        = DF.training,
                DF.validation      = DF.validation,
                output.directory   = output.directory,
                global.objects     = global.objects,
                suppress.child.process.graphics = suppress.child.process.graphics
                );

            } # for (validation.year in validation.years)

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        logger::log_threshold(level = logger::INFO);
        logger::log_info('{this.function.name}(foreach, temp.index = {temp.index}): quits');
        logger::log_threshold(level = original.log.threshold);

        } # foreach::foreach ( temp.index = ... )

    logger::log_info('{this.function.name}(): exits');
    base::return( NULL );

    }

rollingWindowForwardValidation_generate.mock.production.errors <- function(
    validation.window        = NULL,
    list.performance.metrics = NULL,
    output.sub.directory     = NULL
    ) {

    if ( !base::dir.exists(output.sub.directory) ) {
        base::dir.create(path = output.sub.directory, recursive = TRUE);
        }

    list.mock.production.errors <- get.mock.production.errors(
        list.performance.metrics = list.performance.metrics,
        validation.window        = validation.window,
        output.directory         = output.sub.directory
        );

    base::return( list.mock.production.errors );

    }

rollingWindowForwardValidation_generate.performance.metrics <- function(
    metadata.json         = NULL,
    validation.window     = NULL,
    predictions.directory = NULL,
    output.sub.directory  = NULL
    ) {

    if ( !base::dir.exists(output.sub.directory) ) {
        base::dir.create(path = output.sub.directory, recursive = TRUE);
        }

    temp.json  <- jsonlite::read_json(metadata.json);
    model.name <- temp.json[[1]][["learner"]][[1]];

    list.prediction.directories <- base::list();
    list.prediction.directories[[model.name]] <- predictions.directory;

    list.performance.metrics <- get.performance.metrics(
        list.prediction.directories = list.prediction.directories,
        output.directory            = output.sub.directory
        );

    base::return( list.performance.metrics );

    }

rollingWindowForwardValidation_input.validity.checks <- function(
    training.window      = NULL,
    validation.window    = NULL,
    DF.input             = NULL,
    year                 = NULL,
    ecoregion            = NULL,
    crop                 = NULL,
    response.variable    = NULL,
    harvested.area       = NULL,
    evaluation.weight    = NULL,
    predictors           = NULL,
    min.num.parcels      = NULL,
    learner              = NULL,
    by.variables.phase01 = NULL,
    by.variables.phase02 = NULL,
    by.variables.phase03 = NULL,
    search.grid          = NULL,
    output.directory     = NULL
    ) {

    input.validity.checks_variables.needed.for.training(
        DF.input          = DF.input,
        year              = year,
        response.variable = response.variable,
        harvested.area    = harvested.area,
        evaluation.weight = evaluation.weight,
        min.num.parcels   = min.num.parcels
        );

    input.validity.checks_variables.needed.for.prediction(
        DF.input   = DF.input,
        ecoregion  = ecoregion,
        crop       = crop,
        predictors = predictors
        );

    # input.validity.checks_window.compatibility(
    #     training.window   = NULL,
    #     validation.window = NULL,
    #     DF.input          = NULL,
    #     year              = NULL
    #     );
    #
    # input.validity.checks_learner.metadata(
    #     learner              = NULL,
    #     DF.input             = NULL,
    #     ecoregion            = NULL,
    #     crop                 = NULL,
    #     min.num.parcels      = NULL,
    #     by.variables.phase01 = NULL,
    #     by.variables.phase02 = NULL,
    #     by.variables.phase03 = NULL,
    #     search.grid          = NULL
    #     );

    }
