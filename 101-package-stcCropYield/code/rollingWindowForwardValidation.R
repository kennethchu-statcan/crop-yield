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
#' @param predictors character vector of arbitrary length,
#' indicating the column names in \code{DF.input} for the predictor variables
#' (such as NVDI measurements, weather measurements, etc.)
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
#' @param learner character vector of length 1,
#' must be one of c("xgboost_multiphase"), c("rlm_multiphase"), c("lm_multiphase")
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
    predictors           = NULL,
    by.variables.phase01 = base::c(ecoregion,crop),
    by.variables.phase02 = base::c(crop),
    by.variables.phase03 = base::c(ecoregion),
    learner              = "xgboost_multiphase",
    search.grid          = base::list(alpha = base::seq(23,11,-4), lambda = base::seq(23,11,-4), lambda_bias = base::seq(23,11,-4)),
    num.cores            = base::max(1,parallel::detectCores() - 1),
    output.directory     = ".",
    log.threshold        = logger::INFO
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( !base::dir.exists(output.directory) ) {
        base::dir.create(path = output.directory, recursive = TRUE);
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    this.function.name <- "rollingWindowForwardValidation";
    log.file <- base::file.path(output.directory,paste0(this.function.name,".log"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_threshold(level = log.threshold);
    logger::log_appender(logger::appender_tee(file = log.file));
    logger::log_info('{this.function.name}(): starts');

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
        predictors           = predictors,
        by.variables.phase01 = by.variables.phase01,
        by.variables.phase02 = by.variables.phase02,
        by.variables.phase03 = by.variables.phase03,
        learner              = learner,
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
        predictors           = predictors,
        by.variables.phase01 = by.variables.phase01,
        by.variables.phase02 = by.variables.phase02,
        by.variables.phase03 = by.variables.phase03,
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
        output.directory = predictions.directory
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
    list.optimal.final.models <- rollingWindowForwardValidation_save.optimal.final.models(
        DF.input                    = DF.input,
        year                        = year,
        training.window             = training.window,
        learner.metadata            = learner.metadata,
        list.mock.production.errors = list.mock.production.errors,
        output.sub.directory        = output.directory
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_info("{this.function.name}(): getOption('repos'):\n{getOption('repos')}");
    logger::log_info("{this.function.name}(): .libPaths():\n{base::paste(.libPaths(),collapse='\n')}");
    logger::log_info("{this.function.name}(): warnings():\n{base::paste(utils::capture.output(base::warnings()),collapse='\n')}");
    logger::log_info("{this.function.name}(): sessionInfo():\n{base::paste(utils::capture.output(utils::sessionInfo()),collapse='\n')}");

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_info('{this.function.name}(): exits');
    base::return( NULL );

    }

##################################################
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
    output.directory = NULL
    ) {

    this.function.name <- "rollingWindowForwardValidation_generate.predictions";
    logger::log_info('{this.function.name}(): starts');

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
    global.objects   <- NULL;
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

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        learner.name <- base::names(learner.metadata)[temp.index];

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        log.file <- base::file.path(output.directory,paste0(learner.name,".log"));
        logger::log_appender(logger::appender_tee(file = log.file));
        logger::log_info('{this.function.name}(foreach, temp.index = {temp.index}): starts');

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
                global.objects     = global.objects
                );

            } # for (validation.year in validation.years)

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        logger::log_info('{this.function.name}(foreach, temp.index = {temp.index}): quits');

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
    predictors           = NULL,
    by.variables.phase01 = NULL,
    by.variables.phase02 = NULL,
    by.variables.phase03 = NULL,
    learner              = NULL,
    search.grid          = NULL,
    output.directory     = NULL
    ) {
 
    }

