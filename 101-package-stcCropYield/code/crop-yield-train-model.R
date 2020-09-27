#' Train crop yield prediction model for one reference year
#'
#' This function produces a trained model for crop yield prediction for any
#' reference year, given appropriate training data from preceding reference
#' years. No hyperparameter tuning is done. Instead it is assumed that the
#' user has already chosen a particular hyperparameter configuration.
#' This function trains the model subject to this chosen hyperparameter
#' configuration.
#'
#' @param FILE.trained.model data frame containing crop yield data. See Details below for more information.
#'
#' @param DF.training data frame containing crop yield data. See Details below for more information.
#'
#' @param learner.metadata list of key-value pairs, specifying learner metadata. See Details below.
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
#
#' @param hyperparameters list of numeric vectors each of length 1,
#' indicating the single hyperparameter configuration to be used for training.
#'
#' @param log.threshold log threshold.
#' Must be one of the log levels supported by the \code{logger} package. Default: logger::INFO
#'
#' @return an instance of class \code{learner}, e.g. "xgboost_multiphase".
#'
#' @examples
#' \dontrun{
#' set.seed(7654321);
#' n.ecoregions    <-   3;
#' n.crops         <-   5;
#' n.predictors    <-   7;
#' avg.n.parcels   <- 100;
#' min.num.parcels <-  50;
#'
#' DF.synthetic <- getData.synthetic(
#'     years         = base::seq(2015,2020),
#'     n.ecoregions  = n.ecoregions,
#'     n.crops       = n.crops,
#'     n.predictors  = n.predictors,
#'     avg.n.parcels = avg.n.parcels
#'     );
#'
#' DF.training   <- DF.synthetic[DF.synthetic[,"my_year"] != 2020,];
#' DF.production <- DF.synthetic[DF.synthetic[,"my_year"] == 2020,];
#'
#' trained.model <- crop.yield.train.model(
#'     DF.training          = DF.training,
#'     year                 = "my_year",
#'     ecoregion            = "my_ecoregion",
#'     crop                 = "my_crop",
#'     response.variable    = "my_yield",
#'     predictors           = base::grep(x = base::colnames(DF.training), pattern = "x[0-9]*", value = TRUE),
#'     min.num.parcels      = min.num.parcels,
#'     learner              = "xgboost_multiphase",
#'     by.variables.phase01 = base::c("my_ecoregion","my_crop"),
#'     by.variables.phase02 = base::c("my_crop"),
#'     by.variables.phase03 = base::c("my_ecoregion"),
#'     hyperparameters      = base::list(alpha = 23, lambda = 23)
#'     );
#'
#' DF.predictions <- stcCropYield::crop.yield.predict(
#'    trained.model = trained.model,
#'    DF.predictors = DF.production
#'    );
#' }
#'
#' @export

crop.yield.train.model <- function(
    FILE.trained.model   = NULL,
    DF.training          = NULL,
    learner.metadata     = NULL,
    year                 = NULL,
    ecoregion            = NULL,
    crop                 = NULL,
    response.variable    = NULL,
    predictors           = NULL,
    min.num.parcels      = NULL,
    learner              = NULL,
    by.variables.phase01 = NULL,
    by.variables.phase02 = NULL,
    by.variables.phase03 = NULL,
    hyperparameters      = NULL,
    log.threshold        = logger::INFO
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    this.function.name <- "crop.yield.train.model";

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    log.threshold.original <- logger::log_threshold();
    logger::log_threshold(level = log.threshold);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    input.validty.checks_crop.yield.train.model(
        FILE.trained.model   = FILE.trained.model,
        DF.training          = DF.training,
        learner.metadata     = learner.metadata,
        year                 = year,
        ecoregion            = ecoregion,
        crop                 = crop,
        response.variable    = response.variable,
        predictors           = predictors,
        min.num.parcels      = min.num.parcels,
        learner              = learner,
        by.variables.phase01 = by.variables.phase01,
        by.variables.phase02 = by.variables.phase02,
        by.variables.phase03 = by.variables.phase03,
        hyperparameters      = hyperparameters
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( is.null(learner.metadata) ) {
        learner.metadata <- get.learner.metadata_private.helper(
            year                 = year,
            ecoregion            = ecoregion,
            crop                 = crop,
            response.variable    = response.variable,
            predictors           = predictors,
            min.num.parcels      = min.num.parcels,
            learner              = learner,
            by.variables.phase01 = by.variables.phase01,
            by.variables.phase02 = by.variables.phase02,
            by.variables.phase03 = by.variables.phase03,
            search.grid          = hyperparameters
            );
        learner.metadata <- learner.metadata[[1]];
        }

    logger::log_debug('{this.function.name}(): learner.metadata:\n{paste0(capture.output(print(learner.metadata)),collapse="\n")}');

    trained.model <- getLearner(
        learner.metadata = learner.metadata,
        DF.training      = DF.training
        );

    trained.model$fit();

    if ( !base::is.null(FILE.trained.model) ) {
        base::saveRDS(
            object = trained.model,
            file   = FILE.trained.model
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_threshold(level = log.threshold.original);
    base::return( trained.model );

    }

##################################################
input.validty.checks_crop.yield.train.model <- function(
    FILE.trained.model   = NULL,
    DF.training          = NULL,
    learner.metadata     = NULL,
    year                 = NULL,
    ecoregion            = NULL,
    crop                 = NULL,
    response.variable    = NULL,
    predictors           = NULL,
    min.num.parcels      = NULL,
    learner              = NULL,
    by.variables.phase01 = NULL,
    by.variables.phase02 = NULL,
    by.variables.phase03 = NULL,
    hyperparameters      = NULL
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    this.function.name <- "input.validty.checks_crop.yield.train.model";
    logger::log_debug('{this.function.name}(): starts');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_debug('{this.function.name}(): FILE.trained.model = {capture.output({FILE.trained.model})}');
    logger::log_debug('{this.function.name}(): str(DF.training):\n{paste0(capture.output(str(DF.training)),collapse="\n")}');
    logger::log_debug('{this.function.name}(): learner.metadata:\n{paste0(capture.output(print(learner.metadata)),collapse="\n")}');
    logger::log_debug('{this.function.name}(): year = {year}');
    logger::log_debug('{this.function.name}(): ecoregion = {ecoregion}');
    logger::log_debug('{this.function.name}(): crop = {crop}');
    logger::log_debug('{this.function.name}(): response.variable = {response.variable}');
    logger::log_debug('{this.function.name}(): predictors:\n{paste0(predictors,collapse="\n")}');
    logger::log_debug('{this.function.name}(): min.num.parcels = {min.num.parcels}');
    logger::log_debug('{this.function.name}(): learner = {learner}');
    logger::log_debug('{this.function.name}(): by.variables.phase01:\n{paste0(capture.output(by.variables.phase01),collapse="\n")}');
    logger::log_debug('{this.function.name}(): by.variables.phase02:\n{paste0(capture.output(by.variables.phase02),collapse="\n")}');
    logger::log_debug('{this.function.name}(): by.variables.phase03:\n{paste0(capture.output(by.variables.phase03),collapse="\n")}');
    logger::log_debug('{this.function.name}(): hyperparameters:\n{paste0(capture.output(hyperparameters),collapse="\n")}');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( !is.null(learner.metadata) ) {

        base::stopifnot(
            base::is.null(year),
            base::is.null(ecoregion),
            base::is.null(crop),
            base::is.null(response.variable),
            base::is.null(predictors),
            base::is.null(learner),
            base::is.null(by.variables.phase01),
            base::is.null(by.variables.phase02),
            base::is.null(by.variables.phase03),
            base::is.null(hyperparameters)
            );

        base::stopifnot(
           base::is.list(learner.metadata)
           );

        base::stopifnot(base::all(
            base::c('year','ecoregion','crop','response_variable','predictors','learner','min_num_parcels','by_variables_phase01','by_variables_phase02','by_variables_phase03','hyperparameters') %in% base::names(learner.metadata)
            ));

        year                 <- learner.metadata[['year']];
        ecoregion            <- learner.metadata[['ecoregion']];
        crop                 <- learner.metadata[['crop']];
        response.variable    <- learner.metadata[['response_variable']];
        predictors           <- learner.metadata[['predictors']];
        learner              <- learner.metadata[['learner']];
        min.num.parcels      <- learner.metadata[['min_num_parcels']];
        by.variables.phase01 <- learner.metadata[['by_variables_phase01']];
        by.variables.phase02 <- learner.metadata[['by_variables_phase02']];
        by.variables.phase03 <- learner.metadata[['by_variables_phase03']];
        hyperparameters      <- learner.metadata[['hyperparameters']];

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    input.validity.checks_learner.metadata(
        DF.input             = DF.training,
        learner              = learner,
        ecoregion            = ecoregion,
        crop                 = crop,
        min.num.parcels      = min.num.parcels,
        by.variables.phase01 = by.variables.phase01,
        by.variables.phase02 = by.variables.phase02,
        by.variables.phase03 = by.variables.phase03,
        search.grid          = hyperparameters,
        single.configuration = TRUE
        );

    input.validity.checks_variables.needed.for.prediction(
        DF.input   = DF.training,
        ecoregion  = ecoregion,
        crop       = crop,
        predictors = predictors
        );

    input.validity.checks_variables.needed.for.training(
        DF.input             = DF.training,
        year                 = year,
        response.variable    = response.variable,
        single.configuration = TRUE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_debug('{this.function.name}(): exits');
    return( NULL );

    }
