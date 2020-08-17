#' Train crop yield prediction model for one reference year
#'
#' This function produces a trained model for crop yield prediction for any
#' reference year, given appropriate training data from preceding reference
#' years. No hyperparameter tuning is done. Instead it is assumed that the
#' user has already chosen a particular hyperparameter configuration.
#' This function trains the model subject to this chosen hyperparameter
#' configuration. 
#' 
#' @param training.window integer vector of length 1.
#' The number of years to use for each round of training. See Details below.
#'
#' @param DF.training data frame containing crop yield data. See Details below for more information.
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
# 
#' @param learner character vector of length 1,
#' must be one of c("xgboost_multiphase"), c("rlm_multiphase"), c("lm_multiphase")
#'
#' @param hyperparameters list of numeric vectors each of length 1,
#' indicating the single hyperparameter configuration to be used for training.
#' 
#' @param FILE.trained.model data frame containing crop yield data. See Details below for more information.
#'
#' @return an instance of class \code{learner}, e.g. "xgboost_multiphase".
#' 
#' @examples
#'
#' @export

crop.yield.train.model <- function(
    training.window      = NULL,
    DF.training          = NULL,
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
    hyperparameters      = base::list(alpha = 23, lambda = 23, lambda_bias = 23),
    FILE.trained.model   = NULL
    ) {

    learner.metadata <- get.learner.metadata_private.helper(
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
        search.grid          = hyperparameters
        );

    trained.model <- getLearner(
        learner.metadata = learner.metadata[[1]],
        DF.training      = DF.training
        );

    trained.model$fit();

    if ( !base::is.null(FILE.trained.model) ) {
        base::saveRDS(
            object = trained.model,
            file   = FILE.trained.model
            );
        }

    base::return( trained.model );

    }
