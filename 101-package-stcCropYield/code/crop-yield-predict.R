#' Crop yield prediction
#'
#' Computes parcel-level crop yield predictions based on
#' a given trained prediction model and a data frame of predictor variables.
#'
#' @param FILE.trained.model data frame containing crop yield data. See Details below for more information.
#'
#' @param DF.predictors data frame containing crop yield data. See Details below for more information.
#'
#' @return data frame obtained by augmented DF.predictors with predictions produced with the given trained model.
#'
#' @examples
#'
#' @export

crop.yield.predict <- function(
    FILE.trained.model = NULL,
    DF.predictors      = NULL
    ) {
    trained.model  <- base::readRDS( file = FILE.trained.model );
    DF.predictions <- trained.model$predict(newdata = DF.predictors);
    base::return( DF.predictions );
    }
