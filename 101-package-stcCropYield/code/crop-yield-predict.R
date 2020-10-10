#' Crop yield prediction
#'
#' Computes parcel-level crop yield predictions based on
#' a given trained prediction model and a data frame of predictor variables.
#'
#' @param trained.model data frame containing crop yield data. See Details below for more information.
#'
#' @param DF.predictors data frame containing crop yield data. See Details below for more information.
#'
#' @return data frame obtained by augmented DF.predictors with predictions produced with the given trained model.
#'
#' @examples
#' \dontrun{
#' set.seed(13);
#'
#' n.ecoregions    <-   3;
#' n.crops         <-   5;
#' n.predictors    <-   7;
#' avg.n.parcels   <- 100;
#' min.num.parcels <-  50;
#'
#' DF.training <- stcCropYield::getData.synthetic(
#'     years         = seq(2011,2020),
#'     n.ecoregions  = n.ecoregions,
#'     n.crops       = n.crops,
#'     n.predictors  = n.predictors,
#'     avg.n.parcels = avg.n.parcels
#'     )
#'
#' DF.production <- stcCropYield::getData.synthetic(
#'     years        = c(2021),
#'     n.ecoregions = n.ecoregions,
#'     n.crops      = n.crops,
#'     n.predictors = n.predictors,
#'     avg.n.parcels = avg.n.parcels
#'     );
#' DF.production <- DF.production[,setdiff(colnames(DF.production),c("my_yield","my_harvested_area","my_evaluation_weight"))]
#'
#' stcCropYield::rollingWindowForwardValidation(
#'     training.window      = 2,
#'     validation.window    = 3,
#'     DF.input             = DF.training,
#'     year                 = "my_year",
#'     ecoregion            = "my_ecoregion",
#'     crop                 = "my_crop",
#'     response.variable    = "my_yield",
#'     harvested.area       = "my_harvested_area",
#'     seeded.area          = "my_seeded_area",
#'     evaluation.weight    = "my_evaluation_weight",
#'     predictors           = predictor.colnames,
#'     min.num.parcels      = min.num.parcels,
#'     learner              = "xgboost_multiphase",
#'     by.variables.phase01 = c("my_ecoregion","my_crop"),
#'     by.variables.phase02 = c("my_crop"),
#'     by.variables.phase03 = c("my_ecoregion"),
#'     search.grid = list(
#'         alpha  = c(1,12,23),
#'         lambda = c(1,12,23)
#'         ),
#'     output.directory = "rwFV",
#'     log.threshold    = logger::ERROR
#'     );
#'
#' RData.trained.model <- list.files( path = "rwFV", pattern = "\\.RData$" );
#' RData.trained.model <- file.path("rwFV",RData.trained.model);
#'
#' DF.predictions <- stcCropYield::crop.yield.predict(
#'    trained.model = RData.trained.model,
#'    DF.predictors = DF.production
#'    );
#' }
#'
#' @export

crop.yield.predict <- function(
    trained.model = NULL,
    DF.predictors = NULL
    ) {

    input.validity.checks_predict(
        trained.model = trained.model,
        DF.predictors = DF.predictors
        );

    if ( base::is.character(trained.model) ) {
        trained.model <- readRDS(file = trained.model);
        }

    DF.predictions <- trained.model$predict(newdata = DF.predictors);

    base::return( DF.predictions );

    }
