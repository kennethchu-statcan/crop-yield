
input.validity.checks_learner.metadata <- function(
    DF.input             = NULL,
    learner              = NULL,
    ecoregion            = NULL,
    crop                 = NULL,
    min.num.parcels      = NULL,
    by.variables.phase01 = NULL,
    by.variables.phase02 = NULL,
    by.variables.phase03 = NULL,
    search.grid          = NULL
    ) {

    base::stopifnot(
        base::is.character(learner),
        base::length(learner) == 1,
        base::tolower(learner) %in% base::c("xgboost_multiphase","xgboost_bygroup","xgboost") # these are the learners handled by stcCropYield:::getLearner()
        );

    base::stopifnot(
        base::is.character(ecoregion),
        base::length(ecoregion) == 1
        );

    base::stopifnot(
        base::is.character(crop),
        base::length(crop) == 1
        );

    base::stopifnot(
        base::is.numeric(min.num.parcels),
        base::length(min.num.parcels) == 1,
        min.num.parcels == base::as.integer(min.num.parcels),
        min.num.parcels > 0
        );

    base::stopifnot(
        base::is.character(by.variables.phase01),
        base::is.character(by.variables.phase02),
        base::is.character(by.variables.phase03),
        base::length(by.variables.phase01) > 0,
        base::length(by.variables.phase02) > 0,
        base::length(by.variables.phase03) > 0
        );

    base::stopifnot(
        by.variables.phase01 %in% base::colnames(DF.input),
        by.variables.phase02 %in% base::colnames(DF.input),
        by.variables.phase03 %in% base::colnames(DF.input)
        );

    by.variables.all <- base::unique(base::c(
        by.variables.phase01,by.variables.phase02,by.variables.phase03
        ));

    for ( temp.index in 1:length(by.variables.all) ) {
        base::stopifnot(
            base::is.character(    DF.input[,by.variables.all[temp.index]]),
            base::all(!base::is.na(DF.input[,by.variables.all[temp.index]]))
            );
        }

    base::stopifnot(
        base::is.list(search.grid)
        );

    base::return( NULL );

    }

input.validity.checks_learner.search.grid <- function(
    learner     = NULL,
    search.grid = NULL
    ) {

    if ( base::grepl(x = base::tolower(learner), pattern = 'xgboost') ) {
        base::stopifnot(
            base::identical( base::sort(base::names(search.grid)) , c("alpha","lambda") ),
            base::all(base::unique(base::sapply(X = search.grid, FUN = base::class) ) %in% c("integer","numeric"))
            );
        }

    }
