
input.validity.checks_variables.needed.for.prediction <- function(
    DF.input   = NULL,
    ecoregion  = NULL,
    crop       = NULL,
    predictors = NULL
    ) {

    base::stopifnot(
        !base::is.null(DF.input),
        base::is.data.frame(DF.input),
        base::nrow(DF.input) > 0
        );

    base::stopifnot(
        base::is.character(ecoregion),
        base::length(ecoregion) == 1,
        base::is.character(DF.input[,ecoregion])
        );

    base::stopifnot(
        base::is.character(crop),
        base::length(crop) == 1,
        base::is.character(DF.input[,crop])
        );

    base::stopifnot(
        base::is.character(predictors),
        base::length(predictors) > 0
        );

    base::stopifnot(
        base::all( base::c(ecoregion,crop,predictors)%in% base::colnames(DF.input) )
        );

    for ( column.index in 1:length(predictors) ) {
        base::stopifnot(
            base::is.numeric(      DF.input[,predictors[column.index]]),
            base::all(!base::is.na(DF.input[,predictors[column.index]]))
            );
        }

    base::return( NULL );

    }

input.validity.checks_variables.needed.for.training <- function(
    DF.input             = NULL,
    year                 = NULL,
    response.variable    = NULL,
    harvested.area       = NULL,
    seeded.area          = NULL,
    evaluation.weight    = NULL,
    single.configuration = TRUE
    ) {

    base::stopifnot(
        !base::is.null(DF.input),
        base::is.data.frame(DF.input),
        base::nrow(DF.input) > 0
        );

    base::stopifnot(
        base::is.character(year),
        base::length(year) == 1,
        base::is.numeric(DF.input[,year]),
        base::all(DF.input[,year] == base::as.integer(DF.input[,year])),
        base::all(DF.input[,year] > 1956) # 1957 is the year Sputnik 1 (first artificial satellite) was launched.
        );

    base::stopifnot(
        base::is.character(response.variable),
        base::length(response.variable) == 1,
        base::is.numeric(DF.input[,response.variable]),
        base::all(!base::is.na(DF.input[,response.variable])),
        base::all(DF.input[,response.variable] >= 0)
        );

    if ( single.configuration ) {

        # do nothing

    } else {

        base::stopifnot(
            base::is.character(harvested.area),
            base::length(harvested.area) == 1,
            base::is.numeric(DF.input[,harvested.area]),
            base::all(!base::is.na(DF.input[,harvested.area])),
            base::all(DF.input[,harvested.area] >= 0)
            );

        base::stopifnot(
            base::is.character(seeded.area),
            base::length(seeded.area) == 1,
            base::is.numeric(DF.input[,seeded.area]),
            base::all(!base::is.na(DF.input[,seeded.area])),
            base::all(DF.input[,seeded.area] >= 0)
            );

        base::stopifnot(
            base::is.character(evaluation.weight),
            base::length(evaluation.weight) == 1,
            base::is.numeric(DF.input[,evaluation.weight]),
            base::all(!base::is.na(DF.input[,evaluation.weight])),
            base::all(DF.input[,evaluation.weight] >= 0)
            );

        }

    required.colnames <- ifelse(
        single.configuration,
        c(year,response.variable),
        c(year,response.variable,harvested.area,seeded.area,evaluation.weight)
        );

    base::stopifnot(
        0 == length(setdiff(required.colnames,colnames(DF.input)))
        );

    base::return( NULL );

    }
