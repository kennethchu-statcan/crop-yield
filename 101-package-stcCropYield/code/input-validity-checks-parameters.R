
input.validity.checks_parameters <- function(
    training.window   = NULL,
    validation.window = NULL,
    min.num.parcels   = NULL
    ) {

    base::stopifnot(
        base::is.numeric(training.window),
        base::length(training.window) == 1,
        training.window == base::as.integer(training.window),
        training.window > 0
        );

    base::stopifnot(
        base::is.numeric(validation.window),
        base::length(validation.window) == 1,
        validation.window == base::as.integer(validation.window),
        validation.window > 0
        );

    base::stopifnot(
        base::is.numeric(min.num.parcels),
        base::length(min.num.parcels) == 1,
        min.num.parcels == base::as.integer(min.num.parcels),
        min.num.parcels > 0
        );

    base::return( NULL );

    }
