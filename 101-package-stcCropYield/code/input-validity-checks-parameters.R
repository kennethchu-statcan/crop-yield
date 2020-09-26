
input.validity.checks_parameters <- function(
    training.window   = NULL,
    validation.window = NULL,
    num.cores         = NULL
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
        base::is.numeric(num.cores),
        base::length(num.cores) == 1,
        num.cores == base::as.integer(num.cores),
        num.cores > 0
        );

    base::return( NULL );

    }
