
input.validity.checks_window.compatibility <- function(
    training.window   = NULL,
    validation.window = NULL,
    DF.input          = NULL,
    year              = NULL
    ) {

    min.year <- base::min(DF.input[,year]);
    max.year <- base::max(DF.input[,year]);

    start.validations      <- min.year + training.window;
    start.mock.productions <- start.validations + validation.window;

    base::stopifnot(
        min.year               <  start.validations,
        start.validations      <  start.mock.productions,
        start.mock.productions <= max.year
        );

    base::return( NULL );

    }
