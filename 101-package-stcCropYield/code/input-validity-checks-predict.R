
input.validity.checks_predict <- function(
    FILE.trained.model = NULL,
    DF.predictors      = NULL
    ) {

    base::stopifnot(
        base::is.character(FILE.trained.model),
        base::length(FILE.trained.model) == 1,
        base::file.exists(FILE.trained.model),
        base::tolower(tools::file_ext(base::basename(FILE.trained.model))) %in% base::c("rdata","rds")
        );

    trained.model <- readRDS(file = FILE.trained.model);

    base::stopifnot(
        base::identical(base::class(trained.model),base::c("learner.multiphase","R6"))
        );

    base::stopifnot(
        base::all( base::c('learner.metadata') %in% base::names(trained.model) )
        );

    base::stopifnot(
        base::all( base::c('predictors') %in% base::names(trained.model$learner.metadata) )
        );

    required.colnames <- base::unique(base::c(
        trained.model$learner.metadata$predictors,
        trained.model$learner.metadata$by_variables_phase01,
        trained.model$learner.metadata$by_variables_phase02,
        trained.model$learner.metadata$by_variables_phase03
        ));

    base::stopifnot(
        required.colnames %in% base::colnames(DF.predictors)
        );

    base::return( NULL );

    }
