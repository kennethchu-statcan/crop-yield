
input.validity.checks_predict <- function(
    trained.model = NULL,
    DF.predictors = NULL
    ) {

    base::stopifnot(
        base::is.character(trained.model) | base::identical(base::class(trained.model),base::c("learner.multiphase","R6"))
        );

    if ( base::is.character(trained.model) ) {
        base::stopifnot(
            base::length(trained.model) == 1,
            base::file.exists(trained.model),
            base::tolower(tools::file_ext(base::basename(trained.model))) %in% base::c("rdata","rds")
            );
        trained.model <- readRDS(file = trained.model);
        }

    base::stopifnot(
        base::identical(base::class(trained.model),base::c("learner.multiphase","R6"))
        );

    base::stopifnot(
        base::all( base::c('learner.metadata') %in% base::names(trained.model) )
        );

    base::stopifnot(
        base::all( base::c('predictors') %in% base::names(trained.model$learner.metadata) )
        );

    predictor.colnames <- trained.model$learner.metadata$predictors;

    by.variables.colnames <- base::unique(base::c(
        trained.model$learner.metadata$by_variables_phase01,
        trained.model$learner.metadata$by_variables_phase02,
        trained.model$learner.metadata$by_variables_phase03
        ));

    base::stopifnot(
        base::c(predictor.colnames,by.variables.colnames) %in% base::colnames(DF.predictors)
        );

    for ( temp.index in 1:length(predictor.colnames) ) {
        base::stopifnot(
            base::is.numeric(      DF.predictors[,predictor.colnames[temp.index]]),
            base::all(!base::is.na(DF.predictors[,predictor.colnames[temp.index]]))
            );
        }

    for ( temp.index in 1:length(by.variables.colnames) ) {
        base::stopifnot(
            base::is.character(    DF.predictors[,by.variables.colnames[temp.index]]),
            base::all(!base::is.na(DF.predictors[,by.variables.colnames[temp.index]]))
            );
        }

    base::return( NULL );

    }
