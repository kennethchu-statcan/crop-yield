
getLearner <- function(
    learner.metadata = NULL,
    DF.training      = NULL
    ) {

    this.function.name <- "getLearner";
    logger::log_debug('{this.function.name}(): starts');

    log.prefix <- '{this.function.name}():';
    log.prefix <- paste0(log.prefix,' learner.metadata[["learner"]] = {learner.metadata[["learner"]]}');
    logger::log_debug(log.prefix);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_debug(paste0(log.prefix,', instantiation begins'));
    if ( "xgboost" == learner.metadata[["learner"]] ) {

        instantiated.learner <- learner.xgboost$new(
            learner.metadata = learner.metadata,
            training.data    = DF.training
            );

    } else if ( "xgboost_byGroup" == learner.metadata[["learner"]] ) {

        instantiated.learner <- learner.xgboost.byGroup$new(
            learner.metadata = learner.metadata,
            training.data    = DF.training
            );

    } else if ( "xgboost_multiphase" == learner.metadata[["learner"]] ) {

        instantiated.learner <- learner.xgboost.multiphase$new(
            learner.metadata = learner.metadata,
            training.data    = DF.training
            );

        }
    logger::log_debug(paste0(log.prefix,', instantiation complete'));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_debug('{this.function.name}(): exits');
    return( instantiated.learner );

    }

