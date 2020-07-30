
getLearner <- function(
    learner.metadata = NULL,
    DF.training      = NULL
    ) {

    this.function.name <- "getLearner";
    cat(paste0("\n# starting: ",this.function.name,"()\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
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

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# exiting: ",this.function.name,"()\n"));
    return( instantiated.learner );

    }

