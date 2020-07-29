
getLearner <- function(
    learner.metadata = NULL,
    DF.training      = NULL
    ) {

    this.function.name <- "getLearner";
    cat(paste0("\n# starting: ",this.function.name,"()\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( "lm" == learner.metadata[["learner"]] ) {

        instantiated.learner <- learner.lm$new(
            learner.metadata = learner.metadata,
            training.data    = DF.training
            );

    } else if ( "xgboost" == learner.metadata[["learner"]] ) {

        instantiated.learner <- learner.xgboost$new(
            learner.metadata = learner.metadata,
            training.data    = DF.training
            );

    } else if ( "xgboost_byOne" == learner.metadata[["learner"]] ) {

        instantiated.learner <- learner.xgboost.byOne$new(
            learner.metadata = learner.metadata,
            training.data    = DF.training
            );

    } else if ( "xgboost_byTwo" == learner.metadata[["learner"]] ) {

        instantiated.learner <- learner.xgboost.byTwo$new(
            learner.metadata = learner.metadata,
            training.data    = DF.training
            );

    } else if ( "glmnet_byTwo" == learner.metadata[["learner"]] ) {

        instantiated.learner <- learner.glmnet.byTwo$new(
            learner.metadata = learner.metadata,
            training.data    = DF.training
            );

    } else if ( "xgbtree_byTwo" == learner.metadata[["learner"]] ) {

        instantiated.learner <- learner.xgbtree.byTwo$new(
            learner.metadata = learner.metadata,
            training.data    = DF.training
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# exiting: ",this.function.name,"()\n"));
    return( instantiated.learner );

    }

