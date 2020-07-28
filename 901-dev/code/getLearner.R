
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

    } else if ( "crop_xgboost" == learner.metadata[["learner"]] ) {

        instantiated.learner <- learner.crop.xgboost$new(
            learner.metadata = learner.metadata,
            training.data    = DF.training
            );

    } else if ( "region_xgboost" == learner.metadata[["learner"]] ) {

        instantiated.learner <- learner.region.xgboost$new(
            learner.metadata = learner.metadata,
            training.data    = DF.training
            );

    } else if ( "byOne_xgboost" == learner.metadata[["learner"]] ) {

        instantiated.learner <- learner.byOne.xgboost$new(
            learner.metadata = learner.metadata,
            training.data    = DF.training
            );

    } else if ( "byTwo_xgboost" == learner.metadata[["learner"]] ) {

        instantiated.learner <- learner.byTwo.xgboost$new(
            learner.metadata = learner.metadata,
            training.data    = DF.training
            );

    } else if ( "byTwo_glmnet" == learner.metadata[["learner"]] ) {

        instantiated.learner <- learner.byTwo.glmnet$new(
            learner.metadata = learner.metadata,
            training.data    = DF.training
            );

    } else if ( "byTwo_xgbtree" == learner.metadata[["learner"]] ) {

        instantiated.learner <- learner.byTwo.xgbtree$new(
            learner.metadata = learner.metadata,
            training.data    = DF.training
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n# exiting: ",this.function.name,"()\n"));
    return( instantiated.learner );

    }

