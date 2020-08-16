
crop.yield.train.model <- function(
    learner.metadata   = NULL,
    DF.training        = NULL,
    FILE.trained.model = NULL
    ) {
    
    current.learner <- getLearner(
        learner.metadata = learner.metadata,
        DF.training      = DF.training
        );

    current.learner$fit();

    if ( !base::is.null(FILE.trained.model) ) {
        base::saveRDS(
            object = current.learner,
            file   = FILE.trained.model
            );
        }

    base::return( current.learner );

    }

