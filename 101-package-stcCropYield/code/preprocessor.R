
preprocessor = R6::R6Class(

    classname = 'preprocessor',

    public = base::list(

        # instantiation parameters
        learner.metadata = NULL,
        training.data    = NULL,

        # class attributes
        # No class attributes

        initialize = function( 
            learner.metadata = NULL,
            training.data    = NULL
            ) {
            self$learner.metadata <- learner.metadata;
            self$training.data    <- training.data;
            },

        fit = function() {},

        transform = function(newdata = NULL) { base::return( newdata ); }

        ),

    private = base::list()

    ) # R6Class()

