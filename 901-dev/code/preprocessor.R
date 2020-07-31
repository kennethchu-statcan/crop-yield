require(R6)

preprocessor = R6Class(

    classname = 'preprocessor',

    public = list(

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

        transform = function(newdata = NULL) { return( newdata ); }

        ),

    private = list()

    ) # R6Class()

