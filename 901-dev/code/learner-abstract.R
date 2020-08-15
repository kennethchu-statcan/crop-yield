
require(R6);

learner.abstract <- R6::R6Class(

    classname = 'learner.abstract',

    public = list(

        # instantiation parameters
        learner.metadata = NULL,
        training.data    = NULL,

        # class attributes
        response_variable = NULL,
        preprocessor      = NULL,
        preprocessed.data = NULL,
        trained.machine   = NULL,

        initialize = function(
            learner.metadata = NULL,
            training.data    = NULL
            ) {
            self$learner.metadata  <- learner.metadata;
            self$response_variable <- self$learner.metadata[["response_variable"]];
            self$training.data     <- training.data[,c(self$response_variable,self$learner.metadata[["predictors"]])];
            },

        fit = function() {
            self$trained.machine <- NULL;
            },

        predict = function(newdata = NULL) {
            DF.output <- newdata;
            DF.output[,"predicted_response"] <- rep(x = NA, times = nrow(DF.output));
            return ( DF.output );
            }

        ) # public = list()

    ) # R6Class()

