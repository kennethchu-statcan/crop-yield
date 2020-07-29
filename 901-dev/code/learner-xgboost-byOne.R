
require(R6);
require(xgboost);

learner.xgboost.byOne <- R6Class(

    classname = 'learner.xgboost.byOne',

    public = list(

        # instantiation parameters
        learner.metadata = NULL,
        training.data    = NULL,

        # class attributes
        by_variables      = NULL,
        response_variable = NULL,
        trained.machines  = NULL,

        initialize = function(
            learner.metadata = NULL,
            training.data    = NULL
            ) {
            self$learner.metadata  <- learner.metadata;
            self$by_variables      <- self$learner.metadata[["by_variables"]];
            self$response_variable <- self$learner.metadata[["response_variable"]];
            self$training.data     <- training.data[,c(self$response_variable,self$by_variables,self$learner.metadata[["predictors"]])];
            },

        fit = function() {
            my.levels <- unique(as.character(self$training.data[,self$by_variables]));
            self$trained.machines <- list();
            for ( my.level in my.levels ) {
                cat(paste0("\n# fitting: ",my.level,"\n"));
                temp.learner <- learner.xgboost$new(
                    learner.metadata = self$learner.metadata,
                    training.data    = self$training.data[self$training.data[,self$by_variables] == my.level,c(self$response_variable,self$learner.metadata[["predictors"]])]
                    );
                temp.learner$fit();
                self$trained.machines[[my.level]] <- temp.learner;
				}
			},

        predict = function(newdata = NULL) {
            DF.output <- data.frame();
            my.levels <- unique(as.character(newdata[,self$by_variables]));
            for ( my.level in my.levels ) {
                cat(paste0("\n# predicting: ",my.level,"\n"));
                DF.temp   <- self$trained.machines[[my.level]]$predict(newdata = newdata[newdata[,self$by_variables] == my.level,]);
                DF.output <- rbind(DF.output,DF.temp);
                }
            return ( DF.output );
            }

        ) # public = list()

    ) # R6Class()

