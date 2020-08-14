
require(R6);
require(xgboost);

learner.xgboost.byGroup <- R6Class(

    classname = 'learner.xgboost.byGroup',

    public = list(

        # instantiation parameters
        learner.metadata = NULL,
        training.data    = NULL,

        # class attributes
        by.variables      = NULL,
        response.variable = NULL,
        trained.machines  = NULL,

        initialize = function(
            learner.metadata = NULL,
            training.data    = NULL
            ) {

            self$learner.metadata  <- learner.metadata;
            self$by.variables      <- self$learner.metadata[["by_variables"]];
            self$response.variable <- self$learner.metadata[["response_variable"]];

            colnames.training  <- c(self$response.variable,self$by.variables,self$learner.metadata[["predictors"]]);
            self$training.data <- training.data[,colnames.training];

            self$training.data[,"concatenated_by_variable"] <- private$get_concatenated_by_variable(
                DF.input = self$training.data[,self$by.variables]
                );

            self$training.data <- self$training.data[,setdiff(colnames(self$training.data),self$by.variables)];

            },

        fit = function() {
            my.levels <- unique(as.character(self$training.data[,"concatenated_by_variable"]));
            self$trained.machines <- list();
            for ( my.level in my.levels ) {
                # cat(paste0("\n# fitting: ",my.level,"\n"));
                temp.learner <- learner.xgboost$new(
                    learner.metadata = self$learner.metadata,
                    training.data    = self$training.data[self$training.data[,"concatenated_by_variable"] == my.level,c(self$response.variable,self$learner.metadata[["predictors"]])]
                    );
                temp.learner$fit();
                self$trained.machines[[my.level]] <- temp.learner;
                }
            },

        predict = function(newdata = NULL) {
            original.colnames.newdata <- colnames(newdata);
            newdata[,"concatenated_by_variable"] <- private$get_concatenated_by_variable(
                DF.input = newdata[,self$by.variables]
                );
            DF.output <- data.frame();
            my.levels <- unique(as.character(newdata[,"concatenated_by_variable"]));
            for ( my.level in my.levels ) {
                # cat(paste0("\n# predicting: ",my.level,"\n"));
                if ( my.level %in% names(self$trained.machines) ) {
                    temp.machine <- self$trained.machines[[my.level]];
                    DF.temp <- self$trained.machines[[my.level]]$predict(newdata = newdata[newdata[,"concatenated_by_variable"] == my.level,original.colnames.newdata]);
                } else {
                    DF.temp <- newdata[newdata[,"concatenated_by_variable"] == my.level,original.colnames.newdata];
                    DF.temp[,"predicted_response"] <- NA;
                    }
                rownames(DF.temp)   <- NULL;
                DF.output           <- as.data.frame(dplyr::bind_rows(DF.output,DF.temp));
                rownames(DF.output) <- NULL;
                }
            return ( DF.output );
            }

        ), # public = list()

    private = list(
    
        get_concatenated_by_variable = function(DF.input = NULL) {
            if ( is.character(DF.input) ) {
                output.factor <- factor(as.character(DF.input));
            } else {
                output.factor <- factor(
                    apply(
                        X      = DF.input[,self$by.variables],
                        MARGIN = 1,
                        FUN    = function(x) { paste0(x,collapse="_") }
                        )
                    );
                }
            return( output.factor );
            }

        )

    ) # R6Class()

