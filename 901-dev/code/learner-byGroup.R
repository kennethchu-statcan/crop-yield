
#base::require(R6);

learner.byGroup <- R6::R6Class(

    classname = 'learner.byGroup',

    public = base::list(

        # instantiation parameters
        learner.single.group = NULL,
        learner.metadata     = NULL,
        training.data        = NULL,

        # class attributes
        by.variables      = NULL,
        response.variable = NULL,
        trained.machines  = NULL,

        initialize = function(
            learner.single.group = NULL,
            learner.metadata     = NULL,
            training.data        = NULL
            ) {

            self$learner.single.group <- learner.single.group;
            self$learner.metadata     <- learner.metadata;
            self$by.variables         <- self$learner.metadata[["by_variables"]];
            self$response.variable    <- self$learner.metadata[["response_variable"]];

            colnames.training  <- c(self$response.variable,self$by.variables,self$learner.metadata[["predictors"]]);
            self$training.data <- training.data[,colnames.training];

            self$training.data[,"concatenated_by_variable"] <- private$get_concatenated_by_variable(
                DF.input = self$training.data[,self$by.variables]
                );

            self$training.data <- self$training.data[,base::setdiff(base::colnames(self$training.data),self$by.variables)];

            },

        fit = function() {
            my.levels <- base::unique(base::as.character(self$training.data[,"concatenated_by_variable"]));
            self$trained.machines <- base::list();
            for ( my.level in my.levels ) {
                temp.learner.metadata              <- self$learner.metadata;
                temp.learner.metadata[["learner"]] <- self$learner.single.group;
                temp.learner <- getLearner(
                    learner.metadata = temp.learner.metadata,
                    DF.training      = self$training.data[self$training.data[,"concatenated_by_variable"] == my.level,c(self$response.variable,self$learner.metadata[["predictors"]])]
                    );
                temp.learner$fit();
                self$trained.machines[[my.level]] <- temp.learner;
                }
            },

        predict = function(newdata = NULL) {
            original.colnames.newdata <- base::colnames(newdata);
            newdata[,"concatenated_by_variable"] <- private$get_concatenated_by_variable(
                DF.input = newdata[,self$by.variables]
                );
            DF.output <- base::data.frame();
            my.levels <- base::unique(base::as.character(newdata[,"concatenated_by_variable"]));
            for ( my.level in my.levels ) {
                if ( my.level %in% base::names(self$trained.machines) ) {
                    temp.machine <- self$trained.machines[[my.level]];
                    DF.temp <- self$trained.machines[[my.level]]$predict(newdata = newdata[newdata[,"concatenated_by_variable"] == my.level,original.colnames.newdata]);
                } else {
                    DF.temp <- newdata[newdata[,"concatenated_by_variable"] == my.level,original.colnames.newdata];
                    DF.temp[,"predicted_response"] <- NA;
                    }
                base::rownames(DF.temp)   <- NULL;
                DF.output                 <- base::as.data.frame(dplyr::bind_rows(DF.output,DF.temp));
                base::rownames(DF.output) <- NULL;
                }
            base::return ( DF.output );
            }

        ), # public = list()

    private = list(
    
        get_concatenated_by_variable = function(DF.input = NULL) {
            if ( base::is.character(DF.input) ) {
                output.factor <- base::factor(base::as.character(DF.input));
            } else {
                output.factor <- base::factor(
                    base::apply(
                        X      = DF.input[,self$by.variables],
                        MARGIN = 1,
                        FUN    = function(x) { base::paste0(x,collapse="_") }
                        )
                    );
                }
            return( output.factor );
            }

        )

    ) # R6Class()

