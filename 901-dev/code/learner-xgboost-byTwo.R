
require(R6);
require(xgboost);

learner.byTwo.xgboost <- R6Class(

    classname = 'learner.byTwo.xgboost',

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

			self$training.data[,"byOne_byTwo"] <- factor(
				paste0(
					self$training.data[,self$by_variables[1]],
					"_",
					self$training.data[,self$by_variables[2]]
					)
				);

			self$training.data <- self$training.data[,setdiff(colnames(self$training.data),self$by_variables)];

			},

		fit = function() {
			my.levels <- unique(as.character(self$training.data[,"byOne_byTwo"]));
			self$trained.machines <- list();
			for ( my.level in my.levels ) {
				cat(paste0("\n# fitting: ",my.level,"\n"));
		       	byTwo.learner <- learner.xgboost$new(
            		learner.metadata = self$learner.metadata,
		            training.data    = self$training.data[self$training.data[,"byOne_byTwo"] == my.level,c(self$response_variable,self$learner.metadata[["predictors"]])]
            		);
		        byTwo.learner$fit();
		        self$trained.machines[[my.level]] <- byTwo.learner;
				}
			},

		predict = function(newdata = NULL) {
			original.colnames.newdata <- colnames(newdata);
			newdata[,"byOne_byTwo"] <- factor(
				paste0(
					newdata[,self$by_variables[1]],
					"_",
					newdata[,self$by_variables[2]]
					)
				);
			DF.output <- data.frame();
			my.levels <- unique(as.character(newdata[,"byOne_byTwo"]));
			for ( my.level in my.levels ) {
				cat(paste0("\n# predicting: ",my.level,"\n"));
				if ( my.level %in% names(self$trained.machines) ) {
			        DF.temp <- self$trained.machines[[my.level]]$predict(newdata = newdata[newdata[,"byOne_byTwo"] == my.level,original.colnames.newdata]);
				} else {
					DF.temp <- newdata[newdata[,"byOne_byTwo"] == my.level,original.colnames.newdata];
					DF.temp[,"predicted_response"] <- 0;
					}
				rownames(DF.temp)   <- NULL;
				DF.output           <- as.data.frame(dplyr::bind_rows(DF.output,DF.temp));
		        rownames(DF.output) <- NULL;
				}
			return ( DF.output );
			}

		) # public = list()

	) # R6Class()
