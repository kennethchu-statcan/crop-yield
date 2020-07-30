
require(R6);
require(xgboost);

learner.xgboost.multiphase <- R6Class(

    classname = 'learner.xgboost.multiphase',

    public = list(

        # instantiation parameters
        learner.metadata = NULL,
        training.data    = NULL,

        # class attributes
        by.variables      = NULL,
        response.variable = NULL,
        learner.phase01   = NULL,
        learner.phase02   = NULL,

        initialize = function(
            learner.metadata = NULL,
            training.data    = NULL
            ) {

            self$learner.metadata  <- learner.metadata;
            self$response.variable <- self$learner.metadata[["response_variable"]];
            self$training.data     <- training.data;

            print("Y-1");
            cat("\nself$learner.metadata\n");
            print( self$learner.metadata   );
            print("Y-2");

            },

        fit = function() {

            temp.learner.metadata                   <- self$learner.metadata;
            temp.learner.metadata[["learner"]]      <- "xgboost_byGroup";
            temp.learner.metadata[["by_variables"]] <- temp.learner.metadata[["by_variables_phase01"]];

            print("Y-3");
            cat("\nnames(temp.learner.metadata)\n");
            print( names(temp.learner.metadata)   );
            print("Y-4");

            retained.colnames <- c(
                self$response.variable,
                temp.learner.metadata[["by_variables"]],
                self$learner.metadata[["predictors"]]
                );

            temp.learner <- getLearner(
                learner.metadata = temp.learner.metadata,
                DF.training      = self$training.data[,retained.colnames]
                );
            temp.learner$fit();
            self$learner.phase01 <- temp.learner;

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            temp.learner.metadata                   <- self$learner.metadata;
            temp.learner.metadata[["learner"]]      <- "xgboost_byGroup";
            temp.learner.metadata[["by_variables"]] <- temp.learner.metadata[["by_variables_phase02"]];

            print("Y-5");
            cat("\ntemp.learner.metadata\n");
            print( temp.learner.metadata   );
            print("Y-6");

            retained.colnames <- c(
                self$response.variable,
                temp.learner.metadata[["by_variables"]],
                self$learner.metadata[["predictors"]]
                );

            temp.learner <- getLearner(
                learner.metadata = temp.learner.metadata,
                DF.training      = self$training.data[,retained.colnames]
                );
            temp.learner$fit();
            self$learner.phase02 <- temp.learner;

            },

        predict = function(newdata = NULL) {
            newdata[,"synthetic.rowID"] <- seq(1,nrow(newdata),1);
            DF.output <- newdata;
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            print("A-1");
            cat("\nstr(self$learner.phase01)\n");
            print( str(self$learner.phase01)   );
            cat("\nself$learner.phase01$predict\n");
            print( self$learner.phase01$predict   );
            DF.predictions.parcel.phase01 <- self$learner.phase01$predict(newdata = newdata);
            print("A-2");
            colnames(DF.predictions.parcel.phase01) <- gsub(
                x           = colnames(DF.predictions.parcel.phase01),
                pattern     = "predicted_response",
                replacement = "predicted_response_phase01"
                );
            cat("\ncolnames(DF.predictions.parcel.phase01)\n");
            print( colnames(DF.predictions.parcel.phase01)   );
            DF.output <- merge(
                x  = DF.output,
                y  = DF.predictions.parcel.phase01[,c("synthetic.rowID","predicted_response_phase01")],
                by = "synthetic.rowID"
                );
            remove( list = c("DF.predictions.parcel.phase01") );
            print("A-3");
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.predictions.parcel.phase02 <- self$learner.phase02$predict(newdata = newdata);
            print("A-4");
            colnames(DF.predictions.parcel.phase02) <- gsub(
                x           = colnames(DF.predictions.parcel.phase02),
                pattern     = "predicted_response",
                replacement = "predicted_response_phase02"
                );
            DF.output <- merge(
                x  = DF.output,
                y  = DF.predictions.parcel.phase02[,c("synthetic.rowID","predicted_response_phase02")],
                by = "synthetic.rowID"
                );
            remove( list = c("DF.predictions.parcel.phase02") );
            print("A-5");
            ### ~~~~~~~~~~ ###
            DF.output[,"predicted_response"] <- DF.output[,"predicted_response_phase01"]
            print("A-6");
            ### ~~~~~~~~~~ ###
            DF.output <- DF.output[,setdiff(colnames(DF.output),"synthetic.rowID")];
            print("A-7");
			return ( DF.output );
            }

        ) # public = list()

    ) # R6Class()

