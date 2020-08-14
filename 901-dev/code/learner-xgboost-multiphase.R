
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
        learner.phase03   = NULL,

        initialize = function(
            learner.metadata = NULL,
            training.data    = NULL
            ) {
            self$learner.metadata  <- learner.metadata;
            self$response.variable <- self$learner.metadata[["response_variable"]];
            self$training.data     <- training.data;
            },

        fit = function() {

            temp.learner.metadata                   <- self$learner.metadata;
            temp.learner.metadata[["learner"]]      <- "xgboost_byGroup";
            temp.learner.metadata[["by_variables"]] <- temp.learner.metadata[["by_variables_phase01"]];

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

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            temp.learner.metadata                   <- self$learner.metadata;
            temp.learner.metadata[["learner"]]      <- "xgboost_byGroup";
            temp.learner.metadata[["by_variables"]] <- temp.learner.metadata[["by_variables_phase03"]];

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
            self$learner.phase03 <- temp.learner;

            },

        predict = function(newdata = NULL) {
            newdata[,"synthetic.rowID"] <- seq(1,nrow(newdata),1);
            DF.output <- newdata;
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.predictions.parcel.phase01 <- self$learner.phase01$predict(newdata = newdata);
            colnames(DF.predictions.parcel.phase01) <- gsub(
                x           = colnames(DF.predictions.parcel.phase01),
                pattern     = "predicted_response",
                replacement = "predicted_response_phase01"
                );
            DF.output <- merge(
                x  = DF.output,
                y  = DF.predictions.parcel.phase01[,c("synthetic.rowID","predicted_response_phase01")],
                by = "synthetic.rowID"
                );
            remove( list = c("DF.predictions.parcel.phase01") );
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.predictions.parcel.phase02 <- self$learner.phase02$predict(newdata = newdata);
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
            ### ~~~~~~~~~~ ###
            DF.predictions.parcel.phase03 <- self$learner.phase03$predict(newdata = newdata);
            colnames(DF.predictions.parcel.phase03) <- gsub(
                x           = colnames(DF.predictions.parcel.phase03),
                pattern     = "predicted_response",
                replacement = "predicted_response_phase03"
                );
            DF.output <- merge(
                x  = DF.output,
                y  = DF.predictions.parcel.phase03[,c("synthetic.rowID","predicted_response_phase03")],
                by = "synthetic.rowID"
                );
            remove( list = c("DF.predictions.parcel.phase03") );
            ### ~~~~~~~~~~ ###
            DF.output[,"predicted_response"] <- apply(
                X      = DF.output[,grep(x=colnames(DF.output),pattern="predicted_response_phase.+",value=TRUE)],
                MARGIN = 1,
                FUN    = function(x) { ifelse( sum(!is.na(x)) > 0 , x[!is.na(x)][1] , NA ) }
                );
            DF.output[,"phase"] <- apply(
                X      = DF.output[,grep(x=colnames(DF.output),pattern="predicted_response_phase.+",value=TRUE)],
                MARGIN = 1,
                FUN    = function(x) { ifelse( sum(!is.na(x)) > 0 , which(!is.na(x))[1] , NA ) }
                );
            ### ~~~~~~~~~~ ###
            DF.output <- DF.output[order(DF.output[,"synthetic.rowID"]),];
            DF.output <- DF.output[,setdiff(colnames(DF.output),"synthetic.rowID")];
            ### ~~~~~~~~~~ ###
			return ( DF.output );
            }

        ) # public = list()

    ) # R6Class()

