
learner.multiphase <- R6::R6Class(

    classname = 'learner.multiphase',

    public = base::list(

        # instantiation parameters
        global.objects       = NULL,
        learner.single.phase = NULL,
        learner.metadata     = NULL,
        training.data        = NULL,

        # class attributes
        by.variables      = NULL,
        response.variable = NULL,
        learner.phase01   = NULL,
        learner.phase02   = NULL,
        learner.phase03   = NULL,

        initialize = function(
            global.objects       = NULL,
            learner.single.phase = NULL,
            learner.metadata     = NULL,
            training.data        = NULL
            ) {
            self$global.objects       <- global.objects;
            self$learner.single.phase <- learner.single.phase;
            self$learner.metadata     <- learner.metadata;
            self$response.variable    <- self$learner.metadata[["response_variable"]];
            self$training.data        <- training.data;
            },

        fit = function() {

            this.function.name <- "learner.multiphase$fit";
            logger::log_info('{this.function.name}(): starts');

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            if ( "windows" == base::.Platform[["OS.type"]] ) {
                if ( !is.null(self$global.objects) ) {
                    object.names <- base::names(self$global.objects);
                    for ( temp.object.name in object.names ) {
                        temp.object <- self$global.objects[[temp.object.name]];
                        if ( base::is.function(temp.object) | ("R6ClassGenerator" == base::class(temp.object)) ) {
                            base::assign(x = temp.object.name, value = temp.object, envir = base::environment());
                        } else if ( identical(class(temp.object),c("loglevel","integer")) ) {
                            base::assign(x = temp.object.name, value = temp.object, envir = base::environment());
                            logger::log_threshold(level = temp.object);
                            }
                        }
                    }
                }
            logger::log_info( '{this.function.name}(): log.threshold(): {attr(x=logger::log_threshold(),which="level")}');
            logger::log_debug('{this.function.name}(): environment(): {capture.output(environment())}');
            logger::log_debug('{this.function.name}(): ls(environment()):\n{paste(ls(environment()),collapse="\n")}');
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

            logger::log_info('{this.function.name}(): Phase 1 fitting starts');

            temp.learner.metadata                   <- self$learner.metadata;
            temp.learner.metadata[["learner"]]      <- self$learner.single.phase;
            temp.learner.metadata[["by_variables"]] <- temp.learner.metadata[["by_variables_phase01"]];

            retained.colnames <- base::c(
                self$response.variable,
                temp.learner.metadata[["by_variables"]],
                self$learner.metadata[["predictors"]]
                );

            temp.learner <- getLearner(
                learner.metadata = temp.learner.metadata,
                DF.training      = self$training.data[,retained.colnames],
                global.objects   = self$global.objects
                );
            temp.learner$fit();
            self$learner.phase01 <- temp.learner;

            logger::log_info('{this.function.name}(): Phase 1 fitting complete');

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            logger::log_info('{this.function.name}(): Phase 2 fitting starts');

            temp.learner.metadata                   <- self$learner.metadata;
            temp.learner.metadata[["learner"]]      <- self$learner.single.phase;
            temp.learner.metadata[["by_variables"]] <- temp.learner.metadata[["by_variables_phase02"]];

            retained.colnames <- base::c(
                self$response.variable,
                temp.learner.metadata[["by_variables"]],
                self$learner.metadata[["predictors"]]
                );

            temp.learner <- getLearner(
                learner.metadata = temp.learner.metadata,
                DF.training      = self$training.data[,retained.colnames],
                global.objects   = self$global.objects
                );
            temp.learner$fit();
            self$learner.phase02 <- temp.learner;

            logger::log_info('{this.function.name}(): Phase 2 fitting complete');

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            logger::log_info('{this.function.name}(): Phase 3 fitting starts');

            temp.learner.metadata                   <- self$learner.metadata;
            temp.learner.metadata[["learner"]]      <- self$learner.single.phase;
            temp.learner.metadata[["by_variables"]] <- temp.learner.metadata[["by_variables_phase03"]];

            retained.colnames <- base::c(
                self$response.variable,
                temp.learner.metadata[["by_variables"]],
                self$learner.metadata[["predictors"]]
                );

            temp.learner <- getLearner(
                learner.metadata = temp.learner.metadata,
                DF.training      = self$training.data[,retained.colnames],
                global.objects   = self$global.objects
                );
            temp.learner$fit();
            self$learner.phase03 <- temp.learner;

            logger::log_info('{this.function.name}(): Phase 3 fitting complete');

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            logger::log_info('{this.function.name}(): exits');

            },

        predict = function(newdata = NULL) {
            newdata[,"synthetic.rowID"] <- base::seq(1,base::nrow(newdata),1);
            DF.output <- newdata;
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.predictions.parcel.phase01 <- self$learner.phase01$predict(newdata = newdata);
            base::colnames(DF.predictions.parcel.phase01) <- base::gsub(
                x           = base::colnames(DF.predictions.parcel.phase01),
                pattern     = "predicted_response",
                replacement = "predicted_response_phase01"
                );
            DF.output <- base::merge(
                x  = DF.output,
                y  = DF.predictions.parcel.phase01[,c("synthetic.rowID","predicted_response_phase01")],
                by = "synthetic.rowID"
                );
            base::remove( list = base::c("DF.predictions.parcel.phase01") );
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.predictions.parcel.phase02 <- self$learner.phase02$predict(newdata = newdata);
            base::colnames(DF.predictions.parcel.phase02) <- base::gsub(
                x           = base::colnames(DF.predictions.parcel.phase02),
                pattern     = "predicted_response",
                replacement = "predicted_response_phase02"
                );
            DF.output <- base::merge(
                x  = DF.output,
                y  = DF.predictions.parcel.phase02[,base::c("synthetic.rowID","predicted_response_phase02")],
                by = "synthetic.rowID"
                );
            base::remove( list = base::c("DF.predictions.parcel.phase02") );
            ### ~~~~~~~~~~ ###
            DF.predictions.parcel.phase03 <- self$learner.phase03$predict(newdata = newdata);
            base::colnames(DF.predictions.parcel.phase03) <- base::gsub(
                x           = base::colnames(DF.predictions.parcel.phase03),
                pattern     = "predicted_response",
                replacement = "predicted_response_phase03"
                );
            DF.output <- base::merge(
                x  = DF.output,
                y  = DF.predictions.parcel.phase03[,base::c("synthetic.rowID","predicted_response_phase03")],
                by = "synthetic.rowID"
                );
            base::remove( list = base::c("DF.predictions.parcel.phase03") );
            ### ~~~~~~~~~~ ###
            # For each record, we now have a seequence of intermediate predicted responses, one from each phase.
            # The final predicted response is the first one that is not NA.
            # If all the intermediate predicted responses are NA, then the final predicted response is NA.
            DF.output[,"predicted_response"] <- base::apply(
                X      = DF.output[,base::grep(x=base::colnames(DF.output),pattern="predicted_response_phase.+",value=TRUE)],
                MARGIN = 1,
                FUN    = function(x) { base::ifelse( base::sum(!base::is.na(x)) > 0 , x[!base::is.na(x)][1] , NA ) }
                );
            DF.output[,"phase"] <- base::apply(
                X      = DF.output[,base::grep(x=base::colnames(DF.output),pattern="predicted_response_phase.+",value=TRUE)],
                MARGIN = 1,
                FUN    = function(x) { base::ifelse( base::sum(!base::is.na(x)) > 0 , base::which(!base::is.na(x))[1] , NA ) }
                );
            ### ~~~~~~~~~~ ###
            DF.output <- DF.output[base::order(DF.output[,"synthetic.rowID"]),];
            DF.output <- DF.output[,base::setdiff(base::colnames(DF.output),"synthetic.rowID")];
            ### ~~~~~~~~~~ ###
            base::return ( DF.output );
            }

        ), # public = list()

    private = base::list(
        ) # private = list()

    ) # R6Class()

