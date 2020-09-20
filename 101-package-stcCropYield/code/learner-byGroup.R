
learner.byGroup <- R6::R6Class(

    classname = 'learner.byGroup',

    public = base::list(

        # instantiation parameters
        global.objects       = NULL,
        learner.single.group = NULL,
        learner.metadata     = NULL,
        training.data        = NULL,

        # class attributes
        by.variables      = NULL,
        response.variable = NULL,
        trained.machines  = NULL,

        initialize = function(
            global.objects       = NULL,
            learner.single.group = NULL,
            learner.metadata     = NULL,
            training.data        = NULL
            ) {

            this.function.name <- "learner.byGroup$initialize";
            logger::log_debug('{this.function.name}(): starts');

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            self$global.objects <- global.objects;
            if ( "windows" == base::.Platform[["OS.type"]] ) {
                if ( !is.null(self$global.objects) ) {
                    object.names <- base::names(self$global.objects);
                    for ( temp.object.name in object.names ) {
                        temp.object <- self$global.objects[[temp.object.name]];
                        if ( identical(class(temp.object),c("loglevel","integer")) ) {
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

            self$learner.single.group <- learner.single.group;
            self$learner.metadata     <- learner.metadata;
            self$by.variables         <- self$learner.metadata[["by_variables"]];
            self$response.variable    <- self$learner.metadata[["response_variable"]];

            colnames.training  <- c(self$response.variable,self$by.variables,self$learner.metadata[["predictors"]]);
            self$training.data <- training.data[,colnames.training];

            logger::log_debug('{this.function.name}(): str(training.data): {paste(capture.output(str(training.data)),collapse="\n")}');
            logger::log_debug('{this.function.name}(): self$by.variables: {paste(self$by.variables,collapse="\n")}');
            logger::log_debug('{this.function.name}(): str(self$training.data[,self$by.variables]): {paste(capture.output(str(self$training.data[,self$by.variables])),collapse="\n")}');

            self$training.data[,"concatenated_by_variable"] <- private$get_concatenated_by_variable(
                DF.input = self$training.data[,self$by.variables]
                );

            self$training.data <- self$training.data[,base::setdiff(base::colnames(self$training.data),self$by.variables)];

            logger::log_debug('{this.function.name}(): str(self$training.data): {paste(capture.output(str(self$training.data)),collapse="\n")}');

            },

        fit = function() {
            this.function.name <- "learner.byGroup$fit";
            logger::log_debug('{this.function.name}(): starts');
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
            my.levels <- base::unique(base::as.character(self$training.data[,"concatenated_by_variable"]));
            self$trained.machines <- base::list();
            for ( my.level in my.levels ) {
                is.my.level <- (self$training.data[,"concatenated_by_variable"] == my.level);
                if ( sum(is.my.level) >= self$learner.metadata[['min_num_parcels']] ) {
                    temp.learner.metadata              <- self$learner.metadata;
                    temp.learner.metadata[["learner"]] <- self$learner.single.group;
                    temp.learner <- getLearner(
                        learner.metadata = temp.learner.metadata,
                        DF.training      = self$training.data[is.my.level,c(self$response.variable,self$learner.metadata[["predictors"]])],
                        global.objects   = self$global.objects
                        );
                    temp.learner$fit();
                    self$trained.machines[[my.level]] <- temp.learner;
                    }
                }
            logger::log_debug('{this.function.name}(): exits');
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
            if ( base::is.factor(DF.input) ) {
                output.factor <- DF.input;
            } else if ( base::is.character(DF.input) ) {
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
