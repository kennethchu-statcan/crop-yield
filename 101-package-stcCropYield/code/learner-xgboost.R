
learner.xgboost <- R6::R6Class(

    classname = 'learner.xgboost',

    #inherit = learner.abstract,

    public = base::list(

        ### instantiation parameters (inherited)
        global.objects   = NULL,
        learner.metadata = NULL,
        training.data    = NULL,

        ### class attributes (inherited)
        response_variable = NULL,
        preprocessor      = NULL,
        preprocessed.data = NULL,
        trained.machine   = NULL,

        ### initialize() -- inherited
        initialize = function(
            global.objects   = NULL,
            learner.metadata = NULL,
            training.data    = NULL
            ) {
            self$global.objects    <- global.objects;
            self$learner.metadata  <- learner.metadata;
            self$response_variable <- self$learner.metadata[["response_variable"]];
            self$training.data     <- training.data[,c(self$response_variable,self$learner.metadata[["predictors"]])];
            },

        fit = function() {

            this.function.name <- "learner.xgboost$fit";
            log.prefix         <- '{this.function.name}():';
            logger::log_debug(base::paste0(log.prefix,' starts'));

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

            self$preprocessor <- preprocessor$new(
                learner.metadata = self$learner.metadata,
                training.data    = self$training.data
                );

            self$preprocessor$fit();
            self$preprocessed.data <- self$preprocessor$transform(newdata = self$training.data);
            logger::log_debug(base::paste0(log.prefix,' dim(self$preprocessed.data) = c({paste0(dim(self$preprocessed.data),collapse=",")})'));
            logger::log_debug(base::paste0(log.prefix,' colnames(self$preprocessed.data) = c({paste0(colnames(self$preprocessed.data),collapse=",")})'));

            DMatrix.training <- xgboost::xgb.DMatrix(
                data  = base::as.matrix(self$preprocessed.data[,base::setdiff(base::colnames(self$preprocessed.data),self$response_variable)]),
                label = self$preprocessed.data[,self$response_variable]
                );
            logger::log_debug(base::paste0(log.prefix,' class(DMatrix.training) = {class(DMatrix.training)}'));

            logger::log_debug(base::paste0(log.prefix,' calling xgboost::xgb.train()'));
            trained.machine <- xgboost::xgb.train(
                data = DMatrix.training,
                params = base::list(
                    booster     = 'gblinear',
                    objective   = 'reg:squarederror', # deprecated: 'reg:linear',
                    alpha       = self$learner.metadata[["hyperparameters"]][["alpha"]],
                    lambda      = self$learner.metadata[["hyperparameters"]][["lambda"]]
                    #,lambda_bias = self$learner.metadata[["hyperparameters"]][["lambda_bias"]]
                    ),
                verbose       = self$learner.metadata[["hyperparameters"]][["verbose"]],
                print_every_n = self$learner.metadata[["hyperparameters"]][["print_every_n"]],
                nrounds       = self$learner.metadata[["hyperparameters"]][["nrounds"]],
                save_period   = NULL
                );
            logger::log_debug(base::paste0(log.prefix,' finished xgboost::xgb.train()'));

            self$trained.machine <- trained.machine;
            logger::log_debug(base::paste0(log.prefix,' class(self$trained.machine) = {class(self$trained.machine)}'));

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            logger::log_debug(base::paste0(log.prefix,' exits'));

            },

        predict = function(newdata = NULL) {

            preprocessed.newdata <- self$preprocessor$transform(
                newdata = newdata[,base::c(self$response_variable,self$learner.metadata[["predictors"]])]
                );

            DMatrix.preprocessed.newdata <- xgboost::xgb.DMatrix(
                data  = base::as.matrix(preprocessed.newdata[,base::setdiff(base::colnames(preprocessed.newdata),self$response_variable)]),
                label = base::rep(NA,base::nrow(preprocessed.newdata))
                );

            predicted.response <- stats::predict(
                object  = self$trained.machine,
                newdata = DMatrix.preprocessed.newdata
                );

            DF.output <- newdata;
            DF.output[,"predicted_response"] <- predicted.response;

            base::return ( DF.output );
            }

        ) # public = list()

    ) # R6Class()

