
require(R6);
require(xgboost);

learner.xgboost <- R6Class(

    classname = 'learner.xgboost',

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

            this.function.name <- "learner.xgboost$fit";
            log.prefix <- '{this.function.name}():';
            logger::log_debug(paste0(log.prefix,' starts'));

            self$preprocessor = preprocessor$new(
                learner.metadata = self$learner.metadata,
                training.data    = self$training.data
                );

            self$preprocessor$fit();
            self$preprocessed.data <- self$preprocessor$transform(newdata = self$training.data);
            logger::log_debug(paste0(log.prefix,' dim(self$preprocessed.data) = {dim(self$preprocessed.data)}'));
            logger::log_debug(paste0(log.prefix,' colnames(self$preprocessed.data) = {colnames(self$preprocessed.data)}'));

            DMatrix.training <- xgboost::xgb.DMatrix(
                data  = as.matrix(self$preprocessed.data[,setdiff(colnames(self$preprocessed.data),self$response_variable)]),
                label = self$preprocessed.data[,self$response_variable]
                );
            logger::log_debug(paste0(log.prefix,' class(DMatrix.training) = {class(DMatrix.training)}'));

            logger::log_debug(paste0(log.prefix,' calling xgboost::xgb.train()'));
            trained.machine <- xgboost::xgb.train(
                data = DMatrix.training,
                params = list(
                    booster     = 'gblinear',
                    objective   = 'reg:linear',
                    alpha       = self$learner.metadata[["hyperparameters"]][["alpha"]],
                    lambda      = self$learner.metadata[["hyperparameters"]][["lambda"]],
                    lambda_bias = self$learner.metadata[["hyperparameters"]][["lambda_bias"]]
                    ),
                verbose       = self$learner.metadata[["hyperparameters"]][["verbose"]],
                print_every_n = self$learner.metadata[["hyperparameters"]][["print_every_n"]],
                nrounds       = self$learner.metadata[["hyperparameters"]][["nrounds"]],
                save_period   = NULL
                );
            logger::log_debug(paste0(log.prefix,' finished xgboost::xgb.train()'));

            self$trained.machine <- trained.machine;
            logger::log_debug(paste0(log.prefix,' class(self$trained.machine) = {class(self$trained.machine)}'));

            logger::log_debug(paste0(log.prefix,' exits'));

            },

        predict = function(newdata = NULL) {

            preprocessed.newdata <- self$preprocessor$transform(
                newdata = newdata[,c(self$response_variable,self$learner.metadata[["predictors"]])]
                );

            DMatrix.preprocessed.newdata <- xgboost::xgb.DMatrix(
                data  = as.matrix(preprocessed.newdata[,setdiff(colnames(preprocessed.newdata),self$response_variable)]),
                label = preprocessed.newdata[,self$response_variable]
                );
            predicted.response <- predict(
                object  = self$trained.machine,
                newdata = DMatrix.preprocessed.newdata
                );

            DF.output <- newdata;
            DF.output[,"predicted_response"] <- predicted.response;

            return ( DF.output );
            }

        ) # public = list()

    ) # R6Class()

