
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

            self$preprocessor = preprocessor$new(
                learner.metadata = self$learner.metadata,
                training.data    = self$training.data
                );

            self$preprocessor$fit();
            self$preprocessed.data <- self$preprocessor$transform(newdata = self$training.data);

            DMatrix.training <- xgboost::xgb.DMatrix(
                data  = as.matrix(self$preprocessed.data[,setdiff(colnames(self$preprocessed.data),self$response_variable)]),
                label = self$preprocessed.data[,self$response_variable]
                );

            cat("\ndim(self$preprocessed.data)\n");
            print( dim(self$preprocessed.data)   );

            cat("\ncolnames(self$preprocessed.data)\n");
            print( colnames(self$preprocessed.data)   );

            cat("\n# calling xgboost::xgb.train() ...");
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
            cat("\n# finished: xgboost::xgb.train()");

            self$trained.machine <- trained.machine;

            cat("\nstr(self$trained.machine)\n");
            print( str(self$trained.machine)   );

            },

        predict = function(newdata = NULL) {

            cat("\ncolnames(newdata) -- learner.xgboost$predict()\n");
            print( colnames(newdata) );

            preprocessed.newdata <- self$preprocessor$transform(
                newdata = newdata[,c(self$response_variable,self$learner.metadata[["predictors"]])]
                );

            cat("\ndim(preprocessed.newdata) -- learner.xgboost$predict()\n");
            print( dim(preprocessed.newdata) );

            cat("\ncolnames(preprocessed.newdata) -- learner.xgboost$predict()\n");
            print( colnames(preprocessed.newdata) );

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

            cat("\ncolnames(DF.output) -- learner.xgboost$predict()\n");
            print( colnames(DF.output) );

            return ( DF.output );
            }

        ) # public = list()

    ) # R6Class()

