
testthat::context(desc = "correctness test suite");

test.correctness <- function(
    log.threshold = logger::WARN
    ) {

    logger::log_appender(logger::appender_console);
    logger::log_threshold(level = log.threshold);

    test.correctness_xgboost.multiphase();

    }

###################################################
test.correctness_xgboost.multiphase <- function(
    n.ecoregions = 2,
    n.crops      = 3,
    n.predictors = 7
    ) {

    testthat::test_that(
        desc = "correctness of xgboost.multiphase",
        code = {

            n.ecoregions <- n.ecoregions;
            n.crops      <- n.crops;
            n.predictors <- n.predictors;

            DF.synthetic <- getData.synthetic(
                years        = base::seq(2015,2020),
                n.ecoregions = n.ecoregions,
                n.crops      = n.crops,
                n.predictors = n.predictors,
                output.RData = NULL,
                output.csv   = NULL
                );
            cat("\nstr(DF.synthetic)\n");
            print( str(DF.synthetic)   );

            DF.training   <- DF.synthetic[DF.synthetic[,"my_year"] != 2020,];
            cat("\nstr(DF.training)\n");
            print( str(DF.training)   );

            DF.validation <- DF.synthetic[DF.synthetic[,"my_year"] == 2020,];
            cat("\nstr(DF.validation)\n");
            print( str(DF.validation)   );

            learner.metadata <- get.learner.metadata_private.helper(
                year                 = "my_year",
                ecoregion            = "my_ecoregion",
                crop                 = "my_crop",
                response.variable    = "my_yield",
                harvested.area       = "my_harvested_area",
                predictors           = base::grep(x = base::colnames(DF.synthetic), pattern = "x[0-9]*", value = TRUE),
                by.variables.phase01 = base::c("my_ecoregion","my_crop"),
                by.variables.phase02 = base::c("my_crop"),
                by.variables.phase03 = base::c("my_ecoregion"),
                learner              = "xgboost_multiphase",
                search.grid          = base::list(
                    alpha       = base::c(23),
                    lambda      = base::c(23),
                    lambda_bias = base::c(23)
                    )
                );
            cat("\nstr(learner.metadata)\n");
            print( str(learner.metadata)   );

            trained.model <- crop.yield.train.model(
                learner.metadata = learner.metadata[[1]],
                DF.training      = DF.training
                );
            cat("\nclass(trained.model)\n");
            print( class(trained.model)   );

            DF.computed <- trained.model$predict(newdata = DF.validation);
            cat("\nstr(DF.computed)\n");
            print( str(DF.computed)   );

            DF.expected <- test.correctness_xgboost.multiphase_get.expected.output(
                learner.metadata = learner.metadata[[1]],
                DF.training      = DF.training,
                DF.validation    = DF.validation
                );
            cat("\nstr(DF.expected)\n");
            print( str(DF.expected)   );

            cat("\nall.equal(DF.computed,DF.expected)\n");
            print( all.equal(DF.computed,DF.expected)   );
            testthat::expect_equal( DF.computed, DF.expected );

            }
        );
    
    }

test.correctness_xgboost.multiphase_get.expected.output <- function(
    learner.metadata = NULL,
    DF.training      = NULL,
    DF.validation    = NULL
    ) {

    base::require(dplyr);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.training <- DF.training;
    DF.training[,"ecoregion.crop"] <- base::apply(
        X      = ,DF.training[,base::c("my_ecoregion","my_crop")],
        MARGIN = 1,
        FUN    = function(x) { base::paste(x, collapse = "_") }
        );

    DF.validation <- DF.validation;
    DF.validation[,"syntheticID"] <- 1:base::nrow(DF.validation);

    DF.output <- DF.validation;

    DF.validation[,"ecoregion.crop"] <- base::apply(
        X      = ,DF.validation[,base::c("my_ecoregion","my_crop")],
        MARGIN = 1,
        FUN    = function(x) { base::paste(x, collapse = "_") }
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    byVars <- base::list(
        "01" = "ecoregion.crop",
        "02" = "my_crop",
        "03" = "my_ecoregion"
        );

    for ( temp.phase in base::names(byVars) ) {

        colname.prediction <- base::paste0("predicted_response_phase",temp.phase);

        DF.temp.output <- base::data.frame();
        temp.groups <- base::unique(DF.training[,byVars[[temp.phase]]]);

        for ( temp.group in temp.groups ) {

            DF.temp.training   <- DF.training[  temp.group == DF.training[,  byVars[[temp.phase]]],];
            DF.temp.validation <- DF.validation[temp.group == DF.validation[,byVars[[temp.phase]]],];

            DMatrix.training <- xgboost::xgb.DMatrix(
                data  = base::as.matrix(DF.temp.training[,learner.metadata[["predictors"]]]),
                label = DF.temp.training[,learner.metadata[["response_variable"]]]
                );

            trained.machine <- xgboost::xgb.train(
                data = DMatrix.training,
                params = base::list(
                    booster     = 'gblinear',
                    objective   = 'reg:linear',
                    alpha       = learner.metadata[["hyperparameters"]][["alpha"]],
                    lambda      = learner.metadata[["hyperparameters"]][["lambda"]],
                    lambda_bias = learner.metadata[["hyperparameters"]][["lambda_bias"]]
                    ),
                verbose       = learner.metadata[["hyperparameters"]][["verbose"]],
                print_every_n = learner.metadata[["hyperparameters"]][["print_every_n"]],
                nrounds       = learner.metadata[["hyperparameters"]][["nrounds"]],
                save_period   = NULL
                );

            DMatrix.validation <- xgboost::xgb.DMatrix(
                data  = base::as.matrix(DF.temp.validation[,learner.metadata[["predictors"]]]),
                label = rep(NA,base::nrow(DF.temp.validation))
                );

            predicted.response <- predict(
                object  = trained.machine,
                newdata = DMatrix.validation
                );

            DF.temp.validation[,colname.prediction] <- predicted.response;
            DF.temp.output <- base::rbind(DF.temp.output,DF.temp.validation);

            }

        DF.output <- DF.output %>% dplyr::left_join(
            x  = DF.output,
            y  = DF.temp.output[,base::c("syntheticID",colname.prediction)],
            by = "syntheticID"
            );

        }

    DF.output[,"predicted_response"] <- base::apply(
        X      = DF.output[,base::paste0("predicted_response_phase",base::names(byVars))],
        MARGIN = 1,
        FUN    = function(x) {
            base::ifelse(base::all(base::is.na(x)),NA,x[base::which(!base::is.na(x))[1]])
            }
        );

    DF.output[,"phase"] <- base::apply(
        X      = DF.output[,base::paste0("predicted_response_phase",base::names(byVars))],
        MARGIN = 1,
        FUN    = function(x) {
            base::ifelse(base::all(base::is.na(x)),NA,base::which(!base::is.na(x))[1])
            }
        );

    DF.output <- DF.output[,setdiff(colnames(DF.output),"syntheticID")];

    base::return( DF.output );

    }

