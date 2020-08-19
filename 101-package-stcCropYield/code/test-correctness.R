
testthat::context(desc = "correctness test suite");

###################################################
test.correctness <- function(
    log.threshold = logger::TRACE
    ) {

    logger::log_appender(logger::appender_console);
    logger::log_threshold(level = log.threshold);

    #test.correctness_xgboost.multiphase();
    test.correctness_group.then.add.relative.error();

    }

#' @importFrom rlang .data
test.correctness_group.then.add.relative.error <- function(
    ) {

    this.function.name <- "test.correctness_group.then.add.relative.error";

    my.DF.input <- base::data.frame(
        ecoregion            = sample(paste0(   "r0",1:5),size=100,replace=TRUE),
        crop                 = sample(paste0("crop0",1:9),size=100,replace=TRUE),
        harvested_area       = abs(rnorm(100)),
        actual_production    = abs(rnorm(100)),
        predicted_production = abs(rnorm(100))
        );

    testthat::test_that(
        desc = "group.then.add.relative.error(): by.variables = NULL",
        code = {
            DF.computed <- validation.single.year_group.then.add.relative.error(
                DF.input     = my.DF.input,
                by.variables = NULL
                );
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.expected <- my.DF.input %>%
               dplyr::select(
                   .data[["harvested_area"]],
                   .data[["actual_production"]],
                   .data[["predicted_production"]]
                   ) %>%
               dplyr::summarize(
                   harvested_area       = sum(.data$harvested_area),
                   actual_production    = sum(.data$actual_production),
                   predicted_production = sum(.data$predicted_production)
                   ) %>%
               dplyr::mutate(
                   relative_error = abs(
                       .data$predicted_production - .data$actual_production
                       ) / .data$actual_production
                   );
            DF.expected <- as.data.frame(DF.expected);
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            test.result <- base::all.equal(DF.computed,DF.expected);
            logger::log_debug('{this.function.name}(): by.variables = NULL, all.equal(DF.computed,DF.expected) = {test.result}');
            testthat::expect_equal( DF.computed, DF.expected );
            }
        );

    testthat::test_that(
        desc = "group.then.add.relative.error(): by.variables = 'crop'",
        code = {
            DF.computed <- validation.single.year_group.then.add.relative.error(
                DF.input     = my.DF.input,
                by.variables = "crop"
                );
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.expected <- my.DF.input %>%
               dplyr::group_by(
                   .data[["crop"]]
                   ) %>%
               dplyr::select(
                   .data[["harvested_area"]],
                   .data[["actual_production"]],
                   .data[["predicted_production"]]
                   ) %>%
               dplyr::summarize(
                   harvested_area       = sum(.data$harvested_area),
                   actual_production    = sum(.data$actual_production),
                   predicted_production = sum(.data$predicted_production)
                   ) %>%
               dplyr::mutate(
                   relative_error = abs(
                       .data$predicted_production - .data$actual_production
                       ) / .data$actual_production
                   );
            DF.expected <- as.data.frame(DF.expected);
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            test.result <- base::all.equal(DF.computed,DF.expected);
            logger::log_debug("{this.function.name}(): by.variables = 'crop', all.equal(DF.computed,DF.expected) = {test.result}");
            testthat::expect_equal( DF.computed, DF.expected );
            }
        );

    testthat::test_that(
        desc = "group.then.add.relative.error(): by.variables = 'ecoregion'",
        code = {
            DF.computed <- validation.single.year_group.then.add.relative.error(
                DF.input     = my.DF.input,
                by.variables = "ecoregion"
                );
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.expected <- my.DF.input %>%
               dplyr::group_by(
                   .data[["ecoregion"]]
                   ) %>%
               dplyr::select(
                   .data[["harvested_area"]],
                   .data[["actual_production"]],
                   .data[["predicted_production"]]
                   ) %>%
               dplyr::summarize(
                   harvested_area       = sum(.data$harvested_area),
                   actual_production    = sum(.data$actual_production),
                   predicted_production = sum(.data$predicted_production)
                   ) %>%
               dplyr::mutate(
                   relative_error = abs(
                       .data$predicted_production - .data$actual_production
                       ) / .data$actual_production
                   );
            DF.expected <- as.data.frame(DF.expected);
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            test.result <- base::all.equal(DF.computed,DF.expected);
            logger::log_debug("{this.function.name}(): by.variables = 'ecoregion', all.equal(DF.computed,DF.expected) = {test.result}");
            testthat::expect_equal( DF.computed, DF.expected );
            }
        );

    testthat::test_that(
        desc = "group.then.add.relative.error(): by.variables = c('ecoregion','crop')",
        code = {
            DF.computed <- validation.single.year_group.then.add.relative.error(
                DF.input     = my.DF.input,
                by.variables = c("ecoregion","crop")
                );
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            DF.expected <- my.DF.input %>%
               dplyr::group_by(
                   .data[["ecoregion"]],
                   .data[["crop"]]
                   ) %>%
               dplyr::select(
                   .data[["harvested_area"]],
                   .data[["actual_production"]],
                   .data[["predicted_production"]]
                   ) %>%
               dplyr::summarize(
                   harvested_area       = sum(.data$harvested_area),
                   actual_production    = sum(.data$actual_production),
                   predicted_production = sum(.data$predicted_production)
                   ) %>%
               dplyr::mutate(
                   relative_error = abs(
                       .data$predicted_production - .data$actual_production
                       ) / .data$actual_production
                   );
            DF.expected <- as.data.frame(DF.expected);
            DF.expected <- DF.expected[order(DF.expected[,"crop"],DF.expected[,"ecoregion"]),]
            rownames(DF.expected) <- NULL;
            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            test.result <- base::all.equal(DF.computed,DF.expected);
            logger::log_debug("{this.function.name}(): by.variables = c('ecoregion','crop'), all.equal(DF.computed,DF.expected) = {test.result}");
            testthat::expect_equal( DF.computed, DF.expected );
            }
        );

    }

test.correctness_xgboost.multiphase <- function(
    n.ecoregions = 2,
    n.crops      = 3,
    n.predictors = 7
    ) {

    this.function.name <- "test.correctness_xgboost.multiphase";
    logger::log_debug('{this.function.name}(): starts');

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

            DF.training   <- DF.synthetic[DF.synthetic[,"my_year"] != 2020,];
            DF.validation <- DF.synthetic[DF.synthetic[,"my_year"] == 2020,];

            trained.model <- crop.yield.train.model(
                DF.training          = DF.training,
                year                 = "my_year",
                ecoregion            = "my_ecoregion",
                crop                 = "my_crop",
                response.variable    = "my_yield",
                harvested.area       = "my_harvested_area",
                predictors           = base::grep(x = base::colnames(DF.synthetic), pattern = "x[0-9]*", value = TRUE),
                learner              = "xgboost_multiphase",
                by.variables.phase01 = base::c("my_ecoregion","my_crop"),
                by.variables.phase02 = base::c("my_crop"),
                by.variables.phase03 = base::c("my_ecoregion"),
                hyperparameters      = base::list(alpha = 23, lambda = 23, lambda_bias = 23)
                );

            DF.computed <- trained.model$predict(newdata = DF.validation);

            DF.expected <- test.correctness_xgboost.multiphase_get.expected.output(
                DF.training          = DF.training,
                DF.validation        = DF.validation,
                year                 = "my_year",
                ecoregion            = "my_ecoregion",
                crop                 = "my_crop",
                response.variable    = "my_yield",
                harvested.area       = "my_harvested_area",
                predictors           = base::grep(x = base::colnames(DF.synthetic), pattern = "x[0-9]*", value = TRUE),
                learner              = "xgboost_multiphase",
                by.variables.phase01 = base::c("my_ecoregion","my_crop"),
                by.variables.phase02 = base::c("my_crop"),
                by.variables.phase03 = base::c("my_ecoregion"),
                hyperparameters      = base::list(alpha = 23, lambda = 23, lambda_bias = 23)
                );

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            test.result <- base::all.equal(DF.computed,DF.expected);
            logger::log_debug('{this.function.name}(): all.equal(DF.computed,DF.expected) = {test.result}');

            ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            testthat::expect_equal( DF.computed, DF.expected );

            }
        ); # testthat::test_that()
    
    logger::log_debug('{this.function.name}(): exits');

    }

#' @importFrom rlang .data
test.correctness_xgboost.multiphase_get.expected.output <- function(
    DF.training          = NULL,
    DF.validation        = NULL,
    year                 = NULL,
    ecoregion            = NULL,
    crop                 = NULL,
    response.variable    = NULL,
    harvested.area       = NULL,
    predictors           = NULL,
    learner              = NULL,
    by.variables.phase01 = NULL,
    by.variables.phase02 = NULL,
    by.variables.phase03 = NULL,
    hyperparameters      = NULL
    ) {

    this.function.name <- "test.correctness_xgboost.multiphase_get.expected.output";
    logger::log_debug('{this.function.name}(): starts');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_debug('{this.function.name}(): learner metadata starts');

    learner.metadata <- get.learner.metadata_private.helper(
        year                 = year,
        ecoregion            = ecoregion,
        crop                 = crop,
        response.variable    = response.variable,
        harvested.area       = harvested.area,
        predictors           = predictors,
        by.variables.phase01 = by.variables.phase01,
        by.variables.phase02 = by.variables.phase02,
        by.variables.phase03 = by.variables.phase03,
        learner              = learner,
        search.grid          = hyperparameters
        );

    learner.metadata <- learner.metadata[[1]];

    logger::log_debug('{this.function.name}(): learner metadata generation complete');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_debug('{this.function.name}(): assembling of DF.training and DF.validation starts');

    DF.training <- DF.training;
    DF.training[,"ecoregion.crop"] <- base::apply(
        X      = ,DF.training[,base::c(ecoregion,crop)],
        MARGIN = 1,
        FUN    = function(x) { base::paste(x, collapse = "_") }
        );

    DF.validation <- DF.validation;
    DF.validation[,"syntheticID"] <- 1:base::nrow(DF.validation);

    DF.output <- DF.validation;

    DF.validation[,"ecoregion.crop"] <- base::apply(
        X      = ,DF.validation[,base::c(ecoregion,crop)],
        MARGIN = 1,
        FUN    = function(x) { base::paste(x, collapse = "_") }
        );

    logger::log_debug('{this.function.name}(): assembling of DF.training and DF.validation complete');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    byVars <- base::list(
        "01" = "ecoregion.crop",
        "02" = crop,
        "03" = ecoregion
        );

    for ( temp.phase in base::names(byVars) ) {

        colname.prediction <- base::paste0("predicted_response_phase",temp.phase);

        DF.temp.output <- base::data.frame();
        temp.groups <- base::unique(DF.training[,byVars[[temp.phase]]]);

        for ( temp.group in temp.groups ) {

            logger::log_debug('{this.function.name}(): training/prediction for (phase,group) = ({temp.phase},{temp.group}) starts');

            DF.temp.training   <- DF.training[  temp.group == DF.training[,  byVars[[temp.phase]]],];
            DF.temp.validation <- DF.validation[temp.group == DF.validation[,byVars[[temp.phase]]],];

            DMatrix.training <- xgboost::xgb.DMatrix(
                data  = base::as.matrix(DF.temp.training[,learner.metadata[["predictors"]]]),
                label = DF.temp.training[,learner.metadata[["response_variable"]]]
                );

            logger::log_debug('{this.function.name}(): training/prediction for (phase,group) = ({temp.phase},{temp.group}): finished creation of DMatrix.training');

            trained.machine <- xgboost::xgb.train(
                data = DMatrix.training,
                params = base::list(
                    booster     = 'gblinear',
                    objective   = 'reg:squarederror', # deprecated: 'reg:linear',
                    alpha       = learner.metadata[["hyperparameters"]][["alpha"]],
                    lambda      = learner.metadata[["hyperparameters"]][["lambda"]]
                    #,lambda_bias = learner.metadata[["hyperparameters"]][["lambda_bias"]]
                    ),
                verbose       = learner.metadata[["hyperparameters"]][["verbose"]],
                print_every_n = learner.metadata[["hyperparameters"]][["print_every_n"]],
                nrounds       = learner.metadata[["hyperparameters"]][["nrounds"]],
                save_period   = NULL
                );

            logger::log_debug('{this.function.name}(): training/prediction for (phase,group) = ({temp.phase},{temp.group}): finished creation of trained.machine');

            DMatrix.validation <- xgboost::xgb.DMatrix(
                data  = base::as.matrix(DF.temp.validation[,learner.metadata[["predictors"]]]),
                label = rep(NA,base::nrow(DF.temp.validation))
                );

            logger::log_debug('{this.function.name}(): training/prediction for (phase,group) = ({temp.phase},{temp.group}): finished creation of DMatrix.validation');

            predicted.response <- stats::predict(
                object  = trained.machine,
                newdata = DMatrix.validation
                );

            logger::log_debug('{this.function.name}(): training/prediction for (phase,group) = ({temp.phase},{temp.group}): finished creation of predicted.response');

            DF.temp.validation[,colname.prediction] <- predicted.response;
            DF.temp.output <- base::rbind(DF.temp.output,DF.temp.validation);

            logger::log_debug('{this.function.name}(): training/prediction for (phase,group) = ({temp.phase},{temp.group}): finished creation of DF.temp.output');

            logger::log_debug('{this.function.name}(): training/prediction for (phase,group) = ({temp.phase},{temp.group}) complete');

            }

        DF.output <- dplyr::left_join(
            x  = DF.output,
            y  = DF.temp.output[,base::c("syntheticID",colname.prediction)],
            by = "syntheticID"
            );

        logger::log_debug('{this.function.name}(): training/prediction for Phase {temp.phase} complete');

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

    logger::log_debug('{this.function.name}(): creation of DF.output complete');

    logger::log_debug('{this.function.name}(): exits');
    base::return( DF.output );

    }

###################################################
test.correctness();

