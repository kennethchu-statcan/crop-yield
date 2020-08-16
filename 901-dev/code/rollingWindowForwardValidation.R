
base::require(logger);

rollingWindowForwardValidation <- function(
    training.window      = NULL,
    validation.window    = NULL,
    DF.input             = NULL,
    year                 = "year",
    ecoregion            = "ecoregion",
    crop                 = "crop",
    response.variable    = "yield",
    harvested.area       = "harvested_area",
    predictors           = NULL,
    by.variables.phase01 = base::c(ecoregion,crop),
    by.variables.phase02 = base::c(crop),
    by.variables.phase03 = base::c(ecoregion),
    learner              = "xgboost_multiphase",
    search.grid          = base::list(alpha = base::seq(23,11,-4), lambda = base::seq(23,11,-4), lambda_bias = base::seq(23,11,-4)),
    output.directory     = ".",
    log.threshold        = logger::INFO
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    original.wd <- base::getwd();
    if ( !base::dir.exists(output.directory) ) {
        base::dir.create(path = output.directory, recursive = TRUE);
        }
    base::setwd(output.directory);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    this.function.name <- "rollingWindowForwardValidation";
    log.file <- base::file.path(paste0(this.function.name,".log"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_threshold(level = log.threshold);
    logger::log_appender(logger::appender_tee(file = log.file));
    logger::log_info('{this.function.name}(): starts');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    base::require(jsonlite);
    base::require(parallel);
    base::require(foreach);
    base::require(doParallel);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    rollingWindowForwardValidation_input.validity.checks(
        training.window      = training.window,
        validation.window    = validation.window,
        DF.input             = DF.input,
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
        search.grid          = search.grid,
        output.directory     = output.directory
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
            predictions.directory <- base::file.path(output.directory,"010-predictions");
    performance.metrics.directory <- base::file.path(output.directory,"020-performance-metrics");
       mock.productions.directory <- base::file.path(output.directory,"030-mock-productions");

    metadata.json <- base::file.path(predictions.directory,"learner-metadata.json");

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    learner.metadata <- get.learner.metadata(
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
        search.grid          = search.grid,
        output.directory     = predictions.directory,
        metadata.json        = metadata.json
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    rollingWindowForwardValidation_generate.predictions(
        DF.input         = DF.input,
        year             = year,
        training.window  = training.window,
        learner.metadata = learner.metadata,
        output.directory = predictions.directory
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.performance.metrics <- rollingWindowForwardValidation_generate.performance.metrics(
        metadata.json         = metadata.json,
        validation.window     = validation.window,
        predictions.directory = predictions.directory,
        output.sub.directory  = performance.metrics.directory
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.mock.production.errors <- rollingWindowForwardValidation_generate.mock.production.errors(
        validation.window        = validation.window,
        list.performance.metrics = list.performance.metrics,
        output.sub.directory     = mock.productions.directory
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    list.optimal.final.models <- rollingWindowForwardValidation_save.optimal.final.models(
        DF.input                    = DF.input,
        year                        = year,
        training.window             = training.window,
        learner.metadata            = learner.metadata,
        list.mock.production.errors = list.mock.production.errors,
        output.sub.directory        = output.directory
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_info("{this.function.name}(): getOption('repos'):\n{getOption('repos')}");
    logger::log_info("{this.function.name}(): .libPaths():\n{base::paste(.libPaths(),collapse='\n')}");
    logger::log_info("{this.function.name}(): warnings():\n{base::paste(utils::capture.output(base::warnings()),collapse='\n')}");
    logger::log_info("{this.function.name}(): sessionInfo():\n{base::paste(utils::capture.output(utils::sessionInfo()),collapse='\n')}");

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_info('{this.function.name}(): exits');
    base::setwd(original.wd);
    base::return( NULL );

    }

##################################################
rollingWindowForwardValidation_save.optimal.final.models <- function(
    DF.input                    = NULL,
    year                        = NULL,
    training.window             = NULL,
    learner.metadata            = NULL,
    list.mock.production.errors = NULL,
    output.sub.directory        = NULL
    ) {
    
    max.year       <- base::max(DF.input[,year]);
    training.years <- base::seq(max.year - training.window + 1, max.year);
    DF.training    <- DF.input[DF.input[,year] %in%   training.years,];

    list.optimal.final.models <- base::list();
    for ( temp.name in base::names(list.mock.production.errors) ) {

        DF.temp       <- base::as.data.frame(list.mock.production.errors[[temp.name]][["mock_production_errors"]]);
        temp.year     <- DF.temp[base::nrow(DF.temp),"production_year"];
        temp.model.ID <- DF.temp[base::nrow(DF.temp),"model"];
        temp.filename <- base::paste0("production-model-RY",temp.year,"-",temp.model.ID,".RData");
        temp.metadata <- learner.metadata[[temp.model.ID]];

        temp.trained.model    <- crop.yield.train.model(
            learner.metadata   = temp.metadata,
            DF.training        = DF.training,
            FILE.trained.model = base::file.path(output.sub.directory,base::paste0(temp.filename))
            );
        list.optimal.final.models[[ temp.name ]] <- temp.trained.model;

        }

    base::return( list.optimal.final.models );

    }

rollingWindowForwardValidation_generate.predictions <- function(
    DF.input         = NULL,
    year             = NULL,
    training.window  = NULL,
    learner.metadata = NULL,
    output.directory = NULL
    ) {

    this.function.name <- "rollingWindowForwardValidation_generate.predictions";
    logger::log_info('{this.function.name}(): starts');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( logger::log_threshold() >= logger::DEBUG ) {
        num.cores <- 1;
    } else {
        num.cores <- base::max(1,parallel::detectCores() - 1);
        }

    logger::log_info('{this.function.name}(): number of cores to be used in parallel: {num.cores}');

    doParallel::registerDoParallel(cores = num.cores);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    min.validation.year <- base::min(DF.input[,year]) + training.window;
    max.validation.year <- base::max(DF.input[,year]);
    validation.years    <- base::seq(min.validation.year,max.validation.year,1);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_info('{this.function.name}(): training window: {training.window}');
    logger::log_info('{this.function.name}(): validation years: c({paste(validation.years,collapse=",")})');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    foreach::foreach ( temp.index = 1:base::length(learner.metadata) ) %dopar% {

        learner.name <- base::names(learner.metadata)[temp.index];

        for (validation.year in validation.years) {

            log.prefix <- '{this.function.name}(): ({learner.name},{validation.year})';

            training.years <- base::seq(validation.year - training.window, validation.year - 1);
            logger::log_info(base::paste0(log.prefix,', training.years = c({paste(training.years,collapse=",")})'));

            DF.training <- DF.input[DF.input[,year] %in%   training.years,];
            logger::log_info(base::paste0(log.prefix,', nrow(DF.training) = {nrow(DF.training)})'));

            DF.validation <- DF.input[DF.input[,year] ==   validation.year, ];
            logger::log_info(base::paste0(log.prefix,', nrow(DF.validation) = {nrow(DF.validation)})'));

            validation.single.year(
                learner.name     = learner.name,
                validation.year  = validation.year,
                learner.metadata = learner.metadata[[learner.name]],
                DF.training      = DF.training,
                DF.validation    = DF.validation,
                output.directory = output.directory
                );

            }

        }

    logger::log_info('{this.function.name}(): exits');
    base::return( NULL );

    }

rollingWindowForwardValidation_generate.mock.production.errors <- function(
    validation.window        = NULL,
    list.performance.metrics = NULL,
    output.sub.directory     = NULL
    ) {
    
    if ( !base::dir.exists(output.sub.directory) ) {
        base::dir.create(path = output.sub.directory, recursive = TRUE);
        }

    list.mock.production.errors <- get.mock.production.errors(
        list.performance.metrics = list.performance.metrics,
        validation.window        = validation.window,
        output.directory         = output.sub.directory
        );

    base::return( list.mock.production.errors );

    }

rollingWindowForwardValidation_generate.performance.metrics <- function(
    metadata.json         = NULL,
    validation.window     = NULL,
    predictions.directory = NULL,
    output.sub.directory  = NULL
    ) {
    
    if ( !base::dir.exists(output.sub.directory) ) {
        base::dir.create(path = output.sub.directory, recursive = TRUE);
        }

    temp.json  <- jsonlite::read_json(metadata.json);
    model.name <- temp.json[[1]][["learner"]][[1]];

    list.prediction.directories <- base::list();
    list.prediction.directories[[model.name]] <- predictions.directory;

    list.performance.metrics <- get.performance.metrics(
        list.prediction.directories = list.prediction.directories,
        output.directory            = output.sub.directory
        );

    base::return( list.performance.metrics );

    }

rollingWindowForwardValidation_input.validity.checks <- function(
    training.window      = NULL,
    validation.window    = NULL,
    DF.input             = NULL,
    year                 = NULL,
    ecoregion            = NULL,
    crop                 = NULL,
    response.variable    = NULL,
    harvested.area       = NULL,
    predictors           = NULL,
    by.variables.phase01 = NULL,
    by.variables.phase02 = NULL,
    by.variables.phase03 = NULL,
    learner              = NULL,
    search.grid          = NULL,
    output.directory     = NULL
    ) {
 
    }

