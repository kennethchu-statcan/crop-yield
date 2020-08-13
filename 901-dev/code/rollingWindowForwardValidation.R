
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
    by.variables.phase01 = c(ecoregion,crop),
    by.variables.phase02 = c(crop),
    by.variables.phase03 = c(ecoregion),
    search.grid          = list(alpha = seq(23,11,-4), lambda = seq(23,11,-4), lambda_bias = seq(23,11,-4)),
    output.directory     = "."
    ) {

    this.function.name <- "rollingWindowForwardValidation";
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    cat(paste0("starting: ",this.function.name,"()\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(jsonlite);
    require(parallel);
    require(foreach);
    require(doParallel);

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
        search.grid          = search.grid,
        output.directory     = output.directory
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    original.wd <- getwd();
    setwd(output.directory);

            predictions.directory <- file.path(output.directory,"predictions");
    performance.metrics.directory <- file.path(output.directory,"performance-metrics");
       mock.productions.directory <- file.path(output.directory,"mock-productions");

    metadata.json         <- file.path(predictions.directory,"learner-metadata.json");

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
        search.grid          = search.grid,
        output.directory     = predictions.directory,
        metadata.json        = metadata.json
        );

    cat("\nlearner.metadata\n");
    print( learner.metadata   );

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
        output.sub.directory        = mock.productions.directory
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\nexiting: ",this.function.name,"()"));
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    setwd(original.wd);
    return( NULL );

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
    
    max.year       <- max(DF.input[,year]);
    training.years <- seq(max.year - training.window + 1, max.year);
    DF.training    <- DF.input[DF.input[,year] %in%   training.years,];

    list.optimal.final.models <- list();
    for ( temp.name in names(list.mock.production.errors) ) {
        DF.temp       <- as.data.frame(list.mock.production.errors[[temp.name]][["mock_production_errors"]]);
        temp.year     <- DF.temp[nrow(DF.temp),"production_year"];
        temp.model.ID <- DF.temp[nrow(DF.temp),"model"];
        temp.filename <- paste0("production-model-RY",temp.year,"-",temp.model.ID,".RData");
        temp.metadata <- learner.metadata[[temp.model.ID]];
        cat("\ntemp.model.ID\n");
        print( temp.model.ID   );
        cat("\nstr(temp.metadata)\n");
        print( str(temp.metadata)   );
        temp.trained.model    <- crop.yield.train.model(
            learner.metadata   = temp.metadata,
            DF.training        = DF.training,
            FILE.trained.model = file.path(output.sub.directory,paste0(temp.filename))
            );
        list.optimal.final.models[[ temp.name ]] <- temp.trained.model;
        }

    return( list.optimal.final.models );

    }

rollingWindowForwardValidation_generate.predictions <- function(
    DF.input         = NULL,
    year             = NULL,
    training.window  = NULL,
    learner.metadata = NULL,
    output.directory = NULL
    ) {

    num.cores <- max(1,parallel::detectCores() - 1);
    cat("\nnum.cores\n");
    print( num.cores   );

    doParallel::registerDoParallel(cores = num.cores);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    min.validation.year <- min(DF.input[,year]) + training.window;
    max.validation.year <- max(DF.input[,year]);
    validation.years    <- seq(min.validation.year,max.validation.year,1);

    foreach ( temp.index = 1:length(learner.metadata) ) %dopar% {

        learner.name <- names(learner.metadata)[temp.index];
        cat(paste0("\n### Learner: ",learner.name,"\n"));

        for (validation.year in validation.years) {

            training.years <- seq(validation.year - training.window, validation.year - 1);
            DF.training    <- DF.input[DF.input[,year] %in%   training.years,];
            DF.validation  <- DF.input[DF.input[,year] ==   validation.year, ];

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

    return( NULL );

    }

rollingWindowForwardValidation_generate.mock.production.errors <- function(
    validation.window        = NULL,
    list.performance.metrics = NULL,
    output.sub.directory     = NULL
    ) {
    
    if ( !dir.exists(output.sub.directory) ) {
        dir.create(path = output.sub.directory, recursive = TRUE);
        }

    list.mock.production.errors <- get.mock.production.errors(
        list.performance.metrics = list.performance.metrics,
        validation.window        = validation.window,
        output.directory         = output.sub.directory
        );

    cat("\nstr(list.mock.production.errors)\n");
    print( str(list.mock.production.errors)   );

    return( list.mock.production.errors );

    }

rollingWindowForwardValidation_generate.performance.metrics <- function(
    metadata.json         = NULL,
    validation.window     = NULL,
    predictions.directory = NULL,
    output.sub.directory  = NULL
    ) {
    
    if ( !dir.exists(output.sub.directory) ) {
        dir.create(path = output.sub.directory, recursive = TRUE);
        }

    temp.json  <- jsonlite::read_json(metadata.json);
    model.name <- temp.json[[1]][["learner"]][[1]];

    list.prediction.directories <- list();
    list.prediction.directories[[model.name]] <- predictions.directory;

    list.performance.metrics <- get.performance.metrics(
        list.prediction.directories = list.prediction.directories,
        output.directory            = output.sub.directory
        );

    cat("\nstr(list.performance.metrics)\n");
    print( str(list.performance.metrics)   );

    return( list.performance.metrics );

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
    search.grid          = NULL,
    output.directory     = NULL
    ) {
 
    }

