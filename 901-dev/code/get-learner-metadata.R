
get.learner.metadata <- function(
    year                 = "year",
    ecoregion            = "ecoregion",
    crop                 = "crop",
    response.variable    = "yield",
    harvested.area       = "harvested_area",
    predictors           = NULL,
    by.variables.phase01 = c(ecoregion,crop),
    by.variables.phase02 = c(crop),
    by.variables.phase03 = c(ecoregion),
    learner              = "xgboost_multiphase",
    search.grid          = list(alpha = seq(23,11,-4), lambda = seq(23,11,-4), lambda_bias = seq(23,11,-4)),
    output.directory     = "predictions",
    metadata.json        = file.path(output.directory,"learner-metadata.json")
    ) {

    this.function.name <- "get.learner.metadata";
    logger::log_info('{this.function.name}(): starts');

    require(jsonlite);
    require(stringr);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
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
        search.grid          = search.grid
        );

    logger::log_info( '{this.function.name}(): length(learner.metadata): {length(learner.metadata)}');
    logger::log_debug('{this.function.name}(): names(learner.metadata):\n{paste(names(learner.metadata),collapse="\n")}');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( !dir.exists(output.directory) ) {
        dir.create(path = output.directory, recursive = TRUE);
        }

    jsonlite::write_json(
        x      = learner.metadata,
        path   = metadata.json,
        pretty = TRUE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_info('{this.function.name}(): exits');
    return( learner.metadata );

    }

##################################################
get.learner.metadata_private.helper <- function(
    year                 = NULL,
    ecoregion            = NULL,
    crop                 = NULL,
    response.variable    = NULL,
    harvested.area       = NULL,
    predictors           = c(),
    by.variables.phase01 = c(ecoregion,crop),
    by.variables.phase02 = c(crop),
    by.variables.phase03 = c(ecoregion),
    learner              = "xgboost_multiphase",
    search.grid          = list(alpha = seq(23,11,-4), lambda = seq(23,11,-4), lambda_bias = seq(23,11,-4))
    ) {

    temp.list     <- list();
    expanded.grid <- base::expand.grid(search.grid);
    for ( metadata.count in 1:nrow(expanded.grid) ) {

        metadata.suffix <- stringr::str_pad(
            string = metadata.count,
            width  = 1 + floor(log10(nrow(expanded.grid))),
            pad    = "0"
            );

        metadata.ID <- paste0(learner,"_",metadata.suffix);

        inner.list <- list();
        for ( temp.colname in colnames(expanded.grid) ) {
            inner.list[[temp.colname]] = expanded.grid[metadata.count,temp.colname]
            }
        inner.list <- c(
            inner.list,
            verbose       =   2,
            print_every_n =  10,
            nrounds       = 500
            );

        temp.list[[ metadata.ID ]] <- inner.list;

        }

    output.learner.metadata <- lapply(
        X = temp.list,
        FUN = function(x) {
            return(
                list(
                    learner              = learner,
                    year                 = year,
                    ecoregion            = ecoregion,
                    crop                 = crop,
                    response_variable    = response.variable,
                    harvested_area       = harvested.area,
                    predictors           = setdiff(predictors,c(response.variable,harvested.area)),
                    by_variables_phase01 = by.variables.phase01,
                    by_variables_phase02 = by.variables.phase02,
                    by_variables_phase03 = by.variables.phase03,
                    hyperparameters      = x
                    #,binarize_factors     = TRUE
                    )
                );
            }
        );

    return( output.learner.metadata );

    }

