
get.learner.metadata <- function(
    year                 = "year",
    ecoregion            = "ecoregion",
    crop                 = "crop",
    response.variable    = "yield",
    harvested.area       = "harvested_area",
    predictors           = NULL,
    by.variables.phase01 = c(ecoregion,crop),
    by.variables.phase02 = c(crop),
    search.grid          = list(alpha = seq(23,11,-4), lambda = seq(23,11,-4), lambda_bias = seq(23,11,-4))
    ) {

    this.function.name <- "get.learner.metadata";
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    cat(paste0("starting: ",this.function.name,"()\n"));

    require(jsonlite);
    require(stringr);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    learner.metadata <- c(
        get.learner.metadata_xgboost.multiphase(
            year                 = year,
            ecoregion            = ecoregion,
            crop                 = crop,
            response.variable    = response.variable,
            harvested.area       = harvested.area,
            predictors           = predictors,
            by.variables.phase01 = by.variables.phase01,
            by.variables.phase02 = by.variables.phase02,
            search.grid          = search.grid
            )
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    jsonlite::write_json(
        x      = learner.metadata,
        path   = "learner-metadata.json",
        pretty = TRUE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\nexiting: ",this.function.name,"()"));
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    return( learner.metadata );

    }

##################################################
get.learner.metadata_xgboost.multiphase <- function(
    year                 = NULL,
    ecoregion            = NULL,
    crop                 = NULL,
    response.variable    = NULL,
    harvested.area       = NULL,
    predictors           = c(),
    by.variables.phase01 = c(ecoregion,crop),
    by.variables.phase02 = c(crop),
    search.grid          = list(alpha = seq(23,11,-4), lambda = seq(23,11,-4), lambda_bias = seq(23,11,-4))
    ) {

    temp.list     <- list();
    expanded.grid <- base::expand.grid(search.grid);
    for ( metadata.count in 1:nrow(expanded.grid)) {

        metadata.suffix <- stringr::str_pad(
            string = metadata.count,
            width  = 1 + floor(log10(nrow(expanded.grid))),
            pad    = "0"
            );

        metadata.ID <- paste0("xgboost_multiphase","_",metadata.suffix);

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
                    learner              = "xgboost_multiphase",
                    year                 = year,
                    ecoregion            = ecoregion,
                    crop                 = crop,
                    response_variable    = response.variable,
                    harvested_area       = harvested.area,
                    predictors           = setdiff(predictors,c(response.variable,harvested.area)),
                    by_variables_phase01 = by.variables.phase01,
                    by_variables_phase02 = by.variables.phase02,
                    hyperparameters      = x,
                    binarize_factors     = TRUE
                    )
                );
            }
        );

    return( output.learner.metadata );

    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DELETE.get.learner.metadata_xgboost <- function(
    learner           = NULL,
    by.variables      = NULL,
    year              = NULL,
    ecoregion         = NULL,
    crop              = NULL,
    response.variable = NULL,
    harvested.area    = NULL,
    predictors        = c(),
    search.grid       = list(alpha = seq(23,11,-4), lambda = seq(23,11,-4), lambda_bias = seq(23,11,-4))
    ) {

    temp.list     <- list();
    expanded.grid <- base::expand.grid(search.grid);
    for ( metadata.count in 1:nrow(expanded.grid)) {

        metadata.suffix <- stringr::str_pad(
            string = metadata.count,
            width  = 1 + floor(log10(nrow(expanded.grid))),
            pad    = "0"
            );

        metadata.ID <- paste0(
            "xgboost_",
            paste(x = by.variables, collapse = "_"),
            "_",
            metadata.suffix
            );

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
                    learner           = learner,
                    by_variables      = by.variables,
                    year              = year,
                    ecoregion         = ecoregion,
                    crop              = crop,
                    response_variable = response.variable,
                    harvested_area    = harvested.area,
                    predictors        = setdiff(predictors,c(by.variables,response.variable,harvested.area)),
                    hyperparameters   = x,
                    binarize_factors  = TRUE
                    )
                );
            }
        );

    return( output.learner.metadata );

    }

DELETEME.get.retained_predictors <- function() {
    retained_predictors <- c(
        #"year",
        "cropsurv",
        "car16uid",
        "ymecoreg",
        "latitude",
        "longitude",
        paste0("ndvi",16:31),
        "ndvimax7",
        "weekofmax7",
        "ndvitot7",
        "emergwk",
        "gppmax",
        "seeding_jday",
        paste0("sumpcpn",    18:31),
        paste0("sumheatd",   18:31),
        paste0("sumfrostd",  18:31),
        paste0("sumchu",     18:31),
        paste0("avgsi",      18:31),
        paste0("avgprcnawhc",18:31),
        paste0("sumpcpn_",    c( 5, 6, 7)),
        paste0("sumpcpn_",    c(56,57,67)),
        paste0("sumegdd_c_",  c( 5, 6, 7)),
        paste0("sumegdd_c_",  c(56,57,67)),
        paste0("sumchu_",     c( 5, 6, 7)),
        paste0("sumchu_",     c(56,57,67)),
        paste0("sumheatd_",   c( 5, 6, 7)),
        paste0("sumfrostd_",  c( 5, 6, 7)),
        paste0("sumfrostd_",  c(56,57,67)),
        paste0("avgsi_",      c( 5, 6, 7)),
        paste0("avgsi_",      c(56,   67)), ## Why is avgsi_57 missing ???
        paste0("avgprcnawhc_",c( 5, 6, 7)),
        paste0("sdpcpn_",     c( 5, 6, 7)),
        paste0("sdsi_",       c( 5, 6, 7)),
        paste0("sdegdd_c_",   c( 5, 6, 7)),
        paste0("sdchu_",      c( 5, 6, 7))
        );
    return( retained_predictors );
    }

