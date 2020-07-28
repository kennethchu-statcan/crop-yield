
get.learner.metadata <- function(
    ecoregion   = "ecoregion",
    crop        = "crop",
    predictors  = NULL,
    search.grid = list(alpha = seq(23,11,-4), lambda = seq(23,11,-4), lambda_bias = seq(23,11,-4))
    ) {

    this.function.name <- "get.learner.metadata";
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    cat(paste0("starting: ",this.function.name,"()\n"));

    require(jsonlite);
    require(stringr);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    learner.metadata <- c(
        get.learner.metadata_xgboost(
            ecoregion    = ecoregion,
            crop         = crop,
            predictors   = predictors,
            learner      = "byTwo_xgboost",
            by_variables = c(ecoregion,crop),
            search.grid  = search.grid
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
get.learner.metadata_xgboost <- function(
    ecoregion    = NULL,
    crop         = NULL,
    predictors   = c(),
    learner      = NULL,
    by_variables = NULL,
    search.grid  = list(alpha = seq(23,11,-4), lambda = seq(23,11,-4), lambda_bias = seq(23,11,-4))
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
            paste(x = by_variables, collapse = "_"),
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
                    learner             = learner,
                    by_variables        = by_variables,
                    hyperparameters     = x,
                    retained_predictors = setdiff(predictors,by_variables),
                    binarize_factors    = TRUE
                    )
                );
            }
        );

    return( output.learner.metadata );

    }

get.learner.metadata_xgboost.ecoregion.crop <- function(
    year        = NULL,
    ecoregion   = NULL,
    crop        = NULL,
    predictors  = c(),
    learner     = NULL,
    by_variables = NULL,
    search.grid = list(alpha = seq(23,11,-4), lambda = seq(23,11,-4), lambda_bias = seq(23,11,-4))
    ) {

    temp.list     <- list();
    expanded.grid <- base::expand.grid(search.grid);
    for ( metadata.count in 1:nrow(expanded.grid)) {

        metadata.suffix <- stringr::str_pad(string = metadata.count, width = 3, pad = "0");
        metadata.ID     <- paste0("xgboost_ecoregion_crop_",metadata.suffix);

        inner.list <- list();
        for ( temp.colname in colnames(expanded.grid) ) {
            inner.list[[temp.colname]] = expanded.grid[metadata.count,temp.colname]
            }
        inner.list[["verbose"]]       =   2;
        inner.list[["print_every_n"]] =  10;
        inner.list[["nrounds"]]       = 500;

        temp.list[[ metadata.ID ]] <- inner.list;

        }

    output.learner.metadata <- lapply(
        X = temp.list,
        FUN = function(x) {
            return(
                list(
                    learner             = "byTwo_xgboost",
                    by_variables        = c(ecoregion,crop),
                    hyperparameters     = x,
                    retained_predictors = setdiff(predictors,c(ecoregion,crop)),
                    binarize_factors    = TRUE
                    )
                );
            }
        );

    return( output.learner.metadata );

    }

get.learner.metadata_xgboost.ecoregion <- function() {

    temp.list <- list();
    metadata.count <- 0;
    for ( alpha       in seq(9,1,-2) ) {
    for ( lambda      in seq(9,1,-2) ) {
    for ( lambda_bias in seq(9,1,-2) ) {

        metadata.count  <- metadata.count + 1;
        metadata.suffix <- stringr::str_pad(string = metadata.count, width = 3, pad = "0");
        metadata.ID     <- paste0("xgboost_ecoregion_",metadata.suffix);

        temp.list[[ metadata.ID ]] <- list(
            alpha         = alpha,
            lambda        = lambda,
            lambda_bias   = lambda_bias,
            verbose       = 2,
            print_every_n = 10,
            nrounds       = 500
            );

    }}}

    output.learner.metadata <- lapply(
        X = temp.list,
        FUN = function(x) {
            return(
                list(
                    learner             = "byOne_xgboost",
                    by_variable         = "ymecoreg",
                    hyperparameters     = x,
                    retained_predictors = setdiff(get.retained_predictors(),"ymecoreg"),
                    binarize_factors    = TRUE
                    )
                );
            }
        );

    return( output.learner.metadata );

    }

get.learner.metadata_xgboost.crop <- function() {

    temp.list <- list();
    metadata.count <- 0;
    for ( alpha       in seq(9,1,-2) ) {
    for ( lambda      in seq(9,1,-2) ) {
    for ( lambda_bias in seq(9,1,-2) ) {

        metadata.count  <- metadata.count + 1;
        metadata.suffix <- stringr::str_pad(string = metadata.count, width = 3, pad = "0");
        metadata.ID     <- paste0("xgboost_crop_",metadata.suffix);

        temp.list[[ metadata.ID ]] <- list(
            alpha         = alpha,
            lambda        = lambda,
            lambda_bias   = lambda_bias,
            verbose       = 2,
            print_every_n = 10,
            nrounds       = 500
            );

    }}}

    output.learner.metadata <- lapply(
        X = temp.list,
        FUN = function(x) {
            return(
                list(
                    learner             = "byOne_xgboost",
                    by_variable         = "cropsurv",
                    hyperparameters     = x,
                    retained_predictors = setdiff(get.retained_predictors(),"cropsurv"),
                    binarize_factors    = TRUE
                    )
                );
            }
        );

    return( output.learner.metadata );

    }

get.learner.metadata_xgbtree.ecoregion.crop <- function() {

    temp.list <- list();
    metadata.count <- 0;
    for ( gamma            in seq(0,10,2)    ) {
    for ( max_depth        in seq(6,10,1)    ) {
    for ( eta              in seq(1, 9,2)/10 ) {
    for ( subsample        in seq(1, 1,1)    ) {
    for ( colsample_bytree in seq(1, 1,1)    ) {

        metadata.count  <- metadata.count + 1;
        metadata.suffix <- stringr::str_pad(string = metadata.count, width = 3, pad = "0");
        metadata.ID     <- paste0("xgbtree_ecoregion_crop_",metadata.suffix);

        temp.list[[ metadata.ID ]] <- list(
            eta              = eta,
            gamma            = gamma,
            max_depth        = max_depth,
            subsample        = subsample,
            colsample_bytree = colsample_bytree,
            verbose          = 2,
            print_every_n    = 10,
            nrounds          = 500
            );

    }}}}}

    output.learner.metadata <- lapply(
        X = temp.list,
        FUN = function(x) {
            return(
                list(
                    learner             = "byTwo_xgbtree",
                    by_variables        = c("ymecoreg","cropsurv"),
                    hyperparameters     = x,
                    retained_predictors = setdiff(get.retained_predictors(),c("ymecoreg","cropsurv")),
                    binarize_factors    = TRUE
                    )
                );
            }
        );

    return( output.learner.metadata );

    }

get.learner.metadata_glmnet.ecoregion.crop <- function() {

    temp.list <- list();
    metadata.count <- 0;
    for ( alpha in seq(1,1,1) ) {

        metadata.count  <- metadata.count + 1;
        metadata.suffix <- stringr::str_pad(string = metadata.count, width = 3, pad = "0");
        metadata.ID     <- paste0("glmnet_ecoregion_crop_",metadata.suffix);

        temp.list[[ metadata.ID ]] <- list(
            alpha = alpha
            );

    }

    output.learner.metadata <- lapply(
        X = temp.list,
        FUN = function(x) {
            return(
                list(
                    learner             = "byTwo_glmnet",
                    by_variables        = c("ymecoreg","cropsurv"),
                    hyperparameters     = x,
                    retained_predictors = setdiff(get.retained_predictors(),c("ymecoreg","cropsurv")),
                    binarize_factors    = TRUE
                    )
                );
            }
        );

    return( output.learner.metadata );

    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
get.retained_predictors <- function() {
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

