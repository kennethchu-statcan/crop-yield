
getData.synthetic <- function(
    years        = base::seq(2000,2018),
    n.ecoregions =  5,
    n.crops      = 10,
    n.predictors =  7,
    output.RData = "raw-synthetic.RData",
    output.csv   = "raw-synthetic.csv"
    ) {

    this.function.name <- "getData.synthetic";
    logger::log_info('{this.function.name}(): starts');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    base::require(stringr);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if (base::file.exists(output.RData)) {

        logger::log_info('{this.function.name}(): loading file: {output.RData}');
        DF.output <- base::readRDS(file = output.RData);

    } else {

        ecoregions   <- base::paste0("er0",base::seq(1,n.ecoregions));
        list.parcels <- getData.synthetic_parcels.by.ecoregion(
            ecoregions    = ecoregions,
            avg.n.parcels = 1000
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        crops <- base::paste0(
            "crop",
            stringr::str_pad(
                string = base::seq(1,n.crops),
                width  = 1 + base::floor(base::log10(n.crops)),
                side   = "left",
                pad    = "0"
                )
            );

        crop.probs <- getData.synthetic_crop.probs(
            crops = crops
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        DF.output <- base::data.frame();
        for ( temp.year      in years      ) {
        for ( temp.ecoregion in ecoregions ) {

            DF.temp <- getData.synthetic_year.ecoregion(
                year         = temp.year,
                ecoregion    = temp.ecoregion,
                parcels      = list.parcels[[temp.ecoregion]],
                crops        = crops,
                crop.probs   = crop.probs,
                n.predictors = n.predictors
                );
            DF.output <- base::rbind(DF.output,DF.temp);
        
            }}

        logger::log_info('{this.function.name}(): saving file: {output.RData}');
        base::saveRDS(object = DF.output, file = output.RData);

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( !base::file.exists(output.csv) ) {
        logger::log_info('{this.function.name}(): saving file: {output.csv}');
        utils::write.csv(
            file      = output.csv,
            x         = DF.output,
            row.names = FALSE
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_info('{this.function.name}(): exits');
    base::return( DF.output );

    }

##################################################
getData.synthetic_predictors <- function(n.predictors = NULL) {
    base::require(stringr);
    predictors <- base::paste0(
        "x",
        stringr::str_pad(
            string = base::seq(1,n.predictors),
            width  = 1 + base::floor(base::log10(n.predictors)),
            side   = "left",
            pad    = "0"
            )
        );
    base::return( predictors );
    }

getData.synthetic_crop.probs <- function(
    crops = NULL
    ) {
    crop.probs <- stats::rbeta(n = base::length(crops), shape1 = 0.5, shape2 = 0.5);
    crop.probs <- crop.probs / base::sum(crop.probs);
    base::return( crop.probs );
    }

getData.synthetic_parcels.by.ecoregion <- function(
    ecoregions    = NULL,
    avg.n.parcels = 1000
    ) {
    base::require(stringi);
    list.output <- base::list();
    for ( temp.ecoregion in ecoregions ) {
        n.parcels    <- stats::rpois(n = 1, lambda = avg.n.parcels);
        temp.parcels <- stringi::stri_rand_strings(n = n.parcels, length = 8);
        list.output[[ temp.ecoregion ]] <- temp.parcels;
        }
    base::return( list.output );
    }

getData.synthetic_year.ecoregion <- function(
    year         = NULL,
    ecoregion    = NULL,
    parcels      = NULL,
    crops        = NULL,
    crop.probs   = NULL,
    n.predictors = NULL
    ) {
    n.parcels <- base::length(parcels);
    DF.output <- base::data.frame(
        my_year           = base::rep(x = year,      n = n.parcels),
        my_ecoregion      = base::rep(x = ecoregion, n = n.parcels),
        my_parcelID       = parcels,
        my_crop           = base::sample(x = crops, size = n.parcels, replace = TRUE, prob = crop.probs),
        my_harvested_area = stats::runif(n = n.parcels, min = 0.8, max = 1),
        my_yield          = base::rep(x = -9999, n = n.parcels),
        stringsAsFactors  = FALSE
        );
    temp.X <- base::matrix(
        data = stats::rnorm(n = base::nrow(DF.output) * n.predictors, mean = 100, sd = 20),
        nrow = base::nrow(DF.output)
        );
    base::colnames(temp.X) <- getData.synthetic_predictors(n.predictors = n.predictors);
    DF.output <- base::cbind(DF.output,temp.X);
    for ( temp.crop in crops ) {
        is.selected <- (DF.output[,"my_crop"] == temp.crop);
        DF.temp     <- DF.output[is.selected,colnames(temp.X)];
        DF.temp     <- base::as.matrix(base::cbind(x0 = base::rep(x=1,times=base::nrow(DF.temp)),DF.temp));

        temp.mean <- base::abs(stats::rnorm(n = 1, mean = 10, sd = 2));
        temp.sd1  <- base::abs(stats::rnorm(n = 1, mean =  2, sd = 1));
        temp.sd2  <- base::abs(stats::rnorm(n = 1, mean = 10, sd = 1));

        temp.beta  <- base::abs(stats::rnorm(n = base::ncol(DF.temp), mean = temp.mean, sd = temp.sd1));
        temp.noise <- stats::rnorm(n = base::nrow(DF.temp), mean = 0, sd = temp.sd2);

        DF.output[is.selected,"my_yield"] <- DF.temp %*% temp.beta + temp.noise;
        }
    base::return( DF.output );
    }

