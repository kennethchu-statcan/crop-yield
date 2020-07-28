
getData.synthetic <- function(
    years        = seq(2000,2018),
    n.ecoregions =  5,
    n.crops      = 10,
    n.predictors =  7,
    output.RData = "raw-synthetic.RData",
    output.csv   = "raw-synthetic.csv"
    ) {

    this.function.name <- "getData.synthetic";
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    cat(paste0("starting: ",this.function.name,"()\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(stringr);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if (file.exists(output.RData)) {

        cat(paste0("\nloading file: ",output.RData,"\n"));
        base::readRDS(file = output.RData);

    } else {

        ecoregions   <- base::paste0("er0",seq(1,n.ecoregions));
        list.parcels <- getData.synthetic_parcels.by.ecoregion(
            ecoregions    = ecoregions,
            avg.n.parcels = 1000
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        crops <- base::paste0(
            "crop",
            stringr::str_pad(
                string = seq(1,n.crops),
                width  = 1 + floor(log10(n.crops)),
                side   = "left",
                pad    = "0"
                )
            );

        crop.probs <- getData.synthetic_crop.probs(
            crops = crops
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        DF.output <- data.frame();
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
            DF.output <- rbind(DF.output,DF.temp);
        
            }}

        cat(paste0("\nsaving to file: ",output.RData,"\n"));
        base::saveRDS(object = DF.output, file = output.RData);

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( !file.exists(output.csv) ) {
        cat(paste0("\nwriting file: ",output.csv,"\n"));
        write.csv(
            file      = output.csv,
            x         = DF.output,
            row.names = FALSE
            );
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\nexiting: ",this.function.name,"()"));
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    return( DF.output );

    }

##################################################
getData.synthetic_predictors <- function(n.predictors = NULL) {
    require(stringr);
    predictors <- base::paste0(
        "x",
        stringr::str_pad(
            string = seq(1,n.predictors),
            width  = 1 + floor(log10(n.predictors)),
            side   = "left",
            pad    = "0"
            )
        );
    return( predictors );
    }

getData.synthetic_crop.probs <- function(
    crops = NULL
    ) {
    crop.probs <- rbeta(n = length(crops), shape1 = 0.5, shape2 = 0.5);
    crop.probs <- crop.probs / sum(crop.probs);
    return( crop.probs );
    }

getData.synthetic_parcels.by.ecoregion <- function(
    ecoregions    = NULL,
    avg.n.parcels = 1000
    ) {
    require(stringi);
    list.output <- list();
    for ( temp.ecoregion in ecoregions ) {
        n.parcels    <- rpois(n = 1, lambda = avg.n.parcels);
        temp.parcels <- stringi::stri_rand_strings(n = n.parcels, length = 8);
        list.output[[ temp.ecoregion ]] <- temp.parcels;
        }
    return( list.output );
    }

getData.synthetic_year.ecoregion <- function(
    year         = NULL,
    ecoregion    = NULL,
    parcels      = NULL,
    crops        = NULL,
    crop.probs   = NULL,
    n.predictors = NULL
    ) {
    n.parcels <- length(parcels);
    DF.output <- data.frame(
        my_year           = rep(x = year,      n = n.parcels),
        my_ecoregion      = rep(x = ecoregion, n = n.parcels),
        my_parcelID       = parcels,
        my_crop           = sample(x = crops, size = n.parcels, replace = TRUE, prob = crop.probs),
        my_harvested_area = runif(n = n.parcels, min = 0.8, max = 1),
        my_yield          = rep(x = -9999, n = n.parcels),
        stringsAsFactors  = FALSE
        );
    temp.X <- matrix(
        data = rnorm(n = nrow(DF.output) * n.predictors, mean = 100, sd = 20),
        nrow = nrow(DF.output)
        );
    colnames(temp.X) <- getData.synthetic_predictors(n.predictors = n.predictors);
    DF.output <- cbind(DF.output,temp.X);
    for ( temp.crop in crops ) {
        is.selected <- (DF.output[,"my_crop"] == temp.crop);
        DF.temp     <- DF.output[is.selected,colnames(temp.X)];
        DF.temp     <- as.matrix(cbind(x0 = rep(x=1,times=nrow(DF.temp)),DF.temp));

        temp.mean <- abs(rnorm(n = 1, mean = 10, sd = 2));
        temp.sd1  <- abs(rnorm(n = 1, mean =  2, sd = 1));
        temp.sd2  <- abs(rnorm(n = 1, mean = 10, sd = 1));

        temp.beta  <- abs(rnorm(n = ncol(DF.temp), mean = temp.mean, sd = temp.sd1));
        temp.noise <- rnorm(n = nrow(DF.temp), mean = 0, sd = temp.sd2);

        DF.output[is.selected,"my_yield"] <- DF.temp %*% temp.beta + temp.noise;
        }
    return( DF.output );
    }

get.predictors.REAL <- function() {
    predictors <- c(
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
    return( predictors );
    }

