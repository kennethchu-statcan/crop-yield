
getData.synthetic <- function(
    years        = seq(2000,2018),
    n.ecoregions =  5,
    n.crops      = 10,
    output.RData = "raw-synthetic.RData",
    output.csv   = "raw-synthetic.csv"
    ) {

    this.function.name <- "getData.synthetic";
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    cat(paste0("starting: ",this.function.name,"()\n"));

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
                year       = temp.year,
                ecoregion  = temp.ecoregion,
                parcels    = list.parcels[[temp.ecoregion]],
                crops      = crops,
                crop.probs = crop.probs
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
    year       = NULL,
    ecoregion  = NULL,
    parcels    = NULL,
    crops      = NULL,
    crop.probs = NULL
    ) {
    n.parcels <- length(parcels);
    DF.output <- data.frame(
        year             = rep(x = year,      n = n.parcels),
        ecoregion        = rep(x = ecoregion, n = n.parcels),
        parcelID         = parcels,
        crop             = sample(x = crops, size = n.parcels, replace = TRUE, prob = crop.probs),
        stringsAsFactors = FALSE
        );
    return( DF.output );
    }

get.predictors <- function() {
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

