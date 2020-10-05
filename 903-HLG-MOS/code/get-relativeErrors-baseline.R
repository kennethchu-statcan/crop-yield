
get.relativeErrors.baseline <- function(
    dir.baseline   = NULL,
    crops.retained = NULL,
    FILE.output    = "relativeErrors-baseline.RData"
    ) {

    this.function.name <- "get.baseline.results";
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    cat(paste0("starting: ",this.function.name,"()\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    require(haven);
    require(dplyr);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if (file.exists(FILE.output)) {

        load(file = FILE.output);

    } else {

        sas.files <- list.files(dir.baseline, pattern = "ymecoreg");
        sas.files <- grep(x = sas.files, pattern = paste0("(",paste(tolower(crops.retained),collapse="|"),")"), value = TRUE);
        sas.files <- file.path(dir.baseline,sas.files);

        DF.error.baseline.year.region.crop <- data.frame();
        for (temp.sas.file in sas.files) {

            sas.data <- as.data.frame(read_sas(data_file = temp.sas.file));
            sas.data <- sas.data[,c("YEAR","YMEcoReg","CROPSURV","QSTRM","HARVACRE","Yield","actual_production","predicted_production","predicted_yield")];

            write.csv(
                x         = sas.data,
                file      = paste0("relativeErrors-baseline-",gsub(x=basename(temp.sas.file),pattern="sas7bdat",replacement="csv")),
                row.names = FALSE
                );

            ecoregion_results <- sas.data %>%
                mutate( predicted_production = if_else(predicted_yield < 0,        0,predicted_production) ) %>%
                mutate( predicted_production = if_else(is.na(predicted_production),0,predicted_production) ) %>%
                mutate(    actual_production = if_else(is.na(   actual_production),0,   actual_production) ) %>%
                group_by( YEAR, YMEcoReg, CROPSURV ) %>%
                summarize(
                    HARVACRE             = sum(HARVACRE),
                    actual_production    = sum(   actual_production),
                    predicted_production = sum(predicted_production)
                    ) %>%
                mutate(
                    relative_error = abs(predicted_production - actual_production) / (actual_production)
                    );

            DF.error.baseline.year.region.crop <- rbind(DF.error.baseline.year.region.crop,as.data.frame(ecoregion_results));

            }

        cat("\nstr(DF.error.baseline.year.region.crop)\n");
        print( str(DF.error.baseline.year.region.crop)   );

        write.csv(
            x         = DF.error.baseline.year.region.crop,
            file      = paste0("relativeErrors-baseline-year-region-crop.csv"),
            row.names = FALSE
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        validation.years       <- unique(DF.error.baseline.year.region.crop$YEAR);
        DF.error.baseline.year <- data.frame();
        for( validation.year in validation.years ) {

            my_data <- DF.error.baseline.year.region.crop %>% filter( YEAR == validation.year );

            # weights      <- my_data[,"actual_production"] / sum(my_data[,"actual_production"]);
            weights        <- my_data[,         "HARVACRE"] / sum(my_data[,         "HARVACRE"]);
            weighted_error <- weighted.mean(x = my_data$relative_error, weights = weights);
            weighted_std   <- weighted.sd(  x = my_data$relative_error, weights = weights);

            yearly_data <- data.frame(
                model            = "baseline",
                year             = validation.year,
                weighted_error   = weighted_error,
                weighted_std     = weighted_std,
                composite_metric = sum(weighted_error,weighted_std) / sqrt(2)
                );

            DF.error.baseline.year <- rbind(DF.error.baseline.year,yearly_data);
       
            }

        cat("\nstr( DF.error.baseline.year )\n");
        print( str( DF.error.baseline.year )   );

        write.csv(
            x         = DF.error.baseline.year,
            file      = paste0("relativeErrors-baseline-year.csv"),
            row.names = FALSE
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        DF.error.baseline <- DF.error.baseline.year %>%
            summarize(
                model                = "baseline",
                mean_weight_error    = mean(weighted_error), 
                mean_std_error       = mean(weighted_std),
                mean_composite_error = mean(composite_metric)
                );

        cat("\nstr( DF.error.baseline )\n");
        print( str( DF.error.baseline )   );

        write.csv(
            x         = DF.error.baseline,
            file      = paste0("relativeErrors-baseline.csv"),
            row.names = FALSE
            );

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        save(
            file = FILE.output,
            list = c("DF.error.baseline.year.region.crop","DF.error.baseline.year","DF.error.baseline")
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\nexiting: ",this.function.name,"()"));
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    return(
        list(
            baseline_year_region_crop = DF.error.baseline.year.region.crop,
            baseline_year             = DF.error.baseline.year,
            baseline                  = DF.error.baseline
            )
        );

    }

##################################################
