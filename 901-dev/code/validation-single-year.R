
validation.single.year <- function(
    learner.name     = NULL,
    validation.year  = NULL,
    learner.metadata = NULL,
    DF.training      = NULL,
    DF.validation    = NULL
    ) {

    this.function.name <- "validation.single.year";
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    cat(paste0("starting: ",this.function.name,"()\n"));

    require(dplyr);

    original.wd <- normalizePath( getwd() );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\n### validation year: ",validation.year,"\n"));

    output.sub.directory <- file.path(original.wd,"predictions",learner.name,validation.year);
    if ( !dir.exists(output.sub.directory) ) {
        dir.create(path = output.sub.directory, recursive = TRUE);
        }

    setwd(output.sub.directory);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    current.learner <- getLearner(
        learner.metadata = learner.metadata,
        DF.training      = DF.training
        );

    cat("\n# fitting ...\n");
    current.learner$fit();

    cat("\n# predicting ...\n");
    cat("\ndim(DF.validation)\n");
    print( dim(DF.validation)   );

    DF.predictions.parcel <- current.learner$predict(newdata = DF.validation);

    cat("\ndim(DF.predictions.parcel)\n");
    print( dim(DF.predictions.parcel)   );
    cat("\ncolnames(DF.predictions.parcel)\n");
    print( colnames(DF.predictions.parcel)   );

    DF.predictions.parcel[,   "actual_production"] <- DF.predictions.parcel[,learner.metadata[["harvested_area"]]] * DF.predictions.parcel[,learner.metadata[["response_variable"]]];

    DF.predictions.parcel[,"predicted_production"] <- DF.predictions.parcel[,learner.metadata[["harvested_area"]]] * DF.predictions.parcel[,"predicted_response"];

    cat("\ndim(DF.predictions.parcel)\n");
    print( dim(DF.predictions.parcel)   );
    cat("\nstr(DF.predictions.parcel)\n");
    print( str(DF.predictions.parcel)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    output.filename <- paste0("predictions-",learner.name);
    output.RData    <- file.path(output.sub.directory,paste0(output.filename,"-parcel.RData"));
    saveRDS( object = DF.predictions.parcel, file = output.RData );

    #output.CSV <- file.path(output.sub.directory,paste0(output.filename,".csv"));
    # write.csv(
    #     file      = output.CSV,
    #     x         = DF.predictions.parcel,
    #     row.names = FALSE
    #     );

    #output.feather <- file.path(output.sub.directory,paste0(output.filename,".feather"));
    # feather::write_feather(
    #    path = output.feather,
    #    x    = DF.predictions.parcel
    #    );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    do.ftv.diagnostics(
        DF.input             = DF.predictions.parcel,
        learner.metadata     = learner.metadata,
        output.sub.directory = output.sub.directory,
        output.filename      = output.filename
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    setwd( original.wd );
    cat(paste0("\nexiting: ",this.function.name,"()"));
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    return( NULL );

    }

##################################################
do.ftv.diagnostics <- function(
    DF.input             = NULL,
    learner.metadata     = NULL,
    output.sub.directory = NULL,
    output.filename      = NULL
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    selected.colnames <- c(
        learner.metadata[["year"]],
        learner.metadata[["ecoregion"]],
        learner.metadata[["crop"]],
        learner.metadata[["harvested_area"]],
        "actual_production",
        "predicted_production"
        );

    cat("\nselected.colnames\n");
    print( selected.colnames   );

    cat("\ncolnames(DF.input)\n");
    print( colnames(DF.input)   );

    DF.region.crop <- DF.input[,selected.colnames];

    temp.vars <- c("year","ecoregion","crop","harvested_area");
    for ( temp.var in temp.vars ) {
        colnames(DF.region.crop) <- gsub(
            x           = colnames(DF.region.crop),
            pattern     = learner.metadata[[temp.var]],
            replacement = temp.var
            );
        }
    
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.region.crop <- DF.region.crop %>%
        select( year, ecoregion, crop, harvested_area, actual_production, predicted_production ) %>%
        group_by( ecoregion, crop ) %>%
        summarize(
            harvested_area       = sum(harvested_area),
            actual_production    = sum(actual_production),
            predicted_production = sum(predicted_production)
            ) %>%
        mutate( relative_error = abs(predicted_production - actual_production) / actual_production )
        ;

    output.CSV <- file.path(output.sub.directory,paste0(output.filename,"-region-crop.csv"));
    write.csv(
        file      = output.CSV,
        x         = DF.region.crop,
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    FILE.png  <- paste0("plot-predictions-region-crop.png");
    my.ggplot <- initializePlot();
    #my.ggplot <- my.ggplot + ggtitle(paste0( temp.crop, ", ", temp.year ));

#    my.ggplot <- my.ggplot + scale_x_continuous(
#        limits = (1e7) * c(  0,10  ),
#        breaks = (1e7) * seq(0,10,2)
#        );

#    my.ggplot <- my.ggplot + scale_y_continuous(
#        limits = (1e7) * c(  0,10  ),
#        breaks = (1e7) * seq(0,10,2)
#        );

    my.ggplot <- my.ggplot + geom_point(
        data    = DF.region.crop,
        mapping = aes(
            x = actual_production,
            y = predicted_production
            ),
        alpha = 0.9
        );

    my.ggplot <- my.ggplot + geom_abline(
        slope     = 1,
        intercept = 0
        );

    ggsave(file = FILE.png, plot = my.ggplot, dpi = 300, height = 8, width = 8, units = 'in');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    selected.colnames <- c(
        learner.metadata[["year"]],
        learner.metadata[["ecoregion"]],
        learner.metadata[["harvested_area"]],
        "actual_production",
        "predicted_production"
        );

    DF.region <- DF.input[,selected.colnames];

    temp.vars <- c("year","ecoregion","harvested_area");
    for ( temp.var in temp.vars ) {
        colnames(DF.region) <- gsub(
            x           = colnames(DF.region),
            pattern     = learner.metadata[[temp.var]],
            replacement = temp.var
            );
        }
    
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.region <- DF.region %>%
        select( year, ecoregion, harvested_area, actual_production, predicted_production ) %>%
        group_by( ecoregion ) %>%
        summarize(
            harvested_area       = sum(harvested_area),
            actual_production    = sum(actual_production),
            predicted_production = sum(predicted_production)
            ) %>%
        mutate( relative_error = abs(predicted_production - actual_production) / actual_production )
        ;

    output.CSV <- file.path(output.sub.directory,paste0(output.filename,"-region.csv"));
    write.csv(
        file      = output.CSV,
        x         = DF.region,
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    FILE.png  <- paste0("plot-predictions-region.png");
    my.ggplot <- initializePlot();
    #my.ggplot <- my.ggplot + ggtitle(paste0( temp.crop, ", ", temp.year ));

#    my.ggplot <- my.ggplot + scale_x_continuous(
#        limits = (1e7) * c(  0,10  ),
#        breaks = (1e7) * seq(0,10,2)
#        );

#    my.ggplot <- my.ggplot + scale_y_continuous(
#        limits = (1e7) * c(  0,10  ),
#        breaks = (1e7) * seq(0,10,2)
#        );

    my.ggplot <- my.ggplot + geom_point(
        data    = DF.region,
        mapping = aes(
            x = actual_production,
            y = predicted_production
            ),
        alpha = 0.9
        );

    my.ggplot <- my.ggplot + geom_abline(
        slope     = 1,
        intercept = 0
        );

    ggsave(file = FILE.png, plot = my.ggplot, dpi = 300, height = 8, width = 8, units = 'in');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    selected.colnames <- c(
        learner.metadata[["year"]],
        learner.metadata[["crop"]],
        learner.metadata[["harvested_area"]],
        "actual_production",
        "predicted_production"
        );

    DF.crop <- DF.input[,selected.colnames];

    temp.vars <- c("year","crop","harvested_area");
    for ( temp.var in temp.vars ) {
        colnames(DF.crop) <- gsub(
            x           = colnames(DF.crop),
            pattern     = learner.metadata[[temp.var]],
            replacement = temp.var
            );
        }
    
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.crop <- DF.crop %>%
        select( year, crop, harvested_area, actual_production, predicted_production ) %>%
        group_by( crop ) %>%
        summarize(
            harvested_area       = sum(harvested_area),
            actual_production    = sum(actual_production),
            predicted_production = sum(predicted_production)
            ) %>%
        mutate( relative_error = abs(predicted_production - actual_production) / actual_production )
        ;

    output.CSV <- file.path(output.sub.directory,paste0(output.filename,"-crop.csv"));
    write.csv(
        file      = output.CSV,
        x         = DF.crop,
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    FILE.png  <- paste0("plot-predictions-crop.png");
    my.ggplot <- initializePlot();
    #my.ggplot <- my.ggplot + ggtitle(paste0( temp.crop, ", ", temp.year ));

#    my.ggplot <- my.ggplot + scale_x_continuous(
#        limits = (1e7) * c(  0,10  ),
#        breaks = (1e7) * seq(0,10,2)
#        );

#    my.ggplot <- my.ggplot + scale_y_continuous(
#        limits = (1e7) * c(  0,10  ),
#        breaks = (1e7) * seq(0,10,2)
#        );

    my.ggplot <- my.ggplot + geom_point(
        data    = DF.crop,
        mapping = aes(
            x = actual_production,
            y = predicted_production
            ),
        alpha = 0.9
        );

    my.ggplot <- my.ggplot + geom_abline(
        slope     = 1,
        intercept = 0
        );

    ggsave(file = FILE.png, plot = my.ggplot, dpi = 300, height = 8, width = 8, units = 'in');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    selected.colnames <- c(
        learner.metadata[["year"]],
        learner.metadata[["harvested_area"]],
        "actual_production",
        "predicted_production"
        );

    DF.province <- DF.input[,selected.colnames];

    temp.vars <- c("year","harvested_area");
    for ( temp.var in temp.vars ) {
        colnames(DF.province) <- gsub(
            x           = colnames(DF.province),
            pattern     = learner.metadata[[temp.var]],
            replacement = temp.var
            );
        }
    
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.province <- DF.province %>%
        select( year, harvested_area, actual_production, predicted_production ) %>%
        summarize(
            harvested_area       = sum(harvested_area),
            actual_production    = sum(actual_production),
            predicted_production = sum(predicted_production)
            ) %>%
        mutate( relative_error = abs(predicted_production - actual_production) / actual_production )
        ;

    output.CSV <- file.path(output.sub.directory,paste0(output.filename,"-province.csv"));
    write.csv(
        file      = output.CSV,
        x         = DF.province,
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    return( NULL );

    }

