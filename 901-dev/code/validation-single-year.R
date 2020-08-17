
validation.single.year <- function(
    learner.name     = NULL,
    validation.year  = NULL,
    learner.metadata = NULL,
    DF.training      = NULL,
    DF.validation    = NULL,
    output.directory = NULL
    ) {

    this.function.name <- "validation.single.year";
    logger::log_info('{this.function.name}(): starts');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_info('{this.function.name}(): learner.name = {learner.name}, validation.year = {validation.year}');

    output.sub.directory <- base::file.path(output.directory,learner.name,validation.year);
    if ( !base::dir.exists(output.sub.directory) ) {
        base::dir.create(path = output.sub.directory, recursive = TRUE);
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    current.learner <- getLearner(
        learner.metadata = learner.metadata,
        DF.training      = DF.training
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_debug('{this.function.name}(): ({learner.name},{validation.year}), fitting begins');
    current.learner$fit();
    logger::log_debug('{this.function.name}(): ({learner.name},{validation.year}), fitting complete');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_debug('{this.function.name}(): ({learner.name},{validation.year}), dim(DF.validation) = c({paste0(dim(DF.validation),collapse=",")})');
    logger::log_debug('{this.function.name}(): ({learner.name},{validation.year}), str(DF.validation):\n{paste0(capture.output(str(DF.validation)),collapse="\n")}');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_debug('{this.function.name}(): ({learner.name},{validation.year}), predicting begins');
    DF.predictions.parcel <- current.learner$predict(newdata = DF.validation);
    logger::log_debug('{this.function.name}(): ({learner.name},{validation.year}), predicting complete');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.predictions.parcel[,   "actual_production"] <- DF.predictions.parcel[,learner.metadata[["harvested_area"]]] * DF.predictions.parcel[,learner.metadata[["response_variable"]]];
    DF.predictions.parcel[,"predicted_production"] <- DF.predictions.parcel[,learner.metadata[["harvested_area"]]] * DF.predictions.parcel[,"predicted_response"];
    logger::log_debug('{this.function.name}(): ({learner.name},{validation.year}), dim(DF.predictions.parcel) = c({paste0(dim(DF.predictions.parcel),collapse=",")})');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    output.filename <- base::paste0("predictions-",learner.name);
    output.RData    <- base::file.path(output.sub.directory,base::paste0(output.filename,"-parcel.RData"));
    base::saveRDS( object = DF.predictions.parcel, file = output.RData );

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
    validation.single.year_diagnostics(
        DF.input             = DF.predictions.parcel,
        learner.metadata     = learner.metadata,
        output.sub.directory = output.sub.directory,
        output.filename      = output.filename
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_info('{this.function.name}(): exits');
    return( NULL );

    }

##################################################
validation.single.year_diagnostics <- function(
    DF.input             = NULL,
    learner.metadata     = NULL,
    output.sub.directory = NULL,
    output.filename      = NULL
    ) {

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    selected.colnames <- base::c(
        learner.metadata[["year"]],
        learner.metadata[["ecoregion"]],
        learner.metadata[["crop"]],
        learner.metadata[["harvested_area"]],
        "actual_production",
        "predicted_production"
        );

    DF.region.crop <- DF.input[,selected.colnames];

    temp.vars <- base::c("year","ecoregion","crop","harvested_area");
    for ( temp.var in temp.vars ) {
        base::colnames(DF.region.crop) <- base::gsub(
            x           = base::colnames(DF.region.crop),
            pattern     = learner.metadata[[temp.var]],
            replacement = temp.var
            );
        }
 
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.region.crop <- dplyr::select(DF.region.crop,
        year,ecoregion,crop,harvested_area,actual_production,predicted_production
        );
    DF.region.crop <- dplyr::group_by(DF.region.crop,ecoregion,crop);
    DF.region.crop <- dplyr::summarize(DF.region.crop,
        harvested_area       = sum(harvested_area),
        actual_production    = sum(actual_production),
        predicted_production = sum(predicted_production)
        );
    DF.region.crop <- dplyr::mutate(DF.region.crop,
        relative_error = abs(predicted_production - actual_production) / actual_production 
        );
    DF.region.crop <- base::as.data.frame(DF.region.crop);

    output.CSV <- base::file.path(output.sub.directory,base::paste0(output.filename,"-region-crop.csv"));
    utils::write.csv(
        file      = output.CSV,
        x         = DF.region.crop,
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    FILE.png  <- base::file.path(output.sub.directory,base::paste0("plot-predictions-region-crop.png"));
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

    my.ggplot <- my.ggplot + ggplot2::geom_point(
        data    = DF.region.crop,
        mapping = ggplot2::aes(
            x = actual_production,
            y = predicted_production
            ),
        alpha = 0.9
        );

    my.ggplot <- my.ggplot + ggplot2::geom_abline(
        slope     = 1,
        intercept = 0
        );

    ggplot2::ggsave(file = FILE.png, plot = my.ggplot, dpi = 300, height = 8, width = 8, units = 'in');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    selected.colnames <- base::c(
        learner.metadata[["year"]],
        learner.metadata[["ecoregion"]],
        learner.metadata[["harvested_area"]],
        "actual_production",
        "predicted_production"
        );

    DF.region <- DF.input[,selected.colnames];

    temp.vars <- base::c("year","ecoregion","harvested_area");
    for ( temp.var in temp.vars ) {
        base::colnames(DF.region) <- base::gsub(
            x           = base::colnames(DF.region),
            pattern     = learner.metadata[[temp.var]],
            replacement = temp.var
            );
        }
    
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.region <- dplyr::select(DF.region,
        year,ecoregion,harvested_area,actual_production,predicted_production
        );
    DF.region <- dplyr::group_by(DF.region,ecoregion);
    DF.region <- dplyr::summarize(DF.region,
        harvested_area       = sum(harvested_area),
        actual_production    = sum(actual_production),
        predicted_production = sum(predicted_production)
        );
    DF.region <- dplyr::mutate(DF.region,
        relative_error = abs(predicted_production - actual_production) / actual_production 
        );
    DF.region <- base::as.data.frame(DF.region);

    output.CSV <- base::file.path(output.sub.directory,base::paste0(output.filename,"-region.csv"));
    utils::write.csv(
        file      = output.CSV,
        x         = DF.region,
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    FILE.png  <- base::file.path(output.sub.directory,base::paste0("plot-predictions-region.png"));
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

    my.ggplot <- my.ggplot + ggplot2::geom_point(
        data    = DF.region,
        mapping = ggplot2::aes(
            x = actual_production,
            y = predicted_production
            ),
        alpha = 0.9
        );

    my.ggplot <- my.ggplot + ggplot2::geom_abline(
        slope     = 1,
        intercept = 0
        );

    ggplot2::ggsave(file = FILE.png, plot = my.ggplot, dpi = 300, height = 8, width = 8, units = 'in');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    selected.colnames <- base::c(
        learner.metadata[["year"]],
        learner.metadata[["crop"]],
        learner.metadata[["harvested_area"]],
        "actual_production",
        "predicted_production"
        );

    DF.crop <- DF.input[,selected.colnames];

    temp.vars <- base::c("year","crop","harvested_area");
    for ( temp.var in temp.vars ) {
        base::colnames(DF.crop) <- base::gsub(
            x           = base::colnames(DF.crop),
            pattern     = learner.metadata[[temp.var]],
            replacement = temp.var
            );
        }
    
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.crop <- dplyr::select(DF.crop,
        year,crop,harvested_area,actual_production,predicted_production
        );
    DF.crop <- dplyr::group_by(DF.crop,crop);
    DF.crop <- dplyr::summarize(DF.crop,crop,
        harvested_area       = sum(harvested_area),
        actual_production    = sum(actual_production),
        predicted_production = sum(predicted_production)
        );
    DF.crop <- dplyr::mutate(DF.crop,
        relative_error = abs(predicted_production - actual_production) / actual_production
        );
    DF.crop <- base::as.data.frame(DF.crop);

    output.CSV <- base::file.path(output.sub.directory,base::paste0(output.filename,"-crop.csv"));
    utils::write.csv(
        file      = output.CSV,
        x         = DF.crop,
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    FILE.png  <- base::file.path(output.sub.directory,base::paste0("plot-predictions-crop.png"));
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

    my.ggplot <- my.ggplot + ggplot2::geom_point(
        data    = DF.crop,
        mapping = ggplot2::aes(
            x = actual_production,
            y = predicted_production
            ),
        alpha = 0.9
        );

    my.ggplot <- my.ggplot + ggplot2::geom_abline(
        slope     = 1,
        intercept = 0
        );

    ggplot2::ggsave(file = FILE.png, plot = my.ggplot, dpi = 300, height = 8, width = 8, units = 'in');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    selected.colnames <- base::c(
        learner.metadata[["year"]],
        learner.metadata[["harvested_area"]],
        "actual_production",
        "predicted_production"
        );

    DF.province <- DF.input[,selected.colnames];

    temp.vars <- base::c("year","harvested_area");
    for ( temp.var in temp.vars ) {
        base::colnames(DF.province) <- base::gsub(
            x           = base::colnames(DF.province),
            pattern     = learner.metadata[[temp.var]],
            replacement = temp.var
            );
        }
    
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.province <- dplyr::select(DF.province,
        year,harvested_area,actual_production,predicted_production
        );
    DF.province <- dplyr::summarize(DF.province,
        harvested_area       = sum(harvested_area),
        actual_production    = sum(actual_production),
        predicted_production = sum(predicted_production)
        );
    DF.province <- dplyr::mutate(DF.province,
        relative_error = abs(predicted_production - actual_production) / actual_production
        );
    DF.province <- base::as.data.frame(DF.province);

    output.CSV <- base::file.path(output.sub.directory,base::paste0(output.filename,"-province.csv"));
    utils::write.csv(
        file      = output.CSV,
        x         = DF.province,
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    base::return( NULL );

    }

