
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
#    selected.colnames <- base::c("year","ecoregion","crop","harvested_area","actual_production","predicted_production");
#    DF.region.crop <- DF.region.crop[,selected.colnames];
#    DF.region.crop <- stats::aggregate(
#        x   = DF.region.crop[,base::setdiff(base::colnames(DF.region.crop),base::c("ecoregion","crop"))],
#        by  = base::list(DF.region.crop$ecoregion,DF.region.crop$crop),
#        FUN = base::sum
#        );
#    base::colnames(DF.region.crop) <- base::gsub(
#        x           = base::colnames(DF.region.crop),
#        pattern     = "Group\\.1",
#        replacement = "ecoregion"
#        );
#    base::colnames(DF.region.crop) <- base::gsub(
#        x           = base::colnames(DF.region.crop),
#        pattern     = "Group\\.2",
#        replacement = "crop"
#        );
#    DF.region.crop[,"relative_error"] <- base::abs(
#        DF.region.crop[,"predicted_production"] - DF.region.crop[,"actual_production"]
#        ) / DF.region.crop[,"actual_production"];

    DF.region.crop <- validation.single.year_group.then.add.relative.error(
        DF.input     = DF.region.crop,
        by.variables = c("ecoregion","crop")
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
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
#    selected.colnames <- base::c("year","ecoregion","harvested_area","actual_production","predicted_production");
#    DF.region <- DF.region[,selected.colnames];
#    DF.region <- stats::aggregate(
#        x   = DF.region[,base::setdiff(base::colnames(DF.region),base::c("ecoregion"))],
#        by  = base::list(DF.region$ecoregion),
#        FUN = base::sum
#        );
#    base::colnames(DF.region) <- base::gsub(
#        x           = base::colnames(DF.region),
#        pattern     = "Group\\.1",
#        replacement = "ecoregion"
#        );
#    DF.region[,"relative_error"] <- base::abs(
#        DF.region[,"predicted_production"] - DF.region[,"actual_production"]
#        ) / DF.region[,"actual_production"];

    DF.region <- validation.single.year_group.then.add.relative.error(
        DF.input     = DF.region,
        by.variables = c("ecoregion")
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
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
#    selected.colnames <- base::c("year","crop","harvested_area","actual_production","predicted_production");
#    DF.crop <- DF.crop[,selected.colnames];
#    DF.crop <- stats::aggregate(
#        x   = DF.crop[,base::setdiff(base::colnames(DF.crop),c("crop"))],
#        by  = list(DF.crop$crop),
#        FUN = base::sum
#        );
#    base::colnames(DF.crop) <- base::gsub(
#        x           = base::colnames(DF.crop),
#        pattern     = "Group\\.1",
#        replacement = "crop"
#        );
#    DF.crop[,"relative_error"] <- base::abs(
#        DF.crop[,"predicted_production"] - DF.crop[,"actual_production"]
#        ) / DF.crop[,"actual_production"];

    DF.crop <- validation.single.year_group.then.add.relative.error(
        DF.input     = DF.crop,
        by.variables = c("crop")
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
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
#    DF.province <- base::apply(
#        X      = DF.province[,base::c("harvested_area","actual_production","predicted_production")],
#        MARGIN = 2,
#        FUN    = base::sum
#        );
#    DF.province <- as.data.frame(DF.province);
#    if (nrow(DF.province)>1) { DF.province <- as.data.frame(t(DF.province)) };
#    DF.province[,"relative_error"] <- base::abs(
#        DF.province[,"predicted_production"] - DF.province[,"actual_production"]
#        ) / DF.province[,"actual_production"];

    DF.province <- validation.single.year_group.then.add.relative.error(
        DF.input     = DF.province,
        by.variables = NULL
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    output.CSV <- base::file.path(output.sub.directory,base::paste0(output.filename,"-province.csv"));
    utils::write.csv(
        file      = output.CSV,
        x         = DF.province,
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    base::return( NULL );

    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
validation.single.year_group.then.add.relative.error <- function(
    DF.input     = NULL,
    by.variables = NULL
    ) {

    DF.output <- DF.input;

    if ( is.null(by.variables) ) {
    
        DF.output <- base::apply(
            X      = DF.output[,base::c("harvested_area","actual_production","predicted_production")],
            MARGIN = 2,
            FUN    = base::sum
            );
        DF.output <- as.data.frame(DF.output);
        if (nrow(DF.output)>1) { DF.output <- as.data.frame(t(DF.output)) };
        rownames(DF.output) <- NULL;

    } else {
    
        selected.colnames <- base::c(by.variables,"harvested_area","actual_production","predicted_production");
        DF.output <- DF.output[,selected.colnames];

        by.list <- list();
        for ( i in 1:length(by.variables) ) {
            by.list[[i]] <- DF.output[,by.variables[i]];
            }

        DF.output <- stats::aggregate(
            x   = DF.output[,base::setdiff(base::colnames(DF.output),by.variables)],
            by  = by.list,
            FUN = base::sum
            );

        for ( i in 1:length(by.variables) ) {
            base::colnames(DF.output) <- base::gsub(
                x           = base::colnames(DF.output),
                pattern     = paste0("Group\\.",i),
                replacement = by.variables[i]
                );
            }

        }

    DF.output[,"relative_error"] <- base::abs(
        DF.output[,"predicted_production"] - DF.output[,"actual_production"]
        ) / DF.output[,"actual_production"];

    return( DF.output );

    }

