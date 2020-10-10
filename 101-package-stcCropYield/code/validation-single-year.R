
validation.single.year <- function(
    learner.name       = NULL,
    validation.year    = NULL,
    learner.metadata   = NULL,
    DF.training        = NULL,
    DF.validation      = NULL,
    output.directory   = NULL,
    global.objects     = NULL,
    suppress.child.process.graphics = FALSE
    ) {

    this.function.name <- "validation.single.year";
    logger::log_info('{this.function.name}({learner.name},{validation.year}): starts');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( "windows" == base::.Platform[["OS.type"]] ) {
        if ( !is.null(global.objects) ) {
            object.names <- base::names(global.objects);
            for ( temp.object.name in object.names ) {
                temp.object <- global.objects[[temp.object.name]];
                if ( base::is.function(temp.object) | ("R6ClassGenerator" == base::class(temp.object)) ) {
                    #logger::log_debug('{this.function.name}(): replicating the following object from before-forking environment into current environment: {temp.object.name}');
                    base::assign(x = temp.object.name, value = temp.object, envir = base::environment());
                } else if ( identical(class(temp.object),c("loglevel","integer")) ) {
                    base::assign(x = temp.object.name, value = temp.object, envir = base::environment());
                    logger::log_threshold(level = temp.object);
                    }
                }
            }
        }
    logger::log_info( '{this.function.name}(): logger::log_threshold(): {attr(x = logger::log_threshold(), which = "level")}');
    logger::log_debug('{this.function.name}(): environment(): {capture.output(environment())}');
    logger::log_debug('{this.function.name}(): ls(environment()):\n{paste(ls(environment()),collapse="\n")}');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    output.sub.directory <- base::file.path(output.directory,learner.name,validation.year);
    if ( !base::dir.exists(output.sub.directory) ) {
        base::dir.create(path = output.sub.directory, recursive = TRUE);
        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    output.filename <- base::paste0("predictions-",learner.name);
    output.RData    <- base::file.path(output.sub.directory,base::paste0(output.filename,"-parcel.RData"));

    if ( base::file.exists(output.RData) ) {

        logger::log_info('{this.function.name}(): ({learner.name},{validation.year}), loading starts: {output.RData}');
        DF.predictions.parcel <- base::readRDS( file = output.RData );
        logger::log_info('{this.function.name}(): ({learner.name},{validation.year}), loading complete: {output.RData}');

    } else {

        logger::log_info('{this.function.name}(): ({learner.name},{validation.year}), calling: getLearner()');
        current.learner <- getLearner(
            learner.metadata = learner.metadata,
            DF.training      = DF.training,
            global.objects   = global.objects
            );
        logger::log_info('{this.function.name}(): ({learner.name},{validation.year}), execution complete: getLearner()');

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        logger::log_info('{this.function.name}(): ({learner.name},{validation.year}), fitting begins');
        current.learner$fit();
        logger::log_info('{this.function.name}(): ({learner.name},{validation.year}), fitting complete');

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        logger::log_debug('{this.function.name}(): ({learner.name},{validation.year}), dim(DF.validation) = c({paste0(dim(DF.validation),collapse=",")})');
        logger::log_debug('{this.function.name}(): ({learner.name},{validation.year}), str(DF.validation):\n{paste0(capture.output(str(DF.validation)),collapse="\n")}');

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        logger::log_info('{this.function.name}(): ({learner.name},{validation.year}), predicting begins');
        DF.predictions.parcel <- current.learner$predict(newdata = DF.validation);
        logger::log_info('{this.function.name}(): ({learner.name},{validation.year}), predicting complete');

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        DF.predictions.parcel[,   "actual_production"] <- DF.predictions.parcel[,learner.metadata[["harvested_area"]]] * DF.predictions.parcel[,learner.metadata[["response_variable"]]];
        DF.predictions.parcel[,"predicted_production"] <- DF.predictions.parcel[,learner.metadata[[   "seeded_area"]]] * DF.predictions.parcel[,"predicted_response"];
        logger::log_debug('{this.function.name}(): ({learner.name},{validation.year}), dim(DF.predictions.parcel) = c({paste0(dim(DF.predictions.parcel),collapse=",")})');

        ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
        logger::log_info('{this.function.name}(): ({learner.name},{validation.year}), saving to file begins: {output.RData}');
        base::saveRDS( object = DF.predictions.parcel, file = output.RData );
        logger::log_info('{this.function.name}(): ({learner.name},{validation.year}), saving to file complete: {output.RData}');

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

        } # if ( base::file.exists(output.RData) )

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    validation.single.year_diagnostics(
        DF.input             = DF.predictions.parcel,
        learner.name         = learner.name,
        validation.year      = validation.year,
        learner.metadata     = learner.metadata,
        output.sub.directory = output.sub.directory,
        output.filename      = output.filename,
        global.objects       = global.objects,
        suppress.child.process.graphics = suppress.child.process.graphics
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_info('{this.function.name}({learner.name},{validation.year}): exits');
    return( NULL );

    }

##################################################
validation.single.year_diagnostics <- function(
    DF.input             = NULL,
    learner.name         = NULL,
    validation.year      = NULL,
    learner.metadata     = NULL,
    output.sub.directory = NULL,
    output.filename      = NULL,
    global.objects       = NULL,
    suppress.child.process.graphics = NULL
    ) {

    this.function.name <- "validation.single.year_diagnostics";
    logger::log_info('{this.function.name}({learner.name},{validation.year}): starts');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( "windows" == base::.Platform[["OS.type"]] ) {
        if ( !is.null(global.objects) ) {
            object.names <- base::names(global.objects);
            for ( temp.object.name in object.names ) {
                temp.object <- global.objects[[temp.object.name]];
                if ( base::is.function(temp.object) | ("R6ClassGenerator" == base::class(temp.object)) ) {
                    base::assign(x = temp.object.name, value = temp.object, envir = base::environment());
                } else if ( identical(class(temp.object),c("loglevel","integer")) ) {
                    logger::log_threshold(level = temp.object);
                    }
                }
            }
        }
    logger::log_debug('{this.function.name}({learner.name},{validation.year}): environment(): {capture.output(environment())}');
    logger::log_debug('{this.function.name}({learner.name},{validation.year}): ls(environment()):\n{paste(ls(environment()),collapse="\n")}');
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

    logger::log_debug('{this.function.name}({learner.name},{validation.year}): colnames(DF.input):\n{paste(colnames(DF.input),collapse="\n")}');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    selected.colnames <- base::c(
        learner.metadata[["year"]],
        learner.metadata[["ecoregion"]],
        learner.metadata[["crop"]],
        learner.metadata[["evaluation_weight"]],
        "actual_production",
        "predicted_production"
        );

    DF.region.crop <- DF.input[,selected.colnames];

    temp.vars <- base::c("year","ecoregion","crop","evaluation_weight");
    for ( temp.var in temp.vars ) {
        base::colnames(DF.region.crop) <- base::gsub(
            x           = base::colnames(DF.region.crop),
            pattern     = learner.metadata[[temp.var]],
            replacement = temp.var
            );
        }

    DF.region.crop <- validation.single.year_group.then.add.relative.error(
        DF.input     = DF.region.crop,
        by.variables = c("ecoregion","crop")
        );

    output.CSV <- base::file.path(output.sub.directory,base::paste0(output.filename,"-region-crop.csv"));
    utils::write.csv(
        file      = output.CSV,
        x         = DF.region.crop,
        row.names = FALSE
        );

    if (  suppress.child.process.graphics  ) {

        logger::log_debug('{this.function.name}({learner.name},{validation.year}): suppressed generation of: plot-predictions-region-crop.png');

    } else {

        validation.single.year_scatterplot(
            FILE.png        = base::file.path(output.sub.directory,base::paste0("plot-predictions-region-crop.png")),
            DF.input        = DF.region.crop,
            learner.name    = learner.name,
            validation.year = validation.year,
            global.objects  = global.objects
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    selected.colnames <- base::c(
        learner.metadata[["year"]],
        learner.metadata[["ecoregion"]],
        learner.metadata[["evaluation_weight"]],
        "actual_production",
        "predicted_production"
        );

    DF.region <- DF.input[,selected.colnames];

    temp.vars <- base::c("year","ecoregion","evaluation_weight");
    for ( temp.var in temp.vars ) {
        base::colnames(DF.region) <- base::gsub(
            x           = base::colnames(DF.region),
            pattern     = learner.metadata[[temp.var]],
            replacement = temp.var
            );
        }

    DF.region <- validation.single.year_group.then.add.relative.error(
        DF.input     = DF.region,
        by.variables = c("ecoregion")
        );

    output.CSV <- base::file.path(output.sub.directory,base::paste0(output.filename,"-region.csv"));
    utils::write.csv(
        file      = output.CSV,
        x         = DF.region,
        row.names = FALSE
        );

    if (  suppress.child.process.graphics  ) {

        logger::log_debug('{this.function.name}({learner.name},{validation.year}): suppressed generation of: plot-predictions-region.png');

    } else {

        validation.single.year_scatterplot(
            FILE.png        = base::file.path(output.sub.directory,base::paste0("plot-predictions-region.png")),
            DF.input        = DF.region,
            learner.name    = learner.name,
            validation.year = validation.year,
            global.objects  = global.objects
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    selected.colnames <- base::c(
        learner.metadata[["year"]],
        learner.metadata[["crop"]],
        learner.metadata[["evaluation_weight"]],
        "actual_production",
        "predicted_production"
        );

    DF.crop <- DF.input[,selected.colnames];

    temp.vars <- base::c("year","crop","evaluation_weight");
    for ( temp.var in temp.vars ) {
        base::colnames(DF.crop) <- base::gsub(
            x           = base::colnames(DF.crop),
            pattern     = learner.metadata[[temp.var]],
            replacement = temp.var
            );
        }

    DF.crop <- validation.single.year_group.then.add.relative.error(
        DF.input     = DF.crop,
        by.variables = c("crop")
        );

    output.CSV <- base::file.path(output.sub.directory,base::paste0(output.filename,"-crop.csv"));
    utils::write.csv(
        file      = output.CSV,
        x         = DF.crop,
        row.names = FALSE
        );

    if (  suppress.child.process.graphics  ) {

        logger::log_debug('{this.function.name}({learner.name},{validation.year}): suppressed generation of: plot-predictions-crop.png');

    } else {

        validation.single.year_scatterplot(
            FILE.png        = base::file.path(output.sub.directory,base::paste0("plot-predictions-crop.png")),
            DF.input        = DF.crop,
            learner.name    = learner.name,
            validation.year = validation.year,
            global.objects  = global.objects
            );

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    selected.colnames <- base::c(
        learner.metadata[["year"]],
        learner.metadata[["evaluation_weight"]],
        "actual_production",
        "predicted_production"
        );

    DF.province <- DF.input[,selected.colnames];

    temp.vars <- base::c("year","evaluation_weight");
    for ( temp.var in temp.vars ) {
        base::colnames(DF.province) <- base::gsub(
            x           = base::colnames(DF.province),
            pattern     = learner.metadata[[temp.var]],
            replacement = temp.var
            );
        }

    DF.province <- validation.single.year_group.then.add.relative.error(
        DF.input     = DF.province,
        by.variables = NULL
        );

    output.CSV <- base::file.path(output.sub.directory,base::paste0(output.filename,"-province.csv"));
    utils::write.csv(
        file      = output.CSV,
        x         = DF.province,
        row.names = FALSE
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_info('{this.function.name}({learner.name},{validation.year}): exits');
    base::return( NULL );

    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#' @importFrom rlang .data
validation.single.year_scatterplot <- function(
    FILE.png        = NULL,
    DF.input        = NULL,
    learner.name    = NULL,
    validation.year = NULL,
    global.objects  = NULL
    ) {

    this.function.name <- "validation.single.year_scatterplot";
    logger::log_info('{this.function.name}({learner.name},{validation.year}): starts');

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    if ( "windows" == base::.Platform[["OS.type"]] ) {
        if ( !is.null(global.objects) ) {
            object.names <- base::names(global.objects);
            for ( temp.object.name in object.names ) {
                temp.object <- global.objects[[temp.object.name]];
                if ( base::is.function(temp.object) | ("R6ClassGenerator" == base::class(temp.object)) ) {
                    base::assign(x = temp.object.name, value = temp.object, envir = base::environment());
                } else if ( identical(class(temp.object),c("loglevel","integer")) ) {
                    logger::log_threshold(level = temp.object);
                    }
                }
            }
        }
    logger::log_debug('{this.function.name}({learner.name},{validation.year}): environment(): {capture.output(environment())}');
    logger::log_debug('{this.function.name}({learner.name},{validation.year}): ls(environment()):\n{paste(ls(environment()),collapse="\n")}');
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

    my.ggplot <- initializePlot();

    logger::log_debug('{this.function.name}({learner.name},{validation.year}): call to initializePlot() complete');

    my.ggplot <- my.ggplot + ggplot2::ggtitle(
        label    = NULL,
        subtitle = paste0( learner.name, ", ", validation.year )
        );

    logger::log_debug('{this.function.name}({learner.name},{validation.year}): call to ggtitle() complete');

    my.ggplot <- my.ggplot + ggplot2::geom_point(
        data    = DF.input,
        mapping = ggplot2::aes(
            x = .data$actual_production,
            y = .data$predicted_production
            ),
        alpha = 0.9
        );

    logger::log_debug('{this.function.name}({learner.name},{validation.year}): call to geom_point() complete');

    my.ggplot <- my.ggplot + ggplot2::geom_abline(
        slope     = 1,
        intercept = 0
        );

    logger::log_debug('{this.function.name}({learner.name},{validation.year}): call to geom_abline() complete');

    my.ggplot <- my.ggplot + ggplot2::xlab(label =    "actual production");

    logger::log_debug('{this.function.name}({learner.name},{validation.year}): call to xlab() complete');

    my.ggplot <- my.ggplot + ggplot2::ylab(label = "predicted production");

    logger::log_debug('{this.function.name}({learner.name},{validation.year}): call to ylab() complete');

    temp.axis.max <- base::max(
        base::max(DF.input[,   "actual_production"]),
        base::max(DF.input[,"predicted_production"])
        );

    logger::log_debug('{this.function.name}({learner.name},{validation.year}): value assignment of temp.axis.max complete');

    my.ggplot <- my.ggplot + ggplot2::scale_x_continuous(
        limits = 1.1 * c(0,temp.axis.max)
        #,breaks = (1e7) * seq(0,10,2)
        );

    logger::log_debug('{this.function.name}({learner.name},{validation.year}): call to scale_x_continuous() complete');

    my.ggplot <- my.ggplot + ggplot2::scale_y_continuous(
        limits = 1.1 * c(0,temp.axis.max)
        #,breaks = (1e7) * seq(0,10,2)
        );

    logger::log_debug('{this.function.name}({learner.name},{validation.year}): call to scale_y_continuous() complete');
    logger::log_debug('{this.function.name}({learner.name},{validation.year}): FILE.png = {FILE.png}');
    logger::log_debug('{this.function.name}({learner.name},{validation.year}): class(my.ggplot) = {class(my.ggplot)}');

    ggplot2::ggsave(file = FILE.png, plot = my.ggplot, dpi = 300, height = 8, width = 8, units = 'in');

    logger::log_debug('{this.function.name}({learner.name},{validation.year}): call to ggsave() complete');

    logger::log_info('{this.function.name}({learner.name},{validation.year}): exits');

    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
validation.single.year_group.then.add.relative.error <- function(
    DF.input     = NULL,
    by.variables = NULL
    ) {

    DF.output <- DF.input;

    if ( is.null(by.variables) ) {

        DF.output <- base::apply(
            X      = DF.output[,base::c("evaluation_weight","actual_production","predicted_production")],
            MARGIN = 2,
            FUN    = base::sum
            );
        DF.output <- as.data.frame(DF.output);
        if (nrow(DF.output)>1) { DF.output <- as.data.frame(t(DF.output)) };
        rownames(DF.output) <- NULL;

    } else {

        selected.colnames <- base::c(by.variables,"evaluation_weight","actual_production","predicted_production");
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

    # DF.output[,"relative_error"] <- base::abs(
    #     DF.output[,"predicted_production"] - DF.output[,"actual_production"]
    #     ) / abs( DF.output[,"actual_production"] );

    DF.output[,"relative_error"] <- base::apply(
        X      = DF.output[,c("actual_production","predicted_production")],
        MARGIN = 1,
        FUN    = function(x) {
            base::return(base::ifelse(
                test = base::all(0==x),
                yes  = 0,
                no   = base::abs(x[1]-x[2])/base::abs(x[1])
                ));
            }
        );

    is.infinite.relative.error <- base::is.infinite(DF.output[,"relative_error"]);
     max.finite.relative.error <- base::max(DF.output[!is.infinite.relative.error,"relative_error"],na.rm=TRUE);
    DF.output[is.infinite.relative.error,"relative_error"] <- max.finite.relative.error;

    base::return( DF.output );

    }
