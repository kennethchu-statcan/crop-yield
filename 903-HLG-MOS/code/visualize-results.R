
visualize.results <- function(
    DF.baseline.errors        = NULL,
    DF.performance.metrics    = NULL,
    DF.mock.production.errors = NULL,
    plot.limits.y             = c(  0.0,0.5),
    plot.breaks.y             = seq(0.0,0.5,0.1),
    output.sub.directory      = NULL
    ) {

    this.function.name <- "visualize.results";
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    cat(paste0("starting: ",this.function.name,"()\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    model.name <- "xgboost_multiphase";

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    colnames(DF.performance.metrics) <- gsub(
        x           = colnames(DF.performance.metrics),
        pattern     = "year",
        replacement = "production_year"
        );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.performance.metrics[,   "production_year"] <- as.numeric(as.character(DF.performance.metrics[,   "production_year"]));
    DF.mock.production.errors[,"production_year"] <- as.numeric(as.character(DF.mock.production.errors[,"production_year"]));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    my.ggplot <- initializePlot();
    my.ggplot <- my.ggplot + ggplot2::ggtitle(label = NULL, subtitle = model.name);
    my.ggplot <- my.ggplot + ggplot2::geom_line(
        data      = DF.performance.metrics,
        mapping   = ggplot2::aes(x = .data$production_year, y = .data$weighted_error, group = .data$model),
        colour    = "black",
        line_type = 2,
        alpha     = 0.05
        );
    my.ggplot <- my.ggplot + ggplot2::geom_point(
        data      = DF.mock.production.errors,
        mapping   = ggplot2::aes(x = .data$production_year, y = .data$mock_production_error),
        colour    = "orange",
        size      = 5,
        alpha     = 1.0
        );
    my.ggplot <- my.ggplot + ggplot2::geom_line(
        data      = DF.mock.production.errors,
        mapping   = ggplot2::aes(x = .data$production_year, y = .data$mock_production_error),
        colour    = "orange",
        size      = 1,
        alpha     = 1.0
        );
    my.ggplot <- my.ggplot + ggplot2::geom_point(
        data    = DF.baseline.errors,
        mapping = ggplot2::aes(x = year, y = weighted_error),
        size    = 5,
        color   = "red"
        );
    my.ggplot <- my.ggplot + ggplot2::geom_line(
        data    = DF.baseline.errors,
        mapping = ggplot2::aes(x = year, y = weighted_error),
        size    = 1,
        color   = "red"
        );
    x.min <- min(
        min(DF.performance.metrics[,   "production_year"]),
        min(DF.mock.production.errors[,"production_year"])
        );
    x.max <- max(
        max(DF.performance.metrics[,   "production_year"]),
        max(DF.mock.production.errors[,"production_year"])
        );
    x.min <- ifelse(0 == (x.min %% 2),x.min,x.min-1);
    x.max <- ifelse(0 == (x.max %% 2),x.max,x.max+1);
    plot.limits.x <- c(  x.min,x.max);
    plot.breaks.x <- seq(x.min,x.max,2);
    my.ggplot <- my.ggplot + ggplot2::scale_x_continuous(limits = plot.limits.x, breaks = plot.breaks.x);
    my.ggplot <- my.ggplot + ggplot2::scale_y_continuous(limits = plot.limits.y, breaks = plot.breaks.y);
    my.ggplot <- my.ggplot + ggplot2::xlab("production year");
    my.ggplot <- my.ggplot + ggplot2::ylab("mock production error");
    temp.file <- base::file.path(output.sub.directory,base::paste0("plot-",model.name,"-mockProdErr-vs-year.png"));
    ggplot2::ggsave(
        plot   = my.ggplot,
        file   = temp.file,
        dpi    = 300,
        height =   8,
        width  =  16,
        units  = 'in'
        );


    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\nexiting: ",this.function.name,"()"));
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    return( NULL );

	}

##################################################
