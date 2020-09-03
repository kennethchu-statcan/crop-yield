
getLearner <- function(
    learner.metadata = NULL,
    DF.training      = NULL,
    global.objects   = NULL
    ) {

    this.function.name <- "getLearner";
    logger::log_info('{this.function.name}(): starts');

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
    log.prefix <- '{this.function.name}():';
    log.prefix <- base::paste0(log.prefix,' learner.metadata[["learner"]] = {learner.metadata[["learner"]]}');
    logger::log_info(log.prefix);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_info(base::paste0(log.prefix,', instantiation begins'));
    if ( "xgboost" == learner.metadata[["learner"]] ) {

        instantiated.learner <- learner.xgboost$new(
            learner.metadata = learner.metadata,
            training.data    = DF.training,
            global.objects   = global.objects
            );

    } else if ( "xgboost_byGroup" == learner.metadata[["learner"]] ) {

        instantiated.learner <- learner.byGroup$new(
            learner.single.group = "xgboost",
            learner.metadata     = learner.metadata,
            training.data        = DF.training,
            global.objects       = global.objects
            );

    } else if ( "xgboost_multiphase" == learner.metadata[["learner"]] ) {

        instantiated.learner <- learner.multiphase$new(
            learner.single.phase = "xgboost_byGroup",
            learner.metadata     = learner.metadata,
            training.data        = DF.training,
            global.objects       = global.objects
            );

        }
    logger::log_info(paste0(log.prefix,', instantiation complete'));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    logger::log_info('{this.function.name}(): exits');
    base::return( instantiated.learner );

    }

