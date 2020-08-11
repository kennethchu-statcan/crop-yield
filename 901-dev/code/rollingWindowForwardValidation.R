
rollingWindowForwardValidation <- function(
    validation.years     = NULL,
    training.window      = NULL,
    DF.input             = NULL,
    year                 = "year",
    ecoregion            = "ecoregion",
    crop                 = "crop",
    response.variable    = "yield",
    harvested.area       = "harvested_area",
    predictors           = NULL,
    by.variables.phase01 = c(ecoregion,crop),
    by.variables.phase02 = c(crop),
    by.variables.phase03 = c(ecoregion),
    search.grid          = list(alpha = seq(23,11,-4), lambda = seq(23,11,-4), lambda_bias = seq(23,11,-4)),
    output.directory     = "."
    ) {

    this.function.name <- "rollingWindowForwardValidation";
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    cat(paste0("starting: ",this.function.name,"()\n"));

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    original.wd <- getwd();
    setwd(output.directory);

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    learner.metadata <- get.learner.metadata(
        year                 = year,
        ecoregion            = ecoregion,
        crop                 = crop,
        response.variable    = response.variable,
        harvested.area       = harvested.area,
        predictors           = predictors,
        by.variables.phase01 = by.variables.phase01,
        by.variables.phase02 = by.variables.phase02,
        by.variables.phase03 = by.variables.phase03,
        search.grid          = search.grid
        );

    cat("\nlearner.metadata\n");
    print( learner.metadata   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    #learner.count <- 0;
    for (learner.name in names(learner.metadata)) {

        #learner.count <- learner.count + 1;

        cat(paste0("\n### Learner: ",learner.name,"\n"));

        for (validation.year in validation.years) {

            training.years <- seq(validation.year-training.window,validation.year - 1);
            DF.training    <- DF.input[DF.input[,year] %in%   training.years,];
            DF.validation  <- DF.input[DF.input[,year] ==   validation.year, ];

            #forwardTransferValidation(
            validation.single.year(
                learner.name     = learner.name,
                validation.year  = validation.year,
                learner.metadata = learner.metadata[[learner.name]],
                DF.training      = DF.training,
                DF.validation    = DF.validation
                );

            }

        }

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    cat(paste0("\nexiting: ",this.function.name,"()"));
    cat(paste0("\n",paste(rep("#",50),collapse=""),"\n"));
    setwd(original.wd);
    return( NULL );

    }

##################################################

