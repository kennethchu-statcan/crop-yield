
command.arguments <- commandArgs(trailingOnly = TRUE);
dir.data <- normalizePath( command.arguments[1] );
dir.code <- normalizePath( command.arguments[2] );
dir.out  <- normalizePath( command.arguments[3] );

# add custom library using .libPaths()
cat("\ndir.data: ", dir.data );
cat("\ndir.code: ", dir.code );
cat("\ndir.out:  ", dir.out  );
cat("\n\n##### Sys.time(): ",format(Sys.time(),"%Y-%m-%d %T %Z"),"\n");

start.proc.time <- proc.time();
setwd( dir.out );

cat("\n##################################################\n");
require(dplyr);

code.files <- c(
    #"diagnostics-MB.R",
    #"filterData-MB.R",
    #"forwardTransferValidation.R",
    #"getData-MB.R",
    "getData-synthetic.R",
    #"getLearner.R",
    "get-learner-metadata.R"
    #"initializePlot.R",
    #"preprocessor.R",
    #"learner-glmnet.R",
    #"learner-lm.R",
    #"learner-xgboost.R",
    #"learner-xgbtree.R",
    #"learner-byOne-xgboost.R",
    #"learner-byTwo-glmnet.R",
    #"learner-byTwo-xgboost.R",
    #"learner-byTwo-xgbtree.R"
    );

for ( code.file in code.files ) {
    source(file.path(dir.code,code.file));
    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
set.seed(7654321);

n.ecoregions <-  7;
n.crops      <- 15;
n.predictors <-  7;

DF.synthetic <- getData.synthetic(
    years        = seq(2000,2020),
    n.ecoregions = n.ecoregions,
    n.crops      = n.crops,
    n.predictors = n.predictors,
    output.RData = "raw-synthetic.RData",
    output.csv   = "raw-synthetic.csv"
    );

cat("\nstr(DF.synthetic)\n");
print( str(DF.synthetic)   );

cat("\nsummary(DF.synthetic)\n");
print( summary(DF.synthetic)   );

# diagnostics.MB(DF.input = DF.MB);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
learner.metadata <- get.learner.metadata(
    ecoregion   = "my_ecoregion",
    crop        = "my_crop",
    predictors  = paste0("x0",seq(1,n.predictors)),
    search.grid = list(alpha = seq(23,11,-4), lambda = seq(23,11,-4), lambda_bias = seq(23,11,-4))
    );

cat("\nlearner.metadata\n");
print( learner.metadata   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#validation.years <- seq(2008,2017);
#training.window  <- 10;

#learner.count <- 0;
#for (learner.name in names(learner.metadata)) {
#
#    learner.count <- learner.count + 1;
#
#    cat(paste0("\n### Learner: ",learner.name,"\n"));
#
#    for (validation.year in validation.years) {
#
#        training.years <- seq(validation.year-training.window,validation.year - 1);
#        DF.training    <- DF.MB[DF.MB[,"year"] %in%   training.years,];
#        DF.validation  <- DF.MB[DF.MB[,"year"] ==   validation.year, ];
#
#        forwardTransferValidation(
#            learner.name     = learner.name,
#            validation.year  = validation.year,
#            learner.metadata = learner.metadata[[learner.name]],
#            DF.training      = DF.training,
#            DF.validation    = DF.validation
#            );
#
#        }
#	
#    }

cat("\n##################################################\n");
cat("\n##### warnings():\n");
print(       warnings()    );

cat("\n##### getOption('repos'):\n");
print(       getOption('repos')    );

cat("\n##### .libPaths():\n");
print(       .libPaths()    );

cat("\n##### sessionInfo():\n");
print(       sessionInfo()    );

# print system time to log
cat("\n##### Sys.time(): ",format(Sys.time(),"%Y-%m-%d %T %Z"),"\n");

# print elapsed time to log
stop.proc.time <- proc.time();
cat("\n##### stop.proc.time - start.proc.time:\n");
print(       stop.proc.time - start.proc.time    );

