
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
require(stcCropYield);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
set.seed(7654321);

n.ecoregions <-  7;
n.crops      <- 15;
n.predictors <-  7;

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.training <- getData.synthetic(
    years        = seq(2015,2020),
    #years       = seq(2011,2020),
    #years       = seq(2000,2020),
    n.ecoregions = n.ecoregions,
    n.crops      = n.crops,
    n.predictors = n.predictors,
    output.RData = "raw-training.RData",
    output.csv   = "raw-training.csv"
    );

DF.production <- getData.synthetic(
    years        = c(2021),
    n.ecoregions = n.ecoregions,
    n.crops      = n.crops,
    n.predictors = n.predictors,
    output.RData = "raw-production.RData",
    output.csv   = "raw-production.csv"
    );
DF.production <- DF.production[,setdiff(colnames(DF.production),c("my_yield","my_evaluation_weight"))];

cat("\nstr(DF.training)\n");
print( str(DF.training)   );

cat("\nsummary(DF.training)\n");
print( summary(DF.training)   );

cat("\nstr(DF.production)\n");
print( str(DF.production)   );

cat("\nsummary(DF.production)\n");
print( summary(DF.production)   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
rollingWindowForwardValidation(
    training.window      = 2,
    validation.window    = 3,
    #training.window     =  5,
    #validation.window   = 10,
    DF.input             = DF.training,
    year                 = "my_year",
    ecoregion            = "my_ecoregion",
    crop                 = "my_crop",
    response.variable    = "my_yield",
    evaluation.weight    = "my_evaluation_weight",
    predictors           = grep(x = colnames(DF.training), pattern = "x[0-9]*", value = TRUE),
    by.variables.phase01 = c("my_ecoregion","my_crop"),
    by.variables.phase02 = c("my_crop"),
    by.variables.phase03 = c("my_ecoregion"),
    learner     = "xgboost_multiphase",
    search.grid = list(
        alpha       = seq(23,11,-6),
        lambda      = seq(23,11,-6),
        lambda_bias = c(23) #seq(23,11,-8)
        ),
    output.directory = file.path(dir.out,"rwFV"),
    log.threshold    = logger::TRACE # logger::ERROR
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
RData.trained.model <- list.files( path = file.path(dir.out,"rwFV"), pattern = "\\.RData$" );
RData.trained.model <- file.path(dir.out,"rwFV",RData.trained.model);
cat("\nRData.trained.model\n");
print( RData.trained.model   );

cat('\nfile.exists(RData.trained.model)\n');
print( file.exists(RData.trained.model)   );

DF.predictions <- crop.yield.predict(
   FILE.trained.model = RData.trained.model,
   DF.predictors      = DF.production
   );

cat('\nstr(DF.predictions)\n');
print( str(DF.predictions)   );

cat('\nsummary(DF.predictions)\n');
print( summary(DF.predictions)   );

write.csv(
    x         = DF.predictions,
    file      = "predictions.csv",
    row.names = FALSE
    );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###

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
