
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
require(foreach);
require(magrittr);
require(rlang);

code.files <- c(
    "crop-yield-train-model.R",
    "getData-synthetic.R",
    "getLearner.R",
    "get-learner-metadata.R",
    "get-mock-production-errors.R",
    "get-performance-metrics.R",
    "initializePlot.R",
    "preprocessor.R",
    "learner-abstract.R",
    "learner-byGroup.R",
    "learner-multiphase.R",
    "learner-xgboost.R",
    "rollingWindowForwardValidation.R",
    "validation-single-year.R",
    "weighted-statistics.R"
    );

for ( code.file in code.files ) {
    source(file.path(dir.code,code.file));
    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# running unit tests
test.files <- c(
    "test-correctness.R"
    );

for ( test.file in test.files ) {
    source(file.path(dir.code,test.file));
    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
set.seed(13);

n.ecoregions  <-    3;
n.crops       <-    5;
n.predictors  <-    7;
avg.n.parcels <- 1000;

DF.synthetic <- getData.synthetic(
    years         = seq(2011,2020),
    n.ecoregions  = n.ecoregions,
    n.crops       = n.crops,
    n.predictors  = n.predictors,
    avg.n.parcels = avg.n.parcels,
    output.RData  = "raw-synthetic.RData",
    output.csv    = "raw-synthetic.csv"
    );

cat("\nstr(DF.synthetic)\n");
print( str(DF.synthetic)   );

cat("\nsummary(DF.synthetic)\n");
print( summary(DF.synthetic)   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
stcCropYield::rollingWindowForwardValidation(
    training.window      = 2,
    validation.window    = 3,
    DF.input             = DF.synthetic,
    year                 = "my_year",
    ecoregion            = "my_ecoregion",
    crop                 = "my_crop",
    response.variable    = "my_yield",
    harvested.area       = "my_harvested_area",
    predictors           = grep(x = colnames(DF.synthetic), pattern = "x[0-9]*", value = TRUE),
    min.num.parcels      = 50,
    learner              = "xgboost_multiphase",
    by.variables.phase01 = c("my_ecoregion","my_crop"),
    by.variables.phase02 = c("my_crop"),
    by.variables.phase03 = c("my_ecoregion"),
    search.grid = list(
        alpha       = c(1,12,23),
        lambda      = c(1,12,23),
        lambda_bias = c(23) #seq(23,11,-8)
        ),
    output.directory = "rwFV",
    log.threshold    = logger::ERROR
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
