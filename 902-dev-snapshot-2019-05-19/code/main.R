
command.arguments <- commandArgs(trailingOnly = TRUE);
dir.data    <- normalizePath( command.arguments[1] );
dir.package <- normalizePath( command.arguments[2] );
dir.code    <- normalizePath( command.arguments[3] );
dir.out     <- normalizePath( command.arguments[4] );

# add custom library using .libPaths()
cat("\ndir.data: ",    dir.data    );
cat("\ndir.package: ", dir.package );
cat("\ndir.code: ",    dir.code    );
cat("\ndir.out:  ",    dir.out     );
cat("\n\n##### Sys.time(): ",format(Sys.time(),"%Y-%m-%d %T %Z"),"\n");

start.proc.time <- proc.time();
setwd( dir.out );

cat("\n##################################################\n");
require(foreach);
require(magrittr);
require(rlang);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
package.files <- c(
    "crop-yield-train-model.R",
    "getData-synthetic.R",
    "getLearner.R",
    "get-learner-metadata.R",
    "get-mock-production-errors.R",
    "get-performance-metrics.R",
    "initializePlot.R",
    "input-validity-checks-learner-metadata.R",
    "input-validity-checks-parameters.R",
    "input-validity-checks-predict.R",
    "input-validity-checks-variables.R",
    "input-validity-checks-window-compatibility.R",
    "preprocessor.R",
    "learner-abstract.R",
    "learner-byGroup.R",
    "learner-multiphase.R",
    "learner-xgboost.R",
    "rollingWindowForwardValidation.R",
    "validation-single-year.R",
    "weighted-statistics.R"
    );

for ( package.file in package.files ) {
    source(file.path(dir.package,package.file));
    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# running unit tests
test.files <- c(
    "test-correctness.R"
    );

for ( test.file in test.files ) {
    source(file.path(dir.package,test.file));
    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
code.files <- c(
    "filterData-MB.R",
    "getData-MB.R"
    );

for ( code.file in code.files ) {
    source(file.path(dir.code,code.file));
    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
#set.seed(7654321);
#
#n.ecoregions <-  7;
#n.crops      <- 15;
#n.predictors <-  7;
#
#DF.synthetic <- getData.synthetic(
#    years        = seq(2015,2020),
#    #years       = seq(2011,2020),
#    #years       = seq(2000,2020),
#    n.ecoregions = n.ecoregions,
#    n.crops      = n.crops,
#    n.predictors = n.predictors,
#    output.RData = "raw-synthetic.RData",
#    output.csv   = "raw-synthetic.csv"
#    );

#cat("\nstr(DF.synthetic)\n");
#print( str(DF.synthetic)   );

#cat("\nsummary(DF.synthetic)\n");
#print( summary(DF.synthetic)   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
require(dplyr);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
data.snapshot <- "2019-05-19.01";

FILE.non.potato <- file.path(dir.data,data.snapshot,"QSTRM_MODISAndClimate2000-2018_w16-36_Allcrops145ac.tab");
FILE.potato     <- file.path(dir.data,data.snapshot,"MBInputData_potats.csv");

FILE.weekly         <- file.path(dir.data,data.snapshot,"CROPS_2018_Weekly.csv");
FILE.avg.moving     <- file.path(dir.data,data.snapshot,"CROPS_2018_MovingAvg.csv");
FILE.avg.cumulative <- file.path(dir.data,data.snapshot,"CROPS_2018_AccumulatedAvg.csv");

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
set.seed(7654321);

# load data
DF.MB <- getData.MB(
    FILE.non.potato     = FILE.non.potato,
    FILE.potato         = FILE.potato,
    FILE.weekly         = FILE.weekly,
    FILE.avg.moving     = FILE.avg.moving,
    FILE.avg.cumulative = FILE.avg.cumulative
    );

cat("\nstr(DF.MB)\n");
print( str(DF.MB)   );

cat("\nsummary(DF.MB)\n");
print( summary(DF.MB)   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
DF.MB <- filterData.MB(
    DF.input = DF.MB
    );

DF.MB[,"cropsurv"] <- as.character(DF.MB[,"cropsurv"]); 
DF.MB[,"ymecoreg"] <- as.character(DF.MB[,"ymecoreg"]);

DF.MB[DF.MB[,"harvacre"] < 0,"harvacre"] <- 0;

cat("\nstr(DF.MB)\n");
print( str(DF.MB)   );

cat("\nsummary(DF.MB)\n");
print( summary(DF.MB)   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
my.predictors <- setdiff(
    colnames(DF.MB),
    c("year","qstrm","yield","cropsurv","car16uid","ymecoreg","latitude","longitude","harvacre")
    );

rollingWindowForwardValidation(
    training.window      = 5,
    validation.window    = 5,
    DF.input             = DF.MB,
    year                 = "year",
    ecoregion            = "ymecoreg",
    crop                 = "cropsurv",
    response.variable    = "yield",
    harvested.area       = "harvacre",
    evaluation.weight    = "harvacre",
    predictors           = my.predictors,
    by.variables.phase01 = c("ymecoreg","cropsurv"),
    by.variables.phase02 = c("cropsurv"),
    by.variables.phase03 = c("ymecoreg"),
    learner     = "xgboost_multiphase",
    search.grid = list(
        alpha       = seq(33,7,-2),
        lambda      = seq(33,7,-2),
        lambda_bias = c(23) #seq(23,11,-8)
        ),
    output.directory = file.path(dir.out,"rwFV"),
    log.threshold    = logger::TRACE # logger::INFO # logger::DEBUG 
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
