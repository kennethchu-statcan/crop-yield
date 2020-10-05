
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
    "get-baseline-directory.R",
    "get-mock-production-directory.R",
    "get-mock-production-errors.R",
    "get-performance-metrics.R",
    "get-performance-metrics-directory.R",
    "get-relativeErrors-baseline.R",
    "initializePlot.R",
    "visualize-results.R",
    "weighted-statistics.R"
#   "get-prediction-directories.R",
#	"get-relativeErrors-models.R",
#	"test-correctness.R",
    );

for ( code.file in code.files ) {
    source(file.path(dir.code,code.file));
    }

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
set.seed(7654321);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
crops.retained <- c('CANOLA','WHTSPG_HR','OATS','BARLEY','WHTWIN','FLAXSD','SUNFLS')

list.relativeErrors.baseline <- get.relativeErrors.baseline(
    dir.baseline   = get.baseline.directory(dir.data = dir.data),
    crops.retained = crops.retained
    );

cat("\nlist.relativeErrors.baseline\n");
print( list.relativeErrors.baseline   );

DF.baseline.errors <- list.relativeErrors.baseline[["baseline_year"]];

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
performance.metrics.directory <- get.performance.metrics.directory(dir.data = dir.data);
DF.performance.metrics <- get.performance.metrics(
    dir.performance.metrics = performance.metrics.directory,
    output.directory        = file.path(dir.out,"020-performance-metrics")
    );

mock.production.directory <- get.mock.production.directory(dir.data = dir.data);
DF.mock.production.errors <- get.mock.production.errors(
    dir.mock.production.errors = mock.production.directory,
    output.directory           = file.path(dir.out,"030-mock-productions")
    );

cat("\nstr(DF.performance.metrics)\n");
print( str(DF.performance.metrics)   );

cat("\nstr(DF.mock.production.errors)\n");
print( str(DF.mock.production.errors)   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
visualize.results(
	DF.baseline.errors        = DF.baseline.errors,
	DF.performance.metrics    = DF.performance.metrics,
	DF.mock.production.errors = DF.mock.production.errors,
	output.sub.directory      = dir.out
	);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# list.prediction.directories <- get.prediction.directories(
# 	dir.data = dir.data
# 	);

# cat("\nlist.prediction.directories\n");
# print( list.prediction.directories   );

# list.relativeErrors.models <- get.relativeErrors.models(
# 	list.prediction.directories = list.prediction.directories,
# 	crops.retained              = crops.retained
# 	);

# cat("\nstr(list.relativeErrors.models)\n");
# print( str(list.relativeErrors.models)   );

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# test.correctness(
# 	crops.retained              = crops.retained,
# 	dir.out                     = dir.out,
# 	DF.relErrors.baseline.year  = list.relativeErrors.baseline[["baseline_year"]],
# 	list.prediction.directories = list.prediction.directories,
# 	list.relativeErrors.models  = list.relativeErrors.models
# 	);

### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
# visualize.comparisons(
# 	list.relativeErrors.baseline = list.relativeErrors.baseline,
# 	list.relativeErrors.models   = list.relativeErrors.models
# 	);

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
