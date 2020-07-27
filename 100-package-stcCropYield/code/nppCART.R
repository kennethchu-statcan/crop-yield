#' Tree-based Inverse Propensity Weighted Estimator
#'
#' The nppCART function implements the Tree-based Inverse Propensity Weighted estimator, developed by Kenneth Chu and Jean-François Beaumont. It can be used to estimate the self-selection propensity of each member in a non-probability sample and the total population that the sample was taken from. The estimates are calculated by performing recursive binary partitioning on a related probability sample, which shares relevant auxillary variables with the non-probability sample. The nppCART function creates an R6 class, which contains a number of public methods that are available for the user.
#' 
#' @docType class
#'
#' @import R6
#' @import dplyr
#' @export
#'
#' @return The nppCART function returns an instance of a R6 class, which has been initialized with the inputted parameters.
#'
#' @format \code{\link{R6Class}} object
#'
#' @examples
#' # Set population size
#' N <- 1000;
#'
#' # Create population
#' DF.population <- data.frame(
#'     ID = seq(1,N),
#'     y = jitter(rep(10,N)),
#'     x1 = jitter(rep(1,N)),
#'     x2 = jitter(rep(1,N)),
#'     propensity = jitter(rep(0.2,N))
#'     );
#'
#' # Get non-probability sample
#' DF.non.probability <- DF.population;
#' DF.non.probability[,"self.select"] <- sapply(
#'     X   = DF.non.probability[,"propensity"],
#'     FUN = function(x) { sample( x = c(0,1), size = 1, prob = c(1-x,x) ) }
#'     );
#' DF.non.probability <- DF.non.probability[1 == DF.non.probability[,"self.select"], c("ID","y","x1","x2")];
#'    
#' # Set the probability of selection
#' prob.selection <- 0.1;
#'
#' # Get probability sample
#' is.selected <- sample(
#'     x       = c(TRUE,FALSE),
#'     size    = nrow(DF.population),
#'     replace = TRUE,
#'     prob    = c(prob.selection, 1 - prob.selection)
#'     );
#' DF.probability <- DF.population[is.selected,c("ID","x1","x2")];
#' DF.probability[,"weight"] <- 1 / prob.selection;  
#'
#' # nppCART: initialize R6 class 
#' nppTree <- nppCART(
#'     predictors = c("x1","x2"),
#'     np.data    = DF.non.probability,
#'     p.data     = DF.probability,
#'     weight     = "weight"
#'     );
#'
#' # nppCART: grow classification tree
#' nppTree$grow();
#'
#' # nppCART: print classification tree
#' nppTree$print(
#'     FUN.format = function(x) {return( round(x,digits=3) )} 
#'     );
#'
#' # nppCART: get tree-calculated values
#' DF.npdata_with_propensity <- nppTree$get_npdata_with_propensity();
#' colnames(DF.npdata_with_propensity) <- gsub(
#'     x           = colnames(DF.npdata_with_propensity),
#'     pattern     = "propensity",
#'     replacement = "p_hat"
#'     );
#' DF.npdata_with_propensity <- merge(
#'     x  = DF.npdata_with_propensity,
#'     y  = DF.population[,c("ID","propensity")],
#'     by = "ID"
#'     );
#' DF.npdata_with_propensity <- DF.npdata_with_propensity[order(DF.npdata_with_propensity[,"ID"]),];
#'
#' # nppCART: calculate population estimate and propensity correlation
#' Y_total_hat_tree <- sum(
#'     DF.npdata_with_propensity[,"y"] / DF.npdata_with_propensity[,"p_hat"]
#'     );
#' cor_propensity_tree <- cor(
#'     x = DF.npdata_with_propensity[,"p_hat"],
#'     y = DF.npdata_with_propensity[,"propensity"]
#'     );  
#'
#' @param predictors This parameter corresponds to the auxillary variables on which the partitioning is performed. The input must be a string or vector of strings that contain column names shared by both np.data and p.data. If no value is specified, predictors will be set to all the column names in np.data.
#' @param np.data This parameter corresponds to the non-probability sample. The input must be a nonempty matrix-like data type (i.e. matrix, dataframe or tibble). A value must be specified here for initialization to be successful.
#' @param p.data This parameter corresponds to the probability sample. The input must be a nonempty matrix-like data type (i.e. matrix, dataframe or tibble). A value must be specified here for initialization to be successful.
#' @param weight This parameter corresponds to the column in the probability sample that contains the sampling weights. The input must be a string corresponding to a column name in p.data, such that there are only positive numbers in that column. A value must be specified here for initialization to be successful.
#' @param min.cell.size This parameter corresponds to the minimum number of rows remaining in the probability sample and non-probabilty sample to continue partitioning. The input must be a positive integer. If no value is specified, min.cell.size will be set to 10.
#' @param min.impurity This parameter corresponds to the minimum impurity calculated in each leaf node to continue partitioning. The input must be a positive number. If no value is specified, min.impurity will be set to 0.095.
#' @param max.levels This parameter corresponds to the maximum number of levels allowed by each factor in the predictor variables. The input must be a number that is greater than or equal to zero. If no value is specified, max.levels will be set to 10.
#'
#' @field predictors_factor This field contains the inputted predictors that are unordered factors. It is used for internal calcuations.
#' @field predictors_ordered_factor This field contains the inputted predictors that are ordered factors. It is used for internal calcuations.
#' @field predictors_numeric This field contains the inputted predictors that are numeric. It is used for internal calcuations.
#' @field nodes This field contains the nodes of the tree generated by the grow method. It is used for internal calcuations.
#' @field np.syntheticID This field contains synthetic unique identifiers for the data in the non-probability sample. It is used for internal calcuations.
#' @field p.syntheticID This field contains synthetic unique identifiers for the data in the probability sample. It is used for internal calcuations.
#' @field estimatedPopulationSize This field contains the sum of all weights in the probablity sample. It is used for internal calcuations.
#' 
#' @section Methods:
#' \describe{
#'  \item{\code{initialize(predictors, np.data, p.data, weight, min.cell.size, min.impurity)}}{This method is called when the R6 class is created (i.e. when nppCART is called). The arguments passed into nppCART are passed into initialize. This method contains input integrity checks to ensure that the arguments meet the required specifications. In addition, the method does some preprocessing of the input data.}
#'  \item{\code{grow()}}{This method is used to grow a classification tree through recursive binary partitioning of the predictors. It operates in the R6 class internally, and does not have parameters or a return value. This method should be called after the initialization of the class.}
#'  \item{\code{predict()}}{This method is obsolete and should not be used.}
#'  \item{\code{get_npdata_with_propensity(nodes)}}{This method returns a dataframe that contains the non-probability sample, with the tree-calculated values. The tree-calculated values include: the unique identifier for each node (called nodeID); the self-selection propensity for each member in the non-probability sample (called propensity); the number of members in the non-probability sample, which belong to each node (called np.count); the sum of the members’ weights in the probability sample, which belong to each node (called p.weight); and the tree impurity of each node (called impurity). There is one parameter, nodes, which is passed in a value internally by default, and should not be modified. This method should be used after calling grow.}
#'  \item{\code{print()}}{This method is used to print the classification tree in a readable format (each node is on a separate line and indented appropriately). There is one parameter, FUN.format, which is a function that customizes the output format. This method should be used after calling grow.}
#'  \item{\code{get_pruning_sequence(nodes)}}{This method is obsolete and should not be used.}
#' }

nppCART <- function(
	predictors = base::colnames(np.data),
	np.data,
	p.data,
	weight,
	min.cell.size = 10,
	min.impurity  = 0.095, 
	max.levels    = 10
	) {

	base::require(R6);
	base::require(dplyr);

    return(
    	R6_nppCART$new(
    		predictors    = predictors,
    		np.data       = np.data,
    		p.data        = p.data,
    		weight        = weight,
    		min.cell.size = min.cell.size,
    		min.impurity  = min.impurity,
    		max.levels    = max.levels
    		)
    	);
	}

R6_nppCART <- R6Class(
    classname = "R6_nppCART",

    public = list(

        predictors    = NULL,
        np.data       = NULL,
         p.data       = NULL,
        weight        = NULL,
        min.cell.size = NULL,
        min.impurity  = NULL,
        max.levels    = NULL,

        predictors_factor         = NULL,
        predictors_ordered_factor = NULL,
        predictors_numeric        = NULL,

        nodes = NULL,

        np.syntheticID = NULL,
         p.syntheticID = NULL,

        estimatedPopulationSize = NULL,

        initialize = function(predictors = base::colnames(np.data), np.data, p.data, weight, min.cell.size = 10, min.impurity = 0.095, max.levels = 10) {
            
            ############################################
            #          Input Integrity Checks          # 
            ############################################

            # test np.data
            base::stopifnot(    !base::is.null(np.data), # must not be NULL
                                base::is.data.frame(np.data) | base::is.matrix(np.data) | tibble::is_tibble(np.data), # must be matrix-like data type
                                base::nrow(np.data) > 0  ) # must not be empty
            
            # test p.data
            base::stopifnot(    !base::is.null(p.data), # must not be NULL
                                base::is.data.frame(p.data) | base::is.matrix(p.data) | tibble::is_tibble(p.data), # must be matrix-like data type
                                base::nrow(p.data) > 0  ) # must not be empty
            
            # test predictors
            base::stopifnot(    !base::is.null(predictors), # must not be NULL
                                base::is.character(predictors), # must be a string or set of strings
                                base::length(base::setdiff(predictors, base::colnames(np.data))) == 0, # must be contained in column names of np.data
                                base::length(base::setdiff(predictors, base::colnames(p.data))) == 0  ) # must be contained in column names of p.data
            
            # test weight
            base::stopifnot(    !base::is.null(weight), # must not be NULL
                                base::is.character(weight) & (base::length(weight) == 1), # must be a single string
                                base::length(base::setdiff(weight, base::colnames(p.data))) == 0,  # must correspond to a column name of p.data
                                base::is.numeric(p.data$weight), # corresponding column of p.data must contain only numeric data types
                                base::all(p.data$weight > 0)  ) # all numbers in corresponding column must be positive
            
            # test min.cell.size
            base::stopifnot(    !base::is.null(min.cell.size), # must not be NULL
                                base::is.numeric(min.cell.size) & (base::length(min.cell.size) == 1), # must be single number
                                (min.cell.size %% 1 == 0) & (min.cell.size > 0)  ) # must be a positive integer
            
            # test min.impurity
            base::stopifnot(    !base::is.null(min.impurity), # must not be NULL
                                base::is.numeric(min.impurity) & (base::length(min.impurity) == 1), # must be single number
                                min.impurity > 0  ) # must be positive

            # test max.levels
            base::stopifnot(    !base::is.null(max.levels), # must not be NULL
                                base::is.numeric(max.levels) & (base::length(max.levels) == 1), # must be single number
                                (max.levels %% 1 == 0) & (max.levels >= 0)  ) # must be an integer greater than or equal to zero

            self$predictors    <- predictors;
            self$np.data       <- np.data;
            self$p.data        <-  p.data;
            self$weight        <- weight;
            self$min.cell.size <- min.cell.size;
            self$min.impurity  <- min.impurity;
            self$max.levels    <- max.levels;

            for (temp.colname in self$predictors) {
                if (base::is.character(self$np.data[,temp.colname])) {
                    self$np.data[,temp.colname] <- base::as.factor(self$np.data[,temp.colname]);
                    }
                if (base::is.character(self$p.data[,temp.colname])) {
                    self$p.data[,temp.colname]  <- base::as.factor(self$p.data[,temp.colname]);
                    }
                }

            self$predictors_ordered_factor  <- self$predictors[base::sapply(X = self$np.data[1,self$predictors], FUN = function(x) { return( base::is.factor(x) & base::is.ordered(x) ) } )]

            # convert ordered factors to numeric values (corresponding to their index in the list of levels)
            for (temp.colname in self$predictors_ordered_factor) {
                self$np.data[,temp.colname] <- base::unlist(base::lapply(X = self$np.data[,temp.colname], FUN = function(x) { return(base::match(x, base::levels(self$np.data[,temp.colname]))) }))
                self$p.data[,temp.colname]  <- base::unlist(base::lapply(X = self$p.data[,temp.colname], FUN = function(x) { return(base::match(x, base::levels(self$p.data[,temp.colname]))) }))
                }

            self$predictors_factor  <- self$predictors[base::sapply(X = self$np.data[1,self$predictors], FUN = function(x) { return( base::is.factor(x) & !base::is.ordered(x) ) } )]
            self$predictors_numeric <- self$predictors[base::sapply(X = self$np.data[1,self$predictors], FUN = base::is.numeric)]

            #str(self$np.data)
            #print(self$predictors_ordered_factor)
            #print(self$predictors_factor)
            #print(self$predictors_numeric)

            # test if the max number of levels has been exceeded
            if( base::length(self$predictors_factor) > 0 ) {
                base::stopifnot(  base::max(base::unlist(base::lapply(X = self$np.data[,self$predictors_factor], FUN = function(x) { return( base::length(base::levels(x)) ) }))) <= self$max.levels  ) # testing factors in np.data
                base::stopifnot(  base::max(base::unlist(base::lapply(X = self$p.data[,self$predictors_factor], FUN = function(x) { return( base::length(base::levels(x)) ) }))) <= self$max.levels  ) # testing factors in p.data
            }

            # add custom row ID:
            self$np.syntheticID <- base::paste0(base::sample(x=letters,size=10,replace=TRUE),collapse="");
            self$np.data[,self$np.syntheticID] <- base::seq(1,base::nrow(self$np.data));

            self$p.syntheticID <- base::paste0(base::sample(x=letters,size=10,replace=TRUE),collapse="");
            self$p.data[,self$p.syntheticID] <- base::seq(1,base::nrow(self$p.data));

            self$estimatedPopulationSize <- base::sum(self$p.data[,self$weight]);

            },

        grow = function() {

            self$nodes <- list();
            lastNodeID <- 0;

            workQueue <- list(
                private$node$new(
                    parentID  = -1,
                    nodeID    = lastNodeID,
                    depth     = 0,
                    np.rowIDs = self$np.data[,self$np.syntheticID],
                     p.rowIDs =  self$p.data[, self$p.syntheticID]
                    )
                );

            while (0 < base::length(workQueue)) {

                currentNode <- private$pop(workQueue, envir = environment());

                currentNodeID          <- currentNode$nodeID;
                currentParentID        <- currentNode$parentID;
                currentDepth           <- currentNode$depth;
                np.currentRowIDs       <- currentNode$np.rowIDs;
                 p.currentRowIDs       <- currentNode$p.rowIDs;
                current_birthCriterion <- currentNode$birthCriterion;

                if (private$stoppingCriterionSatisfied(np.rowIDs = np.currentRowIDs,p.rowIDs = p.currentRowIDs)) {

                    #cat("\n# ~~~~~~~~~~ #")
                    #cat("\ncurrentNodeID\n");
                    #print( currentNodeID   );
                    #np.subset <- self$np.data[self$np.data[,self$np.syntheticID] %in% np.currentRowIDs,];
                    # p.subset <-  self$p.data[ self$p.data[, self$p.syntheticID] %in%  p.currentRowIDs,];
                    #cat("\nnp.subset\n");
                    #print( np.subset   );
                    #cat("\np.subset\n");
                    #print( p.subset   );
                    #cat("\nnpp_impurity\n");
                    #print( private$npp_impurity(np.rowIDs = np.currentRowIDs, p.rowIDs = p.currentRowIDs) )
                    #cat("# ~~~~~~~~~~ #\n")
                    #print(paste0("stoppingCriterionSatisfied: nodeID - ", currentNodeID))
                    self$nodes <- private$push(
                        list = self$nodes,
                        x    = private$node$new(
                            nodeID    = currentNodeID,
                            parentID  = currentParentID,
                            depth     = currentDepth,
                            np.rowIDs = np.currentRowIDs,
                             p.rowIDs =  p.currentRowIDs,
                            impurity  = private$npp_impurity(np.rowIDs = np.currentRowIDs, p.rowIDs = p.currentRowIDs),
                            birthCriterion = current_birthCriterion
                            )
                        );

                } else {

                    bestSplit <- private$get_best_split(
                        np.currentRowIDs = np.currentRowIDs,
                         p.currentRowIDs =  p.currentRowIDs
                        );

                    if ( base::is.null(bestSplit) ) {
                        #print(paste0("null bestSplit: nodeID - ", currentNodeID))
                        self$nodes <- private$push(
                            list = self$nodes,
                            x = private$node$new(
                                nodeID    = currentNodeID,
                                parentID  = currentParentID,
                                depth     = currentDepth,
                                np.rowIDs = np.currentRowIDs,
                                 p.rowIDs =  p.currentRowIDs,
                                impurity  = private$npp_impurity(np.rowIDs = np.currentRowIDs, p.rowIDs = p.currentRowIDs),
                                birthCriterion = current_birthCriterion
                                )
                            )
                    } else {

                        #print(paste0("valid bestSplit: nodeID - ", currentNodeID))

                        np.satisfied <- self$np.data[self$np.data[,self$np.syntheticID] %in% np.currentRowIDs,self$np.syntheticID][
                            bestSplit$comparison(
                                self$np.data[self$np.data[,self$np.syntheticID] %in% np.currentRowIDs,bestSplit$varname],
                                bestSplit$threshold
                                )
                            ];
                        np.notSatisfied <- base::sort(base::setdiff(np.currentRowIDs,np.satisfied));

                        p.satisfied <- self$p.data[self$p.data[,self$p.syntheticID] %in% p.currentRowIDs,self$p.syntheticID][
                            bestSplit$comparison(
                                self$p.data[self$p.data[,self$p.syntheticID] %in% p.currentRowIDs,bestSplit$varname],
                                bestSplit$threshold
                                )
                            ];
                        p.notSatisfied <- base::sort(base::setdiff(p.currentRowIDs,p.satisfied));

                        # adding 2 here to make ordering of nodeID agree with the order of appearance in self$nodes
                        lastNodeID          <- lastNodeID + 2;
                        notSatisfiedChildID <- lastNodeID;
                        workQueue           <- private$push(
                            list = workQueue,
                            x    = private$node$new(
                                parentID  = currentNodeID,
                                nodeID    = lastNodeID,
                                depth     = currentDepth + 1,
                                np.rowIDs = np.notSatisfied,
                                 p.rowIDs =  p.notSatisfied,
                                birthCriterion = private$birthCriterion$new(
                                    varname    = bestSplit$varname,
                                    threshold  = bestSplit$threshold,
                                    comparison = base::ifelse(bestSplit$varname %in% self$predictors_factor,"!=",">=")
                                    )
                                )
                            );

                        # subtracting 1 here to make ordering of nodeID agree with the order of appearance in self$nodes
                        lastNodeID       <- lastNodeID - 1;
                        satisfiedChildID <- lastNodeID;
                        workQueue        <- private$push(
                            list = workQueue,
                            x    = private$node$new(
                                parentID  = currentNodeID,
                                nodeID    = lastNodeID,
                                depth     = currentDepth + 1,
                                np.rowIDs = np.satisfied,
                                 p.rowIDs =  p.satisfied,
                                birthCriterion = private$birthCriterion$new(
                                    varname    = bestSplit$varname,
                                    threshold  = bestSplit$threshold,
                                    comparison = base::ifelse(bestSplit$varname %in% self$predictors_factor,"=","<")
                                    ),
                                )
                            );
                        # adding 1 here to make ordering of nodeID agree with the order of appearance in self$nodes
                        lastNodeID <- lastNodeID + 1;

                        #cat("\n# ~~~~~~~~~~ #")
                        #cat("\ncurrentNodeID\n");
                        #print( currentNodeID   );
                        #np.subset <- self$np.data[self$np.data[,self$np.syntheticID] %in% np.currentRowIDs,];
                        # p.subset <-  self$p.data[ self$p.data[, self$p.syntheticID] %in%  p.currentRowIDs,];
                        #cat("\nnp.subset\n");
                        #print( np.subset   );
                        #cat("\np.subset\n");
                        #print( p.subset   );
                        #cat("\nnpp_impurity\n");
                        #print( private$npp_impurity(np.rowIDs = np.currentRowIDs, p.rowIDs = p.currentRowIDs) )
                        #cat("# ~~~~~~~~~~ #\n")

                        self$nodes <- private$push(
                            list = self$nodes,
                            x = private$node$new(
                                nodeID    = currentNodeID,
                                parentID  = currentParentID,
                                depth     = currentDepth,
                                np.rowIDs = np.currentRowIDs,
                                 p.rowIDs =  p.currentRowIDs,
                                impurity  = private$npp_impurity(np.rowIDs = np.currentRowIDs, p.rowIDs = p.currentRowIDs),
                                splitCriterion = bestSplit,
                                birthCriterion = current_birthCriterion,
                                satisfiedChildID    =    satisfiedChildID,
                                notSatisfiedChildID = notSatisfiedChildID
                                )
                            );

                        }

                    }
                } 
            # private$order_nodes();
            # return( NULL );
            },

        predict = function() {
            return( NULL );
            },

        get_npdata_with_propensity = function(nodes = self$nodes) {

            DF.leaf_table <- private$nodes_to_table(nodes = nodes);
            DF.leaf_table <- DF.leaf_table[base::is.na(DF.leaf_table[,'satisfiedChildID']),];
            #cat("\nDF.leaf_table\n");
            #print( DF.leaf_table   );

            DF.nprow_to_leaf <- private$nprow_to_leafID(nodes = nodes);
            #cat("\nDF.nprow_to_leaf\n");
            #print( DF.nprow_to_leaf   );

            DF.nprow_to_leaf <- base::merge(
                x  = DF.nprow_to_leaf,
                y  = DF.leaf_table[,base::c("nodeID","propensity","np.count","p.weight","impurity")],
                by = "nodeID"
                );
            #cat("\nDF.nprow_to_leaf\n");
            #print( DF.nprow_to_leaf   );

            DF.output <- base::merge(
                x  = self$np.data,
                y  = DF.nprow_to_leaf,
                by = self$np.syntheticID
                );
            DF.output <- DF.output[,base::setdiff(base::colnames(DF.output),self$np.syntheticID)];
            #cat("\nDF.output\n");
            #print( DF.output   );

            return( DF.output );
            },

        print = function(
            FUN.format = function(x) { return(x) }
            ) {
            if ( 0 == base::length(self$nodes) ) {
                base::cat("\nlist of nodes is empty.\n")
                }
            else {
                for ( i in base::seq(1,base::length(self$nodes)) ) {
                    self$nodes[[i]]$print_node(FUN.format = FUN.format);
                    }
                base::cat("\n");
                }
            },

        get_pruning_sequence = function(nodes = self$nodes) {
            if ( 0 == base::length(nodes) ) {
                base::cat("\nThe supplied list of nodes is empty.\n")
                return( NULL );
                }

            DF.node_table <- private$nodes_to_table(nodes = nodes);

            alpha_subtree <- list(
                alpha                = 0,
                nodes_retained       = DF.node_table[,"nodeID"],
                nodes_pruned         = base::c(),
                nodes_removed        = base::c(),
                nodes_retained_table = DF.node_table
                );

            output_list <- list();
            nAlphas     <- 1;

            output_list[[nAlphas]] <- alpha_subtree;
            while (1 < base::length(alpha_subtree$nodes_retained)) {
                nAlphas       <- 1 + nAlphas;
                alpha_subtree <- private$get_alpha_subtree(DF.input = alpha_subtree$nodes_retained_table);
                output_list[[nAlphas]] <- alpha_subtree;
                }

            return( output_list );
            }
        ),

    private = list(
        pop = function(list, i = base::length(list), envir = NULL) {
            base::stopifnot(base::inherits(list, "list"))
            if (0 == base::length(list)) { return(NULL); }
            result <- list[[i]];
            base::assign(x = base::deparse(base::substitute(list)), value = list[-i], envir = envir);
            return( result );
            },
        push = function(list, x, i = base::length(list)) {
            base::stopifnot(base::inherits(list, "list"));
            return( base::c(list,list(x)) );
            },
        stoppingCriterionSatisfied = function(np.rowIDs = NULL, p.rowIDs = NULL) {

            if ( base::length(np.rowIDs) < self$min.cell.size ) { 
                #print("base::length(np.rowIDs) < self$min.cell.size")
                return(TRUE); 
            }

            estimatedCellPopulationSize <- base::sum(self$p.data[self$p.data[,self$p.syntheticID] %in%  p.rowIDs,self$weight]);
            if ( estimatedCellPopulationSize < length(np.rowIDs) ) { 
                #print("estimatedCellPopulationSize < length(np.rowIDs)")
                return(TRUE); 
            }

            impurity = private$npp_impurity(np.rowIDs = np.rowIDs, p.rowIDs = p.rowIDs);
            if ( impurity < self$min.impurity ) {
                #print("impurity < self$min.impurity") 
                return(TRUE); 
            }
 
            return( FALSE);

            },
        get_descendants = function(nodeIDs = NULL, nodeID = NULL) {
            nodes <- self$nodes[base::unlist(base::lapply(
                X   = self$nodes,
                FUN = function(x) { return( x$nodeID %in% nodeIDs) }
                ))];
            temp <- base::unlist(base::lapply(X = nodes, FUN = function(x) { return( nodeID == x$nodeID ) }));
            if (1 == base::sum(temp)) {
                targetNode  <- nodes[temp][[1]];
                descendants <- base::c();
                if (!base::is.null(targetNode$satisfiedChildID)) {
                    descendants <- base::c(
                        descendants,
                        targetNode$satisfiedChildID,
                        private$get_descendants(nodeIDs = nodeIDs, nodeID = targetNode$satisfiedChildID)
                        );
                    }
                if (!base::is.null(targetNode$notSatisfiedChildID)) {
                    descendants <- base::c(
                        descendants,
                        targetNode$notSatisfiedChildID,
                        private$get_descendants(nodeIDs = nodeIDs, nodeID = targetNode$notSatisfiedChildID)
                        );
                    }
                return( base::sort(descendants) );
            } else {
                return( base::c() );
                }
            },
        nprow_to_leafID = function(nodes = self$nodes) {
            if ( 0 == base::length(nodes) ) {
                base::cat("\nThe supplied list of nodes is empty.\n")
                return( NULL );
                }
            nNodes <- base::length(nodes);
            DF.output <- data.frame(x = numeric(0), y = numeric(0));
            base::colnames(DF.output) <- base::c(self$np.syntheticID,"nodeID");
            for ( i in base::seq(1,nNodes) ) {
                if ( base::is.null(nodes[[i]]$satisfiedChildID) ) {
                    DF.temp <- data.frame(
                        x = nodes[[i]]$np.rowIDs,
                        y = base::rep(x = nodes[[i]]$nodeID, times = base::length(nodes[[i]]$np.rowIDs))
                        );
                    base::colnames(DF.temp) <- base::c(self$np.syntheticID,"nodeID");
                    DF.output <- base::rbind(DF.output,DF.temp);
                    }
                }
            return( DF.output );
            },
        nodes_to_table = function(nodes = self$nodes) {
            if ( 0 == base::length(nodes) ) {
                base::cat("\nThe supplied list of nodes is empty.\n")
                return( NULL );
                }
            nrow.output <- base::length(nodes);
            DF.output <- data.frame(
                nodeID     = numeric(length = nrow.output),
                depth      = numeric(length = nrow.output),
                np.count   = numeric(length = nrow.output),
                p.weight   = numeric(length = nrow.output),
                impurity   = numeric(length = nrow.output),
                propensity = numeric(length = nrow.output),
                #riskWgtd   = numeric(length = nrow.output),
                #riskLeaves = numeric(length = nrow.output),
                #nLeaves    = numeric(length = nrow.output),
                parentID   = numeric(length = nrow.output),
                   satisfiedChildID = numeric(length = nrow.output),
                notSatisfiedChildID = numeric(length = nrow.output)
                );
            totalNumRecords <- base::length(nodes[[1]]$np.rowIDs);
            for ( i in base::seq(1,nrow.output) ) {
                DF.output[i,'nodeID'  ]   <- nodes[[i]]$nodeID;
                DF.output[i,'depth'   ]   <- nodes[[i]]$depth;
                DF.output[i,'np.count']   <- base::length(nodes[[i]]$np.rowIDs);
                DF.output[i,'p.weight']   <- base::sum(self$p.data[self$p.data[,self$p.syntheticID] %in% nodes[[i]]$p.rowIDs,self$weight]);
                DF.output[i,'impurity']   <- nodes[[i]]$impurity;
                DF.output[i,'propensity'] <- DF.output[i,'np.count'] / DF.output[i,'p.weight'];
                #DF.output[i,'riskWgtd']   <- DF.output[i,'risk'] * DF.output[i,'prop'];
                #DF.output[i,'riskLeaves'] <- 0;
                #DF.output[i,'nLeaves']    <- 0;
                DF.output[i,'parentID']   <- nodes[[i]]$parentID;
                DF.output[i,'satisfiedChildID'] <- base::ifelse(
                    base::is.null(nodes[[i]]$satisfiedChildID),
                    NA,
                    nodes[[i]]$satisfiedChildID
                    );
                DF.output[i,'notSatisfiedChildID'] <- base::ifelse(
                    base::is.null(nodes[[i]]$notSatisfiedChildID),
                    NA,
                    nodes[[i]]$notSatisfiedChildID
                    );
                }

            return( DF.output );
            },
        get_alpha_subtree = function(DF.input = NULL) {

            hasNoChildren <- base::is.na(DF.input[,'satisfiedChildID']) & base::is.na(DF.input[,'notSatisfiedChildID']);
            DF.leaves <- DF.input[hasNoChildren,];
            DF.leaves <- DF.leaves %>%
                dplyr::rename( riskWgtd_child = riskWgtd ) %>%
                dplyr::rename( nLeaves_child  = nLeaves  ) %>%
                dplyr::select( - base::c( riskLeaves , satisfiedChildID , notSatisfiedChildID ) ) %>%
                dplyr::mutate( nLeaves_child = 1 )
                ;

            retainedColumns <- base::c("nodeID","riskWgtd_child","nLeaves_child");
            DF.internalNodes <- DF.input[!hasNoChildren,] %>%
                dplyr::left_join(DF.leaves[,retainedColumns], by = base::c(   "satisfiedChildID" = "nodeID")) %>%
                dplyr::rename( riskWgtd_s  = riskWgtd_child ) %>%
                dplyr::rename( nLeaves_s   = nLeaves_child  ) %>%

                dplyr::left_join(DF.leaves[,retainedColumns], by = base::c("notSatisfiedChildID" = "nodeID")) %>%
                dplyr::rename( riskWgtd_ns = riskWgtd_child ) %>%
                dplyr::rename( nLeaves_ns  = nLeaves_child  ) %>%

                dplyr::mutate( riskWgtd_s  = base::ifelse(base::is.na(riskWgtd_s ),0,riskWgtd_s ) ) %>%
                dplyr::mutate( riskWgtd_ns = base::ifelse(base::is.na(riskWgtd_ns),0,riskWgtd_ns) ) %>%
                dplyr::mutate( riskLeaves  = riskLeaves + riskWgtd_s + riskWgtd_ns )    %>%

                dplyr::mutate( nLeaves_s   = base::ifelse(base::is.na(nLeaves_s  ),0,nLeaves_s  ) ) %>%
                dplyr::mutate( nLeaves_ns  = base::ifelse(base::is.na(nLeaves_ns ),0,nLeaves_ns ) ) %>%
                dplyr::mutate( nLeaves     = nLeaves + nLeaves_s + nLeaves_ns         ) %>%

                dplyr::select( - base::c(riskWgtd_s,riskWgtd_ns,nLeaves_s,nLeaves_ns) )
                ;

            retainedColumns <- base::c("nodeID","riskLeaves_child","nLeaves_child");
            for (temp.depth in base::seq(base::max(DF.internalNodes[,"depth"]),1,-1)) {
                DF.depth <- DF.internalNodes %>%
                    dplyr::filter( depth == temp.depth ) %>%
                    dplyr::rename( riskLeaves_child = riskLeaves ) %>%
                    dplyr::rename( nLeaves_child    = nLeaves    ) %>%
                    dplyr::select( - base::c( satisfiedChildID , notSatisfiedChildID ) )
                    ;
                DF.internalNodes <- DF.internalNodes %>%
                    dplyr::left_join(DF.depth[,retainedColumns], by = base::c(   "satisfiedChildID" = "nodeID")) %>%
                    dplyr::rename( riskLeaves_s  = riskLeaves_child ) %>%
                    dplyr::rename( nLeaves_s     = nLeaves_child    ) %>%

                    dplyr::left_join(DF.depth[,retainedColumns], by = base::c("notSatisfiedChildID" = "nodeID")) %>%
                    dplyr::rename( riskLeaves_ns = riskLeaves_child ) %>%
                    dplyr::rename( nLeaves_ns    = nLeaves_child    ) %>%

                    dplyr::mutate( riskLeaves_s  = base::ifelse(base::is.na(riskLeaves_s ),0,riskLeaves_s ) ) %>%
                    dplyr::mutate( riskLeaves_ns = base::ifelse(base::is.na(riskLeaves_ns),0,riskLeaves_ns) ) %>%
                    dplyr::mutate( riskLeaves    = riskLeaves + riskLeaves_s + riskLeaves_ns    ) %>%

                    dplyr::mutate( nLeaves_s     = base::ifelse(base::is.na(nLeaves_s    ),0,nLeaves_s    ) ) %>%
                    dplyr::mutate( nLeaves_ns    = base::ifelse(base::is.na(nLeaves_ns   ),0,nLeaves_ns   ) ) %>%
                    dplyr::mutate( nLeaves       = nLeaves + nLeaves_s + nLeaves_ns             ) %>%

                    dplyr::select( - base::c(riskLeaves_s,riskLeaves_ns,nLeaves_s,nLeaves_ns) )
                    ;
                }
            DF.internalNodes <- DF.internalNodes %>%
                dplyr::mutate( alpha = (risk - riskLeaves) / (nLeaves - 1) );

            min.alpha    <- base::min(DF.internalNodes[,'alpha']);
            nodesToPrune <- DF.internalNodes[DF.internalNodes[,'alpha'] == min.alpha,'nodeID'];

            nodesToRemove <- base::c();
            for (tempNodeID in nodesToPrune) {
                nodesToRemove <- base::unique(c(
                    nodesToRemove,
                    private$get_descendants(nodeIDs = DF.input[,'nodeID'], nodeID = tempNodeID)
                    ));
                }
            nodesToRemove <- base::intersect( base::sort(nodesToRemove) , DF.input[,'nodeID'] );

            nodesToRetain <- base::setdiff(DF.input[,'nodeID'],nodesToRemove);

            DF.output <- DF.input[DF.input[,'nodeID'] %in% nodesToRetain,];
            DF.output[DF.input[,'nodeID'] %in% nodesToPrune,   'satisfiedChildID'] <- NA;
            DF.output[DF.input[,'nodeID'] %in% nodesToPrune,'notSatisfiedChildID'] <- NA;

            output_list <- list(
                alpha = min.alpha,
                nodes_retained       = nodesToRetain,
                nodes_pruned         = nodesToPrune,
                nodes_removed        = nodesToRemove,
                nodes_retained_table = DF.output
                );

            return( output_list );
            },
        get_best_split = function(np.currentRowIDs,p.currentRowIDs) {
            uniqueVarValuePairs_factor  <- list();
            uniqueVarValuePairs_numeric <- list();
            if (base::length(self$predictors_factor) > 0) {
                temp.list <- base::as.list(private$get_non_constant_columns(
                    DF.input       = self$np.data,
                    currentRowIDs  = np.currentRowIDs,
                    input.colnames = self$predictors_factor
                    ));
                if (base::length(temp.list) > 0) {
                    uniqueVarValuePairs_factor <- private$get_var_value_pairs(
                        x = base::lapply(
                            X   = temp.list,
                            FUN = function(x) { return( private$get_midpoints(x) ); }
                            ),
                        comparison = private$is_equal_to
                        );
                    }
                }
            if (base::length(self$predictors_numeric) > 0) {
                temp.list <- base::as.list(private$get_non_constant_columns(
                    DF.input       = self$np.data,
                    currentRowIDs  = np.currentRowIDs,
                    input.colnames = self$predictors_numeric
                    ));
                if (base::length(temp.list) > 0) {
                    uniqueVarValuePairs_numeric <- private$get_var_value_pairs(
                        x = base::lapply(
                            X   = temp.list,
                            FUN = function(x) { return( private$get_midpoints(x) ); }
                            ),
                        comparison = private$is_less_than
                        );
                    }
                }
            uniqueVarValuePairs <- base::c(uniqueVarValuePairs_factor,uniqueVarValuePairs_numeric);
            #print(self$np.data)
            #print(self$p.data)
            impurities <- base::lapply(
                X   = uniqueVarValuePairs,
                FUN = function(x) {

                    np.satisfied <- self$np.data[self$np.data[,self$np.syntheticID] %in% np.currentRowIDs,self$np.syntheticID][
                        x$comparison(
                            self$np.data[self$np.data[,self$np.syntheticID] %in% np.currentRowIDs,x$varname],
                            x$threshold
                            )
                        ];
                    np.notSatisfied <- base::sort(base::setdiff(np.currentRowIDs,np.satisfied));

                    p.satisfied <- self$p.data[self$p.data[,self$p.syntheticID] %in% p.currentRowIDs,self$p.syntheticID][
                        x$comparison(
                            self$p.data[self$p.data[,self$p.syntheticID] %in% p.currentRowIDs,x$varname],
                            x$threshold
                            )
                        ];
                    p.notSatisfied <- base::sort(base::setdiff(p.currentRowIDs,p.satisfied));

                    ##########
                    ##########
                    # p1 <- base::length(   np.satisfied) / self$estimatedPopulationSize;
                    # p2 <- base::length(np.notSatisfied) / self$estimatedPopulationSize;
                    # g1 <- private$npp_impurity(np.rowIDs = np.satisfied,    p.rowIDs =  p.satisfied   );
                    # g2 <- private$npp_impurity(np.rowIDs = np.notSatisfied, p.rowIDs =  p.notSatisfied);
                    # #print(paste0("threshold: ", x$threshold, ", p1: ", p1, ", g1: ", g1, ", p2: ", p2, ", g2: ", g2, ", impurity: ", p1 * g1 + p2 * g2))
                    # return( p1 * g1 + p2 * g2 );
                    ##########
                    ##########
                    p1 <- sum(self$p.data[self$p.data[,self$p.syntheticID] %in% p.satisfied,   self$weight]);
                    p2 <- sum(self$p.data[self$p.data[,self$p.syntheticID] %in% p.notSatisfied,self$weight]);
                    p1 <- p1 / self$estimatedPopulationSize;
                    p2 <- p2 / self$estimatedPopulationSize;
                    g1 <- private$npp_impurity(np.rowIDs = np.satisfied,    p.rowIDs =  p.satisfied   );
                    g2 <- private$npp_impurity(np.rowIDs = np.notSatisfied, p.rowIDs =  p.notSatisfied);
                    temp.impurity <- p1 * g1 + p2 * g2;
                    if ( is.na(temp.impurity) ) { temp.impurity <- Inf; } 
                    return( temp.impurity );

                    }
                );

            # checks for first 3 NULL cases (no best split):
            #   -   uniqueVarValuePairs is empty (no available splits)
            #   -   all impurities are NA/NaN (no meaningful splits)
            #   -   minimum impurity is Inf (no meaningful splits)
            if (    base::length(uniqueVarValuePairs) < 1 | 
                    base::length(impurities[!base::is.na(impurities)]) == 0 | 
                    Inf == base::min(base::unique(base::as.numeric(impurities[!base::is.na(impurities)])))  ) 
                { return(NULL); }
            
            # returns split that corresponds to the minimum impurity (exluding NA values)
            output <- uniqueVarValuePairs[[ base::which.min(impurities[!base::is.na(impurities)]) ]];
            
            check.np.satisfied <- self$np.data[self$np.data[,self$np.syntheticID] %in% np.currentRowIDs,self$np.syntheticID][
                        output$comparison(
                            self$np.data[self$np.data[,self$np.syntheticID] %in% np.currentRowIDs,output$varname],
                            output$threshold
                            )
                        ];
            check.np.notSatisfied <- base::sort(base::setdiff(np.currentRowIDs,check.np.satisfied));

            # checks for last NULL case (no best split):
            #   -   all np.data is satisfied or all np.data is not satisfied
            #       (prevents infinite loop that keeps splitting into same "all or nothing" groups)
            if( length(check.np.satisfied) < 1 | length(check.np.notSatisfied) < 1 ) { return(NULL) }

            return( output );
            },
        get_non_constant_columns = function(DF.input = NULL, currentRowIDs = NULL, input.colnames = NULL) {
            DF.output <- DF.input[DF.input[,self$np.syntheticID] %in% currentRowIDs,input.colnames];

            # If length(input.colnames) == 1, then DF.output will be a vector.
            # In that case, cast DF.output into a data frame.
            DF.output <- base::as.data.frame(DF.output);
            
            base::colnames(DF.output) <- input.colnames;
            nUniqueValues       <- base::apply(X = DF.output, MARGIN = 2, FUN = function(x) { return(base::length(base::unique(x))) } );
            DF.output           <- base::as.data.frame(DF.output[,nUniqueValues > 1]);
            base::colnames(DF.output) <- input.colnames[nUniqueValues > 1];

            return( DF.output );
            },
        get_midpoints = function(x) {
            if (base::is.numeric(x)) {
                y <- base::sort(base::unique(x));
                return( base::apply(X=data.frame(c1=y[2:base::length(y)],c2=y[1:(base::length(y)-1)]),MARGIN=1,FUN=base::mean) );
                }
            else if (base::is.factor(x) & !base::is.ordered(x)) {
                return( private$enumerate_set(base::levels(x)) );
                }
            else {
                return( base::sort(base::unique(x)) );
                }
            },

        # @param: set - a vector or list, containing factors
        # @return: combinationsA - a list of vectors containing 1st group of all possible splits
        # Enumerates all possible ways to split a given set of factors into 2 groups
        # (combinationsA and combinationsB are lists corresponding to each group caused by splitting;
        #  we only need to return 1 group, since we are checking if the factor belongs to the group or not)
        enumerate_set = function(set = NULL) {

            combinationsA <- list()      
            #combinationsB <- list()    
            indexA <- 1                 
            #indexB <- 1                

            # iterate from 1 to half the length of the set (rounded down if odd),
            # where i corresponds to the group size created by combn
            # (this will cover all possible combinations, since we look at the group
            #  returned by combn and the remaining members of the set not in the group)
            for (i in base::seq(1, base::trunc(base::length(set) / 2))) {

                # combn returns a dataframe containing all possible combinations;
                # ncol corresponds to the number of possible combinations 
                size <- base::ncol(utils::combn(set, i))

                # special even case: checks if group size i is equal to half the length of the set
                if ( i == base::length(set) / 2) {
                    matrix <- utils::combn(set, i)

                    # iterate through half of the combinations and store them all
                    # (again, this will cover all possible combinations, since we look at what's
                    #  in the group and what's not in the group)
                    for (j in base::seq(1, size / 2)) {
                        combinationsA[[indexA]] <- matrix[, j]
                        #combinationsB[[indexB]] <- matrix[, size + 1 - j]
                        indexA <- indexA + 1
                        #indexB <- indexB + 1
                    }
                }
                # standard case
                else {
                    matrixA <- utils::combn(set, i)
                    #matrixB <- combn(set, length(set) - i)

                    # iterate through all the combinations and store them all
                    # (here, we are looking at what's in the group of size i, and what's
                    #  not in the group, which has size = length(set) - i)
                    for (j in base::seq(1, size)) {
                        combinationsA[[indexA]] <- matrixA[, j]
                        #combinationsB[[indexB]] <- matrixB[, size + 1 - j]
                        indexA <- indexA + 1
                        #indexB <- indexB + 1
                    }
                }
            }

            return (combinationsA);
            },
        get_var_value_pairs = function(x = NULL, comparison = NULL) {
            names_x  <- base::names(x);
            templist <- list();
            for (i in base::seq(1,base::length(names_x))) {
                for (j in base::seq(1,base::length(x[[i]]))) {
                    templist <- private$push(
                        list = templist,
                        x    = private$splitCriterion$new(
                            varname    = names_x[i],
                            threshold  = x[[i]][j],
                            comparison = comparison
                            )
                        );
                    }
                }
            return( templist );
            },
        is_less_than = function(x,y) {
            return(x < y)
            },

        # @param: x - a singular variable, y - a singular variable or vector
        # @return: bool (TRUE or FALSE)
        # Checks if x is contained in y: useful for checking if factor is in contained in split (combination of factors),
        # but also works for checking numerics and ordered factors (i.e. checking if x == y)
        is_equal_to = function(x,y) {
            ret <- base::lapply(  X = x, 
                            FUN = function(x) {
                                for(element in y) {
                                    if(element == x) {
                                        return(TRUE)
                                    }
                                }
                                return(FALSE)
                                
                                #print(any(ifelse(input == y, TRUE, FALSE)))
                                #return(any(ifelse(input == y, TRUE, FALSE)))
            
                                #print(x %in% y)
                                #return(x %in% y)
                            })
            #print(x %in% y)
            #return(x %in% y)

            return(base::unlist(ret))
            },
        impurity = function(x) {
            # Gini impurity
            p <- base::as.numeric(base::table(x) / base::length(x));
            return( base::sum(p * (1 - p)) );
            },
        npp_impurity = function(np.rowIDs,p.rowIDs) {
            np.subset <- self$np.data[self$np.data[,self$np.syntheticID] %in% np.rowIDs,];
             p.subset <-  self$p.data[ self$p.data[, self$p.syntheticID] %in%  p.rowIDs,];
            
            #print(self$min.cell.size)
            #print(base::sum(p.subset[,self$weight]))
            #print(1 < base::nrow(np.subset) / base::sum(p.subset[,self$weight]))

            if ( base::nrow(np.subset) < self$min.cell.size ) { return(Inf); }
            if ( base::nrow( p.subset) < self$min.cell.size ) { return(Inf); }

            estimatedPopulationSize <- base::sum(p.subset[,self$weight]);
            if ( 0 == estimatedPopulationSize ) { return( Inf ); }

            p <- base::nrow(np.subset) / estimatedPopulationSize;
            if ( 1 < p ) { return(Inf); }

            impurity <- 2 * p * (1 - p);
            return( impurity );
            },
        splitCriterion = R6Class(
            classname  = "splitCriterion",
            public = list(
                varname    = NULL,
                threshold  = NULL,
                comparison = NULL,
                initialize = function(varname = NULL, threshold = NULL, comparison = NULL) {
                    self$varname    = varname;
                    self$threshold  = threshold;
                    self$comparison = comparison;
                    }
                )
            ),
        birthCriterion = R6Class(
            classname  = "birthCriterion",
            public = list(
                varname    = NULL,
                threshold  = NULL,
                comparison = NULL,
                initialize = function(varname = NULL, threshold = NULL, comparison = NULL) {
                    self$varname    = varname;
                    self$threshold  = threshold;
                    self$comparison = comparison;
                    }
                )
            ),
        order_nodes = function() {
            if (base::length(self$nodes) > 0) {
                nodeIDs <- base::as.integer(base::lapply(X = self$nodes, FUN = function(x) { return( x$nodeID ); } ))
                self$nodes <- self$nodes[base::order(nodeIDs)];
                return( NULL );
                }
            },

        node = R6Class(
            classname = "node",

            public = list(

                nodeID    = NULL,
                parentID  = NULL,
                depth     = NULL,
                np.rowIDs = NULL,
                 p.rowIDs = NULL,
                impurity  = NULL,

                splitCriterion = NULL,
                birthCriterion = NULL,

                satisfiedChildID    = NULL,
                notSatisfiedChildID = NULL,

                initialize = function(
                    nodeID    = NULL,
                    parentID  = NULL,
                    depth     = NULL,
                    np.rowIDs = NULL,
                     p.rowIDs = NULL,
                    impurity  = NULL,

                    splitCriterion = NULL,
                    birthCriterion = NULL,

                    satisfiedChildID    = NULL,
                    notSatisfiedChildID = NULL
                    ) {
                        self$nodeID    <- nodeID;
                        self$parentID  <- parentID;
                        self$depth     <- depth;
                        self$np.rowIDs <- np.rowIDs;
                        self$p.rowIDs  <-  p.rowIDs;
                        self$impurity  <- impurity;

                        self$splitCriterion <- splitCriterion;
                        self$birthCriterion <- birthCriterion;

                        self$satisfiedChildID    <- satisfiedChildID;
                        self$notSatisfiedChildID <- notSatisfiedChildID;
                    },

                print_node = function(
                    indent     = '  ',
                    FUN.format = function(x) { return(x) }
                    ) {
                    base::cat("\n");
                    base::cat(base::paste0(base::rep(indent,self$depth),collapse="") );
                    base::cat(base::paste0("(",self$nodeID,") "));
                    if (0 == self$nodeID) {
                        base::cat("[root]");
                        }
                    else {
                        base::cat(base::paste0("[",
                            self$birthCriterion$varname,   " ",
                            self$birthCriterion$comparison," ",
                            #FUN.format(self$birthCriterion$threshold),
                            self$birthCriterion$threshold,
                            "]"));
                        }
                    base::cat(base::paste0(", impurity = ",FUN.format(self$impurity)));
                    base::cat(base::paste0(", np.count = ",FUN.format(base::length(self$np.rowIDs))));
                    base::cat(base::paste0(", p.count = ", FUN.format(base::length(self$p.rowIDs))));
                    }
                )

            )

        )

    );