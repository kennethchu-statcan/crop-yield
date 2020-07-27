#' Chen-Li-Wu Doubly Robust Estimator 
#'
#' The getChenLiWuEstimate function implements the Chen-Li-Wu Doubly Robust estimator, developed by Yilin Chen, Pengfei Li and Changbao Wu. Like nppCART, this function can be used to estimate the self-selection propensities and total population of a non-probability sample, using relevant auxillary information from a probability sample. However, it uses a different method (Doubly Robust estimator), and is provided in this package so that the user can compare results between the two algorithms.
#'
#' @param LIST.input This parameter corresponds to the probability and non-probability samples. The input must be a list containing two matrix-like data types (i.e. matrix, dataframe or tibble), called probability.sample and non.probability.sample. A value must be specified here for the function call to be successful.
#' @param formula This parameter corresponds to the specification of the response and predictor variables. The input must be an expression of the form y ~ model, where y represents the response variable and model represents the predictor variables (each predictor is separated by the + operator). The response variable must be in the non-probability sample, and the predictor variables must be contained in both the probability and non-probability samples. A value must be specified here for the function call to be successful.
#' @param weight This parameter corresponds to the column in the probability sample that contains the sampling weights. The input must be a string corresponding to a column name in probability.sample, such that there are only positive numbers in that column. A value must be specified here for initialization to be successful.
#' @param population This parameter corresponds to the total population that the probability and non-probability samples are taken from. The input must be a matrix-like data type (i.e. matrix, dataframe or tibble), which contains the response and predictor variables, unique identifiers (under ‘ID’), and the true self-selection propensities (under ‘propensity’). A value must be specified here for the function call to be successful.
#'
#' @return The getChenLiWuEstimate function returns a list, which contains the following calculated values: the total population estimate (called estimate); the propensity correlation (called cor.propensity); and the response correlation (called cor.response).
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
#' # Create LIST.samples
#' LIST.samples <- list(
#'     probability.sample = DF.probability, 
#'     non.probability.sample = DF.non.probability
#'     );
#'
#' # getChenLiWuEstimate: calculate population estimate, propensity correlation and response correlation
#' results.CLW <- getChenLiWuEstimate(
#'     LIST.input = LIST.samples,
#'     formula    = y ~ x1 + x2,
#'     weight     = "weight",
#'     population = DF.population
#'     );
#'
#' Y_total_hat_CLW    <- results.CLW[["estimate"]];
#' cor_propensity_CLW <- results.CLW[["cor.propensity"]];
#' cor_response_CLW   <- results.CLW[["cor.response"]];
#'
#' @importFrom stats formula lm coef nlm cor
#' @export

getChenLiWuEstimate <- function(
    LIST.input = NULL,
    formula    = NULL,
    weight     = NULL,
    population = NULL
    ) {

    #cat("\nstr(LIST.input)\n");
    #print( str(LIST.input)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    temp       <- base::all.vars(stats::as.formula(formula));
    response   <- temp[1];
    predictors <- base::setdiff(temp,base::c(response));

    #cat("\nresponse\n");
    #print( response   );
    #cat("\npredictors\n");
    #print( predictors   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    DF.nonprob <- LIST.input[['non.probability.sample']];
    results.lm <- stats::lm(
        formula = stats::as.formula(formula),
        data    = LIST.input[['non.probability.sample']],
        );

    beta.hat <- stats::coef(results.lm);

    #cat("\nresults.lm\n");
    #print( results.lm   );

    #cat("\nbeta.hat\n");
    #print( beta.hat   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    X.nonprob <- base::as.matrix(base::cbind(
        intercept = base::rep(1,base::nrow(LIST.input[['non.probability.sample']])),
        LIST.input[['non.probability.sample']][,predictors]
        ));
    #cat("\nhead(X.nonprob)\n");
    #print( head(X.nonprob)   );

    X.prob <- base::as.matrix(base::cbind(
        intercept = base::rep(1,base::nrow(LIST.input[['probability.sample']])),
        LIST.input[['probability.sample']][,predictors]
        ));
    #cat("\nhead(X.prob)\n");
    #print( head(X.prob)   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    X.prob_beta.hat <- X.prob %*% beta.hat;
    #cat("\nstr(X.prob_beta.hat)\n");
    #print( str(X.prob_beta.hat)   );

    weights <- LIST.input[['probability.sample']][,weight];
    #cat("\nstr(weights)\n");
    #print( str(weights)   ); 

    T.prob  <- base::sum( weights * X.prob_beta.hat );
    #cat("\nT.prob\n");
    #print( T.prob   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    minus_logL <- function(theta) {

        #cat("\ntheta\n");
        #print( theta   );

        X.nonprob_theta   <- X.nonprob %*% theta;
        X.prob_theta      <- X.prob    %*% theta;
        exp.prob          <- base::exp( X.prob_theta );
        one_plus_exp.prob <- 1 + exp.prob;
        minus_logL.out    <- - base::sum(X.nonprob_theta) + base::sum( weights * base::log(one_plus_exp.prob) )

        Pi.prob         <- exp.prob / one_plus_exp.prob
        weights_Pi.prob <- weights * Pi.prob;

        #cat("\nsummary(weights_Pi.prob)\n");
        #print( summary(weights_Pi.prob)   );

        #cat("\nstr(colSums(X.nonprob))\n")
        #print( str(colSums(X.nonprob))   );

        temp          <- base::apply(X=X.prob,MARGIN=2,FUN=function(z){z * weights_Pi.prob});
        temp.gradient <- - base::colSums(X.nonprob) + base::colSums(temp);
        #cat("\ntemp.gradient\n");
        #print( temp.gradient );

        temp         <- weights_Pi.prob * (1 - Pi.prob);
        temp.Hessian <- base::t(X.prob) %*% base::apply(X=X.prob,MARGIN=2,FUN=function(z){z * temp});
        #cat("temp.Hessian");
        #print( temp.Hessian );

        new.theta <- theta - base::solve(temp.Hessian,temp.gradient);
        #cat("\nnew.theta\n");
        #print( new.theta   );

        base::attr(minus_logL.out,"gradient") <- temp.gradient;
        base::attr(minus_logL.out,"hessian" ) <- temp.Hessian;

        #cat("\n~~~~~~~~~~\n")
        return(minus_logL.out);

        }

    #cat("\n~~~~~~~~~~\n")
    theta0 <- base::rep(0,base::ncol(X.prob));
    results.nlm <- stats::nlm(
        f = minus_logL,
        p = theta0
        );
    #cat("\nresults.nlm\n");
    #print( results.nlm   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    theta.hat            <- results.nlm[["estimate"]];
    X.nonprob_theta.hat  <- X.nonprob %*% theta.hat;
    exp.nonprob          <- base::exp( X.nonprob_theta.hat );
    one_plus_exp.nonprob <- 1 + exp.nonprob;
    weights.nonprob      <- one_plus_exp.nonprob / exp.nonprob;

    X.nonprob_beta.hat <- X.nonprob %*% beta.hat;
    T.nonprob <- base::sum( weights.nonprob * (LIST.input[['non.probability.sample']][,response] - X.nonprob_beta.hat) );
    #cat("\nT.nonprob\n");
    #print( T.nonprob   );

    temp <- base::merge(
        x = LIST.input[['non.probability.sample']],
        y = population,
        by = "ID"
        );
    cor.propensity <- stats::cor( temp[,"propensity"] , 1/weights.nonprob );
    #cat("\ncor.propensity\n");
    #print( cor.propensity   );

    cor.response <- stats::cor( LIST.input[['non.probability.sample']][,response] , X.nonprob_beta.hat );
    #cat("\ncor.response\n");
    #print( cor.response   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    Y_total_hat_CLW <- T.nonprob + T.prob;
    #cat("\nY_total_hat_CLW\n");
    #print( Y_total_hat_CLW   );

    ### ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~ ###
    LIST.output <- base::list(
        estimate       = Y_total_hat_CLW,
        cor.propensity = cor.propensity,
        cor.response   = cor.response
        );
    return( LIST.output );

    }

