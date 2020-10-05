
weighted.mean <- function(x = NULL, weights = NULL) {

    return( sum( weights * x ) / sum(weights) );

    }

weighted.var <- function(x = NULL, weights = NULL) {

	my.weighted.mean <- weighted.mean(x = x, weights = weights);
	pre.weighted.var <- sum( weights * ((x - my.weighted.mean)^2) ) / sum(weights) ;

	n.nonzero.weights <- sum( weights > 0 );
	my.factor <- n.nonzero.weights / (n.nonzero.weights - 1); 

    return( my.factor * pre.weighted.var );

    }

weighted.sd <- function(x = NULL, weights = NULL) {

    return( sqrt( weighted.var(x = x, weights = weights) ) );

    }
