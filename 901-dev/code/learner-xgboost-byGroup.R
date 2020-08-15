
require(R6);

learner.xgboost.byGroup <- R6::R6Class(
    classname = 'learner.xgboost.byGroup',
    inherit   = learner.abstract.byGroup,
    public    = list(
        learner.single.group = "xgboost"
        )

    )

