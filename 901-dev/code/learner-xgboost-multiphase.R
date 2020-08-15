
require(R6);

learner.xgboost.multiphase <- R6::R6Class(
    classname = 'learner.xgboost.multiphase',
    inherit   = learner.abstract.multiphase,
    public    = list(
        learner.single.phase = "xgboost_byGroup"
        )
    )

