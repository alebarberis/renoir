setClassUnion(name = "numericOrNULL", members = c("numeric", "NULL"))
setClassUnion(name = "integerOrNULL", members = c("integer", "NULL"))
setClassUnion(name = "characterOrNULL", members = c("character", "NULL"))
setClassUnion(name = "fileOrNULL", members = c("file", "NULL"))

# if (!methods::isGeneric("plot")) methods::setGeneric("plot", function(x, y, ...) standardGeneric("plot"))
#-----------------------------------------------------------------------------------#
#Logger
methods::setGeneric(name = "get_con",          function(object) standardGeneric("get_con"))
methods::setGeneric(name = "get_verbose",      function(object) standardGeneric("get_verbose"))
methods::setGeneric(name = "get_path",         function(object) standardGeneric("get_path"))
methods::setGeneric(name = "get_level",        function(object) standardGeneric("get_level"))
methods::setGeneric(name = "close_con",        function(object) standardGeneric("close_con"))
methods::setGeneric(name = "open_con",         function(object) standardGeneric("open_con"))
methods::setGeneric(name = "print_log",        function(object, ...) standardGeneric("print_log"))
methods::setGeneric(name = "print_log_line",   function(object, ...) standardGeneric("print_log_line"))
methods::setGeneric(name = "log_all",          function(object, ...) standardGeneric("log_all"))
methods::setGeneric(name = "log_trace",        function(object, ...) standardGeneric("log_trace"))
methods::setGeneric(name = "log_debug",        function(object, ...) standardGeneric("log_debug"))
methods::setGeneric(name = "log_info",         function(object, ...) standardGeneric("log_info"))

#-----------------------------------------------------------------------------------#
#Trainer
methods::setGeneric(name = "get_parameters", def = function(object) standardGeneric("get_parameters"))
methods::setGeneric(name = "get_id",         def = function(object) standardGeneric("get_id"))
methods::setGeneric(name = "get_trainer",    def = function(object) standardGeneric("get_trainer"))

# methods::setGeneric(name = "train",          def = function(trainer, object, hyperparameters, indices, ...) standardGeneric("train"))

#'@name train
#'@rdname train
methods::setGeneric(name = "train",          def = function(trainer, features, ...) standardGeneric("train"))

#-----------------------------------------------------------------------------------#
#Trained
methods::setGeneric(name = "get_learning_method", def = function(object) standardGeneric("get_learning_method"))
methods::setGeneric(name = "get_fit",             def = function(object) standardGeneric("get_fit"))
methods::setGeneric(name = "get_config",          def = function(object) standardGeneric("get_config"))
methods::setGeneric(name = "get_nfeatures",       def = function(object) standardGeneric("get_nfeatures"))


# methods::setGeneric(name = "get_summary",       def = function(object, ...) standardGeneric("get_summary"))

methods::setGeneric(name = "get_config_element",  def = function(object, value) standardGeneric("get_config_element"))
methods::setGeneric(name = "add_config_element",  def = function(object, value) standardGeneric("add_config_element"))
methods::setGeneric(name = "del_config_element",  def = function(object, value) standardGeneric("del_config_element"))
methods::setGeneric(name = "add_screened_nvar_to_config",   def = function(object, value) standardGeneric("add_screened_nvar_to_config"))
methods::setGeneric(name = "del_screened_nvar_from_config", def = function(object) standardGeneric("del_screened_nvar_from_config"))
methods::setGeneric(name = "get_screened_nvar_from_config", def = function(object) standardGeneric("get_screened_nvar_from_config"))

methods::setGeneric(name = "set_learning_method", function(object, value) standardGeneric("set_learning_method"))
#-----------------------------------------------------------------------------------#
#Screener
methods::setGeneric(name = "get_screener", def = function(object) standardGeneric("get_screener"))

#'@name screen
#'@rdname screen
methods::setGeneric(name = "screen",       def = function(screener, ...) standardGeneric("screen"))

#-----------------------------------------------------------------------------------#
#Screened
methods::setGeneric(name = "get_method",     function(object) standardGeneric("get_method"))
methods::setGeneric(name = "get_score",      function(object) standardGeneric("get_score"))

methods::setGeneric(name = "update_n",   function(object, value) standardGeneric("update_n"))
methods::setGeneric(name = "set_method", function(object, value) standardGeneric("set_method"))
methods::setGeneric(name = "set_index", function(object, value) standardGeneric("set_index"))
methods::setGeneric(name = "set_score", function(object, value) standardGeneric("set_score"))

#-----------------------------------------------------------------------------------#
#Scorer
methods::setGeneric(name = "get_scorer",  function(object) standardGeneric("get_scorer"))
methods::setGeneric(name = "get_grouped", function(object) standardGeneric("get_grouped"))
methods::setGeneric(name = "get_multi",   function(object) standardGeneric("get_multi"))
methods::setGeneric(name = "get_optimum", function(object) standardGeneric("get_optimum"))

methods::setGeneric(name = "update_scorer", function(object,...) standardGeneric("update_scorer"))

#'@name mean_score
#'@rdname mean_score
methods::setGeneric(name = "mean_score",     function(scorer, true, pred, weights, ...) standardGeneric("mean_score"))

# #'@name se_score
# #'@rdname se_score
# methods::setGeneric(name = "se_score",       function(scorer, true, pred, weights, ...) standardGeneric("se_score"))

#'@name summary_score
#'@rdname summary_score
methods::setGeneric(name = "summary_score",  function(scorer, true, pred, weights, ...) standardGeneric("summary_score"))

methods::setGeneric(name = "select_optimal_score",     function(scorer, measures, ...) standardGeneric("select_optimal_score"))

#'@name score
#'@rdname score
methods::setGeneric(name = "score",    function(scorer, true, pred, weights, ...) standardGeneric("score"))

#-----------------------------------------------------------------------------------#
#Quantifier
methods::setGeneric(name = "get_quantifier",  function(object) standardGeneric("get_quantifier"))
# methods::setGeneric(name = "quantify",    function(quantifier, x, weights, ...) standardGeneric("quantify"))

#'@name quantify
#'@rdname quantify
methods::setGeneric(name = "quantify",    function(quantifier, ...) standardGeneric("quantify"))

#-----------------------------------------------------------------------------------#
#Forecaster
methods::setGeneric(name = "get_forecaster",      function(object) standardGeneric("get_forecaster"))
methods::setGeneric(name = "get_prediction_type", function(object, ...) standardGeneric("get_prediction_type"))

# methods::setGeneric(name = "forecast", function(forecaster, trained, models, ...) standardGeneric("forecast"))

#'@name forecast
#'@rdname forecast
methods::setGeneric(name = "forecast", function(forecaster, models, ...) standardGeneric("forecast"))
#-----------------------------------------------------------------------------------#
#ForecasterList
methods::setGeneric(name = "select_forecaster",   function(object, by, ...) standardGeneric("select_forecaster"))
methods::setGeneric(name = "subset_forecaster",   function(object, by, ...) standardGeneric("subset_forecaster"))

#-----------------------------------------------------------------------------------#
#Tester
# methods::setGeneric(name = "test",     function(trained, tester, newx, newoffset, newy, weights, indices, ...) standardGeneric("test"))
# methods::setGeneric(name = "test",     function(trained, tester, forecaster, scorer, indices, ...) standardGeneric("test"))

#'@name test
#'@rdname test
methods::setGeneric(name = "test",     function(models, tester, forecaster, scorer, indices, ...) standardGeneric("test"))

#-----------------------------------------------------------------------------------#
#Selector

#'@name select
#'@rdname select
methods::setGeneric(name = "select", function(selector, models, merr, sderr, scorer, ...) standardGeneric("select"))

#-----------------------------------------------------------------------------------#
#Tuner
methods::setGeneric(name = "get_screener",  def = function(object) standardGeneric("get_screener"))
methods::setGeneric(name = "get_logger",    def = function(object) standardGeneric("get_logger"))
methods::setGeneric(name = "get_measure",   def = function(object) standardGeneric("get_measure"))
methods::setGeneric(name = "get_tester",    def = function(object) standardGeneric("get_tester"))
methods::setGeneric(name = "get_selector",  def = function(object,...) standardGeneric("get_selector"))


methods::setGeneric(name = "set_trainer",   def = function(object, value) standardGeneric("set_trainer"))
methods::setGeneric(name = "set_screener",  def = function(object, value) standardGeneric("set_screener"))
methods::setGeneric(name = "set_resampler", def = function(object, value) standardGeneric("set_resampler"))

methods::setGeneric(name = "tune",     function(tuner, ...) standardGeneric("tune"))

#-----------------------------------------------------------------------------------#
#Tuned

#'@name get_model
#'@rdname get_model
methods::setGeneric(name = "get_model",             function(object, ...) standardGeneric("get_model"))
# methods::setGeneric(name = "get_mean_error",        function(object, ...) standardGeneric("get_mean_error"))
# methods::setGeneric(name = "get_sd_error",          function(object) standardGeneric("get_sd_error"))
methods::setGeneric(name = "get_mean_score",        function(object, ...) standardGeneric("get_mean_score"))
# methods::setGeneric(name = "get_sem",               function(object) standardGeneric("get_sem"))
methods::setGeneric(name = "get_tuning_method",     function(object) standardGeneric("get_tuning_method"))
methods::setGeneric(name = "get_resampling_method", function(object) standardGeneric("get_resampling_method"))
methods::setGeneric(name = "get_screened",          function(object, ...) standardGeneric("get_screened"))
methods::setGeneric(name = "get_xvars",             function(object) standardGeneric("get_xvars"))
methods::setGeneric(name = "get_xobs",              function(object) standardGeneric("get_xobs"))
methods::setGeneric(name = "get_trained",           function(object) standardGeneric("get_trained"))
methods::setGeneric(name = "get_validated",         function(object) standardGeneric("get_validated"))
methods::setGeneric(name = "get_stability",         function(object) standardGeneric("get_stability"))

methods::setGeneric(name = "set_model",       def = function(object, value) standardGeneric("set_model"))
methods::setGeneric(name = "set_trained",           function(object, value) standardGeneric("set_trained"))
methods::setGeneric(name = "set_validated",         function(object, value) standardGeneric("set_validated"))

# methods::setGeneric(name = "get_screening_method", function(object) standardGeneric("get_screening_method"))
# methods::setGeneric(name = "get_tuned",            function(object, ...) standardGeneric("get_tuned"))
# methods::setGeneric(name = "get_nvars",            function(object) standardGeneric("get_nvars"))
# methods::setGeneric(name = "get_nobs",             function(object, ...) standardGeneric("get_nobs"))

#-----------------------------------------------------------------------------------#
#Learner
methods::setGeneric(name = "Learner",     function(id, ...) standardGeneric("Learner"))

methods::setGeneric(name = "get_resampler",   def = function(object) standardGeneric("get_resampler"))
methods::setGeneric(name = "get_tuner",       def = function(object) standardGeneric("get_tuner"))

methods::setGeneric(name = "set_tuner",       def = function(object, value) standardGeneric("set_tuner"))

# methods::setGeneric(name = "learn",     function(learner, hyperparameters, sampler, ...) standardGeneric("learn"))

#'@name learn
#'@rdname learn
methods::setGeneric(name = "learn",     function(learner, hyperparameters, ...) standardGeneric("learn"))

# methods::setGeneric(name = "evaluate",  function(models, learner, hyperparameters, sampler, forecaster, scorer, ...) standardGeneric("evaluate"))

#-----------------------------------------------------------------------------------#
#Tested
# methods::setGeneric("get_test", function(object, set, config, type.measure, ...) standardGeneric("get_test"))
# methods::setGeneric("get_test_measure", function(object, set, config, type.measure) standardGeneric("get_test_measure"))
# methods::setGeneric("get_test_type_measure", function(object) standardGeneric("get_test_type_measure"))
# methods::setGeneric("get_err",   function(object, ...) standardGeneric("get_err"))
# methods::setGeneric("get_merr",  function(object, ...) standardGeneric("get_merr"))
# methods::setGeneric("get_sderr", function(object, ...) standardGeneric("get_sderr"))


methods::setGeneric("get_score",   function(object, ...) standardGeneric("get_score"))
# methods::setGeneric("get_m",   function(object, ...) standardGeneric("get_m"))
methods::setGeneric("get_mscore",   function(object, ...) standardGeneric("get_mscore"))
methods::setGeneric("get_sem", function(object, ...) standardGeneric("get_sem"))
methods::setGeneric("get_ci", function(object, ...) standardGeneric("get_ci"))
methods::setGeneric("get_opt",   function(object, ...) standardGeneric("get_opt"))
methods::setGeneric("get_1se",   function(object, ...) standardGeneric("get_1se"))


methods::setGeneric("select_by", function(object, ...) standardGeneric("select_by"))
methods::setGeneric("subset_list", function(object, ...) standardGeneric("subset_list"))

#-----------------------------------------------------------------------------------#
#Evaluator
#'@name evaluate
#'@rdname evaluate
methods::setGeneric(name = "evaluate",  function(models, learner, evaluator, npoints, ...) standardGeneric("evaluate"))

#-----------------------------------------------------------------------------------#
#Evaluated
methods::setGeneric(name = "get_sampling",     def = function(object) standardGeneric("get_sampling"))
methods::setGeneric(name = "get_size",         def = function(object) standardGeneric("get_size"))
methods::setGeneric(name = "get_observations", def = function(object) standardGeneric("get_observations"))
methods::setGeneric(name = "get_models",       def = function(object) standardGeneric("get_models"))
methods::setGeneric(name = "get_train",        def = function(object) standardGeneric("get_train"))
methods::setGeneric(name = "get_test",         def = function(object) standardGeneric("get_test"))
methods::setGeneric(name = "get_full",         def = function(object) standardGeneric("get_full"))
methods::setGeneric(name = "get_response",     def = function(object) standardGeneric("get_response"))
methods::setGeneric(name = "get_nout",         def = function(object) standardGeneric("get_nout"))
# methods::setGeneric(name = "get_accuracy",     def = function(object) standardGeneric("get_accuracy"))
methods::setGeneric(name = "get_performance",  def = function(object) standardGeneric("get_performance"))
methods::setGeneric(name = "get_learning",     def = function(object) standardGeneric("get_learning"))
methods::setGeneric(name = "get_screening",    def = function(object) standardGeneric("get_screening"))

#'@name get_sample
#'@rdname get_sample
methods::setGeneric(name = "get_sample", def = function(object, index, ...) standardGeneric("get_sample"))

methods::setGeneric(name = "set_models",   def = function(object, value) standardGeneric("set_models"))

#-----------------------------------------------------------------------------------#
#EvaluatedList
methods::setGeneric(name = "which_best",     def = function(object, set, measure, ...) standardGeneric("which_best"))

#-----------------------------------------------------------------------------------#
#Filter
methods::setGeneric(name = "get_filter", def = function(object) standardGeneric("get_filter"))

#'@name filter
#'@rdname filter
methods::setGeneric(name = "filter",   function(filter, ...) standardGeneric("filter"))


#-----------------------------------------------------------------------------------#
#Recorder
#'@name record
#'@rdname record
methods::setGeneric(name = "record",   function(object, recorder, ...) standardGeneric("record"))
methods::setGeneric(name = "get_recorder",   def = function(object) standardGeneric("get_recorder"))

#'@name features
#'@rdname features
methods::setGeneric(name = "features",  def = function(object, recorder, ...) standardGeneric("features"))

#'@name nfeatures
#'@rdname nfeatures
methods::setGeneric(name = "nfeatures", def = function(object, recorder, ...) standardGeneric("nfeatures"))

#-----------------------------------------------------------------------------------#
#Marker
methods::setGeneric(name = "get_function", def = function(object) standardGeneric("get_function"))
methods::setGeneric(name = "get_marker",   def = function(object) standardGeneric("get_marker"))

#'@name mark
#'@rdname mark
methods::setGeneric(name = "mark",   function(object, ...) standardGeneric("mark"))


#-----------------------------------------------------------------------------------#
#Renoir
methods::setGeneric(name = "get_evaluation", def = function(object) standardGeneric("get_evaluation"))
methods::setGeneric(name = "get_marks",      def = function(object) standardGeneric("get_marks"))
methods::setGeneric(name = "get_call",       def = function(object) standardGeneric("get_call"))
methods::setGeneric(name = "get_nbest",      def = function(object) standardGeneric("get_nbest"))
methods::setGeneric(name = "create_report",  def = function(object, ...) standardGeneric("create_report"))
methods::setGeneric(name = "get_scoring",    def = function(object) standardGeneric("get_scoring"))

#-----------------------------------------------------------------------------------#
# methods::setGeneric(name = "renoir",    function(filter, learner, evaluator, ...) standardGeneric("renoir"))

#'@name stability
#'@rdname stability
methods::setGeneric(name = "stability",    function(object, ...) standardGeneric("stability"))

#'@name importance
#'@rdname importance
methods::setGeneric(name = "importance",   function(object, ...) standardGeneric("importance"))

#'@name summary_table
#'@rdname summary_table
methods::setGeneric(name = "summary_table",   function(object, ...) standardGeneric("summary_table"))

#-----------------------------------------------------------------------------------#

methods::setGeneric(name = "get_resp_type",  function(object) standardGeneric("get_resp_type"))
methods::setGeneric(name = "get_cutoff",     function(object) standardGeneric("get_cutoff"))
methods::setGeneric(name = "get_maxvars",    function(object) standardGeneric("get_maxvars"))
methods::setGeneric(name = "get_default",    function(object) standardGeneric("get_default"))
methods::setGeneric(name = "get_ebayes",     function(object) standardGeneric("get_ebayes"))
methods::setGeneric(name = "get_permutation",function(object) standardGeneric("get_permutation"))
methods::setGeneric(name = "get_args",       function(object, ...) standardGeneric("get_args"))
methods::setGeneric(name = "get_coef",       function(object) standardGeneric("get_coef"))

#'@name signature
#'@rdname signature
methods::setGeneric(name = "signature",       function(object, index, n, cutoff, measure, set, ...) standardGeneric("signature"))






methods::setGeneric(name = "get_hyperparameters",   function(object) standardGeneric("get_hyperparameters"))



methods::setGeneric(name = "get_n",                 function(object) standardGeneric("get_n"))
methods::setGeneric(name = "get_balance",           function(object) standardGeneric("get_balance"))
methods::setGeneric(name = "get_x",       def = function(object) standardGeneric("get_x"))
methods::setGeneric(name = "get_y",       def = function(object) standardGeneric("get_y"))
methods::setGeneric(name = "get_weights", def = function(object) standardGeneric("get_weights"))
methods::setGeneric(name = "get_offset",  def = function(object) standardGeneric("get_offset"))
methods::setGeneric(name = "get_fun",     def = function(object) standardGeneric("get_fun"))

# methods::setGeneric(name = "subset_observations", def = function(object, x, which) standardGeneric("subset_observations"))
methods::setGeneric(name = "subset_observations", def = function(object, which) standardGeneric("subset_observations"))
methods::setGeneric(name = "subset_features",     def = function(object, x, which) standardGeneric("subset_features"))

methods::setGeneric(name = "nobs",     def = function(object) standardGeneric("nobs"))


methods::setGeneric(name = "set_x",          def = function(object, value) standardGeneric("set_x"))
methods::setGeneric(name = "set_y",          def = function(object, value) standardGeneric("set_y"))
methods::setGeneric(name = "set_fit",        def = function(object, value) standardGeneric("set_fit"))
methods::setGeneric(name = "set_weights",    def = function(object, value) standardGeneric("set_weights"))
methods::setGeneric(name = "set_offset",     def = function(object, value) standardGeneric("set_offset"))
methods::setGeneric(name = "set_ebayes",     def = function(object, value) standardGeneric("set_ebayes"))
methods::setGeneric(name = "set_config",     def = function(object, value) standardGeneric("set_config"))
methods::setGeneric(name = "set_maxvars",    def = function(object, value) standardGeneric("set_maxvars"))
# methods::setGeneric(name = "set_hyperparameters", def = function(object, value) standardGeneric("set_hyperparameters"))
methods::setGeneric(name = "set_parameters", def = function(object, value) standardGeneric("set_parameters"))











# #-----------------------------------------------------------------------------------#
# #Filter
# methods::setGeneric(name = "get_cutoff_na",          function(object) standardGeneric("get_cutoff_na"))
# methods::setGeneric(name = "get_cutoff_int",         function(object) standardGeneric("get_cutoff_int"))
# methods::setGeneric(name = "get_cutoff_var",         function(object) standardGeneric("get_cutoff_var"))
# methods::setGeneric(name = "get_na_omit",            function(object) standardGeneric("get_na_omit"))
# methods::setGeneric(name = "get_var_measure",        function(object) standardGeneric("get_var_measure"))
#
# #-----------------------------------------------------------------------------------#
#Filtered
methods::setGeneric(name = "get_filtered",           function(object) standardGeneric("get_filtered"))
methods::setGeneric(name = "get_index",              function(object) standardGeneric("get_index"))
methods::setGeneric(name = "get_summary",            function(object,...) standardGeneric("get_summary"))

#-----------------------------------------------------------------------------------#
#Sampler
methods::setGeneric(name = "get_N",       def = function(object) standardGeneric("get_N"))
methods::setGeneric(name = "get_n",       def = function(object) standardGeneric("get_n"))
methods::setGeneric(name = "get_k",       def = function(object) standardGeneric("get_k"))
methods::setGeneric(name = "get_strata",  def = function(object) standardGeneric("get_strata"))
methods::setGeneric(name = "get_balance", def = function(object) standardGeneric("get_balance"))

methods::setGeneric(name = "get_train_set_size", def = function(object) standardGeneric("get_train_set_size"))

methods::setGeneric(name = "get_grid",    def = function(object, ...) standardGeneric("get_grid"))

methods::setGeneric(name = "set_size", def = function(object, value) standardGeneric("set_size"))

methods::setGeneric(name = "set_N",       def = function(object, value) standardGeneric("set_N"))
methods::setGeneric(name = "set_n",       def = function(object, value) standardGeneric("set_n"))
methods::setGeneric(name = "set_k",       def = function(object, value) standardGeneric("set_k"))
methods::setGeneric(name = "set_strata",  def = function(object, value) standardGeneric("set_strata"))

#-----------------------------------------------------------------------------------#
#Resampler
methods::setGeneric(name = "get_sampler", def = function(object) standardGeneric("get_sampler"))
methods::setGeneric(name = "get_looper",  def = function(object) standardGeneric("get_looper"))
methods::setGeneric(name = "set_sampler", def = function(object, value) standardGeneric("set_sampler"))

#-----------------------------------------------------------------------------------#
#Looper
methods::setGeneric(name = "get_cores",   def = function(object) standardGeneric("get_cores"))



#-----------------------------------------------------------------------------------#
#Marker
methods::setGeneric(name = "get_marking_system",       def = function(object) standardGeneric("get_marking_system"))


#-----------------------------------------------------------------------------------#
#TunedAndTested
methods::setGeneric("get_tested", function(object) standardGeneric("get_tested"))

#-----------------------------------------------------------------------------------#
#TunedAndTestedList
methods::setGeneric(name = "which_best_model", def = function(object, set, config, type.measure, tolerance, ...) standardGeneric("which_best_model"))
methods::setGeneric(name = "get_frequency_of_recruitment", def = function(object, config, ...) standardGeneric("get_frequency_of_recruitment"))

#-----------------------------------------------------------------------------------#
#Learned
methods::setGeneric(name = "get_data", def = function(object) standardGeneric("get_data"))
methods::setGeneric(name = "set_data", def = function(object, value) standardGeneric("set_data"))

#-----------------------------------------------------------------------------------#
#Marked
methods::setGeneric(name = "get_mark", def = function(object) standardGeneric("get_mark"))
methods::setGeneric(name = "get_set",  def = function(object) standardGeneric("get_set"))

#-----------------------------------------------------------------------------------#
#MarkedList
methods::setGeneric(name = "get_marked", def = function(object, marking.system, config, set, type.measure,  ...) standardGeneric("get_marked"))

#-----------------------------------------------------------------------------------#
#Renoir
methods::setGeneric(name = "set_learned",   def = function(object, value) standardGeneric("set_learned"))
methods::setGeneric(name = "set_filtered",  def = function(object, value) standardGeneric("set_filtered"))
methods::setGeneric(name = "set_scored",    def = function(object, value) standardGeneric("set_scored"))
methods::setGeneric(name = "set_marked",   def = function(object, value) standardGeneric("set_marked"))

#-----------------------------------------------------------------------------------#


methods::setGeneric(name = "set_tuned",   def = function(object, value) standardGeneric("set_tuned"))



# methods::setGeneric(name = "subset_observations", function(object, which) standardGeneric("subset_observations"))
# methods::setGeneric(name = "subset_features",     function(object, which) standardGeneric("subset_features"))



methods::setGeneric(name = "clean",    function(object, ...) standardGeneric("clean"))
# methods::setGeneric(name = "tune",     function(object, ...) standardGeneric("tune"))
# methods::setGeneric(name = "score",    function(object, ...) standardGeneric("score"))



# methods::setGeneric(name = "assess",   function(object, ...) standardGeneric("assess"))
# methods::setGeneric(name = "filter",   function(object, ...) standardGeneric("filter"))
# methods::setGeneric(name = "sample",   function(object, ...) standardGeneric("sample"))

#'@name resample
#'@rdname resample
methods::setGeneric(name = "resample", function(object, ...) standardGeneric("resample"))

methods::setGeneric(name = "loop", function(looper, ...) standardGeneric("loop"))
# methods::setGeneric(name = "learn",    function(object, train.size, ...) standardGeneric("learn"))
# methods::setGeneric(name = "learn",    function(object, ...) standardGeneric("learn"))

# methods::setGeneric(name = "mark",    function(object, ...) standardGeneric("mark"))
# methods::setGeneric(name = "mark",    function(object, learned, set, config, type.measure, ...) standardGeneric("mark"))

# methods::setGeneric(name = "renoir",    function(filter, looper, learner, marker, ...) standardGeneric("renoir"))
