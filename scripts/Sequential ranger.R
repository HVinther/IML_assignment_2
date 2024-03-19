dir("utils", full.names = T) |>
  sort(decreasing = T) |>
  lapply(source)

train_new <- train[,-c(3,4)]
train_new$ClaimInd<-as.factor(train_new$ClaimInd)

library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(R6)



Sequential_ranger <- R6Class(
  "Sequential_ranger",
  inherit = LearnerRegr,
  
  public = list(
    classif_model = NULL,
    regr_model = NULL,
    initialize = function() {
      super$initialize(
        id = "regr.custom.claim",
        feature_types = c("integer", "numeric", "factor", "ordered"),
        predict_types = c("response"),
        packages = c("ranger", "mlr3pipelines")
      )
    },
    
    importance = function() {
      if (is.null(self$model)) {
        stop("No model stored")
      }
      fn = self$model$features
      named_vector(fn, 0)
    }
  ),
  
  private = list(
    .train = function(task) {
      
      #Classification
      classif_data<-task$data() #Custom data with censored ClaimAmount
      classif_data$ClaimAmount<-NULL
      task_classif <- as_task_classif(classif_data,target = "ClaimInd")
      classif_rf <- lrn("classif.ranger", predict_type = "prob")
      print("Training classifier")
      classif_rf$train(task_classif)
      self$classif_model <- classif_rf
      
      #Regression
      #Restrict to ClaimInd==1
      regr_data<-task$data()[which(task$data()$ClaimInd==1),]
      regr_data$ClaimInd<-NULL
      
      #Make regression
      task_regr <- as_task_regr(regr_data,target="ClaimAmount")
      regr_rf <- po("encodeimpact") %>>% po("scale") %>>% lrn("regr.ranger") |>as_learner()
      print("Training regression")
      regr_rf$train(task_regr)
      self$regr_model<- regr_rf
    },
    
    .predict = function(task){
      classif_data<-task$data()
      classif_data$ClaimAmount<-NULL
      task_classif <- as_task_classif(classif_data, target = "ClaimInd")
      # Extract classification probabilities
      classif_predict <- self$classif_model$predict(task_classif)
      prob <- classif_predict$prob[,2]
      
      
      regr_data<- task$data()
      regr_data$ClaimInd<-NULL
      task_regr<-as_task_regr(regr_data, target="ClaimAmount")
      regr_predict <- self$regr_model$predict(task_regr)
      response <- regr_predict$response*prob
      
      return_object<-PredictionRegr$new(task=task,response=response)
      return(return_object)
    }
  )
)

task_regr <- TaskRegr$new(id = "claim_prediction", backend = train_new, target = "ClaimAmount")

mlr3::mlr_learners$add("sequential.ranger", Sequential_ranger)
seq_ranger<-lrn("sequential.ranger")

seq_ranger$train(task_regr)
seq_ranger$predict(task_regr)

seq_ranger_explainer = DALEXtra::explain_mlr3(seq_ranger,
                                        data=train_new[,-17],
                                        y=train_new[,17])

plot(predict_parts(seq_ranger_explainer,train_new_sub[374,-17]))


predictor <- Predictor$new(seq_ranger_explainer,data=train[,-17],y=train[,17])

importance<- FeatureImp$new(predictor,loss="mse",n.repetitions=10)











