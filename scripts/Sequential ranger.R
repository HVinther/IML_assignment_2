dir("utils", full.names = T) |>
  sort(decreasing = T) |>
  lapply(source)

train_new <- train[,-c(3,4)]
train_new$ClaimInd<-as.factor(train_new$ClaimInd)

library(mlr3)
library(mlr3learners)
library(mlr3pipelines)
library(R6)


make_learner<-function(classifier=lrn("classif.ranger", predict_type = "prob"),
                       regressor=po("encodeimpact") %>>% po("scale") %>>% lrn("regr.ranger") |>as_learner(),
                       add=F,
                       name="sequential.ranger"){
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
    
    #We define the training and prediction methods
    private = list(
      .train = function(task) {
        
        #Classification
        classif_data<-task$data() #Custom data with censored ClaimAmount
        classif_data$ClaimAmount<-NULL
        task_classif <- as_task_classif(classif_data,target = "ClaimInd")
        classif_rf <- classifier
        print("Training classifier")
        classif_rf$train(task_classif)
        self$classif_model <- classif_rf
        
        #Regression
        #Restrict to ClaimInd==1
        regr_data<-task$data()[which(task$data()$ClaimInd==1),]
        regr_data$ClaimInd<-NULL
        
        #Train regressor
        task_regr <- as_task_regr(regr_data,target="ClaimAmount")
        regr_rf <- regressor
        print("Training regression")
        regr_rf$train(task_regr)
        self$regr_model<- regr_rf
      },
      
      .predict = function(task){
        #Make classification task for prediction
        classif_data<-task$data()
        classif_data$ClaimAmount<-NULL
        task_classif <- as_task_classif(classif_data, target = "ClaimInd")
        # Extract classification probabilities
        classif_predict <- self$classif_model$predict(task_classif)
        prob <- classif_predict$prob[,2]
        
        #Make regression task for prediction
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
  if(add==T){
    mlr3::mlr_learners$add(name, Sequential_ranger)
  }
  else{
    return(Sequential_ranger)
  }
}

reg_graph<-po_VehAge_num %>>%
  po_VehPrice_int %>>% po_SocCat_int %>>%
  po("encodeimpact") %>>% po("scale") %>>%
  lrn("regr.ranger") |>
  as_learner()

make_learner(regressor=reg_graph,add=T,name="sequential.ranger")


task_regr <- TaskRegr$new(id = "claim_prediction", backend = train_new, target = "ClaimAmount")

seq_ranger<-lrn("sequential.ranger")


seq_ranger$train(task_regr)
saveRDS(seq_ranger,"sequential_ranger_trained")

prediction<- seq_ranger$predict(task_regr)
saveRDS(prediction,"sequential_ranger_prediction")
plot(prediction)


seq_ranger_explainer = DALEXtra::explain_mlr3(seq_ranger,
                                        data=train_new[,-17],
                                        y=train_new[,17])

plot(predict_parts(seq_ranger_explainer,train_new[374,-17]))


predictor <- Predictor$new(seq_ranger_explainer,data=train_new[,-17],y=train_new[,17])

importance<- FeatureImp$new(predictor,loss="mse",n.repetitions=10)






