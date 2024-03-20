## Sources all the files in the utils folder
## The files names are sorted st. extra_pipeops
## comes after the libraries file

dir("utils", full.names = T) |>
  sort(decreasing = T) |>
  lapply(source)

loadData()

train_new <- train[,-c(3,4)]
train_new$ClaimInd<-as.factor(train_new$ClaimInd)

make_learner<-function(classifier=lrn("classif.ranger", predict_type = "prob"),
                       regressor=po("encodeimpact") %>>% po("scale") %>>% lrn("regr.ranger") |>as_learner(),
                       add=F,
                       name="sequential.ranger"){
  Sequential_ranger <- R6::R6Class(
    name,
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

## Preprocessing graph
grph_base <-
  po_SocCat_int %>>%
  po_VehPrice_int %>>%
  po_add_weighting %>>%
  po("mutate",
     mutation = list(
       HasKmLimit = ~ as.factor(HasKmLimit)
     )) %>>%
  po("encode") %>>%
  po("scale")


## Steps to produce desired formula
classif_tr_task<- 
  train_new %>%
  select(-"ClaimInd") %>%
  as_task_regr(target = "ClaimAmount")

grph_init <-
  grph_base%>>%
  lrn("regr.featureless") |>
  as_learner()

grph_init$train(task = classif_tr_task)

grph_init$model$regr.featureless$feature_names

## Create formula object from featuretypes of temporary model

form<-
  grph_init$model$regr.featureless$train_task$feature_names %>%
  data.frame(term = .) %>%
  mutate(terms = 
           ifelse(term %in% names(train),
           paste0("s(",term,")"),
           paste0(term))) %>%
  .$terms %>%
  paste(collapse = "+") %>%
  paste0("ClaimAmount ~ ",.)|>
  as.formula()

form

## Definition of GAM model
grph <- 
  grph_base %>>%
  lrn("regr.gam",
      formula = form) |>
  as_learner()

## test af regr. learner
tn<-train_new %>%
  filter(ClaimInd == 1) %>%
  select(-ClaimInd) %>%
  as_task_regr(target = "ClaimAmount")

tna<-train_new %>%
  select(-ClaimInd) %>%
  as_task_regr(target = "ClaimAmount")

grph$train(tn)
grph$predict(tna)


##
seq_gam<-make_learner(
  classifier = lrn("classif.ranger", predict_type = "prob", num.threads = 16),
  regressor = grph,
  name = "sequential.gam")

seq_gam_lrn<-seq_gam$new()

task_regr <- TaskRegr$new(id = "claim_prediction", backend = train_new, target = "ClaimAmount")

seq_gam_lrn$train(task_regr)

prediction <- seq_gam_lrn$predict(task_regr)
plot(prediction)


seq_gam_explainer = DALEXtra::explain_mlr3(seq_gam_lrn,
                                              data=train_new[,-17],
                                              y=train_new[,17])

plot(predict_parts(seq_gam_explainer,train_new[374,-17]))


predictor <- Predictor$new(seq_gam_explainer,data=train_new[,-17],y=train_new[,17])

importance<- FeatureImp$new(predictor,loss="mse",n.repetitions=10)

##
df <- 
  train_new %>%
  select(-c("ClaimInd","ClaimAmount"))

tg <- train_new$ClaimAmount

prediction_wp <- grph$predict(tna)
plot(prediction_wp)

gam_explainer_wp = DALEXtra::explain_mlr3(grph,
                                           data = df,
                                           y = tg)

plot(predict_parts(gam_explainer_wp,df[374,]))

predictor_wp <- Predictor$new(gam_explainer_wp,
                              data = df,
                              y = tg)

importance_wp<- FeatureImp$new(predictor_wp,loss="mse",n.repetitions=10)

