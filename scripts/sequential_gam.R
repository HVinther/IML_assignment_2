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

# regr model --------------------------------------
## Steps to produce desired formula for reg
regr_tr_task<- 
  train_new %>%
  select(-"ClaimInd") %>%
  as_task_regr(target = "ClaimAmount")

grph_init <-
  grph_base%>>%
  lrn("regr.featureless") |>
  as_learner()

grph_init$train(task = regr_tr_task)

grph_init$model$regr.featureless$feature_names

## Create formula object from featuretypes of temporary model

form_regr<-
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

form_regr

## Definition of GAM model
grph_regr <- 
  grph_base %>>%
  lrn("regr.gam",
      formula = form_regr) |>
  as_learner()

## test af regr. learner
tn<-train_new %>%
  filter(ClaimInd == 1) %>%
  select(-ClaimInd) %>%
  as_task_regr(target = "ClaimAmount")

tna<-train_new %>%
  select(-ClaimInd) %>%
  as_task_regr(target = "ClaimAmount")

grph_regr$train(tn)
grph_regr$predict(tna)

# classif model ---------------
## Steps to produce desired formula
classif_tr_task<- 
  train_new %>%
  select(-"ClaimAmount") %>%
  as_task_classif(target = "ClaimInd")

grph_init <-
  grph_base%>>%
  lrn("classif.featureless") |>
  as_learner()

grph_init$train(task = classif_tr_task)

grph_init$model$classif.featureless$feature_names

## Create formula object from featuretypes of temporary model

form_classif<-
  grph_init$model$classif.featureless$train_task$feature_names %>%
  data.frame(term = .) %>%
  mutate(terms = 
           ifelse(term %in% names(train),
                  paste0("s(",term,")"),
                  paste0(term))) %>%
  .$terms %>%
  paste(collapse = "+") %>%
  paste0("ClaimInd ~ ",.)|>
  as.formula()

form_classif

## Definition of GAM model
grph_classif <-
  po("classbalancing") %>>%
  grph_base %>>%
  lrn("classif.gam",
      formula = form_classif,
      predict_type = "prob") |>
  as_learner()

## test af regr. learner
tn_c<-train_new %>%
  select(-ClaimAmount) %>%
  as_task_classif(target = "ClaimInd")

grph_classif$train(tn_c)
grph_classif$predict(tn_c)

## træning af learner ----------------------------------
seq_gam_lrn<-make_learner(
  classifier = grph_classif,
  regressor = grph_regr,
  name = "sequential.gam")$new()

task_regr <- TaskRegr$new(id = "claim_prediction", backend = train_new, target = "ClaimAmount")

seq_gam_lrn$train(task_regr)



seq_gam_explainer = DALEXtra::explain_mlr3(seq_gam_lrn,
                                              data=train_new[,-17],
                                              y=train_new[,17])

df <- 
  train_new %>%
  select(-c("ClaimInd","ClaimAmount"))

regr_explainer = DALEXtra::explain_mlr3(seq_gam_lrn$regr_model,
                                        data = df,
                                        y = train_new$ClaimAmount)

classif_explainer = DALEXtra::explain_mlr3(seq_gam_lrn$classif_model,
                                           data = df,
                                           y = as.integer(train_new$ClaimInd))

pdp<-list(
  "full" = plot(model_profile(seq_gam_explainer))+
    ggtitle("Partial dependence plot for full model",subtitle = ""),
  "classif" = plot(model_profile(classif_explainer))+
    ggtitle("Partial dependence plot for classifier model",subtitle = ""),
  "regr" = plot(model_profile(regr_explainer))+
    ggtitle("Partial dependence plot for regressor model",subtitle = "")
)
saveRDS(pdp,"save_files/gam_pdp.rds")
pdp
ind_of_interest<-c(1386, 12286, 2119, 2238, 27833, 27988)

test_new <- test[,-c(3,4)]
test_new$ClaimInd<-as.factor(test_new$ClaimInd)

make_shapleys<-
  function(
    learner,
    data = test %>% select(-c("RecordEnd","RecordBeg")) %>% mutate(ClaimInd = as.factor(ClaimInd)),
    ind_of_interest = c(1386, 12286, 2119, 2238, 27833, 27988)
  ){
    df <- 
      data %>%
      select(-c("ClaimInd","ClaimAmount"))
    
    df_f <- data %>%
      select(-"ClaimAmount")
    
    full_explainer = DALEXtra::explain_mlr3(learner,
                                            data=df_f,
                                            y=data$ClaimAmount)
    
    regr_explainer = DALEXtra::explain_mlr3(learner$regr_model,
                                            data = df,
                                            y = data$ClaimAmount)
    
    classif_explainer = DALEXtra::explain_mlr3(learner$classif_model,
                                               data = df,
                                               y = as.integer(data$ClaimInd))
    
    
    shapleyplots<-purrr::map(ind_of_interest,\(i){
      p_full<-plot(predict_parts(full_explainer,df_f[i,]))
      p_classif<-plot(predict_parts(classif_explainer,df[i,]))
      p_regr<-plot(predict_parts(regr_explainer,df[i,]))
      list("p_full" = p_full,
           "p_classif" = p_classif,
           "p_regr" = p_regr,
           "obs" = i)
    },
    .progress = T
    )
    
    gridded_sp<-shapleyplots %>% 
      purrr::map(\(x){
        out<- 1:3 %>%
          purrr::map(\(i){
            x[[i]]+ggtitle(substring(paste(names(x)[i],x$obs,sep = "_"),3))
          })
        names(out) <- paste(names(x)[1:3],"obs",x$obs,sep = "_")
        out
      }) %>%
      unlist(recursive = F) %>%
      gridExtra::grid.arrange(grobs = .,nrow = 6)
    
    return(list(
      "gridded_sp" = gridded_sp,
      "sp_list" = shapleyplots
    ))
  }

to_save<-make_shapleys(learner = seq_gam_lrn)
saveRDS(to_save,"save_files/gam_shapley.rds")