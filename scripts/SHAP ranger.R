lrn_obj = lrn("regr.ranger")


  
random_forest <- po_VehAge_num %>>%
  po_VehPrice_int %>>% po_SocCat_int %>>%
  po("encodeimpact") %>>% po("scale") %>>%
  lrn_obj |>
  as_learner()

#A priori we have problems running interpretability with the date variables. I remove these
task = as_task_regr(train[,-c(3,4)],target="ClaimAmount")
task = add_weight(task,weighting="interest")

random_forest$train(task)
saveRDS(random_forest,file="modeller/random_forest_trained")
random_forest<- readRDS("modeller/random_forest_trained")

ranger_explain = DALEXtra::explain_mlr3(random_forest,
                                        data=train[,-19],
                                        y=train[,19])

saveRDS(ranger_explain,file="shap/random_forest_explain")
ranger_explain<- readRDS("shap/random_forest_explain")

my_dataset_with_index <- train %>%
  mutate(original_index = row_number())

set.seed(1)
dat_exp1<-sample_n(filter(my_dataset_with_index,ClaimInd==1),1)#Ignore the index
dat_exp2<-sample_n(filter(my_dataset_with_index,ClaimInd==0),1)

shap_rf1<- plot(predict_parts(ranger_explain,dat_exp1[,-c(19,23)]))
shap_rf2<- plot(predict_parts(ranger_explain,dat_exp2[,-c(19,23)]))

predictor <- Predictor$new(ranger_explain,data=train[,-c(3,4,19)],y=train[,19])

future::plan(multisession,workers=2)

importance<- FeatureImp$new(predictor,loss="mse",n.repetitions=10)

#the importance of exposure is probably overestimated due to the weighting
saveRDS(importance,"random_forest_imp")
importance$plot()

plan(sequential)

#Vi gemmer lige pdp til efter jeg har lavet feature importance
random_forest_pdp <- plot(model_profile(ranger_explain,variables=c("Exposure")))
random_forest_pdp
