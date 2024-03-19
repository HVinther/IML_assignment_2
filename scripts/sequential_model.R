## Sources all the files in the utils folder
## The files names are sorted st. extra_pipeops
## comes after the libraries file

dir("utils", full.names = T) |>
  sort(decreasing = T) |>
  lapply(source)

loadData()

classif_tr_task<- train |> 
  mutate(ClaimInd = as.factor(sign(ClaimAmount))) |> 
  as_task_regr(target = "ClaimAmount")


grph_init <- 
  po_RecBeg_num %>>%
  po_RecEnd_rc %>>%
  po("mutate",
     mutation = list(
       LicAge = ~ as.numeric(LicAge),
       DrivAge = ~ as.numeric(DrivAge),
       BonusMalus = ~as.numeric(BonusMalus),
       RiskVar = ~as.numeric(RiskVar)
     )) %>>%
  lrn("regr.featureless") |>
  as_learner()

grph_init$train(task = classif_tr_task)

form<-
  grph_init$model$regr.featureless$train_task$feature_types |>
  as.data.frame() |>
  filter(id != "ClaimInd") |>
  mutate(terms = 
           ifelse(type == "numeric",
           paste0("s(",id,",by = ClaimInd)"),
           paste0(id,":ClaimInd"))) %>%
  .$terms %>%
  paste(collapse = "+") %>%
  paste0("ClaimAmount ~ ",.)|>
  as.formula()

grph <- 
  po_RecBeg_num %>>%
  po_RecEnd_rc %>>%
  po("imputelearner",
     learner = lrn("classif.ranger"),
     affect_columns = selector_name("ClaimInd"),
     param_vals = list(num.threads = 8)
     ) %>>%
  po_add_weighting %>>%
  po("mutate",
     mutation = list(
       LicAge = ~ as.numeric(LicAge),
       DrivAge = ~ as.numeric(DrivAge),
       BonusMalus = ~as.numeric(BonusMalus),
       RiskVar = ~as.numeric(RiskVar)
     )) %>>%
  lrn("regr.gam",
      formula = form,
      select = TRUE) |>
  as_learner()

grph$train(task = classif_tr_task)
grph$predict(classif_tr_task)$score(msrs(c("regr.mse","regr.mse_inter")), task = classif_tr_task)

