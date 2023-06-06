# make model evaluation into a function to reuse code
eval_knn <- function(k, df_train, df_test){
  pp <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, 
                        data = df_train) |> 
    recipes::step_BoxCox(recipes::all_predictors(), -TA_F) |> # TODO: Check why we should try to boxcox transform TA_F -> it also contains negative values!
    recipes::step_center(recipes::all_numeric(), -recipes::all_outcomes()) |>
    recipes::step_scale(recipes::all_numeric(), -recipes::all_outcomes())
  
  mod <- caret::train(
    pp, 
    data = df_train, 
    method = "knn",
    trControl = caret::trainControl(method = "none"),
    tuneGrid = expand.grid(k = k),
    metric = "MAE"
  )
  
  df_test <- df_test |> 
    drop_na()
  df_test$fitted <- predict(mod, newdata = df_test)
  
  metrics_test <- df_test |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  # extract values from metrics tables
  mae_test <- metrics_test |> 
    filter(.metric == "mae") |> 
    pull(.estimate)
  
  return(mae_test)
}
