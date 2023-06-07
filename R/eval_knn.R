# make model evaluation into a function to reuse code
eval_knn <- function(k, df_train, df_test){
  df_train <- df_train |> tidyr::drop_na()
  df_test <- df_test |> tidyr::drop_na()
  pp <- recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, 
                        data = df_train) |> 
    # Dont apply boxcox for TA_F because it also contains negative values
    recipes::step_BoxCox(recipes::all_predictors(), -TA_F) |>
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
    tidyr::drop_na()
  df_test$fitted <- predict(mod, newdata = df_test)
  
  metrics_test <- df_test |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  # extract values from metrics tables
  mae_test <- metrics_test |> 
    dplyr::filter(.metric == "mae") |> 
    dplyr::pull(.estimate)
  
  return(mae_test)
}
