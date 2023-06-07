# make model evaluation into a function to reuse code
eval_model_metrics <- function(mod, df){
  
  # add predictions to the data frames
  df <- df |> tidyr::drop_na()
  df$fitted <- predict(mod, newdata = df)
  
  # get metrics tables
  metrics <- df |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  # extract values from metrics tables
  rsq <- metrics |> 
    dplyr::filter(.metric == "rsq") |> 
    dplyr::pull(.estimate)
  rmse <- metrics |> 
    dplyr::filter(.metric == "rmse") |> 
    dplyr::pull(.estimate)
  mae <- metrics |> 
    dplyr::filter(.metric == "mae") |> 
    dplyr::pull(.estimate)
  
  df_metrics <- dplyr::tibble(R2 = rsq, RMSE = rmse, MAE = mae)
  return(df_metrics)
}
