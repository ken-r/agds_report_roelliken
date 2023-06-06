# make model evaluation into a function to reuse code
eval_model_metrics <- function(mod, df){
  
  # add predictions to the data frames
  df <- df |> drop_na()
  df$fitted <- predict(mod, newdata = df)
  
  # get metrics tables
  metrics <- df |> 
    yardstick::metrics(GPP_NT_VUT_REF, fitted)
  
  # extract values from metrics tables
  rsq <- metrics |> 
    filter(.metric == "rsq") |> 
    pull(.estimate)
  rmse <- metrics |> 
    filter(.metric == "rmse") |> 
    pull(.estimate)
  mae <- metrics |> 
    filter(.metric == "mae") |> 
    pull(.estimate)
  
  df_metrics <- tibble(R2 = rsq, RMSE = rmse, MAE = mae)
  return(df_metrics)
}
