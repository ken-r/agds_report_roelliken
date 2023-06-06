knn <- function(recipe, df_train, k) {
  return(caret::train(recipe, 
                      data = df_train |> drop_na(), 
                      method = "knn",
                      trControl = caret::trainControl(method = "none"),
                      tuneGrid = data.frame(k = k),
                      metric = "RMSE"))
}

knn_cv <- function(recipe, df_train, k_grid, metric) {
  return (caret::train(recipe, 
               data = df_train |> drop_na(), 
               method = "knn",
               trControl = caret::trainControl(method = "cv", 
                                               number = 15),
               tuneGrid = k_grid,
               metric = metric))
}

