recipe_daily_fluxes <- function(daily_fluxes) {
  return(recipes::recipe(GPP_NT_VUT_REF ~ SW_IN_F + VPD_F + TA_F, 
                         data = daily_fluxes) |> 
           recipes::step_center(all_numeric(), -all_outcomes()) |>
           recipes::step_scale(all_numeric(), -all_outcomes()))
}