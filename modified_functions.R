row_col_dup = function (sel_data_kp= sel_data_kp) {
  
  row_col_ct = sel_data_kp %>%
    count(trial_name, col_number, row_number, sort=TRUE) %>%
    filter(n>1) %>%
    arrange(row_number, col_number)
  
  if (nrow(row_col_ct) >0) {
    print("ERROR: The duplicated row and column combination:")
    print(row_col_ct)
    
    row_col_ct_bind = row_col_ct %>%
      mutate(trial_row_col = paste(trial_name, col_number, row_number, sep = "_"))
    
    duplicated_plot = sel_data_kp %>%
      mutate(trial_row_col = paste(trial_name, col_number, row_number, sep = "_")) %>%
      filter(trial_row_col %in% row_col_ct_bind$trial_row_col) %>%
      select(plot_name, col_number, row_number, trial_name, plot_number) %>%
      arrange(trial_name, plot_number, row_number, col_number )
    
    print("Here are the plot names:")
    print(duplicated_plot)
    print("Please fix the ERROR!")
    return(duplicated_plot)
    
  }
  if (nrow(row_col_ct) == 0) {
    print("Good, there is no duplicated combination of row and column.")
  }
  
}



# Trial layout

trial_layout <- function(trial = sel_data_kp) {
  trial_list <- unique(trial$trial_name)
  for (i in 1:length(trial_list)) {
    trial_i <- trial %>%
      filter(trial_name %in% trial_list[i])
    myplot <- ggplot(trial_i, aes(x = factor(col_number), y = factor(row_number), fill = factor(rep_number))) +
      geom_tile(color = "black", size = 0.5) + # Black border on tiles
      labs(x = "col_number", y = "row_number", fill = "rep", title = trial_list[i]) +
      coord_fixed() + # Square tiles
      theme_minimal() + # Minimal theme, no grey background
      geom_tile(
        data = trial_i %>% filter(check_test == "check"),
        aes(fill = check_test), col = "black"
      ) +
      theme_xiaofei(
      )
    print(myplot)
    # layout <<- myplot Save layout in output folder
  }
}


# function visualize the layout - families

trial_layout_family <- function(trial = sel_data_kp) {
  trial_list <- unique(trial$trial_name)
  for (i in 1:length(trial_list)) {
    trial_i <- trial %>%
      filter(trial_name %in% trial_list[i])
    myplot <- ggplot(trial_i, aes(x = factor(col_number), y = factor(row_number))) +
      geom_tile(color = "black", size = 0.5) + # Black border on tiles
      labs(x = "col_number", y = "row_number", title = trial_list[i]) +
      coord_fixed() + # Square tiles
      theme_xiaofei() + # Minimal theme, no grey background
      geom_tile(data = trial_i %>%
        filter(check_test == "check"), fill = "black", show.legend = F) +
      geom_tile(
        data = trial_i,
        aes(fill = family_name), col = "black"
      )
    theme(
      panel.grid = element_blank(), # No underlying grid lines
      axis.text.x = element_text( # Vertical text on x axis
        angle = 0, vjust = 0.5, hjust = 0
      )
    )
    print(myplot)
    # layout <<- myplot Save layout in output folder
  }
}


R.square <- function(Model){
  
  response      <- Model$data[,Model$model$response]  
  mean.response <- mean(response,na.rm = T)
  fitted        <- Model$fitted
  SS.fitted     <- sum( (response-fitted)^2 , na.rm = T)
  SS.response   <- sum( (response-mean.response)^2 , na.rm = T )
  
  R <- 1- SS.fitted/SS.response
  
  names(R) <- "r.square"
  return(round(R,3))
}
