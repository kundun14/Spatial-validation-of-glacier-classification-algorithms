train_gpkg_list <- list.files(path = "./data/sampling_points_split/",
                              recursive = TRUE,
                              full.name = TRUE,
                              pattern = ".*_train\\.gpkg$")  # Only include files ending with "_train.gpkg"

train_gpkg <- lapply(train_gpkg_list, FUN = sf::read_sf)





# Create an empty data frame to store the summary results
summary_df <- data.frame(sf_name = character(),
                         variable = character(),
                         mean = numeric(),
                         std_dev = numeric(),
                         stringsAsFactors = FALSE)

# Iterate over each sf object in the list
for (i in seq_along(train_gpkg)) {
  sf_object <- train_gpkg[[i]] %>% dplyr::select(!c("CLASS","geom"))
  
  # Iterate over each variable in the sf object
  for (col_name in names(sf_object)) {

    # Calculate mean and standard deviation
    mean_val <- mean(sf_object[[col_name]], na.rm = TRUE)
    std_dev_val <- sd(sf_object[[col_name]], na.rm = TRUE)
    
    # Add the summary to the data frame
    
    summary_df <- summary_df %>%
      bind_rows(data.frame(sf_name = names(sf_object)[i],
                           variable = col_name,
                           mean = mean_val,
                           std_dev = std_dev_val,
                           stringsAsFactors = FALSE)
                )
  }
}

# Reshape the summary data frame
summary_df <- summary_df %>%
  tidyr::pivot_wider(names_from = variable,
                     values_from = c(mean, std_dev),
                     names_sep = "_")

# Print the summary data frame
print(summary_df)

