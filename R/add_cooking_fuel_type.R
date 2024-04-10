#' Function to add the variable cooking fuel type


add_cooking_fuel_type  <- function(dhs_data, coal, p_recode = "IR") {

  # Load the data that makes the correspondence between cooking fuel and its type.
  # This dataframe was made by collecting all the cooking fuels labels from all the surveys (automatically with
  # the function get_labels_cooking_fuel()), and then the classifying manually between modern/traditional
  #------------
  # Wood, crop residues, dung, charcoal are traditional fuels.
  # Electricity, LPG, natural gas, biogas, parafine, Kerosene are considered as modern
  # Coal is considered as traditional or modern (thats why we have two files)

  # If the variable has only NA values, return the df without the dummies
  if(p_recode == "IR" & all(is.na(dhs_data$v161))){
    return(dhs_data)
  }
  if(p_recode == "HR" & all(is.na(dhs_data$hv226))){
    return(dhs_data)
  }

  if(coal == "modern"){
    all_labels_completed <- read.csv(file = here("analysis/data/raw_data/all_labels_cooking_fuel_completed.csv"))
  }
  if(coal == "traditional"){
    all_labels_completed <- read.csv(file = here("analysis/data/raw_data/all_labels_cooking_fuel_completed_coal_as_traditional.csv"))
  }

  # In the HR recode and IR recode, the variable type of cooking fuel does not have the same name
  if(p_recode == "IR"){
    labels =  stack(attr(dhs_data$v161, 'labels'))
  }

  if(p_recode == "HR"){
    labels =  stack(attr(dhs_data$hv226, 'labels'))
  }

  labels = labels  %>%
    rename(cooking_fuel_label = ind, cooking_fuel_value = values) %>%
    # Add whether the cooking fuel is modern or traditional in the label list
    left_join(all_labels_completed, by = "cooking_fuel_label") %>% unique()

  # Add whether the cooking fuel is modern or traditional in the survey
  dhs_data_with_cooking_fuel_type = dhs_data %>%
    {if(p_recode == "IR") rename(., cooking_fuel_value = v161)
      else .} %>%
    {if(p_recode == "HR") rename(.,cooking_fuel_value = hv226)
      else .} %>%
   # rename(cooking_fuel_value = v161) %>%
    mutate(cooking_fuel_value = as.numeric(as.character(cooking_fuel_value))) %>%
    left_join(labels, by = "cooking_fuel_value") %>%
    # Add a binary variable and make it numerical
    mutate(modern_cooking_fuel = str_replace_all(.$cooking_fuel_type,
                                                 c("modern" = "1", "traditional" = "0", "other" = NA))) %>%
    mutate(modern_cooking_fuel = as.numeric(modern_cooking_fuel))

  return(dhs_data_with_cooking_fuel_type)

}

