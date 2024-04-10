
#' Function to replace values corresponding to missing data
#' that are coded with numbers in the DHS

recode_na = function(., threshold_NA){
  . = ifelse(. <=threshold_NA, ., NA)
  .
}
