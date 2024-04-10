create_base_population = function(metadata_dhs_PR, p_surveyid){

  # p_surveyid = "ZM2018DHS"

  metadata_survey = metadata_dhs_PR  %>%  filter(SurveyId == p_surveyid)
  dhs_data_raw = readRDS(file = paste0(path_PR_data, metadata_survey$file_name))

  # Select variables of interest
  base_pop = dhs_data_raw %>%
    select(hhid, # household id
           weight = hv005, PSU = hv021, strata = hv022,
           cluster_num = hv001,
           hh_number = hv002,
           line_number = hvidx, # line number in the household
           sex = hv104, age = hv105, relationship_hh_head = hv101, educ_level3 = hv106, educ_years = hv108,
           mother_alive = hv111, # base: all children below 18
           mother_line_number = hv112, # base: all children below 18. Coded 00 if mother not a member of the hh
           elec = hv206,
           urban = hv025,
           hv226 ) %>%
    # This is a way to create a unique identifier for the household that has no space inside
    mutate(hh_id = paste0("c",cluster_num, "h", hh_number )) %>%
#    mutate(hh_id = str_replace_all(hhid," ", "")) %>%  # transform the hhid into numerical
 #   mutate(hh_id = as.numeric(hh_id)) %>%
    # add type of cooking fuel
    {if("hv226" %in% names(.))   add_cooking_fuel_type(., coal = "traditional", p_recode = "HR") else .}  %>%
    mutate(mcf =
             ifelse(cooking_fuel_type == "modern", 1,
                    ifelse(is.na(cooking_fuel_type), NA, 0))) %>%
    # remove those whose age is 98 because it means "i don't know"
    filter(!(age == 98)) %>%
    # create age group variable
    mutate(age_group = case_when(age <5 ~ 1, # "0--4",
                                 age < 10 ~2, # "5--9",
                                 age < 15 ~ 3, # "10--14",
                                 age < 20 ~ 4, #"15--19",
                                 age < 25 ~ 5, # "20--24",
                                 age < 30 ~ 6, #"25--29",
                                 age < 35 ~ 7, #"30--34",
                                 age < 40 ~ 8, # "35--39",
                                 age < 45 ~ 9, #"40--44",
                                 age < 50 ~ 10, # "45--49",
                                 age < 55 ~ 11, #"50--54",
                                 age < 60 ~ 12, # "55--59",
                                 age < 65 ~ 13, #"60--64",
                                 age < 70 ~ 14, #"65--69",
                                 age < 75 ~ 15, #"70--74",
                                 age < 80 ~ 16, #"75--79",
                                 age < 85 ~ 17, #"80--84",
                                 age < 90 ~ 18, # "85--89",
                                 age < 95 ~ 19, #"90--94",
                                 age > 94 ~ 20)) %>%  # 95+
                                # age  100 ~ 20, #"95--99",
                                # age > 99 ~ 21 )) %>% # "100+"
    # make education levels. Levels of education in Zambia:
    # 7 complete primary, 9 complete lower sec, 12 complete higher sec
    # according to https://www.scholaro.com/pro/Countries/Zambia/Education-System
    mutate(educ_level = case_when(educ_years == 0 ~ 1, #"e1",
                                  educ_years > 0 & educ_years < 7 ~ 2, #"e2",
                                  educ_years == 7 ~ 3, #"e3",
                                  educ_years >7 & educ_years < 10 ~ 4, #"e4",
                                  educ_years > 9 & educ_years < 13 ~ 5, #"e5",
                                  educ_years > 12 ~ 6)) %>%  #"e6"
    mutate(sex = as.numeric(as.character(sex)), # Make sex into numeric variable
           indiv_id = seq(1,nrow(.),1), # Make a unique identifier
           alive = 1,  # all individual start alive
           year_birth = NA,
           urban = ifelse(urban == 1, 1, 0)) %>%
    mutate(across(any_of(c("elec","age_grop","mcf", "year")), as.factor)) %>%
    # remove the education level of kids below 15 year old
    mutate(educ_level = ifelse(age < 15, NA, educ_level)) %>%
    mutate(weight = weight/1e6#,
           #indiv_id = paste0(hhid, line_number) # TODO later: add an unique identifier for the individual
           )

  # Get the education of mother for children below 18
  # TODO later: associate the unique ID of the mom to the child
  mums = base_pop %>%
    group_by(hh_id) %>%
    select(hh_id,age,  line_number, educ_level, mother_line_number) %>%
    left_join(select(., mother_educ_level = educ_level, mother_line_number = line_number),
              by = c("hh_id","mother_line_number" )) %>%
    # mark mothers who have a child in the same household
    mutate(mother_of_child_in_hh = ifelse(line_number %in% mother_line_number, T, F)) %>%
    ungroup() %>%
    # distinguish between NA value due to actual missing value or due to the fact that data is not reported for adults over 18
    mutate(mother_educ_level = ifelse(age > 17 , 9999, mother_educ_level))

  base_pop = base_pop %>%
    left_join(mums %>% select(hh_id, line_number, mother_educ_level, mother_of_child_in_hh), by = c("hh_id", "line_number")) %>%
    # a column that marks people under 18 that have not a mom in the HH OR
    mutate(under18_without_mother_in_hh_not_mother = ifelse(is.na(mother_educ_level) & mother_of_child_in_hh == F, T, F)) %>%
  # remove those
  filter(under18_without_mother_in_hh_not_mother == F) %>%
    filter( !is.na(modern_cooking_fuel)) %>%
    mutate(mother_educ_level = ifelse(mother_educ_level == 9999, NA, mother_educ_level))

  # look how many missing data are on mums education
  #test = base_pop %>% filter(age < 18) %>% select(hhid, age,age_group, educ_level, mother_educ_level)

  # Re-weight the sample so that the weights sum to the sample size
  sum_weight_original = sum(base_pop$weight)
  base_pop = base_pop %>%
    mutate(weight = weight * nrow(.) / sum_weight_original)

  # Remove some unnecessary (for the moment) columns in the base pop dataframe
  base_pop = base_pop %>%
    select( -relationship_hh_head,-educ_years,  -educ_level3)

  base_pop



}
