# Function that displace fuels over time.
# now the displacement is linear from the value of 2020

displace_fuels = function(.data, round){
  # round is the round of fuel displacement. currently there are four rounds

  # vector with years at which displacement occurs (can become parameters eventually)
  v_year = c(2025, 2030, 2035, 2040)
  # vector with rate at which displacement occurs at the years above
  v_rate = c(0.25,0.50, 0.75, 1)

  .data %>%
    add_column(., firewood_GJ_to_displace = filter(., fuel == "firewood") %>%
                 select(., cap_GJ_average_2020) %>% pull(.)* v_rate[round]) %>%
    add_column(., charcoal_GJ_to_displace = filter(., fuel == "charcoal") %>%
                 select(., cap_GJ_average_2020) %>% pull(.)* v_rate[round]) %>%
    add_column(., coal_GJ_to_displace = filter(., fuel == "coal") %>%
                 select(., cap_GJ_average_2020) %>% pull(.)* v_rate[round]) %>%
    add_column(., paraffin_GJ_to_displace = filter(., fuel == "paraffin") %>%
                 select(., cap_GJ_average_2020) %>% pull(.)* v_rate[round]) %>%
    add_column(., kerosene_GJ_to_displace = filter(., fuel == "kerosene") %>%
                 select(., cap_GJ_average_2020) %>% pull(.)* v_rate[round])  %>%
    # calculate energy use per capita in the year considering fuel displacement
    mutate(name_tmp = case_when(fuel == "firewood" ~ cap_GJ_average_2020*(1- v_rate[round]),
                                fuel == "gas" ~ cap_GJ_average_2020,
                                fuel == "coal" ~ cap_GJ_average_2020*(1- v_rate[round]),
                                fuel == "charcoal" ~ cap_GJ_average_2020*(1- v_rate[round]),
                                fuel == "diesel_home" ~ cap_GJ_average_2020,
                                fuel == "electricity" ~
                                  cap_GJ_average_2020 +
                                  firewood_GJ_to_displace * r_eff_firewood +
                                  charcoal_GJ_to_displace * r_eff_charcoal +
                                  coal_GJ_to_displace * r_eff_coal +
                                  paraffin_GJ_to_displace * r_eff_paraffin +
                                  kerosene_GJ_to_displace * r_eff_kerosene,
                                fuel == "paraffin" ~ cap_GJ_average_2020*(1- v_rate[round]),
                                fuel == "kerosene" ~ cap_GJ_average_2020*(1- v_rate[round]))) %>%
    select(-contains("to_displace")) %>%
    {if (round == 1) rename(.,cap_GJ_average_2025 = name_tmp) else .} %>%
    {if (round == 2) rename(.,cap_GJ_average_2030 = name_tmp) else .} %>%
    {if (round == 3) rename(.,cap_GJ_average_2035 = name_tmp) else .} %>%
    {if (round == 4) rename(.,cap_GJ_average_2040 = name_tmp) else .}
  # rename(!!name_column_current = name_tmp)

}
