blank_fpds <- function(){
  tibble(
    piid3 = "",
    idv_piid = "",
    reason_for_modification = "",
    obligated_amount = "",
    total_obligated_amount = "",
    previous_base_and_exercised_options_value = "",
    base_and_exercised_options_value = "",
    total_base_and_exercised_options_value = "",
    previous_ultimate_contract_value = "",
    ultimate_contract_value = "",
    base_and_all_options_value = "",
    total_ultimate_contract_value = "",
    fees_paid_for_use_of_service = "",
    total_estimated_order_value = ""
  )
}

read_page <- function(url) {
  page_data <- read_html(url)
  
  piid3 <- page_data %>%                                      #BOTH
    html_node("#PIID") %>%
    html_attr("value") 
  
  idv_piid <- page_data %>%                                   #BOTH
    html_node("#idvPIID") %>%
    html_attr("value") 
  
  reason_for_modification <- page_data %>%                    #BOTH
    html_node("#reasonForModification") %>%
    html_attr("value") 
  
  #Obligated Amount
  
  previous_obligated_amount <- page_data %>%                  #BOTH
    html_node("#previousObligatedAmount") %>%
    html_attr("value") 
  
  obligated_amount <- page_data %>%                           #BOTH
    html_node("#obligatedAmount") %>%
    html_attr("value") 
  
  total_obligated_amount <- page_data %>%                     #BOTH
    html_node("#totalObligatedAmount") %>%
    html_attr("value") 
  
  #Base And Exercised Options Value
  
  previous_base_and_exercised_options_value <- page_data %>%  #AWARD
    html_node("#previousBaseAndExercisedOptionsValue") %>%
    html_attr("value") 
  
  base_and_exercised_options_value <- page_data %>%           #AWARD
    html_node("#baseAndExercisedOptionsValue") %>%
    html_attr("value") 
  
  total_base_and_exercised_options_value <- page_data %>%     #AWARD
    html_node("#totalBaseAndExercisedOptionsValue") %>%
    html_attr("value") 
  
  #Ultimate Contract Value
  
  previous_ultimate_contract_value <- page_data %>%           #BOTH
    html_node("#previousUltimateContractValue") %>%
    html_attr("value") 
  
  ultimate_contract_value <- page_data %>%                    #AWARD
    html_node("#ultimateContractValue") %>%
    html_attr("value") 
  # Noticed no difference in ^&v beyond name, and not sure why all aren't like IDV
  base_and_all_options_value <- page_data %>%                 #IDV
    html_node("#baseAndAllOptionsValue") %>%
    html_attr("value") 
  
  total_ultimate_contract_value <- page_data %>%              #BOTH
    html_node("#totalUltimateContractValue") %>%
    html_attr("value") 
  
  #Fees/Total Order Value (End Singular Items)
  
  fees_paid_for_use_of_service <- page_data %>%               #AWARD
    html_node("#feesPaidForUseOfService") %>%
    html_attr("value") 
  
  total_estimated_order_value <- page_data %>%                #IDV
    html_node("#totalEstimatedOrderValue") %>%
    html_attr("value") 
  
  tibble(
    piid3,
    idv_piid,
    reason_for_modification,
    obligated_amount,
    total_obligated_amount,
    previous_base_and_exercised_options_value,
    base_and_exercised_options_value,
    total_base_and_exercised_options_value,
    previous_ultimate_contract_value,
    ultimate_contract_value,
    base_and_all_options_value,
    total_ultimate_contract_value,
    fees_paid_for_use_of_service,
    total_estimated_order_value
  )
}

scrape_fpds <- function(contracts) {
  for (link in contracts$fpds_link){
    results <- read_page(link) # Get First Row
    break
  }
  
  for (link in contracts$fpds_link){
    fpds <- read_page(link)
    results <- bind_rows(results, fpds)
  }
  
  results <- as.data.frame(results)[-1] #Convert to a dataframe (removing first duplicate row)
  results
}
