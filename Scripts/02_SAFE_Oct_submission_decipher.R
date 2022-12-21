# PROJECT: Silent Bobs
# PURPOSE: Munge and Analysis of SAFE October Submision
# AUTHOR: Tim Essam | SI
# REF ID:   9e0e6b17
# LICENSE: MIT
# DATE: 2022-12-21
# NOTES: Tim Essam | SI

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(extrafont)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(glue)
    library(readxl)
    
    
  # SI specific paths/functions  
    load_secrets()
      
  # Grab metadata
   get_metadata()
  
  # REF ID for plots
    ref_id <- "9e0e6b17"
    
  # Functions  
  # catoptcombo crosswalk
  ccombo <-   tibble::tribble(
       ~catoptcombo,   ~description,
      "C6YiLLhSpAk", "15-19 Female",
      "fhZExDJJHmU",       "10-14",
      "Jtks7hM1p25",   "15-19 Male",
      "N6i16vWwGz5",        "15-19",
      "o49KnWr94Ix",           "<5",
      "UX7GjJfSkTX", "10-14 Female",
      "V38TSkGda64",   "10-14 Male",
      "zAZQkzhJOMp",         "5-9"
      )

  

# LOAD DATA ============================================================================  

  bob_path <- "Data/SAFE Peds BOB Generator PEDs-Oct 22_FY 23.xlsx"   
  excel_sheets(bob_path)
  safe_bobs <- read_excel(bob_path, sheet = "ETL_Import File") %>% 
    pivot_longer(cols = where(is.numeric),
                 names_to = "indicator",
                 values_to = "values") 
  
  
  safe_bobs_transit <- read_excel(bob_path, sheet = "Transport") %>% 
    pivot_longer(cols = where(is.numeric),
                 names_to = "indicator",
                 values_to = "values") %>% 
    group_by(indicator, period) %>% 
    summarise(total = sum(values, na.rm = T)) %>% 
    ungroup() %>% 
    separate(indicator, into = c("indicator", "disagg"), sep = "\\+") %>% 
    left_join(., ccombo, by = c("disagg" = "catoptcombo"))

# MUNGE ============================================================================
  
  safe_bobs %>% 
    left_join(., ccombo) %>% 
    group_by(dataelement, description) %>% 
    summarise(totals = sum(values, na.rm = T)) %>% prinf()
  
  
  safe_bobs %>%    
    left_join(., ccombo) %>% 
    filter(str_detect(dataelement, c("HTS_TST_POS|TX_CURR|TX_NEW|TX_PVLS")),
          str_detect(description, "15", negate = T)) %>% 
    mutate(dataelement = str_remove_all(dataelement, "Pediatric ")) %>% 
    mutate(indicator = case_when(
      str_detect(dataelement, "HTS_TST") ~ "HTS_TST_POS",
      str_detect(dataelement, "TX_CU") ~ 'TX_CURR',
      str_detect(dataelement, "TX_N") ~ "TX_NEW",
      str_detect(dataelement, "TX_PVLS \\(D") ~ "TX_PVLS_D",
      str_detect(dataelement, "TX_PVLS \\(N") ~ "TX_PVLS_N"
    )) %>% 
    group_by(indicator) %>% 
    summarise(totals = sum(values, na.rm = T))

  
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

