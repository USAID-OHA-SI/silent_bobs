# PROJECT: Backcasting MER proportions to BOBs data
# PURPOSE: Munge and Analysis of MER data for age/sex proportions
# AUTHOR: Tim Essam | SI
# REF ID:   62d443fd
# LICENSE: MIT
# DATE: 2023-01-23
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
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
      pattern = "Genie-PSNUByIMs-Zambia-Daily-2023-02-02")
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "62d443fd"
    
  # Indicator groups
  
  # QUESTION: HTS_RECENT (N OR D?) 
  tst <- c("HTS_TST", "HTS_TST_POS", "HTS_SELF", "HTS_RECENT", "HTS_INDEX_NEWPOS", "HTS_INDEX")
  disag_tst <- c("Age/Sex/Result", "Age/Sex/HIVIndication", 
                 "Age/Sex/HIVSelfTest", "Modality/Age/Sex/Result",
                 "4:Age/Sex/Result", "Age/Sex/HIVSelfTest", "Modality/Age/Sex/RTRI/HIVStatus")
  
  
  
  # QUESTION: PMTCT_STAT (N OR D?) 
  pmtct <- c("PMTCT_STAT", "PMTCT_STAT_POS", "PMTCT_EID", "PMTCT_HEI_POS")
  disag_pmtct <- c("Age/Sex/KnownNewResult", "Age/Sex", "Age/NewExistingArt/Sex/HIVStatus", "Age/HIVStatus")
  
  
  # Question
  tx <- c("TX_NEW", "TX_CURR", "TX_ML", "TX_PVLS", "PMTCT_ART", "TX_RTT")
  disag_tx <- c("Age/Sex/HIVStatus", "Age/Sex/ARTNoContactReason/HIVStatus", "Age/Sex/Indication/HIVStatus")
  
  prev <- c("PrEP_NEW", "TB_PREV", "TB_PREV", "CXCA_SCRN", "OVC_SERV")
  disag_prev <- c("Age/Sex", "Age/Sex/NewExistingArt/HIVStatus", 
                  "Age/Sex/HIVStatus/ScreenResult/ScreenVisitType", "Age/Sex/ProgramStatus")

# LOAD DATA ============================================================================  

  df_msd <- read_msd(file_path) %>% 
    filter(fiscal_year == 2022, funding_agency == "USAID") 
  
  df_msd %>% filter(indicator %in% c(tst, pmtct, tx, prev)) %>% 
    clean_indicator() %>% 
    filter(standardizeddisaggregate %in% 
                      c(disag_tst, disag_pmtct, disag_tx, disag_prev, "Total Numer")) %>% 
    count(indicator, ageasentered, sex) %>% 
    spread(ageasentered, n) %>% prinf()
    write_csv("Dataout/bobs_retrofitting_indicators.csv", na = "")
  

# MUNGE ============================================================================
  
  # Pull ACTION HIVs #s to start
    df_action <- 
      df_msd %>% 
      filter(indicator %in% c(tst, pmtct, tx, prev),
             mech_code == 82075) %>% 
      clean_indicator() %>% 
      filter(standardizeddisaggregate %in% 
               c(disag_tst, disag_pmtct, disag_tx, disag_prev, "Total Numer")) %>% 
      group_by(indicator, ageasentered, sex, mech_name, mech_code, standardizeddisaggregate) %>% 
      summarise(across(matches("qtr"), sum, na.rm = T), .groups = "drop") %>% 
      group_by(sex, mech_name, mech_code, indicator, standardizeddisaggregate) %>% 
      mutate(across(matches("qtr"), ~ .x / sum(.x), na.rm = T, .names = "{.col}_share")) %>% 
      mutate(across(c(qtr1:qtr4), sum, na.rm = T, .names = "{.col}_total")) %>% 
      ungroup() %>% 
      mutate(age_sex = str_c(ageasentered, sex, sep = ", ")) %>% 
    write_csv("Dataout/ZMB_82075_Action_sample_MER_proportions.csv")
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

