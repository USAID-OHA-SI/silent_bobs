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
      pattern = "Site_IM_.*Zambia")
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "62d443fd"
    
  # Indicator groups
  
  # QUESTION: HTS_RECENT (N OR D?) 
  tst <- c("HTS_TST", "HTS_TST_POS", "HTS_SELF", "HTS_RECENT", "HTS_INDEX_POS")
  
  
  # QUESTION: PMTCT_STAT (N OR D?) 
  pmtct <- c("PMTCT_STAT", "PMTCT_STAT_POS", "PMTCT_EID", "PMTCT_HEI_POS")
  
  # Question
  tx <- c("TX_NEW", "TX_CURR", "TX_ML", "TX_IIT", "TX_PVLS_D", "TX_PVLS_N", "PMTCT_ART")
  
  
  prev <- c("PrEP_NEW", "TB_PREV_D", "TB_PREV_N", "CXCA_SCRN", "OVC_SERV")

# LOAD DATA ============================================================================  

  df_msd <- read_msd(file_path) %>% filter(fiscal_year == 2022, funding_agency == "USAID")
  
  disag_tst <- c("Age/Sex/Result", "Age/Sex/HIVIndication", "Age/Sex/HIVSelfTest", "Modality/Age/Sex/Result")
  
  disag_pmtct <- c("Age/Sex/KnownNewResult", "Age/Sex")
  
  disag_tx <- c("Age/Sex/HIVStatus", "Age/Sex/ARTNoContactReason/HIVStatus", "Age/Sex/Indication/HIVStatus")
  
  df_tst <- df_msd %>% filter(indicator %in% tst, standardizeddisaggregate %in% disag_tst) 
  

  df_tx <- df_msd %>% filter(indicator %in% tx, standardizeddisaggregate %in% disag_tx)
  
  

# MUNGE ============================================================================
  
  #  
  
# VIZ ============================================================================

  #  

# SPINDOWN ============================================================================

