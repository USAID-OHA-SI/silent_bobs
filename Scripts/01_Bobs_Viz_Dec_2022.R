# PROJECT: Silent Bobs
# PURPOSE: Munge and Analysis of BOBs data in FY23
# AUTHOR: Tim Essam | SI
# REF ID:   37b2106f
# LICENSE: MIT
# DATE: 2022-12-15
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
    library(gt)
    library(selfdestructin5)
    library(ggarchery)
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    file_path <- return_latest(folderpath = merdata,
      pattern = "Site_IM_FY20-23_20221114.*_Zambia")
    
    bobs_path <- "Data/ZMB_BOB_Summary_report_to update monthly_TO OCTOBER 2022_For 12.9.22 Action Point.xlsx"
    sheet = "Oct_update_te"
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "37b2106f"
    
  # Functions  
    munge_indics <- function(df){
      df %>% 
        filter(indicator %in% c("HTS_TST_POS", "TX_CURR", "TX_NEW", "TX_PVLS"),
               standardizeddisaggregate %in% c(
                 "Age/Sex/HIVStatus",
                 "Age/Sex/Indication/HIVStatus",
                 "Modality/Age/Sex/Result")
        ) %>%
        clean_indicator() %>% 
        group_by(partner, mech_code, indicator, fiscal_year) %>% 
        summarise(across(matches("targets|cumulative"), sum, na.rm = T))
    }
  
    
# LOAD DATA ============================================================================  

  bob_oct <- read_excel(bobs_path, sheet = sheet) %>% 
      pivot_longer(cols = where(is.numeric),
                   names_to = "month",
                   values_to = "values") %>% 
    group_by(Partner, Age, Indicator) %>% 
      mutate(month_seq = row_number()) %>% 
      ungroup() %>% 
      rename_with(., tolower)
    

# MUNGE ============================================================================
  
  x_labs <- c("", "Oct-21", "", "", "Jan-22", "", "", "Apr-22", "", "", "Jul-22", "", "", "Oct-22")
  
  #  Plot TESTing results across time / age
    plot_bobs <- function(df, plot_var, export = T){
    
    df %>% 
      filter(indicator == plot_var) %>% 
      group_by(age) %>% 
      mutate(maxval = max(values, na.rm = T)) %>% 
      ungroup()  %>% 
      ggplot(aes(x = month_seq, y = values)) +
      annotate(geom = "rect", xmin = 2, xmax = 13, 
               ymin = 0, ymax = Inf, alpha = 0.25, fill = grey10k) +
      geom_area(fill = grey10k, alpha = 0.7) +
      annotate(geom = "segment", x = 2, xend = 2, 
               y = 0, yend = Inf, alpha = 0.25, linetype = "dotted") +
      annotate(geom = "segment", x = 13, xend = 13, 
               y = 0, yend = Inf, alpha = 0.25, linetype = "dotted") +
      annotate(geom = "text", x = 7.5, y = 0, label = "<-- FY22 -->",
               family = "Source Sans Pro", size = 9/.pt, color = grey50k,
               vjust = -0.25) +
      geom_blank(aes(y = maxval)) +
      geom_blank(aes(y = 0)) +
      geom_line() +
      #geom_point(size = 2, shape = 19, color = grey50k) +
      geom_point(data = . %>% filter(month_seq == min(month_seq)), size = 3, shape = 19) +
      geom_point(data = . %>% filter(month_seq == max(month_seq)), size = 3, shape = 21, fill = "white") +
      #geom_smooth(linewidth = 0.75, color = grey90k, fill = grey40k) +
      #geom_point(size = 2, shape = 19, color = scooter_med) +
      facet_wrap(age ~ partner, scales = "free_y",  labeller = label_wrap_gen(multi_line=FALSE)) +
      scale_y_continuous(labels =  label_number(scale_cut = cut_short_scale())) +
      scale_x_continuous(labels = x_labs, breaks = 1:14) +
      si_style_ygrid() +
      theme(axis.text.x = element_text(size = 8),
        axis.ticks.x = element_line(linewidth = 0.25, color = grey50k), 
            ) +
      expand_limits(x = c(0.5, 14.5)) +
      labs(x = NULL, y = NULL, 
           title = glue("{plot_var} TRENDS FY FY22"), 
           caption = "Source: FY22 October BOBs extract from E4H")
  
      
       if(export == TRUE){
         si_save(glue("Images/{plot_var}_Bobs_summary_Oct_2022_V2.png"))
       }
  
    }
plot_bobs(bob_oct, "HTS_TST_POS")
  
  
# VIZ ============================================================================

  # Loop over inicators
    indic_list <- bob_oct %>% distinct(indicator) %>% pull()
    map(indic_list, ~plot_bobs(bob_oct, .x))

# COMPARING RESULTS TO MER ============================================================================

    df_msd <- read_msd(file_path) %>% 
      filter(fiscal_year == 2022,
             mech_code %in% c("17413", "17399", "82075")) %>% 
      mutate(partner = case_when(
        mech_code == "17413" ~ "SAFE",
        mech_code == "17399" ~ "Discover",
        mech_code == "82075" ~ "ACTION HIV"
      ))
    
    
    df_comp_peds <- munge_indics(df_msd %>% filter(trendscoarse == "<15")) %>% 
      mutate(age = "<15")
    
    df_comp <- munge_indics(df_msd) %>% 
      mutate(age = "All")
    
    df_comp <- bind_rows(df_comp, df_comp_peds)
    
    bobs_compare <- bob_oct %>% 
      filter(indicator %ni% c("VLS", "VLC"),
             month_seq %in% c(2:13)) %>% 
      group_by(partner, age, indicator) %>% 
      mutate(bob_cumul = case_when(
        indicator %ni% c("TX_CURR", "TX_PVLS_N", "TX_PVLS_D") ~ sum(values, na.rm = T),
        indicator %in% c("TX_CURR", "TX_PVLS_N", "TX_PVLS_D") & month_seq == 13 ~ values
      )) %>% 
      filter(month_seq == 13) %>% mutate(indicator = case_when(
        indicator == "TX_PVLS_N" ~ "TX_PVLS",
        TRUE ~ indicator
      )) %>% 
      ungroup() %>% 
      select(-c(month_seq, month, values)) %>% 
      left_join(df_comp) %>% 
      mutate(achv_mer = cumulative / targets, 
             bobs_diff = bob_cumul - cumulative) %>% 
    select(-c(`fiscal year`, fiscal_year)) %>% 
      select(partner, mech_code, indicator, age, targets, 
             mer = cumulative, bobs = bob_cumul, bobs_diff, 
             achv_mer)

# COMBINE AND PLOT DIFFERENCES RELATIVE TO TARGETS ------------------------

    bobs_compare %>% 
    rowwise() %>% 
    mutate(min = min(mer, bobs)) %>% 
    ggplot(aes(y = partner)) +
    geom_linerange(aes(xmin = min, xmax = targets)) +
    geom_point(aes(x = mer), shape = 21, fill = scooter_med, 
               size = 4, position = position_nudge(y = 0.1)) +
    geom_point(aes(x = bobs), shape = 21, fill = old_rose, 
               size = 4, position = position_nudge(y = -0.1)) +
      facet_wrap(age ~ indicator, scale = "free_x") +
      scale_x_continuous(labels = label_number_si())
              
    
    bobs_compare %>% 
      filter(age == "All") %>% 
      arrange(indicator, partner) %>% 
      select(-age, -mech_code) %>% 
      gt(groupname_col = "indicator") %>% 
      fmt_number(columns = targets:bobs_diff, 
                 decimals = 0) %>% 
      fmt_percent(columns = last_col(),
                  decimals = 0) %>% 
      cols_label(bobs_diff = "difference", 
                 partner = "") %>% 
      opt_all_caps(locations = c("column_labels", "row_group")) %>% 
      tab_header(
        title = md("COMPARISON OF BOB TO MER RESULTS FOR FY22 -- ALL AGES")
      ) %>% 
      gtExtras::gt_hulk_col_numeric(bobs_diff, domain = c(-2600, 2600)) %>% 
    gtExtras::gtsave_extra(path = "Images", filename = "ZMB_fy22_bobs_mer_compare_all_ages.png")
    
    bobs_compare %>% 
      filter(age == "<15") %>% 
      arrange(indicator, partner) %>% 
      select(-age, -mech_code) %>% 
      gt(groupname_col = "indicator") %>% 
      fmt_number(columns = targets:bobs_diff, 
                 decimals = 0) %>% 
      fmt_percent(columns = last_col(),
                  decimals = 0) %>% 
      cols_label(bobs_diff = "difference", 
                 partner = "") %>% 
      opt_all_caps(locations = c("column_labels", "row_group")) %>% 
      tab_header(
        title = md("COMPARISON OF BOB TO MER RESULTS FOR FY22 -- <15")
      ) %>% 
      gtExtras::gt_hulk_col_numeric(bobs_diff, domain = c(-68, 68)) %>% 
      gtExtras::gtsave_extra(path = "Images", filename = "ZMB_fy22_bobs_mer_compare_peds.png")

    
