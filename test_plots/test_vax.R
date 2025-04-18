ds_vax_epic <- vax_epic  %>% 
  filter( age_level == '3-4 Years' & geography %in% c(state.name,'District of Columbia') ) %>%
  rename(state=geography) %>%
  dplyr::select(state, Outcome_value1) %>%
  mutate(ds_name='Epic')


ds_vax_nis <- vax_age %>% 
  filter(age=="35 Months" & Vaccine=='≥1 Dose MMR' & Geography %in% c(state.name,'District of Columbia') & birth_year=='2021') %>%
  dplyr::select(Geography,Outcome_value1) %>%
  rename( state=Geography)  %>%
  mutate(ds_name='NIS')


ds_vax_combo <- ds_vax_nis  %>%
  rename(vax_nis = Outcome_value1) %>%
  full_join(ds_vax_epic, by='state') %>%
  rename(vax_epic = Outcome_value1)

 p.vax.comp <-  ggplot(ds_vax_combo, aes(x = vax_nis, y = vax_epic, 
             text=paste0(state, "<br>", 
                         'Epic: ', vax_epic, "%" ,
                         'NIS: ', vax_nis, "%" )))+
    geom_point() +
    ylim(55,100)+
    xlim(55,100) +
    theme_minimal()+
    labs(title = paste0("MMR Immunization (any dose) Rates by State, "  ), x = "NIS (%)", y = 'Epic Cosmos (%)')+
   geom_abline(intercept=0, slope=1)
    
  ggplotly(p.vax.comp, tooltip='text')

  
  ds_vax_combo_long <-  bind_rows(ds_vax_nis,ds_vax_epic)
  
  p.vax.comp.long <-  ggplot(ds_vax_combo_long, aes(x = state, y = Outcome_value1, color=ds_name,
                                          text=paste0(state, "<br>", 
                                                      'Uptake: ', Outcome_value1, "%")))+
    geom_point() +
    ylim(60,100)+
    geom_hline(yintercept=90, lty=2)+
    theme_minimal()+
    labs(title = paste0("MMR Immunization (any dose) Rates by State: NIS vs Epic Cosmos"  ), x = "", y = 'Estimated vaccine uptake (%)')+
    geom_abline(intercept=0, slope=1) +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))+
    labs(color = "Source")+
    scale_color_manual(
      values = c(
        "NIS" = "#1b9e77",
        "Epic" = "#d95f02"
      ) )
  
  p.vax.comp.long
  