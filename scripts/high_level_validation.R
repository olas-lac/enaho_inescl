

#### Validation 1: Sums of variables that should sum to 100 ####
    data_genw <- data[data$indicator%in%c("aguafred_ch","aguafpublico_ch","aguafembotellada_ch","aguafpozoprot_ch","aguaflluvia_ch","aguafcamion_ch","aguafotramej_ch","aguafsuperficial_ch","aguafotranm_ch","aguafdesconocido_ch")& data$area=="Total" & data$migration=="Total"& data$disability =="Total" & data$quintile =="Total"& data$ethnicity=="Total",]
    genw<-data_genw%>%
      group_by(isoalpha3, year)%>%
      dplyr::summarize(water_general= sum(value, na.rm=T))
    View(pivot_wider(genw, values_from = water_general, names_from = year))

    data_genwan <- data[data$indicator%in%c("aguafmejorada_ch","aguafdesconocido_ch","aguafnomejorada_ch")& data$area=="Total" & data$migration=="Total"& data$disability =="Total" & data$quintile =="Total"& data$ethnicity=="Total",]
    genwan<-data_genwan%>%
      group_by(isoalpha3, year)%>%
      dplyr::summarize(water_general= sum(value, na.rm=T))
    genwan_val<-pivot_wider(genwan, values_from = water_general, names_from = year)
    write.csv(genwan_val,"results/validation/general water use validation - high-level.csv")
    
    data_conw <- data[data$indicator%in%c("aguafconnulo_ch","aguafredcon_ch","aguafpublicocon_ch","aguafembotcon_ch","aguafpozoprotcon_ch","aguaflluviacon_ch","aguafcamioncon_ch","aguafotramejcon_ch","aguafsupercon_ch","aguafotranmcon_ch","aguafdesconcon_ch")& data$area=="Total" & data$migration=="Total"& data$disability =="Total" & data$quintile =="Total"& data$ethnicity=="Total",]
    conw<-data_conw%>%
      group_by(isoalpha3, year)%>%
      dplyr::summarize(water_consume= sum(value, na.rm=T))
    View(pivot_wider(conw, values_from = water_consume, names_from = year))
    
    data_san <- data[data$indicator%in%c("sanred_ch", "sanseptic_ch", "sanotramejorado_ch", "sanotranm_ch", "sanambiente_ch", "sandesconocido_ch","sinsan_ch")& data$area=="Total" & data$migration=="Total"& data$disability =="Total" & data$quintile =="Total"& data$ethnicity=="Total",]
    san<-data_san%>%
      group_by(isoalpha3, year)%>%
      dplyr::summarize(sanitation= sum(value,na.rm=T))
    
    View(pivot_wider(san, values_from = sanitation, names_from = year))



#### Indicators available by country, most recent year  ####

most_recent_year <- aggregate(data$year, by = list(data$isoalpha3), max)
colnames(most_recent_year) <- c("isoalpha3", "year")

data_ry<-merge(most_recent_year, data,  by = c("isoalpha3", "year"))   ## OLAS_update_upload is for upload to the site. 

ry  <- data_ry[data_ry$area =="Total" & data_ry$quintile == "Total" & data_ry$migration == "Total" & data_ry$ethnicity == "Total" & data_ry$disability=="Total",]



water_general_sources<-filter(ry, indicator %in% c("aguafred_ch", 
                                                   "aguafpublico_ch",
                                                   "aguafembotellada_ch",
                                                   "aguafpozoprot_ch",
                                                   "aguaflluvia_ch",
                                                   "aguafcamion_ch",
                                                   "aguafotramej_ch",
                                                   "aguafsuperficial_ch",
                                                   "aguafotranm_ch",
                                                   "aguafdesconocido_ch",
                                                   "aguafmejorada_ch"))

wgs<-select(water_general_sources, isoalpha3, indicator, value)

water_table<-pivot_wider(wgs, names_from = indicator, values_from = value)


san_instalations<-filter(ry, indicator %in% c( "sinsan_ch",
                                               "sanred_ch",
                                               "sanseptic_ch",
                                               "sanotramejorado_ch",
                                               "sanotranm_ch",
                                               "sanambiente_ch",
                                               "sandesconocido_ch",
                                               "sanmejorado_ch"))
si<-select(san_instalations, isoalpha3, indicator, value)

san_table<-pivot_wider(si, names_from = indicator, values_from = value)


list_of_datasets <- list("tabla_agua" = water_table,
                         "tabla_saneamiento" = san_table)
require(openxlsx)
write.xlsx(list_of_datasets, file = "results/data_info/datos_disponibles_sinapsis2.xlsx")






#### Creation of outputs per variables for general visualization ####

water_con_sources<-filter(DF, year %in% c(2013:2021) & indicator %in% c("aguafconnulo_ch",
                                                                        "aguafredcon_ch",
                                                                        "aguafpublicocon_ch",
                                                                        "aguafembotcon_ch",
                                                                        "aguafpozoprotcon_ch",
                                                                        "aguaflluviacon_ch",
                                                                        "aguafcamioncon_ch",
                                                                        "aguafotramejcon_ch",
                                                                        "aguafsupercon_ch",
                                                                        "aguafotranmcon_ch",
                                                                        "aguafdesconcon_ch"))

water_con_overview<-filter(DF, year %in% c(2013:2021) & indicator %in% c(
  "aguafdesconcon_ch",
  "aguafnmcon_ch",
  "aguafmejoradacon_ch"))

water_general_sources<-filter(DF, year %in% c(2013:2021) & indicator %in% c("aguafred_ch",                                                         "aguared_ch",
                                                                            "aguafpublico_ch",
                                                                            "aguafembotellada_ch",
                                                                            "aguafpozoprot_ch",
                                                                            "aguaflluvia_ch",
                                                                            "aguafcamion_ch",
                                                                            "aguafotramej_ch",
                                                                            "aguafsuperficial_ch",
                                                                            "aguafotranm_ch",
                                                                            "aguafdesconocido_ch"))

water_general_overview<-filter(DF, year %in% c(2013:2021) & indicator %in% c("aguafdesconocido_ch",                                                      
                                                                             "aguafnomejorada_ch",
                                                                             "aguafmejorada_ch"))



san_instalations<-filter(DF, year %in% c(2013:2021) & indicator %in% c( "sinsan_ch",
                                                                        "nosanvecino_ch",
                                                                        "nosanairelibre_ch",
                                                                        "nosandesc_ch",
                                                                        "sanred_ch",
                                                                        "sanseptic_ch",
                                                                        "sanotramejorado_ch",
                                                                        "sanotranm_ch",
                                                                        "sanambiente_ch",
                                                                        "sandesconocido_ch"))                                                      

san_instalations_overview<-filter(DF, year %in% c(2013:2021) & indicator %in% c( "sandesconocido_ch","sanmejorado_ch",
                                                                                 "sannomejorado_ch"))                                                      






list_of_datasets <- list("san_instalations_overview" = san_instalations_overview,
                         "san_instalations" = san_instalations, 
                         "water_general_overview" = water_general_overview,
                         "water_general_sources"=water_general_sources, 
                         "water_con_overview" = water_con_overview,
                         "water_con_sources"=water_con_sources)
require(openxlsx)
write.xlsx(list_of_datasets, file = "results/data_info/inescl.xlsx")


