
print("Combining SCL data files") 
rm(list=ls())

library(tidyr)
library(plyr)
library(dplyr)
library(ggplot2)

#### Create full datasets ####
  files<- list.files("inputs/Indicators_R")
  
  for (f in 1:length(files)){
    print(paste0("File Num: ",f,"/",length(files), " File: ", files[f], " Completed: ", round((f/length(files)),2)*100,"%"))
      df1<- read.csv(paste0("inputs/Indicators_R/",files[f]))   # read the file
      if(f == 1){
        DF <-df1                        # Create if it does not exists
      }
      else{
        DF <-rbind.fill(DF, df1)        # append the current file
      }
    }


    df<-filter(DF, year %in% c(2003:2022) & indicator %in% c("jefe_ci","aguatrat_ch",
                                                         "aguafconnulo_ch",
                                                         "aguafredcon_ch",
                                                         "aguafpublicocon_ch",
                                                         "aguafembotcon_ch",
                                                         "aguafpozoprotcon_ch",
                                                         "aguaflluviacon_ch",
                                                         "aguafcamioncon_ch",
                                                         "aguafotramejcon_ch",
                                                         "aguafsupercon_ch",
                                                         "aguafotranmcon_ch",
                                                         "aguafdesconcon_ch",
                                                         "aguafnmcon_ch",
                                                         "aguafmejoradacon_ch",
                                                         "jmpconaguasegura_ch",
                                                         "jmpaguasegura_ch",
                                                         "aguared_ch",
                                                         "aguafred_ch",
                                                         "aguafpublico_ch",
                                                         "aguafembotellada_ch",
                                                         "aguafpozoprot_ch",
                                                         "aguaflluvia_ch",
                                                         "aguafcamion_ch",
                                                         "aguafotramej_ch",
                                                         "aguafsuperficial_ch",
                                                         "aguafotranm_ch",
                                                         "aguafdesconocido_ch",
                                                         "aguafnomejorada_ch",
                                                         "aguafmejorada_ch",
                                                         "aguadispnulo_ch",
                                                         "aguadispcontinuo_ch",
                                                         "aguadisp_ch",
                                                         "aguamide_ch",
                                                         "sinsan_ch",
                                                         "nosanvecino_ch",
                                                         "nosanairelibre_ch",
                                                         "nosandesc_ch",
                                                         "sanred_ch",
                                                         "sanseptic_ch",
                                                         "sanotramejorado_ch",
                                                         "sanotranm_ch",
                                                         "sanambiente_ch",
                                                         "sandesconocido_ch",
                                                         "sanmejorado_ch",
                                                         "sannomejorado_ch",
                                                         "sanexclusivo_ch",
                                                         "sanexclusivonulo_ch",
                                                         
                                                         "aguatrat_ch_nivel",
                                                         "aguafconnulo_ch_nivel",
                                                         "aguafredcon_ch_nivel",
                                                         "aguafpublicocon_ch_nivel",
                                                         "aguafembotcon_ch_nivel",
                                                         "aguafpozoprotcon_ch_nivel",
                                                         "aguaflluviacon_ch_nivel",
                                                         "aguafcamioncon_ch_nivel",
                                                         "aguafotramejcon_ch_nivel",
                                                         "aguafsupercon_ch_nivel",
                                                         "aguafotranmcon_ch_nivel",
                                                         "aguafdesconcon_ch_nivel",
                                                         "aguafnmcon_ch_nivel",
                                                         "aguafmejoradacon_ch_nivel",
                                                         "jmpconaguasegura_ch_nivel",
                                                         "jmpaguasegura_ch_nivel",
                                                         "aguared_ch_nivel",
                                                         "aguafred_ch_nivel",
                                                         "aguafpublico_ch_nivel",
                                                         "aguafembotellada_ch_nivel",
                                                         "aguafpozoprot_ch_nivel",
                                                         "aguaflluvia_ch_nivel",
                                                         "aguafcamion_ch_nivel",
                                                         "aguafotramej_ch_nivel",
                                                         "aguafsuperficial_ch_nivel",
                                                         "aguafotranm_ch_nivel",
                                                         "aguafdesconocido_ch_nivel",
                                                         "aguafnomejorada_ch_nivel",
                                                         "aguafmejorada_ch_nivel",
                                                         "aguadispnulo_ch_nivel",
                                                         "aguadispcontinuo_ch_nivel",
                                                         "aguadisp_ch_nivel",
                                                         "aguamide_ch_nivel",
                                                         "sinsan_ch_nivel",
                                                         "nosanvecino_ch_nivel",
                                                         "nosanairelibre_ch_nivel",
                                                         "nosandesc_ch_nivel",
                                                         "sanred_ch_nivel",
                                                         "sanseptic_ch_nivel",
                                                         "sanotramejorado_ch_nivel",
                                                         "sanotranm_ch_nivel",
                                                         "sanambiente_ch_nivel",
                                                         "sandesconocido_ch_nivel",
                                                         "sanmejorado_ch_nivel",
                                                         "sannomejorado_ch_nivel",
                                                         "sanexclusivo_ch_nivel",
                                                         "sanexclusivonulo_ch_nivel"))
    
    
    
    
    df_filter<-dplyr::filter(DF, year %in% c(2003:2022) & indicator %in% c("aguatrat_ch",
                                                             "aguafconnulo_ch",
                                                             "aguafredcon_ch",
                                                             "aguafpublicocon_ch",
                                                             "aguafembotcon_ch",
                                                             "aguafpozoprotcon_ch",
                                                             "aguaflluviacon_ch",
                                                             "aguafcamioncon_ch",
                                                             "aguafotramejcon_ch",
                                                             "aguafsupercon_ch",
                                                             "aguafotranmcon_ch",
                                                             "aguafdesconcon_ch",
                                                             "aguafnmcon_ch",
                                                             "aguafmejoradacon_ch",
                                                             "jmpconaguasegura_ch",
                                                             "jmpaguasegura_ch",
                                                             "aguared_ch",
                                                             "aguafred_ch",
                                                             "aguafpublico_ch",
                                                             "aguafembotellada_ch",
                                                             "aguafpozoprot_ch",
                                                             "aguaflluvia_ch",
                                                             "aguafcamion_ch",
                                                             "aguafotramej_ch",
                                                             "aguafsuperficial_ch",
                                                             "aguafotranm_ch",
                                                             "aguafdesconocido_ch",
                                                             "aguafnomejorada_ch",
                                                             "aguafmejorada_ch",
                                                             "aguadispnulo_ch",
                                                             "aguadispcontinuo_ch",
                                                             "aguadisp_ch",
                                                             "aguamide_ch",
                                                             "sinsan_ch",
                                                             "nosanvecino_ch",
                                                             "nosanairelibre_ch",
                                                             "nosandesc_ch",
                                                             "sanred_ch",
                                                             "sanseptic_ch",
                                                             "sanotramejorado_ch",
                                                             "sanotranm_ch",
                                                             "sanambiente_ch",
                                                             "sandesconocido_ch",
                                                             "sanmejorado_ch",
                                                             "sannomejorado_ch",
                                                             "sanexclusivo_ch",
                                                             "sanexclusivonulo_ch"))
    
    
    
    # Delete below after issue with NAs is solved
    df_filter$value<-ifelse(df_filter$value == 0, NA, df_filter$value)
    
    sum<-df_filter%>%
      group_by(year, isoalpha3)%>%
      dplyr::summarise(mean=mean(value, na.rm = T))
    
    with_data<-filter(sum, mean != 1 & !is.nan(mean) & mean !=0)
    year_country_with_data<-select(with_data, year, isoalpha3)
    
    test<-pivot_wider(year_country_with_data, names_from = year, values_from = 1)
    
    no_years<-year_country_with_data%>%
      group_by(isoalpha3)%>%
      dplyr::summarise(n())
    
    write.csv(test, "results/data_info/years_with_data.csv")
    
    data<-left_join(year_country_with_data, df, by=c("year"="year", "isoalpha3" ="isoalpha3"))

#### Add fields for data visualizations ####

    iso <- tribble(~country, ~iso3, ~pais,
                   "Mexico"	,"MEX", "México",
                   "Guatemala"	,"GTM","Guatemala", 
                   "El Salvador"	,"SLV","El Salvador",
                   "Honduras"	,"HND","Honduras"	,
                   "Nicaragua"	,"NIC","Nicaragua",
                   "Costa Rica"	,"CRI","Costa Rica",
                   "Panama"	,"PAN","Panamá",
                   "Colombia"	,"COL","Colombia",
                   "Ecuador"	,"ECU","Ecuador",
                   "Bolivia"	,"BOL","Bolivia",
                   "Peru"	,"PER","Perú",
                   "Paraguay"	,"PRY","Paraguay",
                   "Chile"	,"CHL","Chile",
                   "Uruguay"	,"URY","Uruguay",
                   "Brazil"	,"BRA","Brasil",	
                   "Venezuela"	,"VEN","Venezuela",
                   "Argentina"	,"ARG","Argentina",
                   "Dominican Republic"	,"DOM","República Dominicana",
                   "Haiti"	,"HTI", "Haití",
                   "Jamaica"	,"JAM", "Jamaica",
                   "Guyana"	,"GUY", "Guyana",
                   "Trinidad and Tobago"	,"TTO", "Trinidad y Tobago",
                   "Belize"	,"BLZ", "Belice",
                   "Suriname"	,"SUR", "Surinam",
                   "Bahamas"	,"BHS", "Bahamas",
                   "Barbados"	,"BRB", "Barbados") 
    
    data <- data %>% 
      left_join(iso, by= c("isoalpha3" ="iso3"))
    data$area_es <- ifelse(data$area == "urban", "Urbano", 
                           ifelse(data$area == "rural", "Rural",data$area))
    data$quintil_es <- ifelse(data$quintile == "quintile_1", "Quintil 1", 
                              ifelse(data$quintile == "quintile_2", "Quintil 2",
                                     ifelse(data$quintile == "quintile_3", "Quintil 3",
                                            ifelse(data$quintile == "quintile_4", "Quintil 4",
                                                   ifelse(data$quintile == "quintile_5", "Quintil 5",data$quintile)))))
    data$etnicidad_es <-data$ethnicity 
    data$ethnicity<-ifelse(data$etnicidad_es == "Otro", "Other",data$etnicidad_es)
    
    data$migracion_es <- ifelse(data$migration == "migrant", "Migrante",
                                ifelse(data$migration == "non_migrant", "Non-migrante", data$migration))
    data$discapacidad_es<- ifelse(data$disability == "person_with_disability", "Persona con discapacidad",
                                  ifelse(data$disability == "person_with_no_disability", "Persona sin discapacidad", data$disability))
    
      
      
    library(stringr)
    options(scipen=999)
    
    data$value<- ifelse(str_sub(data$indicator, -6,-1) != "_nivel", data$value*100, data$value)
    data$value<-ifelse(data$value == 0, NA, data$value)
    write.csv(data, "results/datasets/indicators_inescl_R.csv", row.names = F)



#### WEIGHTED REGIONAL VALUES ####
    
    #Regional level data
    
    View(pivot_wider(year_country_with_data, names_from =year , values_from = 1 ))
    
    year_big_countries<-data%>%
      dplyr::select( year, isoalpha3)%>%
      filter(isoalpha3 %in% c("MEX", "BRA","COL", "ARG", "PER"))%>%
      unique()%>%
      group_by(year)%>%
      summarise(n())%>%
      filter(`n()`==5)
    
    data$hh_surveyed<- data$level/(data$value/100)
    sum_l<-data%>% filter(year %in% year_big_countries$year)%>%
      dplyr::group_by(indicator, year, area,quintile, ethnicity, migration, disability, age,education_level,sex, idgeo) %>%
      dplyr::summarise(year_hh_indicator = sum(level,rm.na=F),
                       year_hh_surveyed = sum(hh_surveyed, rm.na=F))
    regional_data<-left_join(sum_l, data)
    
    
    
    regional_data$weighted_value1<- (regional_data$year_hh_indicator/regional_data$year_hh_surveyed)
    
    regional_data$weight2<-regional_data$hh_surveyed/regional_data$year_hh_surveyed
    regional_data$weighted_value2<- regional_data$value*regional_data$weight2
    
    region<-regional_data%>%
      dplyr::group_by(indicator, year, area,quintile, ethnicity, migration, disability, age,education_level,sex, idgeo) %>%
      dplyr::summarise(validation = mean(weighted_value1),
                       year_hh_surveyed = mean(year_hh_surveyed),
                       year_hh_indicator = mean(year_hh_indicator),
                       weighted_value = sum(weighted_value2),
                       count_countries = n())
    ### Unimproved is over estimated because there are so many countries without values, the numbers will not sum to 100% although they will be close.
    
    ### Validation of regional data ###
    
    # For regional data we only include indicators that have estimates for 10 or more countries to ensure that the numbers are not very skewed
    t<- filter(region, count_countries>9)
    
    ggplot(filter(t, indicator == "aguafred_ch" & area=="Total" & migration=="Total"& disability =="Total" & quintile =="Total"& ethnicity=="Total"), aes(x = year, y =weighted_value))+
      geom_line()+
      ylim(0,100)
    
    ggplot(filter(t, indicator == "aguafred_ch" & area=="Total" & migration=="Total"& disability =="Total" & quintile =="Total"& ethnicity=="Total"), aes(x = year, y =weighted_value))+
      geom_line()+
      geom_line(data =filter(t, indicator == "aguafred_ch" & area=="Total" & migration=="Total"& disability =="Total" & quintile =="quintile_1"& ethnicity=="Total"), aes(x = year, y =weighted_value))+
        geom_line(data =filter(t, indicator == "aguafred_ch" & area=="Total" & migration=="Total"& disability =="Total" & quintile =="quintile_5"& ethnicity=="Total"), aes(x = year, y =weighted_value))+
        geom_line(data =filter(t, indicator == "aguafred_ch" & area=="rural" & migration=="Total"& disability =="Total" & quintile =="Total"& ethnicity=="Total"), aes(x = year, y =weighted_value))+
        geom_line(data =filter(t, indicator == "aguafred_ch" & area=="urban" & migration=="Total"& disability =="Total" & quintile =="Total"& ethnicity=="Total"), aes(x = year, y =weighted_value))+
        ylim(0,100)
      
      data_genw <- t[t$indicator%in%c("aguafred_ch","aguafpublico_ch","aguafembotellada_ch","aguafotranomejorada_ch","aguafpozoprot_ch","aguaflluvia_ch","aguafcamion_ch","aguafotramej_ch","aguafsuperficial_ch","aguafotranm_ch","aguafdesconocido_ch")& t$area=="Total" & t$migration=="Total"& t$disability =="Total" & t$quintile =="Total"& t$ethnicity=="Total",]
      genw<-data_genw%>%
        group_by( year)%>%
        dplyr::summarize(water_general= sum(weighted_value, na.rm=T))
      pivot_wider(genw, values_from = water_general, names_from = year)
      
      
      data_genwan <- t[t$indicator%in%c("aguafmejorada_ch","aguafdesconocido_ch","aguafnomejorada_ch")& t$area=="Total" & t$migration=="Total"& t$disability =="Total" & t$quintile =="Total"& t$ethnicity=="Total",]
      genwan<-data_genwan%>%
        group_by(year)%>%
        dplyr::summarize(water_general= sum(weighted_value, na.rm=T))
      pivot_wider(genwan, values_from = water_general, names_from = year)
      
      write.csv(region, "results/datasets/region_indicators_inescl_R.csv", row.names = F)
    
    

