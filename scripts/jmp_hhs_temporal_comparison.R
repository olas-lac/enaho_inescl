#JMP HHS comparison

# Sewer
# Septic
# piped
rm(list=ls())
library(tidyverse)
library(plyr)
library(readxl)



#### HHS ####

      hhs_full<-read.csv("results/datasets/indicators_inescl_R.csv")
      hhs_full$value <- hhs_full$value/100
      hhs<-hhs_full[ hhs_full$area == "Total" & hhs_full$quintile == "Total" & hhs_full$ethnicity == "Total" & hhs_full$disability == "Total" & hhs_full$migration == "Total" & hhs_full$indicator%in% c("aguafdesconocido_ch","aguafredcon_ch","aguafdesconcon_ch","aguared_ch","aguafred_ch","aguafmejorada_ch","aguafmejoradacon_ch","aguadispcontinuo_ch","aguadisp_ch","sanred_ch","sanseptic_ch","sanotramejorado_ch","sandesconocido_ch","sinsan_ch"),]

      hhs$description<-ifelse(hhs$indicator =="aguafred_ch", "Water, Piped",
                              ifelse(hhs$indicator =="aguafredcon_ch", "Drinking water, Piped improved",
                                     ifelse(hhs$indicator =="aguadisp_ch","Drinking water, Available",
                                            ifelse(hhs$indicator =="aguafdesconcon_ch","Unclassifiable drinking water source",
                                                   ifelse(hhs$indicator =="aguafdesconocido_ch","Unclassifiable water source",
                                                          ifelse(hhs$indicator=="aguafmejoradacon_ch","Drinking water, At least basic service",
                                                                 ifelse(hhs$indicator=="aguafmejorada_ch","Water, At least basic service",
                                                                        ifelse(hhs$indicator=="sinsan_ch","Sanitation, Open defecation",
                                                                               ifelse(hhs$indicator=="sanred_ch","Sanitation, Sewer",
                                                                                      ifelse(hhs$indicator=="sanseptic_ch","Sanitation, Septic tank", 
                                                                                             ifelse(hhs$indicator=="sanotramejorado_ch","Sanitation, Improved latrine and other",
                                                                                                    ifelse(hhs$indicator == "sandesconocido_ch", "Unclassifiable sanitation facility", NA))))))))))))
      
      
      
      hhs_country<-select(hhs, isoalpha3, description, value, year, se)
      
      
      hhs_tab<-dplyr::rename(hhs_country, "iso3" = "isoalpha3",
                             "var" = "description")
      
      hhs_tab$dataset <- "National Household Surveys"
      hhs_tab$estimate <- "exact"
      
      sum_iu<-filter(hhs_tab, dataset %in% c("National Household Surveys") & var %in%c("Unclassifiable water source", "Water, At least basic service"))
      sum_iu2<-sum_iu%>%
        group_by(iso3, year)%>%
        dplyr::summarise("Improved water source, max estimate" = sum(value,na.rm = F))
      
      ui<-pivot_longer(sum_iu2, `Improved water source, max estimate`, names_to = "var", values_to = "value")
      ui$dataset<- "National Household Surveys"  
      hhs_tab2<-select(hhs_tab,iso3, var, value, dataset, estimate, year, se)
      hhs_table<-rbind.fill(hhs_tab2,ui )
      
      hhs_comparison<- filter(hhs_table, var %in% c("Sanitation, Sewer","Sanitation, Septic tank","Sanitation, Open defecation","Water, Piped","Improved water source, max estimate")) #"Water, At least basic service", "Improved water source, max estimate"
     
      ## Comment out the following if you want comparison graphics, use the following if you want wilcoxon ##
       hhs_comparison$var<-ifelse(hhs_comparison$var =="Water, Piped", "Drinking water, Piped improved" ,
                                ifelse(hhs_comparison$var == "Improved water source, max estimate", "Drinking water, At least basic service", hhs_comparison$var))
#### JMP ####
      jmp<- readxl::read_xlsx("inputs/jmp_inputs/sdg6data_download-all_levels.xlsx") 
      
      jmp<-jmp[jmp$Location == "National",]
      
      jmp<-filter(jmp, `Indicator name` %in% c("Water, Piped","Drinking water, Non-piped improved", "Drinking water, Piped improved", "Drinking water, Available","Drinking water, At least basic service",
                                               "Sanitation, Septic tank", "Sanitation, Sewer", "Sanitation, Improved latrine and other", "Sanitation, Open defecation")) 
      jmp_pivot<-select(jmp,`Geographical area name`, `Indicator name`, Value, Year)
      
      jmp_pivot$iso3<-ifelse(jmp_pivot$`Geographical area name` == "Argentina", "ARG",
                             ifelse(jmp_pivot$`Geographical area name` == "Bolivia (Plurinational State of)" ,"BOL" ,
                                    ifelse(jmp_pivot$`Geographical area name` =="Brazil"   ,"BRA" ,
                                           ifelse(jmp_pivot$`Geographical area name` == "Colombia","COL",
                                                  ifelse(jmp_pivot$`Geographical area name` == "Costa Rica" ,"CRI", 
                                                         ifelse(jmp_pivot$`Geographical area name` == "Dominican Republic" ,"DOM" ,
                                                                ifelse(jmp_pivot$`Geographical area name` == "Guatemala"  ,"GTM",
                                                                       ifelse(jmp_pivot$`Geographical area name` == "Honduras" ,"HND",
                                                                              ifelse(jmp_pivot$`Geographical area name` == "Jamaica"  ,"JAM",
                                                                                     ifelse(jmp_pivot$`Geographical area name` ==  "Mexico" ,"MEX",
                                                                                            ifelse(jmp_pivot$`Geographical area name` == "Panama" ,"PAN",
                                                                                                   ifelse(jmp_pivot$`Geographical area name` == "Peru" ,"PER",
                                                                                                          ifelse(jmp_pivot$`Geographical area name` == "El Salvador"  ,"SLV",
                                                                                                                 ifelse(jmp_pivot$`Geographical area name` == "Uruguay" ,"URY", NA))))))))))))))
      
      jmp_table<-filter(jmp_pivot, !is.na(iso3)) 
      jmp_table<-select(jmp_table,iso3, `Indicator name`, Value, Year)
      
      jmp_table <- dplyr::rename(jmp_table, "var"="Indicator name" , "value" = "Value", "year" = "Year")
      jmp_table$estimate <- "exact"
      jmp_table$value <- jmp_table$value/100
      jmp_table$dataset <- "Joint Monitoring Programme"
      
      jmp_table$upp<-NA
      
#### Join datasets ####
      
    data<-plyr::rbind.fill(hhs_table, jmp_table)
    data<- data[data$year %in% c(2003:2022)& data$iso3 %in% c("ARG", "BOL", "BRA", "COL", "CRI", "DOM", "GTM", "HND", "MEX", "PAN", "PER", "SLV", "URY"),]
    
    
    datacomp<-plyr::rbind.fill(hhs_comparison, jmp_table)
    data1<- datacomp[datacomp$year %in% c(2003:2022)& datacomp$iso3 %in% c("ARG", "BOL", "BRA", "COL", "CRI", "DOM", "GTM", "HND", "MEX", "PAN", "PER", "SLV", "URY"),]
    data2<- filter(data1, var %in% c("Drinking water, Piped improved","Drinking water, At least basic service","Sanitation, Sewer","Sanitation, Septic tank","Sanitation, Open defecation"))
    
    
    datawide<-select(data2, iso3, year, dataset, var, value)
    data_comparison<-pivot_wider(datawide, values_from = value,names_from = dataset)
    data_comparison$dif <- data_comparison$`Joint Monitoring Programme`-data_comparison$`National Household Surveys`  
    data_comparison$dif_pct<- (data_comparison$`Joint Monitoring Programme`-data_comparison$`National Household Surveys`)/((data_comparison$`Joint Monitoring Programme`+data_comparison$`National Household Surveys`)/2)
    dif<-data_comparison%>%
      group_by(var)%>%
      dplyr::summarise(mean_dif = mean(dif, na.rm = T),
                       median_dif = median(dif, na.rm=T),
                       max_dif = max(dif, na.rm = T),
                       min_dif = min(dif, na.rm = T))
    
    
    # ggplot(dif, aes(x=iso3, y = mean_dif, color=var))+
    #   geom_point( pch = 18, size =4, alpha =.4)+
    #   geom_line(aes(group = var))+
    #   theme(legend.title = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    #         panel.background=element_blank(),legend.spacing=unit(0, "cm"),  legend.position = "bottom", plot.caption.position = "panel")
    library(rstatix)
    library(ggpubr)
    datawide2<-filter(datawide,!is.na(datawide$value))
    stat.indicator.test <- datawide2 %>% 
      group_by(var) %>%
      wilcox_test(value ~ dataset) %>%
      add_significance()
    
    effect.indicator<-datawide2 %>%
      group_by(var) %>%
      wilcox_effsize(value ~ dataset)
    
    stat.test <- stat.indicator.test %>% add_xy_position(x = "dataset")

    
      stat.test$y.position <- 15
      stat.test$xmin <- .3
      stat.test$xmax <- .7
    ggplot(datawide2, aes( x = value))+
      geom_density(aes(fill = dataset), alpha = 0.2)+
      xlim(0,1)+
      xlab("Value")+
      ylab("Density")+
      facet_wrap(~var) +
      stat_pvalue_manual(stat.test, tip.length = 0)+
      theme(legend.title = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
            panel.background=element_blank(),legend.spacing=unit(0, "cm"),  legend.position = "bottom", plot.caption.position =  "panel")+
      labs(title = "Comparación entre indicadores claves, JMP y ENAHO",caption ="El asterisco en el centro de cada gráfico indica el nivel de significación de una prueba de suma clasificada de Wilcoxon entre las dos encuestas para una variable determinada.
                   Source: OLAS Conjunto de datos ENAHO, Datos JMP, años 2003-2021")
    
    
    
    
    var<-c("Drinking water, Piped improved","Drinking water, At least basic service","Sanitation, Sewer","Sanitation, Septic tank","Sanitation, Open defecation")
    for(i in 1:length(var)){
      data_filtered <- filter(data_comparison, var ==var[i] )
      ggplot(data_filtered, aes(x=dif, fill = var[i],alpha =.2))+
        geom_density()+
        facet_grid(var[i]~iso3)
      }
    dc<-data_comparison[data_comparison$dif>-.2& !is.na(data_comparison$dif),]
  

    
    
    write.csv(dif, "results/data_info/dif_jmp_hhs.csv")
    
    
    
    
    
     
      
      cols <- c("Joint Monitoring Programme" = "#6ec1d0","National Household Surveys" ="coral")
 
      piped <- data[data$iso3 %in% c( "BOL","BRA" , "COL", "CRI", "GTM","SLV", "HND", "MEX","PER","PAN", "URY", "DOM") & data$year%in% c(2003:2021),]
      

        ggplot(data = filter(piped, dataset %in% c("National Household Surveys") & var == "Water, Piped"),aes(x=year,y =value)) +
          geom_point(color ="coral4", size =.3)+ 
          geom_smooth(data = filter(piped, dataset %in% c("National Household Surveys") & var == "Water, Piped"),aes(group=iso3,color = dataset),method="lm")+
          geom_line(data=filter(piped, dataset == "Joint Monitoring Programme" & var == "Drinking water, Piped improved"), aes(x= year,y =value,color = dataset, group = iso3)) + 
          facet_wrap(~iso3)+
          ylim(0,1)+
          xlab("Años")+
          ylab("Percent household / Percent population")+
          scale_colour_manual(values = cols)+
          scale_x_discrete(labels = c("","",2005,"","","","",2010,"","","","",2015,"","","","",2020,""))+
          theme(legend.title = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background=element_blank(),legend.spacing=unit(0, "cm"),  legend.position = "bottom", plot.caption.position = "panel")+
          labs(title = "Piped water access comparison, JMP and OLAS HHS dataset",caption ="Sources: 
          OLAS Household Survey dataset
          Joint Monitoring Program")

        
        imp <- data[data$iso3 %in% c( "BOL", "SLV", "HND", "PAN", "URY", "DOM") & data$year%in% c(2003:2021),]
        ggplot(data = filter(imp, dataset %in% c("National Household Surveys") & var == "Water, At least basic service"),aes(x=year,y =value)) +
          geom_point(color ="coral", size =.1)+ 
          geom_smooth(data = filter(imp, dataset %in% c("National Household Surveys") & var == "Water, At least basic service"),aes(group=iso3,color = dataset),method="lm")+
          geom_line(data = filter(imp, dataset %in% c("National Household Surveys") & var ==  "Improved water source, max estimate"),aes(group=iso3), lty=2,color ="coral", size =.1)+
          geom_smooth(data = filter(imp, dataset %in% c("National Household Surveys") & var =="Improved water source, max estimate"),aes(group=iso3,color = dataset),method="lm")+
        #  geom_ribbon(data = filter(imp, dataset %in% c("National Household Surveys") & var == "Water, At least basic service")aes(x=consumpt,y =mean_metered,ymin = lowerbound_metered, ymax = upperbound_metered), alpha = 0.1)+ 
          geom_line(data=filter(imp, dataset == "Joint Monitoring Programme" & var == "Drinking water, At least basic service"), aes(x= year,y =value,color = dataset, group = iso3)) + 
          facet_wrap(~iso3)+
          ylim(0,1)+
          xlab("Años")+
          ylab("Percent household / Percent population")+
          scale_colour_manual(values = cols)+
         # scale_x_discrete(labels = c("","",2005,"","","","",2010,"","","","",2015,"","","","",2020,""))+
          theme(legend.title = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background=element_blank(),legend.spacing=unit(0, "cm"),  legend.position = "bottom", plot.caption.position = "panel")+
          labs(title = "Water, at least basic service, JMP and OLAS HHS dataset",caption ="Sources: 
          OLAS Household Survey dataset
          Joint Monitoring Program")
        
      ### Improved water source
        
        impjmp <- data[data$iso3 %in% c( "BOL", "SLV", "HND", "PAN", "URY", "DOM") & data$year%in% c(2003:2021) ,]
        
        imp <- data[data$iso3 %in% c( "BOL", "SLV", "HND", "PAN", "URY", "DOM") & data$year%in% c(2003:2021) & data$var %in%c("Water, At least basic service","Improved water source, max estimate"),]
        
        imp1<- select(imp, iso3, var, value, year)
        imp2<-pivot_wider(imp1,names_from = var, values_from = value)
        imp3<-imp2[!is.na(imp2$`Water, At least basic service`) & !is.na(imp2$`Improved water source, max estimate`), ]
        
         ggplot() +
          geom_ribbon(data=imp3,aes(x=year,y =`Water, At least basic service`,ymin =`Water, At least basic service`, ymax =`Improved water source, max estimate`, group = iso3),fill = "coral", alpha = 0.2)+ 
          geom_smooth(data = filter(imp, dataset %in% c("National Household Surveys") & var =="Water, At least basic service"),aes(group=iso3,color = dataset,x=year,y =value),method="lm")+
          geom_line(data=filter(impjmp, dataset == "Joint Monitoring Programme" & var == "Drinking water, At least basic service"), aes(x= year,y =value,color = dataset, group = iso3), size =1) + 
          facet_wrap(~iso3)+
          ylim(0,1)+
          xlab("Años")+
          ylab("Percent household / Percent population")+
          scale_colour_manual(values = cols)+
          scale_x_discrete(labels = c("","",2005,"","","","",2010,"","","","",2015,"","","","",2020,""))+
          theme(legend.title = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                panel.background=element_blank(),legend.spacing=unit(0, "cm"),  legend.position = "bottom", plot.caption.position = "panel")+
          labs(title = "Water, at least basic service, JMP and OLAS HHS dataset",caption ="Sources: 
          OLAS Household Survey dataset
          Joint Monitoring Program")
      

      
    ### Sewer  
        sewer<- data[data$iso3 %in% c( "BOL","BRA" , "COL", "CRI", "GTM","SLV", "HND", "MEX","PER","PAN", "URY", "DOM") & data$year%in% c(2003:2021),]
         
         ggplot(data = filter(sewer, dataset %in% c("National Household Surveys") & var == "Sanitation, Sewer"),aes(x=year,y =value)) +
           geom_point(color ="coral4", size =.3)+ 
           geom_smooth(data = filter(sewer, dataset %in% c("National Household Surveys") & var == "Sanitation, Sewer"),aes(group=iso3,color = dataset),method="lm")+
           geom_line(data=filter(sewer, dataset == "Joint Monitoring Programme" & var == "Sanitation, Sewer"), aes(x= year,y =value,color = dataset, group = iso3)) + 
           facet_wrap(~iso3)+
          # ylim(0,1)+
           xlab("Años")+
           ylab("Ratio de hogares / Ratio de population")+
           scale_colour_manual(values = cols)+
           scale_x_discrete(labels = c("","",2005,"","","","",2010,"","","","",2015,"","","","",2020,""))+
           theme(legend.title = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background=element_blank(),legend.spacing=unit(0, "cm"),  legend.position = "bottom", plot.caption.position = "panel")+
           labs(title = "Sewer access comparison, JMP and OLAS HHS dataset",caption ="Sources: 
          OLAS Household Survey dataset
          Joint Monitoring Program")     
         
    ### Septic
         
         
         septic<- data[data$iso3 %in% c( "BOL","BRA" , "COL", "CRI", "GTM","SLV", "HND", "MEX","PER","PAN", "URY", "DOM") & data$year%in% c(2003:2021),]
         
         ggplot(data = filter(septic, dataset %in% c("National Household Surveys") & var == "Sanitation, Septic tank"),aes(x=year,y =value)) +
           geom_point(color ="coral4", size =.3)+ 
           geom_smooth(data = filter(septic, dataset %in% c("National Household Surveys") & var == "Sanitation, Septic tank"),aes(group=iso3,color = dataset),method="lm")+
           geom_line(data=filter(septic, dataset == "Joint Monitoring Programme" & var == "Sanitation, Septic tank"), aes(x= year,y =value,color = dataset, group = iso3)) + 
           facet_wrap(~iso3)+
          # ylim(0,1)+
           xlab("Años")+
           ylab("Percent household / Percent population")+
           scale_colour_manual(values = cols)+
           scale_x_discrete(labels = c("","",2005,"","","","",2010,"","","","",2015,"","","","",2020,""))+
           theme(legend.title = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background=element_blank(),legend.spacing=unit(0, "cm"),  legend.position = "bottom", plot.caption.position = "panel")+
           labs(title = "Septic access comparison, JMP and OLAS HHS dataset",caption ="Sources: 
          OLAS Household Survey dataset
          Joint Monitoring Program")  
    
    ### Sin instalaciones   
         ggplot(data = filter(data, dataset %in% c("National Household Surveys") & var == "Sanitation, Open defecation"),aes(x=year,y =value)) +
           geom_point(color ="coral4", size =.3)+ 
           geom_smooth(data = filter(data, dataset %in% c("National Household Surveys") & var == "Sanitation, Open defecation"),aes(group=iso3,color = dataset),method="lm")+
           geom_line(data=filter(data, dataset == "Joint Monitoring Programme" & var == "Sanitation, Open defecation"), aes(x= year,y =value,color = dataset, group = iso3)) + 
           facet_wrap(~iso3)+
           #(0,1)+
           xlab("Años")+
           ylab("Percent household / Percent population")+
           scale_colour_manual(values = cols)+
           scale_x_discrete(labels = c("","",2005,"","","","",2010,"","","","",2015,"","","","",2020,""))+
           theme(legend.title = element_blank(),panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                 panel.background=element_blank(),legend.spacing=unit(0, "cm"),  legend.position = "bottom", plot.caption.position = "panel")+
           labs(title = "Without sanitation access comparison, JMP and OLAS HHS dataset",caption ="Sources: 
          OLAS Household Survey dataset
          Joint Monitoring Program")  

      