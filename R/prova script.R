source(here('R', 'librerie.R'))


dati <- read_excel("dati/Dati COVID 2022 per corso R.xlsx", 
                   col_types = c("numeric", "text", "text", 
                                 "numeric", "text", "text", "date", 
                                 "text", "text", "date", "text", "date", 
                                 "text", "text", "text", "skip"))




dati <- clean_names(dati)
#view(dati)


#unique(dati$Specie)
#conferimenti <- unique(dati$`Conf. orig`)
#table(conferimenti)

# Riempire celle vuote con valore precedente ------------------------------


# dati_2022 <- dati %>% 
#   mutate(conf_orig = na.locf(conf_orig),
#          provenienza = na.locf(provenienza),
#          sacco = na.locf(sacco)) %>% 
#   group_by(specie,materiale) %>% 
#   select(-campioni_conf)

dati_2022 <- dati %>% 
  mutate(across(c("conf_orig","provenienza","sacco"), na.locf)) %>% 
  select(-campioni_conf)  #più ordinato di versione precedente

 #n_distinct(dati_2022$conf_orig)

# Tabella frequenza per provincia -----------------------------------------


prov <- table(dati$provenienza)

round(prov/n_distinct(dati_2022$conf_orig)*100, 0)

prov2 <- as.data.frame(prov)
prov2 %>% add_column(percent = round(prov2$Freq/n_distinct(dati_2022$conf_orig)*100,0)) %>% 
  rename(Provincia = Var1, Totale = Freq, Percentuale= percent) %>% arrange(desc(Totale)) %>% view()
  


# Prove analisi --------------------------------------------
str(dati_2022)

dati_2022 <- dati_2022 %>%
  filter(!is.na(materiale)) %>% 
  filter(!is.na(progr)) %>% 
  filter(!is.na(specie)) %>% 
  filter(specie != "CINGHIALE")

#unique(dati_2022$materiale)

dati_2022 <- dati_2022 %>% 
  mutate(materiale = replace(materiale, materiale %in% "T. NAS", "T.NAS"),
         materiale = replace(materiale, materiale %in% "T. RETT", "T.RETT"),
         materiale = replace(materiale, materiale %in% c("ILEO/DIGIUNO", "DIGIUNO","DUODENO"), "INTESTINO"),
         materiale = replace(materiale, materiale %in% "SENI NASALI", "T.NAS"))

#unique(dati_2022$specie)


#voglio arrivare a fare un summary di quanti animali positivi a pancov per specie

tab_pc <- dati_2022 %>% 
  mutate(pancov = replace_na(pancov, "NEG"),
         esito = replace_na(esito, "Negativo"),
         pancov = ifelse(pancov=="POS",1,0)) %>% 
  pivot_wider(names_from = materiale, values_from = pancov, values_fill = 0) %>% 
  select(-progr, -conf_mo) %>% 
  select(-3,-5,-6,-8,-9,-10) %>%
  group_by(conf_orig, specie,provenienza) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = T))) %>% #collasso righe di stesso conferimento sommando i valori sulle colonne
  ungroup() %>% 
  rowwise() %>% 
  mutate(somma = sum(c_across(c(4:17))),
         Pos_Pancov = ifelse(somma >=1, "Pos", "Neg")) %>% 
  select(-(4:17)) %>% #filter(Pos_Pancov == "Pos") %>%  
  group_by(specie) %>% 
  summarise(N_individui = n(), POS = sum(Pos_Pancov == "Pos"), NEG= sum(Pos_Pancov == "Neg"))
   
tab_pc %>% filter(NEG > 1) %>%  select(-2) %>% pivot_longer(!specie, names_to = "Pancov", values_to = "totali") %>%
  ggplot()+
  aes(x=fct_reorder(specie, totali), y=totali, fill=Pancov)+
  geom_col(position = "stack")+
  geom_text(aes(specie, label = ifelse(totali>0, totali,"")), position = position_stack(vjust = 1.02))+#, size = 5, hjust = 0.5, vjust = -0.5, position = position_dodge(width = 1))+
  labs(x="Specie", y="Totali", title = "Risultati Pancov 2022", subtitle = "escluse le specie di cui presente un solo individuo negativo")+
  theme_minimal()+
  coord_flip()
  
#non riesco ad aggiungere etichette... provare a vedere lezioni

tab_pc %>% write.xlsx(file = here("Summary positivi Pancov per specie.xlsx"))
  
            
  

#guardare codice delle zecche per proseguire... ora dovrei collassare sommando le righe di stessi conf


#è necessario pulire i nomi dei conferimenti per renderli univoci, facendo attenzione
#ad anni e numero del campione... colonna a parte sarebbe adatta

#girare dati in modo che ogni matrice sia su una colonna, con esito come 0 o 1,
# così facendo posso sommare e sapere quando una è positiva
#potrei anche mettere le due prove su righe diverse





# bozze -------------------------------------------------------------------



#dati$`Conf. orig` %>% na.locf() #riempie celle vuote con ultimo valore sopra

#glimpse(dati)
#view(mutate(dati,`Conf. orig`= na.locf(`Conf. orig`)))
