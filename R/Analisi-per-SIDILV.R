source(here('R', 'librerie.R'))


dt2022 <- read_excel("dati/Dati COVID 2022 per corso R.xlsx", 
                   col_types = c("numeric", "text", "text", 
                                 "numeric", "text", "text", "date", 
                                 "text", "text", "date", "text", "date", 
                                 "text", "text", "text", "skip"))

dt2023 <- read_excel("dati/Dati COVID 2022 per corso R.xlsx", 
                                                    sheet = "Campioni PRC 2023", col_types = c("numeric", 
                                                                                               "text", "text", "numeric", "text", 
                                                                                               "text", "text", "text", "date", "text", 
                                                                                              "date", "text", "text", "text", "skip"))



dt2023 <- add_column(dt2023,Anno= 2023, .after="Materiale")
dt2022 <- dt2022 %>% dplyr::rename(Anno=Data) %>% mutate(Anno=2022)
dt2022 <- dt2022[-(3871:3991),]
dt2023 <- dt2023[-(3250:4023),]

dati <- bind_rows(dt2022,dt2023)

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

dati <- dati %>% 
  mutate(across(c("conf_orig","provenienza","sacco"), na.locf)) %>% 
  select(-campioni_conf)  #più ordinato di versione precedente

#n_distinct(dati_2022$conf_orig)


# Selezionare solo sacchi già analizzati ----------------------------------

#ho il problema del sacco delle Trachee, quindi lo aggiro chiamandolo sacco 0

dati <- 
  dati %>% mutate(sacco= ifelse(sacco =="T", 0, as.numeric(sacco)))

dati <- dati %>% filter(sacco < 100 | sacco>=112 & sacco <=114)





# Tabella frequenza per provincia -----------------------------------------


prov <- table(dati$provenienza) #numero di campioni per sede

round(prov/length(dati$provenienza)*100, 0) #percentuale per sede

prov2 <- as.data.frame(prov)
prov2 %>% add_column(percent = round(prov/length(dati$provenienza)*100, 0)) %>% 
  rename(Provincia = Var1, Totale = Freq, Percentuale= percent) %>% arrange(desc(Totale)) %>% view()
  


# Prove analisi --------------------------------------------
str(dati)

dt_an <- dati %>%
  filter(!is.na(materiale)) %>% 
  filter(!is.na(progr)) %>% 
  filter(!is.na(specie)) %>% 
  filter(specie != "CINGHIALE")

unique(dt_an$materiale)

dt_an <- dt_an %>% 
  mutate(materiale = replace(materiale, materiale %in% "T. NAS", "T.NAS"),
         materiale = replace(materiale, materiale %in% "T. RETT", "T.RETT"),
         materiale = replace(materiale, materiale %in% c("ILEO/DIGIUNO", "DIGIUNO","DUODENO"), "INTESTINO"),
         materiale = replace(materiale, materiale %in% "SENI NASALI", "T.NAS"))

unique(dt_an$specie)


#voglio arrivare a fare un summary di quanti animali positivi a pancov per specie

tab_pc <- dt_an %>% 
  mutate(pancov = replace_na(pancov, "NEG"),
         esito = replace_na(esito, "Negativo"),
         pancov = ifelse(pancov=="POS",1,0)) %>% 
  pivot_wider(names_from = materiale, values_from = pancov, values_fill = 0) %>% 
  select(-progr, -conf_mo) %>% 
  select(-sacco, -piastra_estrazione, -data_esito, -conferimento_pancov,-note) %>%
  group_by(conf_orig, specie,provenienza, anno) %>% 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = T))) %>% #collasso righe di stesso conferimento sommando i valori sulle colonne
  ungroup() %>% 
  rowwise() %>% 
  mutate(somma = sum(c_across(c(5:18))),
         Pos_Pancov = ifelse(somma >=1, "Pos", "Neg")) %>% 
  select(-(5:18)) %>% #filter(Pos_Pancov == "Pos") %>%  view() 
  group_by(specie) %>% 
  summarise(N_individui = n(), POS = sum(Pos_Pancov == "Pos"), NEG= sum(Pos_Pancov == "Neg")) %>% 
  arrange(desc(N_individui))
   
tab_pc %>% filter(NEG > 1) %>%  select(-2) %>% pivot_longer(!specie, names_to = "Pancov", values_to = "totali") %>%
  ggplot()+
  aes(x=fct_reorder(specie, totali, .desc = T), y=totali, fill=Pancov)+
  geom_col(position = "stack")+
  geom_text(aes(specie, label = ifelse(totali>0, totali,"")), position = position_stack(vjust = 1), size=5, hjust = 0.5, vjust = -0.5)+#, position = position_dodge(width = 1))+
  labs(x="Specie", y="Totali", title = "Risultati Pancov", subtitle = "escluse le specie di cui presente un solo individuo negativo")+
  theme_minimal()+
  theme(title = element_text(size = 14),
        axis.text = element_text(size=10 ,face = "bold"),
        legend.text = element_text(size=10 ,face = "bold"),
        axis.text.x.bottom = element_text(vjust= 10))
#coord_flip()
  
#non riesco ad aggiungere etichette... provare a vedere lezioni

tab_pc %>% write.xlsx(file = here("2023.07.05 Summary positivi Pancov per specie.xlsx"))

tab_pc %>% summarise(Totali=sum(N_individui), Positivi=sum(POS), Negativi=sum(NEG)) %>% view()  
            


# Sierologia --------------------------------------------------------------

sieri2022 <-  read_excel("dati/23062023 Elenco Campioni PRC 2021_006.xlsx", 
                         sheet = "SIERI 2022", col_types = c("skip", 
                                                             "text", "skip", "text", "text", "skip", 
                                                             "skip", "skip", "skip", "text", "text", 
                                                             "numeric", "text"))

sieri2023 <-  read_excel("dati/23062023 Elenco Campioni PRC 2021_006.xlsx", 
                         sheet = "SIERI 2023", col_types = c("text", 
                                                             "skip", "text", "text", "skip", "skip", 
                                                             "skip", "text", "text", "numeric", 
                                                             "text", "text"))
sieri2023 <- sieri2023[-194,] %>% select(-8)
sieri <- bind_rows(sieri2022, sieri2023) #ho rimosso dall'excel le S e le U dai nconf...

#voglio fare tabella tipo tab_pc per ELISA ed sELISA, poi aggiungerla a tab_pc






















