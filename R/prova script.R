library(tidyverse)




dati <- readxl::read_excel(here("dati","Dati COVID 2022 per corso R.xlsx"))

#unique(dati$Specie)
conferimenti <- unique(dati$`Conf. orig`)
table(conferimenti)
