source(here('R', 'librerie.R'))

#readr, tidyr, dplyr, ggplot2 sono insieme di pacchetti del tidyverse
#in tidy, ogni variabile ha una colonna, ogni informazione è una riga, 
#ogni cella è una singola misura
#per questo primo step è organizzare dati e occupa maggior parte del tempo
#verbi utili:
filter () #per filtrare su valori
select () #filtra per nome
mutate()  #creare nuove variabili  con funzioni di quelle esistenti
summarise() # collassare valori in un riassunti
arrange() #riordinare le righe

#paradigma SPKIT-APPLY-COMBINE
group_by() #posso usare ad esempio per dividere in funzione di variabili, sommare 
           #entro variabile e poi riaggregare i risultati

pivot_longer() #utile per ggplot, trasforma la tabella pivot in modo da riorganizzare
pivot_wider()  #fa il contrario di pivot_loger, gira la tabella allargandola invece di lunga
join() #può servire per unire dataset diversi in cui almeno una variabile (soggetto?) è comune
#ci sono 3 tipi di join: left, right full: ognuno dà prevalenza alla prima, alla seconda
#o equipara le tabelle
#posso anche specificare io quale sia la/le colonna/e sulla quale basare il join
# es: dt1 %>% left_join(dt2, by = "PassengerID")

# Esempi di tabelle standard ----------------------------------------------


table1
table2
table3
pivot_wider(table2, names_from = "type", values_from = "count") #riportare table2 a table1

table4a #casi
table4b #popolazione

cases <- table4a %>% 
  pivot_longer(cols = 2:3, names_to = "year", values_to = "cases")

pop <- table4b %>% 
  pivot_longer(cols = 2:3, names_to = "year", values_to = "population")

cases %>% left_join(pop)
#cases %>% right_join(pop, by= c("Country" = "Paese))
#cases %>% full_join(pop) %>% view() in questo caso sono identiche perché complete

table4a %>% 
  pivot_longer(cols = 2:3, names_to = "year", values_to = "cases") %>% 
  
  left_join(
    table4b %>% 
      pivot_longer(cols = 2:3, names_to = "year", values_to = "population")
  )

#bind_rows()


# Compiti a casa Covid 19----------------------------------------------------------

# provare a scaricare excel di dati covid istituto da rshiny.izsler.it/covid19 e
# giocare con dati

covid <- read.csv("C:/Users/alessandro.reggiani/OneDrive - Istituto Zooprofilattico Sperimentale L. E/Desktop/Git projects/corsoR/dati/Dati_Covid_19.csv")

covid <- clean_names(covid)
glimpse(covid)
#attenzione che nconf è univoco solo finché associato all'anno


#tot esami eseguiti per anno
covid %>% 
  group_by(anno) %>% 
  summarise(esami = sum(tot_eseguiti, na.rm = T)) %>% view()

#tot esami eseguiti per reparto
covid %>% 
  group_by(reparto) %>% 
  summarise(esami = sum(tot_eseguiti, na.rm = T)) %>% view()


#tot esami esegguiti per anno e reparto
covid %>% 
  group_by(anno,reparto) %>% 
  summarise(esami = sum(tot_eseguiti, na.rm = T)) %>% view

#per fare pivot_wider i nomi devono essere caratteri
covid %>% 
  mutate(anno = as.character(anno)) %>% 
  group_by(anno, reparto) %>% 
  summarise(esami = sum(tot_eseguiti, na.rm=T))%>%
  pivot_wider(names_from = "anno", values_from = "esami") %>% view() #attenzione perché ggplot vuole solo formato long

#tot esami eseguiti per provincia
covid %>% 
  group_by(provincia) %>% 
  summarise(esami = sum(tot_eseguiti, na.rm = T)) %>% view()

#numero esami per prova ed anno
tab1 <- covid %>% 
  mutate(anno = as.character(anno)) %>% 
  group_by(anno, prova) %>% 
  summarise(esami = sum(tot_eseguiti, na.rm=T))%>%
  pivot_wider(names_from = "anno", values_from = "esami")

rowSums(tab1[1,-1], na.rm=T) #somma esami prova sars-cov-2 1295363

#numero di esami prova "SARS-CoV-2: agente eziologico" per reparto ed anno
tab2 <- covid %>% 
  filter(prova == "SARS-CoV-2: agente eziologico") %>% 
  mutate(anno = as.character(anno)) %>% 
  group_by(anno,reparto) %>% 
  summarise(esami = sum(tot_eseguiti, na.rm = T)) %>% 
  pivot_wider(names_from = "anno", values_from = "esami", values_fill = 0) %>% view()

sum(tab2[,-1], na.rm= T) #somma esami prova Sars-cov-2 1295363


#numero di esami per materiale
unique(covid$materiale)

covid %>% mutate(materiale = replace(materiale, materiale %in% c("TAMPONE ", "TAMPOE", "TAMPONI"), "TAMPONE"),
                 materiale = replace(materiale, materiale %in% c("SALIVA ", "SALIVARI"), "SALIVA"),
                 materiale = replace(materiale, materiale %in% "RNA", "RNA SARS-CoV-2"),
                 materiale = replace(materiale, materiale %in% "materiale vari", "ALTRI MATERIALI"),
                 materiale = replace(materiale, materiale %in% "espettorato", "ESPETTORATO")) %>%
          group_by(materiale) %>% 
          summarise(esami = sum(tot_eseguiti, na.rm = T)) %>%
          pivot_wider(names_from = "materiale", values_from = "esami") #fino qui per esami/materiale
      # %>% names() così mi elenca i diversi materiali, che sono 7

#altro modo di farlo è concatenare if else, aggiungendo condizioni... va sistemato
covid %>% mutate(materiale = if_else(materiale %in% c("TAMPONE ", "TAMPOE", "TAMPONI"), "TAMPONE",
                                     if_else(materiale %in% c("SALIVA ", "SALIVARI"), "SALIVA",
                                             if_else(materiale %in% "RNA", "RNA SARS-CoV-2",
                                                     if_else(materiale %in% "materiale vari", "ALTRI MATERIALI",
                                                             if_else(materiale %in% "espettorato", "ESPETTORATO")))),materiale)) %>%
                   group_by(materiale) %>% 
                   summarise(esami = sum(tot_eseguiti, na.rm = T))


#numero dei comuni
names(covid)
unique(covid$comune)
NA %in% unique(covid$comune)
length(unique(covid$comune)) 

#numero di conferenti
unique(covid$conferente)
NA %in% unique(covid$conferente)
length(unique(covid$conferente))

#numero di esami per anno e per conferente
covid %>% 
  mutate(anno = as.character(anno)) %>% 
  group_by(anno, conferente) %>% 
  summarise(esami = sum(tot_eseguiti, na.rm=T))%>%
  pivot_wider(names_from = "anno", values_from = "esami") %>% view()
