source(here('R', 'librerie.R'))


#library(readxl)
titanic <- read_excel("dati/titanic.xlsx")
#View(titanic) #righe ottenute facendo file/import dataset/excel e selezionando il file

#diversi modi per vedere i dati
head(titanic)
str(titanic)
#in questo dataset non c'è l'equipaggio
#nel nome del passeggero è presente anche il titolo, interessante poi separarlo
#età risulta essere un carattere
#embarked, sono saliti in 3 porti diversi
glimpse(titanic) #utile perché dice anche se dato è raggruppato
#notare come due colonne siano comprese tra virgolette o backend, meglio pulirle
#names(titanic)

clean_names(titanic) #pulisce nomi mettendo tutto in minuscolo e sostituendo spazi con _ %>% 
view(titanic)

titanic <- clean_names(titanic)

titanic <- rename(titanic, sblsp = siblings_and_spouses_on_board,
                  parch = parent_children_on_board) #rinominiamo colonne con nomi lunghi

glimpse(titanic)


# Verbs -------------------------------------------------------------------

select(titanic, pclass, name, sex) #seleziono solo queste
select(titanic, -name) #escludo quella
select(titanic, -c(6:8)) #rimuovo le colonne da 6 a 8
select(titanic, where(is.numeric)) #selezione le colonne con valori numerici

slice(titanic, 1:3) #visualizza certe righe
top_n(titanic, 10)

glimpse(arrange(titanic, fare))
view(arrange(titanic, desc(fare)))

filter(titanic, sex == "male")
filter(titanic, sex != "male")
filter(titanic, sex != "male", sex != "Male", sex != "male_")
filter(titanic, sex %in% c("male", "Male", "male_"))
filter(titanic, !sex %in% c("male", "Male", "male_"))

view(filter(titanic,
            sex == "female",
            pclass == 3,
            !is.na(cabin)))

mutate(titanic, sex = if_else(sex %in% c("Male", "male_"), "male", sex))
#posso usarlo per creare una nuova variabile o sovrascrivere quella esistente
#sostituisce i male scritti male con "male", e lascia il valore di sex se trova female o altro


view(mutate(titanic, sex2 = if_else(sex %in% c("Male", "male_"), "male", sex))
)

#è importante in analisi dei dati che le variabili categoriche siano definite
#come fattori anziché come carattere

glimpse(mutate(titanic, survived = as.factor(survived)))

glimpse(mutate(titanic, across(c("survived", "sex", "embarked"), as.factor)))
#across fa uno scan del dataset facendo l'operazione specificate sulle colonne che gli dici

view(summarise(titanic, mean(fare, na.rm = T)))
view(
  summarise(titanic, tariffa_media = mean(fare, na.rm = T),
            std = sd(fare, na.rm = T),
            n= n()
            )
    )

dtgrouped <- group_by(titanic, pclass, sex) #non vedo nulla così, ma se faccio glimpse vedo che lui ha raggruppato
glimpse(dtgrouped)

summarise(dtgrouped, tariffa_media = mean(fare, na.rm = T),
          n = n()) #fa la media della tariffa e lo restituisce come tabella raggruppata

write.xlsx(summarise(dtgrouped, tariffa_media = mean(fare, na.rm = T),
                     n = n()), file = "tabella.xlsx")

#come calcolare il tasso di sopravvivenza complessivo?

surv <- filter(titanic, survived == 1)
glimpse(surv)
494/1309
mean(titanic$survived)

summarise(dtgrouped, surv = mean(survived))


# Pipe --------------------------------------------------------------------

#posso vedere ogni singolo passaggio dando %>% glimpse/view

titanic <- read_excel("dati/titanic.xlsx")
titanic <- clean_names(titanic)

titanic %>%
  clean_names() %>% 
  #rename(sblp = `siblings and spouses on board`) #dà errore dovuto al clean names, meglio farlo prima
  rename(sblp = siblings_and_spouses_on_board,
         parch = parent_children_on_board) %>%
  mutate(age = replace(age, age %in% c("//", "ND"), NA), #in age ci sono degli NA scritti e dei //
         age = as.numeric(age),
         sex = if_else(sex %in% c("Male", "male_"), "male", sex),
         across (c("sex", "embarked", "pclass"), as.factor)) %>%
  group_by(pclass, sex) %>% view()
  # summarise(Surv = round(mean(survived), 2), #media dei sopravvissuti perché è 0/1
  #           n= n()) %>% 
  # mutate(Surv = paste0(Surv, " ", "(", n, ")")) %>%
  # select (-n) %>%
  # pivot_wider(names_from = "sex", values_from = "Surv") %>% view()

#Ctrl+Shift+C per commentare blocco di righe

# na.omit() per escludere righe con degli NA

view(who) #dataset del WHO su casi tubercolosi, voglio separare colonne che hanno molte info

library(stringr)

who %>% 
  pivot_longer(
    cols = new_sp_m014:newrel_f65,
    names_to = "key",
    values_to = "cases",
    values_drop_na = T,
  ) %>% 
  mutate(
    key = stringr::str_replace(key, "newrel", "new_rel")
  ) %>% 
 separate(key, c("new", "type", "sexage")) %>%
  select(-new, -iso2, -iso3) %>% 
  separate(sexage, c("sex", "age"), sep = 1) %>%
  group_by(country, year) %>% view()
  # summarise(totcasi = sum(cases)) %>%
  # arrange(year) %>%
  # pivot_wider(names_from = "year", values_from = "totcasi", values_fill = 0) %>% view()
  #values_fill sostituisce con 0 gli NA



# Data visualization - Grafici --------------------------------------------

#useremo ggplot2, pacchetto forse più vecchio del tidyverse
#implementa idea di "grammatica della grafica", una sorta di regolamento per fare grafici
# a graphic maps data to the aesthetic attributes (colour, shape, size) of geometric objects
#(points, lines, bars).
#grafici vengono fatti a strati, aggiungendo strati con un +
#ggplot(dati) [oppure dati %>% ggplot() +...] + aes(x,y..) + geom_...(aes(...)) + facet_...(...~...) + theme(...)
#I dati devono essere in formato long

#c'è anche pacchetto patchwork che serve per aggregare elementi differenti



mouse <- read_excel("dati/SPERIMENTAZIONE prova132.xlsx", 
                                       range = "A2:T22", col_types = c("text", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "numeric", 
                                                                       "numeric", "numeric", "skip", "skip"))
view(mouse)

#prima cosa è togliere le righe vuote

dt <- mouse %>% 
  filter(!row_number() %in% c(1,3,7,11,16)) %>% 
  mutate(...1 = if_else(is.na(...1),"idmouse", ...1)) %>%
  row_to_names(row_number = 1) %>% 
  pivot_longer(2:18, names_to = "days", values_to = "weight") %>% 
  mutate(gruppi = ifelse(str_detect(idmouse, "1",), "A",
                         ifelse(str_detect(idmouse, "2",),"B",
                                ifelse(str_detect(idmouse, "3",),"C", "Controllo"))))

view(dt)
p <- ggplot(dt) +
  aes(x = weight) +
  #geom_point()
   #geom_jitter(height=0.3) +  #li distribuisce in verticale senza particolare senso, posso allargare o stringere ma non ha valenza, utile per vedere se ci sono addensamenti in certe aree della scala
  # geom_histogram(aes(y=..density..), bins = 20, col="blue", fill="lightgrey") + #bins per modificare numero di barre
  # geom_rug()
  # geom_density(adjust= 1/3)
  
  # geom_boxplot(aes(y=""), color = "navy", fill = "lightblue", width =0.3) + 
  # geom_jitter(aes(y=""), height = 0.2,
  #             color = ifelse(dt$weight== max(dt$weight, na.rm = T), "red",
  #                             ifelse(dt$weight== min(dt$weight, na.rm = T), "blue","black"))) #gli faccio colorare massimo e minimo differenti
  
  geom_violin(aes(y=""), scale="width")+
  geom_hline(yintercept = 1) + #evidenzia come violin plot deriva da density curve
  ggforce::geom_sina(aes(y="")) +
  geom_boxplot(aes(y=""), width=0.2)
 

dt <- dt %>% mutate(days = as.numeric(days))
dt <- dt %>% mutate(days = as.factor(days))


p1 <- ggplot(dt)+
  aes(x=days, y=weight)+
  geom_point()+
  geom_line() #queste linee sono per giorno, ma non mi interessano... le voglio per singolo topo nel tempo

p2 <- ggplot(dt)+
  aes(x=days, y=weight, col=gruppi)+
  geom_point() +
  geom_line(aes(group = idmouse)) # mancano osservazioni ai giorni 4 e 11 perché domeniche. potrei toglierle o inserire medie dei gironi prima e dopo


# voglio fare pannelli per avere sia punti che linee per andamento tra i vari gruppi

p3 <- ggplot(dt)+
  aes(x=days, y=weight, col=gruppi)+
  geom_point() +
  geom_line(aes(group = idmouse)) + 
  facet_wrap(.~gruppi)

p4 <- ggplot(dt)+
  aes(x=days, y=weight, col=gruppi)+
  geom_point() +
  geom_line(aes(group = idmouse)) + 
  facet_wrap(.~gruppi)+
  theme(legend.position = "none")

p5 <- ggplot(dt)+
  aes(x= as.numeric(days), y=weight, col=gruppi)+ #devo dargli i giorni come numeri per fare la regressione lineare!
  geom_point( alpha = 0.2) +
  geom_line(aes(group = idmouse), alpha = 0.2) + #alpha è per trasparenza
  facet_wrap(.~gruppi, scales = "free")+
  geom_smooth(se = F, method = "lm") + #lm è linear model, ma non gli faccio mettere l'errore standard che avrebbe di default
  theme(legend.position = "none")
  
#tolgo i pannelli per confrontare le pendenze delle curve
  
p6 <- ggplot(dt)+
  aes(x= as.numeric(days), y=weight, col=gruppi)+ #devo dargli i giorni come numeri per fare la regressione lineare!
  geom_point( alpha = 0.2) +
  #geom_line(aes(group = idmouse), alpha = 0.2) + #alpha è per trasparenza
  #facet_wrap(.~gruppi, scales = "free")+
  geom_smooth(se = F, method = "lm") + #lm è linear model, ma non gli faccio mettere l'errore standard che avrebbe di default
theme(legend.position = "bottom")

p7 <- ggplot(dt)+
  aes(x= as.numeric(days), y=weight, col=gruppi)+ #devo dargli i giorni come numeri per fare la regressione lineare!
  #geom_point( alpha = 0.2) +
  #geom_line(aes(group = idmouse), alpha = 0.2) + #alpha è per trasparenza
  #facet_wrap(.~gruppi, scales = "free")+
  geom_smooth(se = F, method = "lm") + #lm è linear model, ma non gli faccio mettere l'errore standard che avrebbe di default
  theme(legend.position = "bottom")

library(patchwork)

((p1+p2)/
    (p5+p6))|p7


p8 <- p7 +
  labs(
    title = "Effetto della dieta sull'accrescimento dei topi neonati",
    subtitle = "confronto tra quattro tipi di diete",
    caption = "le linee rappresentano le rette di regressione del peso nei giorni di osservszione per i differenti gruppi di dieta",
    y = "Weight (gr)",
    x = "Days")+
  xlim(0,max(as.numeric(dt$days)))+
  ylim(0,max(as.numeric(dt$days)))+
  scale_x_continuous(breaks = c(1:17))+
  theme_bw() + 
  
  #modificare gli elementi del testo
  
  theme(
    plot.title = element_text(colour = "blue", size = 18),
    plot.subtitle = element_text(size = 16),
    legend.title = element_blank(),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    axis.text = element_text(size=14))


gruppi <- c(A= "Gruppo A: Dieta iperproteica",
            B= "Gruppo B: Dieta ipoproteica",
            C= "Gruppo C: Dieta ipoproteica + intergatore",
            Controllo = "Gruppo di controllo: dieta normoproteica")

p9 <- ggplot(dt)+
  aes(x= as.numeric(days), y=weight, col=gruppi)+ #devo dargli i giorni come numeri per fare la regressione lineare!
  geom_point( alpha = 0.2) +
  geom_line(aes(group = idmouse), alpha = 0.2) + #alpha è per trasparenza
  facet_wrap(.~gruppi, scales = "free", labeller = labeller(gruppi = gruppi))+ #non funziona labeller, forse sintassi sbagliata
  geom_smooth(se = F, method = "lm") + #lm è linear model, ma non gli faccio mettere l'errore standard che avrebbe di default
  theme(legend.position = "none")
  
#r-graph-gallery.com













  