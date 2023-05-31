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

clean_names(titanic) #pulisce nomi mettendo tutto in minuscolo e sostituendo spazi con _
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
titanic <- clean_names(titani)

titanic %>%
  clean_names() %>% 
  #rename(sblp = `siblings and spouses on board`) #dà errore dovuto al clean names, meglio farlo prima
  rename(sblp = siblings_and_spouses_on_board,
         parch = parent_children_on_board) %>%
  mutate(age = replace(age, age %in% c("//", "ND"), NA), #in age ci sono degli NA scritti e dei //
         age = as.numeric(age),
         sex = if_else(sex %in% c("Male", "male_"), "male", sex),
         across (c("sex", "embarked", "pclass"), as.factor)) %>%
  group_by(pclass, sex) %>%
  summarise(Surv = round(mean(survived), 2), #media sei sopravvissuti perché è 0/1
            n= n()) %>% 
  mutate(Surv = paste0(Surv, " ", "(", n, ")")) %>%
  select (-n) %>%
  pivot_wider(names_from = "sex", values_from = "Surv") %>% view()

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
  group_by(country, year) %>%
  summarise(totcasi = sum(cases)) %>%
  arrange(year) %>% 
  pivot_wider(names_from = "year", values_from = "totcasi", values_fill = 0) %>% view()
  #values_fill sostituisce con 0 gli NA









