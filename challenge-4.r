### --- R Challange 4, Alvin Aziz, 13.04.2021 --- ###

# P. Kessling
# Challenge 5: Datensatz wiederzusammenfrickeln
# Wir haben einen Datensatz aus der Wikipedia geladen und ihn als JSON abgelegt.
# Leider hat der fiese Dozent einen Fehler bei der Ausgabe gemacht.
#
# * Lade die Datensätze in `data/`, danke an `map_dfr`
# * Stelle daraus eine *flache* Tabelle her. Die Tabelle soll die folgenden Spalten beinhalten: name, lebensdaten, land, wahlkreis, erststimmen, bemerkungen, fraktion.
# * Berechne das durchschnittliche Alter für die einzelnen Fraktionen und sortiere den Datensatz nach dieser Variable.

library(assertthat)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(jsonlite)

### --- Load JSON data --- ### delete "as tibble" to revert
parteien <- list.files("./data/", full.names = T) %>% 
  map_dfr(fromJSON) %>% 
  as_tibble(
    key = c("abgeordnete", "fraktion"),
    tibble = (list(
      tibble(
        value = c(partei)
      ),
      tibble(
        value = c(abgeordnete)
      )
    ))
  )

### prev ver.
parteien <- list.files("./data/", full.names = T) %>% 
  map_dfr(fromJSON) %>% 
  as_tibble()

colnames(parteien)[2] <- "partei"

### --- Unnest dataset --- ###
bt2 <- parteien %>% 
  unnest(partei) %>% 
  unnest(abgeordnete)
  
# testing unnest, delete to revert
bt2 <- parteien %>% 
  unnest(c(partei, abgeordnete))


### --- Average political party age --- ###
ans1 <- bt2 %>% 
  mutate(age = 2021 - lebensdaten) %>% 
  group_by(partei) %>% 
  summarise(mean(age)) %>% 
  arrange(desc(`mean(age)`))

colnames(ans1)[2] <- "avg_age"
  
if (
  assert_that(
    openssl::md5(paste(map_chr(bt2, paste, collapse = "") , collapse = "")) == "926c50623af03708fa768e0003cc18c6"
  ) &&
  assert_that((
    openssl::md5(paste(map_chr(ans1, paste, collapse = "") , collapse = "")) == "7607c77648da79b8d09ff0d4db41ed0d"
  ))
) {
  writeLines("10/10, gratuliere!")
}
