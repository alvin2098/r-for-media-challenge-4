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
library(purrr)
library(tidyr)
library(jsonlite)

### --- Load JSON data, unnest politicians --- ### 
bt2 <- list.files("./data/", full.names = T) %>% 
  map_dfr(fromJSON) %>% 
  as_tibble() %>% 
  unnest(c(abgeordnete)) %>% 
  mutate(part0et, .keep = "unused")

### --- Average political party age --- ###
ans1 <- bt2 %>% 
  group_by(fraktion) %>% 
  summarise(avg_age = 2021 - mean(lebensdaten)) %>% 
  arrange(desc(avg_age))
  
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
