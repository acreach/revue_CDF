
# chargement packages ####
library(tidyverse)
library(rvest)
library(magrittr)
library(lubridate)
library(stringr)
library(stringi)


# liste de toutes les saisons ####
liste_saisons <-
  read_html('http://www.deux-zero.com/index.php?Code=L1&Item=Stades&Edition=1999-2000') %>%
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "Select12", " " )) and (((count(preceding-sibling::*) + 1) = 3) and parent::*)]/option') %>%
  html_text() %>%
  stringr::str_replace_all("              \n       ", "")


# fontion : à partir d'une page saison, recupération des tous les stades possibles ####

get_liste_url_stadeXsaison <- function(saison){
  stades.S <- paste0('http://www.deux-zero.com/index.php?Code=L1&Item=Stades&Edition=',saison) %>%
    read_html() %>% 
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "Txt11", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "Select12", " " ))]/option') %>%
    html_attr("value") %>%
    discard(~ .x == 0)
  # pause
  Sys.sleep(sample(2, 1))
  url <- paste0('http://www.deux-zero.com/index.php?Code=L1&Item=Stades&Edition=',saison,'&StadeId=',stades.S)
}

# liste des urls des pages stadeXsaison ####
liste_url_stadeXsaison <-
  liste_saisons %>%
  map(get_liste_url_stadeXsaison) %>%
  unlist()



# fonction : récupération de toutes les infos d'une page stadeXsaison ####
get_infos_matchs_stade <- function(url){

# tip https://stackoverflow.com/questions/31615435/rvest-extract-option-value-and-text-from-select

# championnat
nom_championnat <- url %>%
  read_html() %>%
  html_nodes("td td td .Titre") %>%
  html_text()

# saison
saison <- url %>%
  read_html() %>%
  # xpath du menu dropdown suivi de /option cf code html
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "Select12", " " )) and (((count(preceding-sibling::*) + 1) = 3) and parent::*)]/option') %>%
  # on garde uniquement l'élément sélectionné
  keep( ~ grepl("selected", .x)) %>%
  html_text() 

# nom stade
nom_stade <- url %>%
  read_html() %>%
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "Txt11", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "Select12", " " ))]/option') %>%
  keep( ~ grepl("selected", .x)) %>%
  html_text() 

# capacité stade
capacite_stade <- url %>%
  read_html() %>%
  html_nodes("td tr:nth-child(1) td:nth-child(1) tr:nth-child(1) .Txt11:nth-child(6)") %>%
  html_text() %>%
  stringr::str_replace_all(" places", "") %>%
  as.numeric()

# journée championnat
journee <- url %>%
  read_html() %>%
  html_nodes('.Txt12:nth-child(1)') %>%
  html_text() %>%
  str_extract("\\(?[0-9,.]+\\)?") %>%
  as.numeric()

# date match
horaire <- url %>%
  read_html() %>%
  html_nodes('font') %>%
  html_text() %>%
  str_replace_all("[a-z]", "") %>%
  trimws() %>%
  as.POSIXct( format="%d-%m-%Y %H:%M") 

# club_domicile
club_domicile <- url %>%
  read_html() %>%
  html_nodes('.Txt12Droit') %>%
  html_text()

# club visiteur
club_visiteur <- url %>%
  read_html() %>%
  html_nodes('.Txt12:nth-child(5)') %>%
  html_text()

#affluence
affluence <- url %>%
  read_html() %>%
  # uniquement bloc données tableau
  #html_nodes('.SousCadre') %>%
  #html_nodes(':nth-child(8)') %>%
  # correction pour matchs reportés
  html_nodes('.Txt12Centre:nth-child(7)') %>%
  html_text() %>%
  discard(~ .x == "\n        ") %>%
  #discard(~ is.na(x)) %>%
  as.numeric()

#url fiche match
url_IDmatch <-  url %>%
  read_html() %>%
  html_nodes('.LigneMatch') %>%
  html_attr("onclick") %>%
  stringr::str_replace_all("document.location.href=", "") %>%
  stringr::str_replace_all("'|;", "")

# pause
Sys.sleep(20)

# df
tibble(nom_championnat = nom_championnat,
       saison = saison,
       nom_stade = nom_stade,
       capacite_stade = capacite_stade, 
       journee = journee, 
       horaire = horaire, 
       club_domicile = club_domicile, 
       club_visiteur = club_visiteur,
       affluence = affluence,
       url_IDmatch = url_IDmatch
)
}


# récupération de l'ensemble des infos match ####
df_stades_matchs <- map_df(liste_url_stadeXsaison, get_infos_matchs_stade) %>%  filter(!url_IDmatch %in% '')


# feuille de match ####


# fonction pour récupérer toutes les infos d'une page stadeXsaison ####
get_infos_matchs_detail <- function(url){
  
  page <- read_html(paste0('http://www.deux-zero.com/',url)) 
  
  # tip https://stackoverflow.com/questions/31615435/rvest-extract-option-value-and-text-from-select
  
  # score final
  score_final <- page %>%
    html_nodes(".Score") %>%
    html_text()
  
  score_final_club_domicile <- strsplit(score_final, split="-")[[1]][1]
  score_final_club_visiteur <- strsplit(score_final, split="-")[[1]][2]
  
  # score MT
  score_MT <- page %>%
    html_nodes(".ScoreMiTemps") %>%
    html_text() %>%
    stringr::str_replace_all("\\(|\\)", "")
  
  score_mt_club_domicile <- strsplit(score_MT, split="-")[[1]][1]
  score_mt_club_visiteur <- strsplit(score_MT, split="-")[[1]][2]
  
  # arbitre
  arbitre <- page %>%
    #html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "Score", " " ))]') %>%
    #html_nodes("tr:nth-child(22) .Txt12") %>%
    html_nodes(".Txt12") %>%
    keep( ~ grepl("Arbitre", .x)) %>%
    html_text() %>%
    #stringr::str_replace_all("Arbitre principal", "") %>%
    stringr::str_replace_all("Arbitre|principal|:", "") %>%
    str_trim()

  # diffusion
  diffusion <- page %>%
    html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "Txt12", " " ))]//img') %>%
    html_attr("title") %>% 
    # renvoi NA si pas d'info, sinon ça ne retourne rien
    {if(length(.) == 0) NA else .}
  
  # minute_but <- page %>%
  # html_nodes(".Txt11Droit") %>%
  #   #keep( ~ .x == "") %>%
  #   keep( ~ grepl("'", .x)) %>%
  #   html_text() 
  #   
  # type_but <- page %>%
  #   #html_nodes(":nth-child(2)") %>%
  #   html_nodes(".Txt11Centre") %>%
  #   #html_nodes(xpath = '//tr[(((count(preceding-sibling::*) + 1) = 12) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "Txt11Droit", " " )) and (((count(preceding-sibling::*) + 1) = 1) and parent::*)]') %>%
  #   #html_attr("title")
  #   keep( ~ grepl("alt", .x)) %>%
  #   html_attr("img src")
  # 
  # buteur_but <- page %>%
  #   #html_nodes(':nth-child(4)') %>%
  #   html_nodes('.Txt11:nth-child(4)') %>%
  #   #discard( ~ .x == "") %>%
  #   html_text()
  
  # pause
  Sys.sleep(4)
  
  # df
  tibble(url = url,
         score_final_club_domicile = score_final_club_domicile,
         score_final_club_visiteur = score_final_club_visiteur,
         score_mt_club_domicile = score_mt_club_domicile, 
         score_mt_club_visiteur = score_mt_club_visiteur, 
         arbitre = arbitre, 
         diffusion = diffusion#,
         #minute_but = minute_but,
         #type_but=type_but,
         #buteur_but = buteur_but
  )
}

df_stades_matchs_detail <- map_df(df_stades_matchs$url_IDmatch, get_infos_matchs_detail)  %>%
  mutate_at(.vars = c(2:5), .funs =as.numeric) %>%
  mutate_if(is.numeric, funs(ifelse(is.na(.), 0, .)))


#######################
### scraping classement journée
#######################

url <- 'http://www.deux-zero.com/index.php?Code=L1&Item=Classement&Edition=2016-2017&Type=1&JInit=1&JFin=5'
S <- '1990-1991'
J <- 10


# fonction pour récupérer toutes les infos d'une page classement journeeXsaison #####
get_infos_classement_journee <- function(S,J){
  
  url <- paste0('http://www.deux-zero.com/index.php?Code=L1&Item=Classement&Edition=',S,'&Type=1&JInit=1&JFin=',J)
  
  # championnat
  position <- url %>%
    read_html() %>%
    #html_nodes( xpath ='//*[contains(concat( " ", @class, " " ), concat( " ", "LigneClassement", " " ))]//td[(((count(preceding-sibling::*) + 1) = 1) and parent::*)]') %>%
    html_nodes(".LigneClassement td:nth-child(1)") %>%
    html_text() 
    
    # read_html() %>%
    #   # xpath du menu dropdown suivi de /option cf code html
    #   html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "Select12", " " )) and (((count(preceding-sibling::*) + 1) = 3) and parent::*)]/option') %>%
    #   # on garde uniquement l'élément sélectionné
    #   keep( ~ grepl("selected", .x)) %>%
    #   html_text() 
  
  # image .LigneClassement:nth-child(3) img
  
  nom_club <- url %>%
    read_html() %>%
   # html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "Txt11", " " )) and (((count(preceding-sibling::*) + 1) = 4) and parent::*)] | //*[contains(concat( " ", @class, " " ), concat( " ", "FlagPlaceLCb_1992", " " ))]//*[contains(concat( " ", @class, " " ), concat( " ", "LigneClassement", " " )) and (((count(preceding-sibling::*) + 1) = 3) and parent::*)]//*[contains(concat( " ", @class, " " ), concat( " ", "FlagPlaceLC_1992", " " ))]') %>%
    html_nodes( ".Txt12Gras") %>%
    html_text()
  
  nb_points <- url %>%
    read_html() %>%
    #html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "Txt11Droit", " " )) and (((count(preceding-sibling::*) + 1) = 5) and parent::*)]') %>%
    html_nodes( ".Txt12Droit:nth-child(4)") %>%
    html_text()
  
  nb_matchs <- url %>%
    read_html() %>%
    #html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "Txt11Droit", " " )) and (((count(preceding-sibling::*) + 1) = 6) and parent::*)]') %>%
    html_nodes( ".Txt12Droit:nth-child(5)") %>%
    html_text()
  
  nb_victoires <- url %>%
    read_html() %>%
    #html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "Txt11Droit", " " )) and (((count(preceding-sibling::*) + 1) = 7) and parent::*)]') %>%
    html_nodes( ".Txt12Droit:nth-child(6)") %>%
    html_text()
  
  nb_nuls <- url %>%
    read_html() %>%
    #html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "Txt11Droit", " " )) and (((count(preceding-sibling::*) + 1) = 8) and parent::*)]') %>%
    html_nodes( ".Txt12Droit:nth-child(7)") %>%
    html_text()
  
  nb_defaites <- url %>%
    read_html() %>%
    #html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "Txt11Droit", " " )) and (((count(preceding-sibling::*) + 1) = 9) and parent::*)]') %>%
    html_nodes( ".Txt12Droit:nth-child(8)") %>%
    html_text()
  
  nb_buts_pour <- url %>%
    read_html() %>%
    #html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "Txt11Droit", " " )) and (((count(preceding-sibling::*) + 1) = 10) and parent::*)]') %>%
    html_nodes( ".Txt12Droit:nth-child(9)") %>%
    html_text()
  
  nb_buts_contre <- url %>%
    read_html() %>%
    #html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "Txt11Droit", " " )) and (((count(preceding-sibling::*) + 1) = 11) and parent::*)]') %>%
    html_nodes( ".Txt12Droit:nth-child(10)") %>%
    html_text()
  
  nb_buts_diff <- url %>%
    read_html() %>%
    #html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "Txt11Droit", " " )) and (((count(preceding-sibling::*) + 1) = 12) and parent::*)]') %>%
    html_nodes( ".Txt12Droit:nth-child(11)") %>%
    html_text()
  
  serie_resultats <- url %>%
    read_html() %>%
    #html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "Txt11Droit", " " )) and (((count(preceding-sibling::*) + 1) = 13) and parent::*)]') %>%
    html_nodes( ".Txt12Droit:nth-child(12)") %>%
    html_text()
  
saison <- rep(S, length(position))
journee <- rep(J, length(position))

  # pause
  Sys.sleep(4)
  
  # df
  tibble(saison = saison,
         journee = journee,
         nom_club = nom_club,
         position = position,
         nb_points = nb_points,
         nb_matchs = nb_matchs,
         nb_victoires = nb_victoires,
         nb_nuls = nb_nuls,
         nb_defaites = nb_defaites,
         nb_buts_pour = nb_buts_pour,
         nb_buts_contre = nb_buts_contre,
         nb_buts_diff = nb_buts_diff,
         serie_resultats = serie_resultats
  )
}

get_infos_classement_journee.s <- safely(get_infos_classement_journee)
df_par <- expand.grid(liste_saisons,seq(1,38,1))

histo_classements <- map2(df_par$Var1,df_par$Var2, 
                   ~ get_infos_classement_journee.s(.x,.y)) %>%
  map("result") %>%
  compact() %>%
  reduce(bind_rows) 

## nettoyage du fichier classements ####

library(stringr)
histo_classements <-
  histo_classements %>%
  mutate(serie_resultats.un = toupper( str_sub(serie_resultats,-1,-1)))

# oups , on enleve les journees 35 à 38 pour les saisons  1997-1998 à 2001-2002

histo_classements <- histo_classements %>%
  filter(!(saison %in% '1997-1998' & journee > 34)) %>%
  filter(!(saison %in% '1998-1999' & journee > 34)) %>%
  filter(!(saison %in% '1999-2000' & journee > 34)) %>%
  filter(!(saison %in% '2000-2001' & journee > 34)) %>%
  filter(!(saison %in% '2001-2002' & journee > 34)) 



library(zoo)
histo_classements <- histo_classements %>% 
  mutate(position.dedup = ifelse(position %in% '-', NA, position)) %>%
  group_by(saison, journee) %>% mutate(position.dedup=zoo::na.locf(position.dedup)) %>%
  ungroup() %>% as.data.frame() %>%
  mutate_at(.vars =c(5:12,14), .funs = as.numeric)



