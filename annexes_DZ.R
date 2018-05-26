
# petites corrections
df_stades_matchs <- df_stades_matchs %>%
  mutate(saison = stringr::str_replace_all(saison,"              \n       ", ""))

# correspondance nom club
corresp_nomclubs <- tribble(
  ~ nom_club_old, ~ nom_club_actuel,  
  'AS Monaco FC', 'AS Monaco',
  'ATAC Troyes', 'ESTAC Troyes',
  "Évian Thonon Gaillard FC ", 'Évian Thonon Gaillard FC',
  'FC Nantes Atlantique', 'FC Nantes',
  'Girondins de Bordeaux FC', 'FC Girondins de Bordeaux',
  'RC Strasbourg', 'RC Strasbourg Alsace',
  'SCO Angers', 'Angers SCO',
  'US Valenciennes-Anzin', 'Valenciennes FC',
  'Brest Armorique FC', 'Stade Brestois 29')

liste_clubs <- df_stades_matchs %>% distinct(club_visiteur_actuel)

# initiales clubs
corresp_initialesclubs <- tribble(
  ~ nom_club_actuel, ~ initiales_nom_club_actuel,  
  "Angers SCO" , "SCO",
  "OGC Nice", "OGCN",
  "Olympique de Marseille", "OM",
  "AS Monaco", "ASM",
  "Lille OSC", "LOSC",
  "Dijon FCO" ,"DFCO",              
  "Olympique Lyonnais","OL",
  "FC Nantes" ,"FCN",
  "FC Girondins de Bordeaux", "FCGB",
  "FC Metz" ,"FCM",
  "Toulouse FC" ,"TFC",
  "Paris Saint-Germain FC" , "PSG",
  "Stade Rennais FC" ,"SRFC",
  "Montpellier Hérault SC", "MHSC",
  "ESTAC Troyes","ESTAC",
  "EA Guingamp" ,"EAG",
  "AS Saint-Etienne", "ASSE",
  "RC Strasbourg Alsace","RCS",
  "Amiens SC" ,"AMSC",
  "SM Caen","SMC",
  "FC Lorient" ,"FCL",
  "AS Nancy Lorraine" , "ASNL",
  "SC Bastia","SCB",
  "Stade de Reims" ,"SR",
  "GFC Ajaccio","GFCA",
  "Évian Thonon Gaillard FC", "ETGFC",
  "RC Lens","RCL",
  "Valenciennes FC", "VAFC",
  "FC Sochaux-Montbéliard","FCSM",
  "AC Ajaccio", "ACA",
  "Stade Brestois 29" , "SB29",
  "AJ Auxerre", "AJA",
  "AC Arles Avignon" , "ACAA",
  "Grenoble Foot 38" ,"GF38",
  "US Boulogne CO" ,"USB",
  "Le Mans UC 72" , "MUC72",
  "Havre AC","HAC",
  "CS Sedan" ,"CSSA",
  "FC Istres", "FCI",
  "LB Châteauroux" ,"LBC",
  "AS Cannes" ,"ASC",
  "FC Gueugnon" ,"FCG",
  "FC Martigues" ,"FCMA",
  "SC Toulon et du Var", "SCT",
  "Nîmes Olympique", "NO",
  "Racing Paris 1" ,"RCF",
  "FC Mulhouse", "FCMU")

# import logos clubs
corresp_initialesclubs <- corresp_initialesclubs %>%
 mutate(logo_png = paste0("./logos_clubs/",initiales_nom_club_actuel,".png"))

# img_logos <- 'http://www.deux-zero.com/index.php?Code=L1&Item=Participations&EquipeId=0' %>%
#   read_html() %>% 
#   html_nodes('.Txt11Centre img') %>%
#   html_attr("src") %>%
#   stringr::str_replace_all("Logos/Fanions/Pictos/", "")
#   
# dl_logo <- function(nom){
#   url <- paste0('http://www.deux-zero.com/Logos/Fanions/Pictos/',nom)
#     download.file(url, destfile = basename(nom))
# }
# #download.file(paste0("http://www.deux-zero.com/", imgsrc), destfile = basename(imgsrc))
# img_logos %>%
#   map(dl_logo)

# rectification du df classements ####

histo_classements <- histo_classements %>%
  left_join(corresp_nomclubs, by = c('nom_club' = "nom_club_old")) %>%
  mutate(nom_club_actuel = ifelse(!is.na(nom_club_actuel),nom_club_actuel, as.character(nom_club))) %>%
  left_join(corresp_initialesclubs, by = c('nom_club_actuel'= 'nom_club_actuel'))


# choper liste club

liste_clubs <-  'http://www.lfp.fr/LFPStats/stats_possession?competition=D1&saison=101&club=&lieu=G&butsME=M' %>%
  read_html() %>%
  html_nodes('.club a')

img_logos <- 'http://www.lfp.fr/club/stade-rennais-fc' %>%
  read_html() %>%
  html_nodes('.logo_club')
  html_nodes(xpath = '//*[contains(concat( " ", @class, " " ), concat( " ", "club", " " ))]//img') %>%
  html_attr("src") %>%
  stringr::str_replace_all("Logos/Fanions/Pictos/", "")




# modifs capacité stade
df_stades_matchs <- df_stades_matchs %>%
  mutate(capacite_stade = ifelse(nom_stade %in% 'Parc des Sports (Avignon)', 17518,as.numeric(capacite_stade) ))

# géocodage stades
liste_stades <-
  df_stades_matchs %>% ungroup() %>% as.data.frame() %>% 
  distinct(nom_stade) %>%
  separate(nom_stade, into = c("stade", "ville"), sep ="\\(", remove=F) %>%
  mutate(ville = str_replace(ville,"\\)",""))

# # correspondance nom stade
# corresp_nomstades <- tribble(
#   ~ nom_stade_old, ~ nom_stade_actuel,  
#   'Stade Jean Bouin', 'Stade Raymond Kopa',
#   'Nouveau Stade Bordeaux', 'Matmut Atlantique',
#   'Stade Jacques Chaban-Delmas', 'Matmut Atlantique',
#   'Parc Lescure', 'Matmut Atlantique',
#   'Stade de Venoix' = "Stade Michel d'Ornano",
#   'Parc Olympique Lyonnais' = "Groupama Stadium",
#   'Groupama Stadium' = 'Stade Municipal de Gerland',
#   )


library(banR)
liste_stades_geoc <-
geocode_tbl(tbl = liste_stades , adresse = ville) %>%
  glimpse() 

liste_stades_geoc <- liste_stades_geoc %>%
  select(nom_stade, stade, ville,latitude,longitude ) %>%
  mutate(latitude = ifelse(ville %in% 'Monaco', 43.727493,as.numeric(latitude)),
         longitude = ifelse(ville %in% 'Monaco', 7.416153,as.numeric(longitude)))


