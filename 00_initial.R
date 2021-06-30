# Verkefni til ad bera saman innflutning & utflutning a gardyrykjuvorum
# thetta forrit sameinar gognin fra HAX og Eurostat i eitt skjal
# Asamt thvi ad sameina thad vid gogn ur tollskra um hvort numerin bera toll eda
# ekki. 
############## 00 LOAD PACKAGES #########################
library(pxweb)
library(data.table)
library(stringr)
library(tidyr)
library(dplyr)
library(readr)

############## 01 LOAD HAX DATA ###################
# Kodi til ad na i gogn fra hagstofu

# bua til query - hledur inn json skjali ur repoinu
pxq <- pxweb_query("hax_2018_2019_6_7.json")

# thetta saekir gognin fra hagstofu
pxd <- pxweb_get("https://px.hagstofa.is:443/pxis/api/v1/is/Efnahagur/utanrikisverslun/1_voruvidskipti/03_inntollskra/UTA03801.px",
                 pxq)
# breyta yfir i data.frame
pxdf <- as.data.frame(pxd, column.name.type = "text", variable.value.type = "text")

# breyta yfir i data.table thar sem thette er stor tafla
HAX_2019 <- pxdf
#henda df
rm(pxdf)
# til ad na i metadata med toflunni
# pxdc <- pxweb_data_comments(pxd)
# pxdc

#breyta colnames i eitthvad styttra
colnames(HAX_2019) <- c("toll_langt", "land", "Manudur", "Eining", "gildi")


# vista skjalid
save(HAX_2019, file = "hax_innflutn.Rdata")

## Saekja 2012- 2016
# thad tharf ad na i thrju mismunandi skjol
# vegna breytinga a tollskra og hja HAX
# 2012-2016 eru ekki gerd upp eftir manudum, heldur arum - 
# tharfnast thvi urvinnslu til ad geta steypt saman
# Einnig vegna breytinga a tollanumerum er bara haegt ad nota 6 stafa numerin
pxq <- pxweb_query("C:/Users/kari/github_rep/gardyrkja/HAX_2012_2016_6_7.json")

# thetta saekir gognin fra hagstofu
HAX_12_16 <- pxweb_get("https://px.hagstofa.is:443/pxis/api/v1/is/Efnahagur/utanrikisverslun/1_voruvidskipti/06_tollskrarnumereldra/UTA13821.px",
                       pxq)
# breyta yfir i data.frame
HAX_12_16 <- as.data.frame(HAX_12_16, column.name.type = "text", variable.value.type = "text")
# breyta yfir i data.table thar sem thette er stor tafla

#breyta colnames i eitthvad styttra
colnames(HAX_12_16) <- c("toll_langt", "land", "year", "Eining", "gildi")
save(HAX_12_16, file="HAX_12_16.Rdata")

# Saekja 2017-2018
pxq <- pxweb_query("HAX_2017_2018_6_7.json")

# thetta saekir gognin fra hagstofu
HAX_17_18 <- pxweb_get("https://px.hagstofa.is:443/pxis/api/v1/is/Efnahagur/utanrikisverslun/1_voruvidskipti/06_tollskrarnumereldra/UTA13811.px",
                       pxq)
# breyta yfir i data.frame
HAX_17_18 <- as.data.frame(HAX_17_18, column.name.type = "text", variable.value.type = "text")
# breyta yfir i data.table thar sem thette er stor tafla

#breyta colnames i eitthvad styttra
colnames(HAX_17_18) <- c("toll_langt", "land", "year", "Eining", "gildi")
save(HAX_17_18, file="HAX_17_18.Rdata")

############## 02 EDIT HAX DATA ###################################################
# FLOKKAR I RETTA TOLLFLOKKA
# MERGEAR TOLLSKILMALUM

load("hax_innflutn.Rdata")
load("HAX_12_16.Rdata")
load("HAX_17_18.Rdata")
# splitta tollnumerinu fra nafninu
HAX_2019 <-
  HAX_2019 %>% separate(toll_langt, c("toll_nr", "toll_nafn"), sep = "(?<=\\d)(-)") 
HAX_2019 <- HAX_2019 %>% mutate(samraemd_toll_nr = str_extract(pattern="\\d{6}", string=toll_nr))

HAX_12_16 <- HAX_12_16 %>% separate(toll_langt, c("toll_nr", "toll_nafn"), sep = "(?<=\\d)(-)") 
# samraema toll nr
HAX_12_16 <- HAX_12_16 %>% mutate(samraemd_toll_nr = str_extract(pattern="\\d{6}", string=toll_nr))

HAX_17_18 <- HAX_17_18 %>% separate(toll_langt, c("toll_nr", "toll_nafn"), sep = "(?<=\\d)(-)") 
HAX_17_18 <- HAX_17_18 %>% mutate(samraemd_toll_nr = str_extract(pattern="\\d{6}", string=toll_nr))


############## 03 SAMEINA TOLLSKRA VID HAX GOGN #################
# Fra tollinum fekkst skjal med ollum tollskrarnumerum
# thad var einfaldad og flokkad eftir thvi hvort CN-8 numer innan HS-6 
# bera toll, bera toll hluta ur ari eda bera ekki toll
# Baeta thurfti vid akv. tollnumerum vid utskrift fra RSK vegna thess
# ad nokkrir (n=22) CN numer fellu ur gildi vid aramotin 2019/2020


tollar_ESB <- read_delim("tollar_final.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE)
tollar_ESB <- transform (tollar_ESB, "toll_nr" = paste0("0",tollar_ESB$toll_nr))

HAX_2019 <- merge(HAX_2019, tollar_ESB, by.x = "toll_nr", by.y="toll_nr", all.x = T)
HAX_12_16 <- merge(HAX_12_16, tollar_ESB, by.x = "toll_nr", by.y="toll_nr", all.x = T)
HAX_17_18 <- merge(HAX_17_18, tollar_ESB, by.x = "toll_nr", by.y="toll_nr", all.x = T)
HAX_12_18 <- rbind(HAX_12_16,HAX_17_18)
rm(HAX_12_16, HAX_17_18, tollar_ESB)
############## 04 SAMEINA I EINA TIME SERIES#####################################
# collapse'a arunum svo að serian se eins. Ekki gogn nidurbrotid a manudi 
# fyrir arin 2014-2018
HAX_2019 <- HAX_2019 %>% mutate(year = as.numeric(str_extract(pattern="\\d{4}", string=Manudur)))


HAX_2019 <-
  HAX_2019 %>% group_by(
    toll_nr,
    toll_nafn,
    Eining,
    land,
    year,
    A._.,
    A._Kr,
    B._.,
    B._Kr,
    EF_.,
    EF_Kr,
    samraemd_toll_nr
  ) %>%
  group_trim() %>% summarise(gildi = sum(gildi))

HAX_by_yr_14_19 <- rbind(HAX_12_18,HAX_2019)




############## 05 VISTA SKJOL ###################################################
# HAX_2019 <- HAX_2019[, toll_nr:=as.numeric(toll_nr)]
save(HAX_2019, file = "hax_innflutn.Rdata")
save(HAX_12_18, file="HAX_12_18.Rdata")
save(HAX_by_yr_14_19, file="HAX_by_yr_14_19.Rdata")

############## 06 IMPORT EUROSTAT DATA #############################################
# Hlada inn eurostat gognum - varud mjog leidinlegt vidmot
# link for future ref 
# https://ec.europa.eu/eurostat/web/international-trade-in-goods/data/database

# data for 2014-2019 from eurostat. Chapter 6 and 7
EU_14_19 <- read.csv("C:/Users/kari/github_rep/gardyrkja/eurostat/67_14_19//DS-645593_1_Data.csv",sep=",")
EU_14_19$Value <- gsub(pattern= ",", replacement = "", x= EU_14_19$Value)
EU_14_19$Value <- gsub(pattern= ":", replacement = "0", x= EU_14_19$Value)

EU_14_19 <-EU_14_19 %>% mutate(Value = as.numeric(Value)) 
save(EU_14_19, file="EU_14_19.Rdata")




############## 07 PREPARE EUROSTAT FILES FOR MERGE ################################
# til ad geta merge'ad vid hax, tharf ad collapse'a allri timalinunni
# glatar uppl um manudi

EU_14_19 <- EU_14_19  %>% mutate(year = str_extract(pattern="\\d{4}", string=PERIOD))

# Henda ut numerum sem vantar hja HAX
# thvi sem er hennt ut eru tollflokkar sem eru ekki til stadar hja hax
# i ollum tilfellum er enginn innflutningur a timabilinu fra EU

EU_14_19_export_year <-    EU_14_19 %>% group_by(PRODUCT, year, REPORTER) %>% filter(
  PRODUCT != "TOTAL",  #|
  year != "2021",
  INDICATORS == "QUANTITY_IN_100KG",
  PRODUCT != "060291" ,
  PRODUCT != "060299" ,
  PRODUCT != "060310" ,
  PRODUCT != "060410" ,
  PRODUCT != "060491" ,
  PRODUCT != "060499" ,
  PRODUCT != "070910" ,
  PRODUCT != "070952" ,
  PRODUCT != "070990" ,
  PRODUCT != "071110" ,
  PRODUCT != "071130" ,
  PRODUCT != "071210" ,
  PRODUCT != "071230" ,
  .preserve = TRUE) %>%
  group_trim() %>% summarise(Samtals_utflutt = sum(Value))


save(EU_14_19_export_year, file="EU_14_19_export_year")

############## 08 PREPARE HAX FILES FOR MERGE ##################################
# litid sem tharf ad gera, thar sem collapse'id a arunum gerdist her ad ofan, i 04 

summary_IS_14_20 <-
  HAX_by_yr_14_19 %>% group_by(samraemd_toll_nr, land, year, tollad) %>% filter(
    Eining == "Kíló",
    samraemd_toll_nr !="070930",
    .preserve = TRUE) %>%
  group_trim() %>% summarise(Samtals_innflutt = sum(gildi))
save(summary_IS_14_20, file ="summary_14_20.Rdata")

############## 9 MERGE HAX/EUROSTAT ###########################################
# bua til IDs og merge'a

# fyrsta skref, replace'a islensk nofn a londum
summary_IS_14_20$land <-   str_replace_all(summary_IS_14_20$land, 
                                     c(
                                       "Austurríki" = "AT",
                                       "Belgía" = "BE",
                                       "Búlgaría"= "BG",
                                       "Danmörk"="DK",
                                       "Eistland"="EE",
                                       "Finnland"="FI",
                                       "Frakkland"="FR",
                                       "Grikkland"="GR",
                                       "Holland \\(Niðurland\\)"="NL",
                                       "Írland"="IE",
                                       "Ítalía"="IT",
                                       "Króatía"="HR",
                                       "Kýpur"="CY",
                                       "Lettland \\(Latvija\\)"="LV",
                                       "Litháen \\(Lietuva\\)"="LT",
                                       "Lúxemborg"="LU",
                                       "Malta"="MT" ,
                                       "Portúgal"="PT",
                                       "Pólland"="PL", 
                                       "Rúmenía"="RO",
                                       "Slóvakía"="SK",
                                       "Slóvenía"="SI",
                                       "Spánn"="ES",
                                       "Svíþjóð"="SE",
                                       "Tékkland"="CZ",
                                       "Ungverjaland"="HU",
                                       "Þýskaland" = "DE"
                                     ))
summary_IS_14_20$land <- str_trim(summary_IS_14_20$land, side = "right")

summary_IS_14_20$ID <- paste0(summary_IS_14_20$samraemd_toll_nr,summary_IS_14_20$land,summary_IS_14_20$year)

EU_14_19_export_year$ID <- paste0(EU_14_19_export_year$PRODUCT,EU_14_19_export_year$REPORTER, EU_14_19_export_year$year)

EU_14_19_export_year$ID <- str_trim(EU_14_19_export_year$ID, side = "right")
summary_IS_14_20$ID <- str_trim(summary_IS_14_20$ID, side = "right")

sameinad_stort <- merge(summary_IS_14_20, EU_14_19_export_year, by="ID", all.x = TRUE)

# samraema einingar i inn og utflutningi
sameinad_stort <- transform(sameinad_stort, Samtals_innflutt = Samtals_innflutt/1000)
# eurostat gefur upp einingar i hundrad kiloum (nota s.s. hestburdi)
sameinad_stort <- transform(sameinad_stort, Samtals_utflutt = Samtals_utflutt/1000*100)

# misraemid er leitt ut svona
sameinad_stort$diff <- (sameinad_stort$Samtals_utflutt-sameinad_stort$Samtals_innflutt)

sameinad_stort <- as.data.table(sameinad_stort)
# rm(summary_IS_14_20, EU_14_19,EU_14_19_export_year)
colnames(sameinad_stort)
colnames(sameinad_stort) <-
  c(
    "ID",
    "samraemd_toll_nr",
    "land",
    "year",
    "tollad",
    "Samtals_innflutt",
    "fjöldi",
    "PRODUCT",
    "year.y",
    "REPORTER",
    "Samtals_utflutt",
    "diff"
  )
sameinad_stort$year.y <- NULL
sameinad_stort$PRODUCT <- NULL
save(sameinad_stort, file="sameinad_stort.Rdata")


############## 10 MERGE CUSTOMS 6 DIGIT NAMES TO FILE ####################
sameinad_stort$tollad <- NULL
# CSV SKJAL sem geymir hvort 6 stafa numerin eru tollud eda ekki - einnig lysing
# a theim vorum sem eru i tollnumerinu
tollflokkar <-read.csv2(file="tollflokkar.csv")
tollflokkar$athugasemd <- NULL
tollflokkar$samraemd_toll_nr <- paste0("0",tollflokkar$samraemd_toll_nr)
sameinad_stort <- merge(sameinad_stort, tollflokkar, by = "samraemd_toll_nr")
save(sameinad_stort, file="sameinad_stort.Rdata")
