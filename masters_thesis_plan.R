#' Географічний розподіл соціальних цінностей у Європі: фактори та закономірності
#' Geographical distribution of social values in Europe: factors and patterns
#' Географическое распределение социальных ценностей в Европе: факторы и закономерности


library(drake)

source("functions.R")
source("predef.R")

plan <- drake_plan(
  ess = purrr::map_dfr(list.files(path = "~/social_data_analysis/", pattern = ".sav", full.names = T), 
                       function(x) { 
                         foreign::read.spss(x, to.data.frame = T) %>% as_tibble()
                       }) %>% 
    mutate(
      cntry = as.character(cntry),
      round_year = case_when(
        essround == 1 ~ 2002, essround == 2 ~ 2004,
        essround == 3 ~ 2006, essround == 4 ~ 2008,
        essround == 5 ~ 2010, essround == 6 ~ 2012,
        essround == 7 ~ 2014, essround == 8 ~ 2016
      ),
      region = dplyr::select(.,starts_with("region")) %>% as.matrix() %>% 
        apply(1, function(x) x[!is.na(x)][1]),
      region = ifelse(is.na(region), cntry, region) %>% stringr::str_squish(),
      agea = as.numeric(agea)
    ) %>% 
    shwartz_4() %>% 
    shwartz_10() %>% 
    dplyr::select(
      round_year, cntry, region, agea,
      security, conformity, tradition, benevolence, universalism, universalism,
      self_direction, stimulation, hedonism, achievement, power,
      `Self-Enhancement`, Conservation, `Self-Trancendence`, `Openness to Change`
    ) %>% ess_region(),
  reg = rbind(
    readr::read_rds("~/social_data_analysis/gadm36_NOR_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Akershus" ~ "Oslo and Akershus",
          NAME_1 == "Ãstfold" ~ "South Eastern Norway",
          NAME_1 == "Aust-Agder" ~ "Agder and Rogaland",
          NAME_1 == "Buskerud" ~ "South Eastern Norway",
          NAME_1 == "Finnmark" ~ "Northern Norway",
          NAME_1 == "Hedmark" ~ "Hedmark and Oppland",
          NAME_1 == "Hordaland" ~ "Western Norway",
          NAME_1 == "Møre og Romsdal" ~ "Western Norway",
          NAME_1 == "Nord-Trøndelag" ~ "Trøndelag",
          NAME_1 == "Nordland" ~ "Northern Norway",
          NAME_1 == "Oppland" ~ "Hedmark and Oppland",
          NAME_1 == "Oslo" ~ "Oslo and Akershus",
          NAME_1 == "Rogaland" ~ "Agder and Rogaland",
          NAME_1 == "Sogn og Fjordane" ~ "Western Norway",
          NAME_1 == "Sør-Trøndelag" ~ "Trøndelag",
          NAME_1 == "Telemark" ~ "South Eastern Norway",
          NAME_1 == "Troms" ~ "Northern Norway",
          NAME_1 == "Vest-Agder" ~ "Agder and Rogaland",
          NAME_1 == "Vestfold" ~ "South Eastern Norway"
        )
      ) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_BEL_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Bruxelles" ~ "Brussels region",
          NAME_1 == "Wallonie" ~ "Walloon region",
          NAME_1 == "Vlaanderen" ~ "Flemish region"
        )
      ) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_CHE_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Valais" ~ "Genferseeregion",
          NAME_1 == "Vaud" ~ "Genferseeregion",
          NAME_1 == "Genève" ~ "Genferseeregion",
          NAME_1 == "Bern" ~ "Zentrales Mittelland",
          NAME_1 == "Fribourg" ~ "Zentrales Mittelland",
          NAME_1 == "Jura" ~ "Zentrales Mittelland",
          NAME_1 == "Solothurn" ~ "Zentrales Mittelland",
          NAME_1 == "Neuchâtel" ~ "Zentrales Mittelland",
          NAME_1 == "Aargau" ~ "Nordschweiz",
          NAME_1 == "Basel-Landschaft" ~ "Nordschweiz",
          NAME_1 == "Basel-Stadt" ~ "Nordschweiz",
          NAME_1 == "Uri" ~ "Zentralschweiz",
          NAME_1 == "Schwyz" ~ "Zentralschweiz",
          NAME_1 == "Obwalden" ~ "Zentralschweiz",
          NAME_1 == "Nidwalden" ~ "Zentralschweiz",
          NAME_1 == "Lucerne" ~ "Zentralschweiz",
          NAME_1 == "Zug" ~ "Zentralschweiz",
          NAME_1 == "Ticino" ~ "Tessin",
          NAME_1 == "Zürich" ~ "Nordschweiz",
          NAME_1 == "Uri" ~ "Zentralschweiz",
          NAME_1 == "Sankt Gallen" ~ "Ostschweiz",
          NAME_1 == "Thurgau" ~ "Ostschweiz",
          NAME_1 == "Appenzell Innerrhoden" ~ "Ostschweiz",
          NAME_1 == "Appenzell Ausserrhoden" ~ "Ostschweiz",
          NAME_1 == "Glarus" ~ "Ostschweiz",
          NAME_1 == "Schaffhausen" ~ "Ostschweiz",
          NAME_1 == "Graubünden" ~ "Ostschweiz"
        )
      ) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_CZE_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Jihočeský" ~ "South Bohemia",
          NAME_1 == "Jihomoravský" ~ "South Moravia",
          NAME_1 == "Karlovarský" ~ "Karlovy Vary Reg.",
          NAME_1 == "Kraj Vysočina" ~ "Vysocina",
          NAME_1 == "Královéhradecký" ~ "Hradec Kralove Reg.",
          NAME_1 == "Liberecký" ~ "Liberec Reg.",
          NAME_1 == "Moravskoslezský" ~ "Moravian Silesia Reg.",
          NAME_1 == "Olomoucký" ~ "Olomouc Reg.",
          NAME_1 == "Pardubický" ~ "Pardubice Reg.",
          NAME_1 == "Plzeňský" ~ "Plzen Reg.",
          NAME_1 == "Prague" ~ "Prague",
          NAME_1 == "Středočeský" ~ "Central Bohemia",
          NAME_1 == "Ústecký" ~ "Usti Reg.",
          NAME_1 == "Zlínský" ~ "Zlin Reg."
        )
      ) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_SWE_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Blekinge" ~ "Sydsverige",
          NAME_1 == "Dalarna" ~ "Norra Mellansverige",
          NAME_1 == "Gävleborg" ~ "Norra Mellansverige",
          NAME_1 == "Gotland" ~ "Småland och Öarna",
          NAME_1 == "Halland" ~ "Västsverige",
          NAME_1 == "Jämtland" ~ "Mellemsta Norrland",
          NAME_1 == "Jönköping" ~ "Småland och Öarna",
          NAME_1 == "Kalmar" ~ "Småland och Öarna",
          NAME_1 == "Kronoberg" ~ "Småland och Öarna",
          NAME_1 == "Norrbotten" ~ "Övre Norrland",
          NAME_1 == "Orebro" ~ "Östra Mellansverige",
          NAME_1 == "Östergötland" ~ "Östra Mellansverige",
          NAME_1 == "Skåne" ~ "Sydsverige",
          NAME_1 == "Södermanland" ~ "Östra Mellansverige",
          NAME_1 == "Stockholm" ~ "Stockholm",
          NAME_1 == "Uppsala" ~ "Östra Mellansverige",
          NAME_1 == "Värmland" ~ "Norra Mellansverige",
          NAME_1 == "Västerbotten" ~ "Övre Norrland",
          NAME_1 == "Västernorrland" ~ "Mellemsta Norrland",
          NAME_1 == "Västmanland" ~ "Östra Mellansverige",
          NAME_1 == "Västra Götaland" ~ "Västsverige"
        )
      ) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_EST_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Harju" ~ "Põhja-Eesti",
          NAME_1 == "Hiiu" ~ "Lääne-Eesti",
          NAME_1 == "Ida-Viru" ~ "Kirde-Eesti",
          NAME_1 == "Järva" ~ "Kesk-Eesti",
          NAME_1 == "Jõgeva" ~ "Lõuna-Eesti",
          NAME_1 == "Lääne" ~ "Lääne-Eesti",
          NAME_1 == "Lääne-Viru" ~ "Kesk-Eesti",
          NAME_1 == "Pärnu" ~ "Lääne-Eesti",
          NAME_1 == "Peipsi" ~ "Lõuna-Eesti",
          NAME_1 == "Põlva" ~ "Lõuna-Eesti",
          NAME_1 == "Rapla" ~ "Kesk-Eesti",
          NAME_1 == "Saare" ~ "Lääne-Eesti",
          NAME_1 == "Tartu" ~ "Lõuna-Eesti",
          NAME_1 == "Valga" ~ "Lõuna-Eesti",
          NAME_1 == "Viljandi" ~ "Kesk-Eesti",
          NAME_1 == "Võru" ~ "Lõuna-Eesti"
        )
      ) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_LUX_1_sp.rds") %>% 
      mutate(
        NAME_1 = "Luxembourg"
      ) %>% unite(),
    readr::read_rds("~/social_data_analysis/gadm36_FRA_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Auvergne-Rhône-Alpes" ~ "Sud Est",
          NAME_1 == "Bourgogne-Franche-Comté" ~ "Bassin Parisien Est",
          NAME_1 == "Bretagne" ~ "Ouest",
          NAME_1 == "Centre-Val de Loire" ~ "Bassin Parisien Ouest",
          NAME_1 == "Corse" ~ "Méditerranée",
          NAME_1 == "Grand Est" ~ "Est",
          NAME_1 == "Hauts-de-France" ~ "Nord",
          NAME_1 == "Île-de-France" ~ "Région parisienne",
          NAME_1 == "Normandie" ~ "Nord",
          NAME_1 == "Nouvelle-Aquitaine" ~ "Sud Ouest",
          NAME_1 == "Occitanie" ~ "Sud Ouest",
          NAME_1 == "Pays de la Loire" ~ "Ouest",
          NAME_1 == "Provence-Alpes-Côte d'Azur" ~ "Méditerranée"
        )
      ) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_HRV_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Bjelovarska-Bilogorska" ~ "Sredisnja i Istocna (Panonska) Hrvatska",
          NAME_1 == "Brodsko-Posavska" ~ "Sredisnja i Istocna (Panonska) Hrvatska",
          NAME_1 == "Dubrovacko-Neretvanska" ~ "Jadranska Hrvatska",
          NAME_1 == "Grad Zagreb" ~ "Sjeverozapadna Hrvatska",
          NAME_1 == "Istarska" ~ "Jadranska Hrvatska",
          NAME_1 == "Karlovacka" ~ "Sredisnja i Istocna (Panonska) Hrvatska",
          NAME_1 == "Koprivničko-Križevačka" ~ "Sjeverozapadna Hrvatska",
          NAME_1 == "Krapinsko-Zagorska" ~ "Sjeverozapadna Hrvatska",
          NAME_1 == "Licko-Senjska" ~ "Jadranska Hrvatska",
          NAME_1 == "Medimurska" ~ "Sjeverozapadna Hrvatska",
          NAME_1 == "Osjecko-Baranjska" ~ "Sredisnja i Istocna (Panonska) Hrvatska",
          NAME_1 == "Požeško-Slavonska" ~ "Sredisnja i Istocna (Panonska) Hrvatska",
          NAME_1 == "Primorsko-Goranska" ~ "Jadranska Hrvatska",
          NAME_1 == "Šibensko-Kninska" ~ "Jadranska Hrvatska",
          NAME_1 == "Sisacko-Moslavacka" ~ "Sredisnja i Istocna (Panonska) Hrvatska",
          NAME_1 == "Splitsko-Dalmatinska" ~ "Jadranska Hrvatska",
          NAME_1 == "Varaždinska" ~ "Sjeverozapadna Hrvatska",
          NAME_1 == "Viroviticko-Podravska" ~ "Sredisnja i Istocna (Panonska) Hrvatska",
          NAME_1 == "Vukovarsko-Srijemska" ~ "Sredisnja i Istocna (Panonska) Hrvatska",
          NAME_1 == "Zadarska" ~ "Jadranska Hrvatska",
          NAME_1 == "Zagrebačka" ~ "Sjeverozapadna Hrvatska"
        )
      ) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_PRT_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Aveiro" ~ "Centro",
          NAME_1 == "Azores" ~ "Algarve",
          NAME_1 == "Beja" ~ "Alentejo",
          NAME_1 == "Braga" ~ "Norte",
          NAME_1 == "Bragança" ~ "Norte",
          NAME_1 == "Castelo Branco" ~ "Centro",
          NAME_1 == "Coimbra" ~ "Centro",
          NAME_1 == "Évora" ~ "Alentejo",
          NAME_1 == "Faro" ~ "Algarve",
          NAME_1 == "Guarda" ~ "Centro",
          NAME_1 == "Leiria" ~ "Centro",
          NAME_1 == "Lisboa" ~ "Lisboa e Vale do Tejo",
          NAME_1 == "Madeira" ~ "Algarve",
          NAME_1 == "Portalegre" ~ "Alentejo",
          NAME_1 == "Porto" ~ "Norte",
          NAME_1 == "Santarém" ~ "Alentejo",
          NAME_1 == "Setúbal" ~ "Alentejo",
          NAME_1 == "Viana do Castelo" ~ "Norte",
          NAME_1 == "Vila Real" ~ "Norte",
          NAME_1 == "Viseu" ~ "Centro"
        )
      ) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_SVK_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Banskobystrický" ~ "Banska Bystrica Reg.",
          NAME_1 == "Bratislavský" ~ "Bratislava Reg.",
          NAME_1 == "Košický" ~ "Kosice Reg.",
          NAME_1 == "Nitriansky" ~ "Nitra Reg.",
          NAME_1 == "Prešovský" ~ "Presov Reg.",
          NAME_1 == "Trenčiansky" ~ "Trencin Reg.",
          NAME_1 == "Trnavský" ~ "Trnava Reg.",
          NAME_1 == "Žilinský" ~ "Zilina Reg."
        )
      ) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_LTU_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Alytaus" ~ "South Lithuania",
          NAME_1 == "Klaipedos" ~ "West Lithuania",
          NAME_1 == "Marijampoles" ~ "South Lithuania",
          NAME_1 == "Panevezio" ~ "Nord Lithuania",
          NAME_1 == "Šiauliai" ~ "Nord Lithuania",
          NAME_1 == "Taurages" ~ "West Lithuania",
          NAME_1 == "Telšiai" ~ "West Lithuania",
          NAME_1 == "Utenos" ~ "Nord Lithuania",
          T ~ NAME_1
        )
      ) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_LVA_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Kurzeme" ~ "Nord-West Latvia",
          NAME_1 == "Latgale" ~ "South-East Latvia",
          NAME_1 == "Riga" ~ "Nord-West Latvia",
          NAME_1 == "Vidzeme" ~ "South-East Latvia",
          NAME_1 == "Zemgale" ~ "South-East Latvia"
        )
      ) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_XKO_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Đakovica" ~ "South Kosovo",
          NAME_1 == "Gnjilane" ~ "South Kosovo",
          NAME_1 == "Kosovska Mitrovica" ~ "Nord Kosovo",
          NAME_1 == "Pećki" ~ "Nord Kosovo",
          NAME_1 == "Pristina" ~ "Nord Kosovo",
          NAME_1 == "Prizren" ~ "South Kosovo",
          NAME_1 == "Uroševac" ~ "South Kosovo"
        )
      ) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_SVN_1_sp.rds") %>% unite(),
    readr::read_rds("~/social_data_analysis/gadm36_ROU_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Alba" ~ "Centru",
          NAME_1 == "Arad" ~ "Vest",
          NAME_1 == "Argeș" ~ "Sud-Muntenia",
          NAME_1 == "Bacău" ~ "Nord-Est",
          NAME_1 == "Bihor" ~ "Nord-Vest",
          NAME_1 == "Bistrița-Năsăud" ~ "Nord-Vest",
          NAME_1 == "Botoșani" ~ "Nord-Est",
          NAME_1 == "Brașov" ~ "Centru",
          NAME_1 == "Brăila" ~ "Sud-Est",
          NAME_1 == "Bucharest" ~ "Bucuresti-Ilfov",
          NAME_1 == "Buzău" ~ "Sud-Est",
          NAME_1 == "Călărași" ~ "Sud-Muntenia",
          NAME_1 == "Caraș-Severin" ~ "Vest",
          NAME_1 == "Cluj" ~ "Nord-Vest",
          NAME_1 == "Constanța" ~ "Sud-Est",
          NAME_1 == "Covasna" ~ "Centru",
          NAME_1 == "Dâmbovița" ~ "Sud-Muntenia",
          NAME_1 == "Dolj" ~ "Sud-Vest Oltenia",
          NAME_1 == "Galați" ~ "Sud-Est",
          NAME_1 == "Giurgiu" ~ "Sud-Muntenia",
          NAME_1 == "Gorj" ~ "Sud-Vest Oltenia",
          NAME_1 == "Harghita" ~ "Centru",
          NAME_1 == "Hunedoara" ~ "Vest",
          NAME_1 == "Iași" ~ "Nord-Est",
          NAME_1 == "Ialomița" ~ "Sud-Muntenia",
          NAME_1 == "Ilfov" ~ "Bucuresti-Ilfov",
          NAME_1 == "Maramureș" ~ "Nord-Vest",
          NAME_1 == "Mehedinți" ~ "Sud-Vest Oltenia",
          NAME_1 == "Mureș" ~ "Centru",
          NAME_1 == "Neamț" ~ "Nord-Est",
          NAME_1 == "Olt" ~ "Sud-Vest Oltenia",
          NAME_1 == "Prahova" ~ "Sud-Muntenia",
          NAME_1 == "Sălaj" ~ "Nord-Vest",
          NAME_1 == "Satu Mare" ~ "Nord-Vest",
          NAME_1 == "Sibiu" ~ "Centru",
          NAME_1 == "Suceava" ~ "Nord-Est",
          NAME_1 == "Teleorman" ~ "Sud-Muntenia",
          NAME_1 == "Timiș" ~ "Vest",
          NAME_1 == "Tulcea" ~ "Sud-Est",
          NAME_1 == "Vâlcea" ~ "Sud-Vest Oltenia",
          NAME_1 == "Vaslui" ~ "Nord-Est",
          NAME_1 == "Vrancea" ~ "Sud-Est"
        )) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_HUN_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Bács-Kiskun" ~ "South- Plain",
          NAME_1 == "Baranya" ~ "South-Transdanubia",
          NAME_1 == "Békés" ~ "South- Plain",
          NAME_1 == "Borsod-Abaúj-Zemplén" ~ "North Regio",
          NAME_1 == "Budapest" ~ "Central regio",
          NAME_1 == "Csongrád" ~ "South- Plain",
          NAME_1 == "Fejér" ~ "Middle- Transdanubia",
          NAME_1 == "Gyor-Moson-Sopron" ~ "West- Transdanubia",
          NAME_1 == "Hajdú-Bihar" ~ "North- Plain",
          NAME_1 == "Heves" ~ "North Regio",
          NAME_1 == "Jász-Nagykun-Szolnok" ~ "North- Plain",
          NAME_1 == "Komárom-Esztergom" ~ "Middle- Transdanubia",
          NAME_1 == "Nógrád" ~ "North Regio",
          NAME_1 == "Pest" ~ "Central regio",
          NAME_1 == "Somogy" ~ "South-Transdanubia",
          NAME_1 == "Szabolcs-Szatmár-Bereg" ~ "North- Plain",
          NAME_1 == "Tolna" ~ "South-Transdanubia",
          NAME_1 == "Vas" ~ "West- Transdanubia",
          NAME_1 == "Veszprém" ~ "Middle- Transdanubia",
          NAME_1 == "Zala" ~ "West- Transdanubia"
        )) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_BGR_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Blagoevgrad" ~ "South Western",
          NAME_1 == "Burgas" ~ "South Eastern",
          NAME_1 == "Dobrich" ~ "North Eastern",
          NAME_1 == "Gabrovo" ~ "North Central",
          NAME_1 == "Grad Sofiya" ~ "South Western",
          NAME_1 == "Haskovo" ~ "South Central",
          NAME_1 == "Kardzhali" ~ "South Central",
          NAME_1 == "Kyustendil" ~ "South Western",
          NAME_1 == "Lovech" ~ "North Western",
          NAME_1 == "Montana" ~ "North Western",
          NAME_1 == "Pazardzhik" ~ "South Central",
          NAME_1 == "Pernik" ~ "South Western",
          NAME_1 == "Pleven" ~ "North Western",
          NAME_1 == "Plovdiv" ~ "South Central",
          NAME_1 == "Razgrad" ~ "North Central",
          NAME_1 == "Ruse" ~ "North Central",
          NAME_1 == "Shumen" ~ "North Eastern",
          NAME_1 == "Silistra" ~ "North Central",
          NAME_1 == "Sliven" ~ "South Eastern",
          NAME_1 == "Smolyan" ~ "South Central",
          NAME_1 == "Sofia" ~ "South Western",
          NAME_1 == "Stara Zagora" ~ "South Eastern",
          NAME_1 == "Targovishte" ~ "North Eastern",
          NAME_1 == "Varna" ~ "North Eastern",
          NAME_1 == "Veliko Tarnovo" ~ "North Central",
          NAME_1 == "Vidin" ~ "North Western",
          NAME_1 == "Vratsa" ~ "North Western",
          NAME_1 == "Yambol" ~ "South Eastern"
        )) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_IRL_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Carlow" ~ "South-Ireland",
          NAME_1 == "Cavan" ~ "Border",
          NAME_1 == "Clare" ~ "South-Ireland",
          NAME_1 == "Cork" ~ "South-Ireland",
          NAME_1 == "Donegal" ~ "Border",
          NAME_1 == "Dublin" ~ "Dublin",
          NAME_1 == "Galway" ~ "West",
          NAME_1 == "Kerry" ~ "South-Ireland",
          NAME_1 == "Kildare" ~ "South-Ireland",
          NAME_1 == "Kilkenny" ~ "South-Ireland",
          NAME_1 == "Laoighis" ~ "Midland",
          NAME_1 == "Leitrim" ~ "Border",
          NAME_1 == "Limerick" ~ "South-Ireland",
          NAME_1 == "Longford" ~ "Midland",
          NAME_1 == "Louth" ~ "Border",
          NAME_1 == "Mayo" ~ "West",
          NAME_1 == "Meath" ~ "South-Ireland",
          NAME_1 == "Monaghan" ~ "Border",
          NAME_1 == "Offaly" ~ "Midland",
          NAME_1 == "Roscommon" ~ "West",
          NAME_1 == "Sligo" ~ "Border",
          NAME_1 == "Tipperary" ~ "South-Ireland",
          NAME_1 == "Waterford" ~ "South-Ireland",
          NAME_1 == "Westmeath" ~ "South-Ireland",
          NAME_1 == "Wexford" ~ "South-Ireland",
          NAME_1 == "Wicklow" ~ "South-Ireland"
        )) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_RUS_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Adygey" ~ "North Caucasus",
          NAME_1 == "Altay" ~ "West Siberia",
          NAME_1 == "Amur" ~ "Far East",
          NAME_1 == "Arkhangel'sk" ~ "North and North West",
          NAME_1 == "Astrakhan'" ~ "Volga",
          NAME_1 == "Bashkortostan" ~ "Urals",
          NAME_1 == "Belgorod" ~ "Central-Chernozhem",
          NAME_1 == "Bryansk" ~ "Center",
          NAME_1 == "Buryat" ~ "Far East",
          NAME_1 == "Chechnya" ~ "North Caucasus",
          NAME_1 == "Chelyabinsk" ~ "Urals",
          NAME_1 == "Chukot" ~ "Far East",
          NAME_1 == "Chuvash" ~ "Volgo-Vyatsky",
          NAME_1 == "City of St. Petersburg" ~ "North and North West",
          NAME_1 == "Dagestan" ~ "North Caucasus",
          NAME_1 == "Gorno-Altay" ~ "East Siberia",
          NAME_1 == "Ingush" ~ "North Caucasus",
          NAME_1 == "Irkutsk" ~ "East Siberia",
          NAME_1 == "Ivanovo" ~ "Center",
          NAME_1 == "Kabardin-Balkar" ~ "North Caucasus",
          NAME_1 == "Kaliningrad" ~ "North and North West",
          NAME_1 == "Kalmyk" ~ "Volga",
          NAME_1 == "Kaluga" ~ "Center",
          NAME_1 == "Kamchatka" ~ "Far East",
          NAME_1 == "Karachay-Cherkess" ~ "North Caucasus",
          NAME_1 == "Karelia" ~ "North and North West",
          NAME_1 == "Kemerovo" ~ "West Siberia",
          NAME_1 == "Khabarovsk" ~ "Far East",
          NAME_1 == "Khakass" ~ "East Siberia",
          NAME_1 == "Khanty-Mansiy" ~ "West Siberia",
          NAME_1 == "Kirov" ~ "Volgo-Vyatsky",
          NAME_1 == "Komi" ~ "North and North West",
          NAME_1 == "Kostroma" ~ "Center",
          NAME_1 == "Krasnodar" ~ "North Caucasus",
          NAME_1 == "Krasnoyarsk" ~ "East Siberia",
          NAME_1 == "Kurgan" ~ "Urals",
          NAME_1 == "Kursk" ~ "Central-Chernozhem",
          NAME_1 == "Leningrad" ~ "North and North West",
          NAME_1 == "Lipetsk" ~ "Central-Chernozhem",
          NAME_1 == "Maga Buryatdan" ~ "Far East",
          NAME_1 == "Mariy-El" ~ "Volgo-Vyatsky",
          NAME_1 == "Mordovia" ~ "Volgo-Vyatsky",
          NAME_1 == "Moscow City" ~ "Center",
          NAME_1 == "Moskva" ~ "Center",
          NAME_1 == "Murmansk" ~ "North and North West",
          NAME_1 == "Nenets" ~ "North and North West",
          NAME_1 == "Nizhegorod" ~ "Volgo-Vyatsky",
          NAME_1 == "North Ossetia" ~ "North Caucasus",
          NAME_1 == "Novgorod" ~ "North and North West",
          NAME_1 == "Novosibirsk" ~ "West Siberia",
          NAME_1 == "Omsk" ~ "West Siberia",
          NAME_1 == "Orel" ~ "Center",
          NAME_1 == "Orenburg" ~ "Urals",
          NAME_1 == "Penza" ~ "Volga",
          NAME_1 == "Perm'" ~ "Urals",
          NAME_1 == "Primor'ye" ~ "Far East",
          NAME_1 == "Pskov" ~ "North and North West",
          NAME_1 == "Rostov" ~ "North Caucasus",
          NAME_1 == "Ryazan'" ~ "Center",
          NAME_1 == "Sakha" ~ "Far East",
          NAME_1 == "Sakhalin" ~ "Far East",
          NAME_1 == "Samara" ~ "Volga",
          NAME_1 == "Saratov" ~ "Volga",
          NAME_1 == "Smolensk" ~ "Center",
          NAME_1 == "Stavropol'" ~ "North Caucasus",
          NAME_1 == "Sverdlovsk" ~ "Urals",
          NAME_1 == "Tambov" ~ "Central-Chernozhem",
          NAME_1 == "Tatarstan" ~ "Volga",
          NAME_1 == "Tomsk" ~ "West Siberia",
          NAME_1 == "Tula" ~ "Center",
          NAME_1 == "Tuva" ~ "East Siberia",
          NAME_1 == "Tver'" ~ "Center",
          NAME_1 == "Tyumen'" ~ "West Siberia",
          NAME_1 == "Udmurt" ~ "Urals",
          NAME_1 == "Ul'yanovsk" ~ "Volga",
          NAME_1 == "Vladimir" ~ "Center",
          NAME_1 == "Volgograd" ~ "Volga",
          NAME_1 == "Vologda" ~ "North and North West",
          NAME_1 == "Voronezh" ~ "Central-Chernozhem",
          NAME_1 == "Yamal-Nenets" ~ "West Siberia",
          NAME_1 == "Yaroslavl'" ~ "Center",
          NAME_1 == "Yevrey" ~ "Far East",
          NAME_1 == "Zabaykal'ye" ~ "East Siberia",
        )
      ) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_ITA_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Molise" ~ "Centre Italy",
          NAME_1 == "Valle d'Aosta" ~ "Piemonte",
          NAME_1 == "Trentino-Alto Adige" ~ "Veneto",
          NAME_1 == "Friuli-Venezia Giulia" ~ "Veneto",
          NAME_1 == "Veneto" ~ "Veneto",
          NAME_1 == "Liguria" ~ "Piemonte",
          NAME_1 == "Piemonte" ~ "Piemonte",
          NAME_1 == "Emilia-Romagna" ~ "Emilia-Romagna-Toscana",
          NAME_1 == "Toscana" ~ "Emilia-Romagna-Toscana",
          NAME_1 == "Lazio" ~ "Centre Italy",
          NAME_1 == "Abruzzo" ~ "Centre Italy",
          NAME_1 == "Marche" ~ "Centre Italy",
          NAME_1 == "Umbria" ~ "Centre Italy",
          NAME_1 == "Campania" ~ "South Italy",
          NAME_1 == "Apulia" ~ "South Italy",
          NAME_1 == "Calabria" ~ "South Italy",
          NAME_1 == "Basilicata" ~ "South Italy",
          NAME_1 == "Sardegna" ~ "Sicily-Sardegna",
          NAME_1 == "Sicily" ~ "Sicily-Sardegna",
          T ~ NAME_1
        )
      ) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_GBR_1_sp.rds") %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_GRC_1_sp.rds") %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_DNK_2_sp.rds") %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_NLD_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Drenthe" ~ "Nord Netherlands",
          NAME_1 == "Flevoland" ~ "Nord Netherlands",
          NAME_1 == "Friesland" ~ "Nord Netherlands",
          NAME_1 == "Groningen" ~ "Nord Netherlands",
          NAME_1 == "IJsselmeer" ~ "Nord Netherlands",
          NAME_1 == "Zeeland" ~ "Zuid-Holland",
          NAME_1 == "Zeeuwse meren" ~ "Nord Netherlands",
          T ~ NAME_1
        )
      ) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_TUR_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Adana" ~ "Mediterranean",
          NAME_1 == "Adiyaman" ~ "East",
          NAME_1 == "Afyon" ~ "Aegean",
          NAME_1 == "Agri" ~ "Anatolia",
          NAME_1 == "Aksaray" ~ "Anatolia",
          NAME_1 == "Amasya" ~ "Black Sea",
          NAME_1 == "Ankara" ~ "Anatolia",
          NAME_1 == "Antalya" ~ "Mediterranean",
          NAME_1 == "Ardahan" ~ "Black Sea",
          NAME_1 == "Artvin" ~ "Black Sea",
          NAME_1 == "Aydin" ~ "Aegean",
          NAME_1 == "Balikesir" ~ "Marmara",
          NAME_1 == "Bartın" ~ "Black Sea",
          NAME_1 == "Batman" ~ "East",
          NAME_1 == "Bayburt" ~ "Anatolia",
          NAME_1 == "Bilecik" ~ "Marmara",
          NAME_1 == "Bingöl" ~ "Anatolia",
          NAME_1 == "Bitlis" ~ "East",
          NAME_1 == "Bolu" ~ "Marmara",
          NAME_1 == "Burdur" ~ "Mediterranean",
          NAME_1 == "Bursa" ~ "Marmara",
          NAME_1 == "Çanakkale" ~ "Marmara",
          NAME_1 == "Çankiri" ~ "Anatolia",
          NAME_1 == "Çorum" ~ "Black Sea",
          NAME_1 == "Denizli" ~ "Aegean",
          NAME_1 == "Diyarbakir" ~ "East",
          NAME_1 == "Düzce" ~ "Black Sea",
          NAME_1 == "Edirne" ~ "Marmara",
          NAME_1 == "Elazığ" ~ "Anatolia",
          NAME_1 == "Erzincan" ~ "Anatolia",
          NAME_1 == "Erzurum" ~ "Anatolia",
          NAME_1 == "Eskisehir" ~ "Anatolia",
          NAME_1 == "Gaziantep" ~ "East",
          NAME_1 == "Giresun" ~ "Black Sea",
          NAME_1 == "Gümüshane" ~ "Black Sea",
          NAME_1 == "Hakkari" ~ "East",
          NAME_1 == "Hatay" ~ "Mediterranean",
          NAME_1 == "Iğdır" ~ "East",
          NAME_1 == "Isparta" ~ "Mediterranean",
          NAME_1 == "Istanbul" ~ "Istanbul",
          NAME_1 == "Izmir" ~ "Aegean",
          NAME_1 == "K. Maras" ~ "Mediterranean",
          NAME_1 == "Karabük" ~ "Black Sea",
          NAME_1 == "Karaman" ~ "Anatolia",
          NAME_1 == "Kars" ~ "East",
          NAME_1 == "Kastamonu" ~ "Black Sea",
          NAME_1 == "Kayseri" ~ "Anatolia",
          NAME_1 == "Kilis" ~ "East",
          NAME_1 == "Kinkkale" ~ "Anatolia",
          NAME_1 == "Kirklareli" ~ "Marmara",
          NAME_1 == "Kirsehir" ~ "Anatolia",
          NAME_1 == "Kocaeli" ~ "Marmara",
          NAME_1 == "Konya" ~ "Anatolia",
          NAME_1 == "Kütahya" ~ "Aegean",
          NAME_1 == "Malatya" ~ "Anatolia",
          NAME_1 == "Manisa" ~ "Aegean",
          NAME_1 == "Mardin" ~ "East",
          NAME_1 == "Mersin" ~ "Mediterranean",
          NAME_1 == "Mugla" ~ "Mediterranean",
          NAME_1 == "Mus" ~ "Anatolia",
          NAME_1 == "Nevsehir" ~ "Anatolia",
          NAME_1 == "Nigde" ~ "Anatolia",
          NAME_1 == "Ordu" ~ "Black Sea",
          NAME_1 == "Osmaniye" ~ "Mediterranean",
          NAME_1 == "Rize" ~ "Black Sea",
          NAME_1 == "Sakarya" ~ "Black Sea",
          NAME_1 == "Samsun" ~ "Black Sea",
          NAME_1 == "Sanliurfa" ~ "East",
          NAME_1 == "Siirt" ~ "East",
          NAME_1 == "Sinop" ~ "Black Sea",
          NAME_1 == "Sirnak" ~ "East",
          NAME_1 == "Sivas" ~ "Anatolia",
          NAME_1 == "Tekirdag" ~ "Marmara",
          NAME_1 == "Tokat" ~ "Black Sea",
          NAME_1 == "Trabzon" ~ "Black Sea",
          NAME_1 == "Tunceli" ~ "Anatolia",
          NAME_1 == "Usak" ~ "Aegean",
          NAME_1 == "Van" ~ "Anatolia",
          NAME_1 == "Yalova" ~ "Marmara",
          NAME_1 == "Yozgat" ~ "Anatolia",
          NAME_1 == "Zinguldak" ~ "Black Sea"
        )) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_POL_1_sp.rds") %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_DEU_1_sp.rds") %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_UKR_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Crimea" ~ "South Ukraine",
          NAME_1 == "Sevastopol'" ~ "South Ukraine",
          NAME_1 == "Ivano-Frankivs'k" ~ "West Ukraine",
          NAME_1 == "Rivne" ~ "West Ukraine",
          NAME_1 == "Poltava" ~ "Center Ukraine",
          NAME_1 == "Sumy" ~ "Nord Ukraine",
          NAME_1 == "Chernihiv" ~ "Nord Ukraine",
          NAME_1 == "Chernivtsi" ~ "West Ukraine",
          NAME_1 == "Kharkiv" ~ "East Ukraine",
          NAME_1 == "Zhytomyr" ~ "Nord Ukraine",
          NAME_1 == "Kirovohrad" ~ "Center Ukraine",
          NAME_1 == "Cherkasy" ~ "Center Ukraine",
          NAME_1 == "Volyn" ~ "West Ukraine",
          NAME_1 == "Mykolayiv" ~ "South Ukraine",
          NAME_1 == "Kherson" ~ "South Ukraine",
          NAME_1 == "Zaporizhzhya" ~ "South Ukraine",
          NAME_1 == "Dnipropetrovs'k" ~ "Center Ukraine",
          NAME_1 == "L'viv" ~ "West Ukraine",
          NAME_1 == "Luhans'k" ~ "East Ukraine",
          NAME_1 == "Donets'k" ~ "East Ukraine",
          NAME_1 == "Transcarpathia" ~ "West Ukraine",
          NAME_1 == "Khmel'nyts'kyy" ~ "West Ukraine",
          NAME_1 == "Kiev" ~ "Nord Ukraine",
          NAME_1 == "Kiev City" ~ "Nord Ukraine",
          NAME_1 == "Odessa" ~ "South Ukraine",
          NAME_1 == "Vinnytsya" ~ "Center Ukraine",
          NAME_1 == "Ternopil'" ~ "West Ukraine"
        )
      ) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_ESP_1_sp.rds") %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_ALB_1_sp.rds") %>% 
      mutate(
        NAME_1 = case_when(
          NAME_1 == "Shkodër" ~ "Nord Albania",
          NAME_1 == "Kukës" ~ "Nord Albania",
          NAME_1 == "Lezhë" ~ "Nord Albania",
          NAME_1 == "Dibër" ~ "Nord Albania",
          NAME_1 == "Durrës" ~ "Nord Albania",
          NAME_1 == "Tiranë" ~ "Nord Albania",
          T ~ "South Albania"
        )
      ) %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_AUT_1_sp.rds") %>% unite(), 
    readr::read_rds("~/social_data_analysis/gadm36_FIN_1_sp.rds") %>% unite()
  ),
  regions_gSimplify = rmapshaper::ms_simplify(reg, keep = 0.005, keep_shapes = T),
  tendenz = map_dfr(unique(ess$round_year), 
                    function(x) {
                      cnt <- ess %>% 
                        filter(round_year == x) %>% 
                        dplyr::select(region, cntry) %>% 
                        distinct(.keep_all = T)
                      
                      tbl <- ess %>% 
                        filter(round_year == x) %>% 
                        filter(region %in% cnt$region[cnt$region %in% reg$id]) %>% 
                        group_by(region) %>% summarise(
                          `Openness to Change` = median(`Openness to Change`, na.rm = T),
                          Conservation = median(Conservation, na.rm = T),
                          `Self-Enhancement` = median(`Self-Enhancement`, na.rm = T),
                          `Self-Trancendence` = median(`Self-Trancendence`, na.rm=T)
                        )
                      
                      regions_gSimplify@data <- regions_gSimplify@data %>% left_join(tbl, by = c("id" = "region"))
                      regions_gSimplify <- regions_gSimplify[(is.na(regions_gSimplify@data) %>% apply(1, sum))==0,]
                      
                      return(imoran(regions_gSimplify))
                    }, .id = "round"),
  tendenz_lines = ggplot(tendenz, aes(as.numeric(round), estimate1, linetype = value)) + 
    xlab("Хвиля ЄСС") +
    ylab("Значення Морана І") +
    scale_linetype_discrete(name = "Цінності") +
    geom_path() + 
    theme_minimal() + 
    theme(legend.position="top"),
  ages = map2_dfr(c(12,35,61), c(34, 60, 150), 
                  function(x, y) {
                    cnt <- ess %>% 
                      dplyr::select(region, cntry) %>% 
                      distinct(.keep_all = T)
                    
                    tbl <- ess %>% 
                      filter(between(agea, x, y)) %>% 
                      filter(region %in% cnt$region[cnt$region %in% reg$id]) %>% 
                      group_by(region) %>% summarise(
                        `Openness to Change` = median(`Openness to Change`, na.rm = T),
                        Conservation = median(Conservation, na.rm = T),
                        `Self-Enhancement` = median(`Self-Enhancement`, na.rm = T),
                        `Self-Trancendence` = median(`Self-Trancendence`, na.rm=T)
                      )
                    
                    regions_gSimplify@data <- regions_gSimplify@data %>% left_join(tbl, by = c("id" = "region"))
                    regions_gSimplify <- regions_gSimplify[(is.na(regions_gSimplify@data) %>% apply(1, sum))==0,]
                    
                    return(imoran(regions_gSimplify))
                  }, .id = "Вікова група") %>% 
    dplyr::select(`Вікова група`, value, estimate1) %>% 
    tidyr::spread("value", "estimate1") %>% mutate(
      `Вікова група` = c("Молодь (до 35 років)", "Середній вік (35-60 років)", "Старший вік (60+ років)")
    ),
  country_groups = map_dfr(list(es2002, es2004, es2007, es2013, all_year), 
                           function(x) {
                             cnt <- ess %>% 
                               dplyr::select(region, cntry) %>% 
                               distinct(.keep_all = T)
                             
                             tbl <- ess %>% 
                               filter(cntry %in% x) %>% 
                               filter(region %in% cnt$region[cnt$region %in% reg$id]) %>% 
                               group_by(region) %>% summarise(
                                 `Openness to Change` = median(`Openness to Change`, na.rm = T),
                                 Conservation = median(Conservation, na.rm = T),
                                 `Self-Enhancement` = median(`Self-Enhancement`, na.rm = T),
                                 `Self-Trancendence` = median(`Self-Trancendence`, na.rm=T)
                               )
                             
                             regions_gSimplify@data <- regions_gSimplify@data %>% left_join(tbl, by = c("id" = "region"))
                             regions_gSimplify <- regions_gSimplify[(is.na(regions_gSimplify@data) %>% apply(1, sum))==0,]
                             
                             return(imoran(regions_gSimplify))
                           }, .id = "Група країн") %>% 
    dplyr::select(`Група країн`, value, estimate1) %>% 
    tidyr::spread("value", "estimate1") %>% mutate(
      `Група країн` = c("Члени ЄС станом на 2002 рік", 
                        "Члени ЄС станом на 2004 рік",
                        "Члени ЄС станом на 2007 рік",
                        "Члени ЄС станом на 2013 рік",
                        "Всі країни-учасники ЄСС"
      )
    ),
  tendenz_all = map_dfr(unique(ess$round_year), 
                        function(x) {
                          cnt <- ess %>% 
                            filter(round_year == x) %>% 
                            dplyr::select(region, cntry) %>% 
                            distinct(.keep_all = T)
                          
                          tbl <- ess %>% 
                            filter(round_year == x) %>% 
                            filter(cntry %in% all_year) %>% 
                            filter(region %in% cnt$region[cnt$region %in% reg$id]) %>% 
                            group_by(region) %>% summarise(
                              `Openness to Change` = median(`Openness to Change`, na.rm = T),
                              Conservation = median(Conservation, na.rm = T),
                              `Self-Enhancement` = median(`Self-Enhancement`, na.rm = T),
                              `Self-Trancendence` = median(`Self-Trancendence`, na.rm=T)
                            )
                          
                          regions_gSimplify@data <- regions_gSimplify@data %>% left_join(tbl, by = c("id" = "region"))
                          regions_gSimplify <- regions_gSimplify[(is.na(regions_gSimplify@data) %>% apply(1, sum))==0,]
                          
                          return(imoran(regions_gSimplify))
                        }, .id = "Хвиля"),
  tendenz_all_line = ggplot(tendenz_all, aes(as.numeric(Хвиля), estimate1, linetype = value)) + 
    xlab("Хвиля ЄСС") +
    ylab("Значення Морана І") +
    scale_linetype_discrete(name = "Цінності") +
    geom_path() + 
    theme_minimal() + 
    theme(legend.position="top"),
  tendenz_all_table = tendenz_all %>% 
    dplyr::select(Хвиля, value, estimate1) %>% 
    tidyr::spread("value", "estimate1"),
  tendenz_all_smooth = ggplot(tendenz_all, aes(as.numeric(Хвиля), estimate1, linetype = value)) + 
    xlab("Хвиля ЄСС") +
    ylab("Значення Морана І") +
    scale_linetype_discrete(name = "Цінності") +
    geom_smooth(se = F, method = "lm") + 
    theme_minimal() + 
    theme(legend.position="top"),
  cnt = ess %>% dplyr::select(region, cntry) %>% distinct(.keep_all = T),
  tbl = ess %>% 
    filter(region %in% cnt$region[cnt$region %in% reg$id]) %>% 
    group_by(region) %>% 
    top_n(1, round_year) %>% 
    summarise(
      `Openness to Change` = median(`Openness to Change`, na.rm = T),
      Conservation = median(Conservation, na.rm = T),
      `Self-Enhancement` = median(`Self-Enhancement`, na.rm = T),
      `Self-Trancendence` = median(`Self-Trancendence`, na.rm=T),
      security = median(security, na.rm = T),
      conformity = median(conformity, na.rm = T),
      tradition = median(tradition, na.rm = T),
      benevolence = median(benevolence, na.rm = T),
      universalism = median(universalism, na.rm = T),
      self_direction = median(self_direction, na.rm = T),
      stimulation = median(stimulation, na.rm = T),
      hedonism = median(hedonism, na.rm = T),
      achievement = median(achievement, na.rm = T),
      power = median(power, na.rm = T)
    ) %>% norm_tbl(),
  regions_gSimplify_df = fortify(regions_gSimplify, region = "id")  %>% 
    left_join(
      left_join(
        reg[reg$id %in% cnt$region,]@data %>% 
          mutate(id = as.character(id)), tbl, by = c("id" = "region")
      ), by = "id"),
  values_distribution_plot = ggpubr::ggarrange(
    (ggplot() +
       geom_map(data = regions_gSimplify_df, map = regions_gSimplify_df, 
                aes(long, lat, map_id = id, fill = `Openness to Change`)) + theme_void() + 
       scale_fill_gradient(low = "white", high = "black", name = "Відкритість до змін") +
       coord_map("gilbert", xlim = c(-10, 50), ylim = c(33, 71)) + theme(legend.position="top")),
    (ggplot() +
       geom_map(data = regions_gSimplify_df, map = regions_gSimplify_df, 
                aes(long, lat, map_id = id, fill = `Self-Trancendence`)) + theme_void() + 
       scale_fill_gradient(low = "white", high = "black" , name = "Самоперевершення") +
       coord_map("gilbert", xlim = c(-10, 50), ylim = c(33, 71)) + theme(legend.position="top")),
    (ggplot() +
       geom_map(data = regions_gSimplify_df, map = regions_gSimplify_df, 
                aes(long, lat, map_id = id, fill = Conservation)) + theme_void() + 
       scale_fill_gradient(low = "white", high = "black" , name = "Збереження") +
       coord_map("gilbert", xlim = c(-10, 50), ylim = c(33, 71)) + theme(legend.position="top")),
    (ggplot() +
       geom_map(data = regions_gSimplify_df, map = regions_gSimplify_df, 
                aes(long, lat, map_id = id, fill = `Self-Enhancement`)) + theme_void() + 
       scale_fill_gradient(low = "white", high = "black" , name = "Самовдосконалення") +
       coord_map("gilbert", xlim = c(-10, 50), ylim = c(33, 71)) + theme(legend.position="top")),ncol = 2, nrow = 2
  ),
  regions_gSimplify_wide = regions_gSimplify_df %>% 
    select(long, lat, order, hole, piece, id, group, security, 
           conformity, tradition, benevolence, universalism, 
           self_direction, stimulation, hedonism, achievement, power) %>% 
    tidyr::gather(key = value, value = score, security, conformity, 
                  tradition, benevolence, universalism, self_direction, 
                  stimulation, hedonism, achievement, power),
  values_choropleth = ggplot(regions_gSimplify_wide, aes(map_id = id)) +
    geom_map(map = regions_gSimplify_wide, 
             aes(fill = score)) + 
    expand_limits(x = regions_gSimplify_wide$long, 
                  y = regions_gSimplify_wide$lat) +
    scale_fill_gradient(low = "#fff5eb", high = "#7f2704") +
    theme_void() + 
    coord_map("ortho", orientation = c(41, 22, -10), 
              xlim = c(-10, 43), ylim = c(33, 70)) + 
    theme(
      legend.position = "bottom", 
      strip.text = element_text(family = "PT Sans", size = 15, face = "bold"),
      legend.title = element_blank(), 
      legend.text = element_text(family = "PT Sans", size = 15),
      plot.margin = unit(c(0, 0, 0, 0),"cm")
    ) +
    guides(fill = guide_legend(title.position = "left", ncol = 6, 
                               keywidth = 2, keyheight = 2,
                               label.position = "bottom")) + 
    facet_wrap(~value, ncol = 2),
  D0 = tbl[6:15] %>% 
    as.matrix() %>% 
    dist(method = "minkowski"),
  idx = sapply(
    (tbl %>% select(region) %>% 
       left_join(
         regions_gSimplify@data %>% 
           mutate(id = as.character(id)), by = c("region" = "id")
       ) %>% 
       distinct() %>% pull(region)) %>% unique(), function(x) {
         which(x == as.character(regions_gSimplify$id %>% unique()))
       }
  ) %>% unlist() %>% unique(),
  A = spdep::nb2mat(
    spdep::poly2nb(
      regions_gSimplify
    ), style = "B", zero.policy = T) %>% 
    modify_A(idx, tbl),
  D1 = as.dist(1 - A),
  fit = ClustGeo::hclustgeo(D0, D1, alpha = 0.18),
  dendrogram = ggdendro::ggdendrogram(fit, rotate = TRUE, theme_dendro = FALSE) +
    ylab("") +
    xlab("") +
    geom_hline(yintercept = 0.01, linetype = "dashed") +
    geom_hline(yintercept = 0.005, linetype = "dashed") +
    theme_minimal() + theme(
      axis.text.y = element_text(family = "PT Sans", size = 5),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank()
    ),
  tbl2 = tbl %>% mutate(
    two_cluster_solution = cutree(fit, h = 0.01),
    five_cluster_solution = cutree(fit, h = 0.005)
  ) %>% group_by(two_cluster_solution) %>% 
    mutate(
      five_cluster_solution = paste0(two_cluster_solution, ".", as.numeric(as.factor(five_cluster_solution)))
    ) %>% ungroup(),
  cluster_decision = ClustGeo::choicealpha(D0, D1, 
                                           range.alpha = seq(0, 0.5, by = 0.01),
                                           K = 33, graph = F)$Qnorm %>% 
    as_tibble(rownames = "alpha") %>% 
    rename(`D0 model` = "Q0norm",`D1 model` = "Q1norm") %>% 
    mutate(alpha = readr::parse_number(alpha)) %>% 
    tidyr::gather("clustering", "explained inertia", - alpha) %>% 
    ggplot(aes(alpha, `explained inertia`, color = clustering)) + 
    geom_path() +
    scale_color_manual(values = c("#a6cee3", "#b2df8a")) +
    theme_minimal() + theme(
      axis.text = element_text(family = "PT Sans", size = 9),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(colour = "black", 
                                      linetype = "dashed", size = 0.05)
    ),
  regions_gSimplify_df2 = fortify(regions_gSimplify, region = "id")  %>% 
    left_join(
      left_join(
        reg[reg$id %in% cnt$region,]@data %>% 
          mutate(id = as.character(id)), tbl2, by = c("id" = "region")
      ), by = "id"),
  clusters_plot = ggplot() +
    geom_map(data = regions_gSimplify_df2 %>% filter(!is.na(five_cluster_solution)),
             map = regions_gSimplify_df2 %>% filter(!is.na(five_cluster_solution)), 
             aes(map_id = id, fill = five_cluster_solution)) + 
    expand_limits(x = regions_gSimplify_df2$long, y = regions_gSimplify_df2$lat) +
    theme_void() + 
    scale_fill_manual(values = unname(clusters_fill),
                      na.value = "lightgrey") +
    coord_map("gilbert", xlim = c(-10, 50), ylim = c(33, 71)) + 
    geom_text(aes(x=-20, y = 70, label = "European values clusters", 
                  hjust = 0, vjust=1), family = "PT Sans", color = "black", size = 5)+
    geom_text(aes(x=-20, y = 68.6, label = "    Based on ESS data", 
                  hjust = 0, vjust = 1), family = "PT Sans", color = "black", size = 3)+
    theme(
      legend.position = "bottom", 
      legend.title = element_text(family = "PT Sans", size = 14, face = "bold"), 
      legend.text = element_text(family = "PT Sans", size = 13)
    ) +
    guides(fill = guide_legend(title = "values cluster",
                               title.position = "left", ncol = 6, 
                               keywidth = 2, keyheight = 2,
                               label.position = "bottom")),
  ess_economy = readr::read_rds("ess_new.rds") %>% mutate(
    detrend_SE = lm(`Self-Enhancement` ~ birth_date, data = .) %>% resid,
    detrend_CO = lm(Conservation ~ birth_date, data = .) %>% resid,
    detrend_ST = lm(`Self-Trancendence` ~ birth_date, data = .) %>% resid,
    detrend_OC = lm(`Openness to Change` ~ birth_date, data = .) %>% resid,
    deterendPG = lm(gdp_temp ~ birth_date, data = .) %>% resid
  ),
  conservation = ess_economy %>% group_by(birth_date) %>% 
    summarise(Conservation = mean(Conservation, na.rm = T)) %>% pull(Conservation),
  acf_conservation = acf(conservation, lag.max = 80),
  growth = ess_economy %>% group_by(birth_date) %>% 
    summarise(percent_growth = mean(percent_growth, na.rm = T)) %>% pull(percent_growth),
  acf_growth = acf(growth, lag.max = 80),
  se_cor = ess_economy %>% 
    dplyr::select(cntry, detrend_SE, percent_growth) %>% 
    group_by(cntry) %>% do(
      ft = cor.test(~ detrend_SE + percent_growth, data = .) %>% broom::tidy()
    ),
  se_cor_df = se_cor$ft %>% bind_rows() %>% mutate(
    cntry = se_cor$cntry
  ) %>% dplyr::select(9,1:6) %>% 
    mutate_at(2:7, round, 2) %>% 
    arrange(desc(abs(estimate))) %>% 
    knitr::kable(caption = "Кореляція частки років економічного зростання з рівнем самовдосконалення за країнами"),
  se_anova = ess_economy %>% 
    dplyr::select(cntry, detrend_SE, percent_growth) %>% 
    group_by(cntry) %>% do(
      ft = anova(lm(detrend_SE ~ percent_growth, data = .)) %>% broom::tidy() %>% dplyr::slice(1)
    ),
  se_anova_df = se_anova$ft %>% bind_rows() %>% mutate(
    cntry = se_anova$cntry
  ) %>% dplyr::select(7, 3:6) %>% 
    mutate_at(2:5, round, 2) %>% 
    arrange(p.value) %>% 
    knitr::kable(caption = "Зв'язок частки років економічного зростання з рівнем самовдосконалення за країнами"),
  co_cor = ess_economy %>% 
    dplyr::select(cntry, detrend_CO, percent_growth) %>% 
    group_by(cntry) %>% do(
      ft = cor.test(~ detrend_CO + percent_growth, data = .) %>% broom::tidy()
    ),
  co_cor_df = co_cor$ft %>% bind_rows() %>% mutate(
    cntry = co_cor$cntry
  ) %>% dplyr::select(9, 1:6) %>% 
    mutate_at(2:7, round, 2) %>% arrange(desc(abs(estimate))) %>% 
    knitr::kable(caption = "Кореляція частки років економічного зростання з рівнем консервативності за країнами"),
  co_anova = ess_economy %>% 
    dplyr::select(cntry, detrend_CO, percent_growth) %>% 
    group_by(cntry) %>% do(
      ft = anova(lm(detrend_CO ~ percent_growth, data = .)) %>% broom::tidy() %>% dplyr::slice(1)
    ),
  co_anova_df = co_anova$ft %>% bind_rows() %>% mutate(
    cntry = co_anova$cntry
  ) %>% dplyr::select(7,3:6) %>% mutate_at(2:5, round, 2) %>% arrange(`p.value`) %>% 
    knitr::kable(caption = "Зв'язок частки років економічного зростання з рівнем консервативності за країнами"),
  #'
  #' Читаємо файл з політичними даними
  #'
  polits_df = readr::read_rds("polits.rds") %>% 
    filter(!is.na(Conservation)) %>% 
    mutate_all(
      ~ifelse(is.infinite(.) | is.nan(.) | is.na(.), 0, .)
    ) %>% select_if(
      ~((sd(.,na.rm = T) != 0) | (is.na(sd(.,na.rm = T))))
    ) %>% mutate(
      defmin_mean = ifelse(is.na(defmin_mean), 0, defmin_mean),
      defmin_max = ifelse(is.na(defmin_max), 0, defmin_max),
      defmin_sd = ifelse(is.na(defmin_sd), 0, defmin_sd),
      defmin_n_distinct = ifelse(is.na(defmin_n_distinct), 0, defmin_n_distinct),
    ) %>% polit_residual(),
  ps = skimr::skim(polits_df),
  #'
  #' Список назв змінних з розшифровкою
  #'
  variable_dictionary = tibble::enframe(unlist(map(haven::read_stata("Database DPI2017/DPI2017.dta"), ~attr(.,"label")))),
  #'
  #' Робимо аналіз середніх
  #'
  t_test_summary = map_dfr(colnames(polits_df)[30:ncol(polits_df)], function(x) t_all(var = x, polits_df = polits_df)),
  #' 
  #' Отримуємо таблиці важливості ознак на основі градієнтного бустингу
  #' 
  dt = data.matrix(polits_df[, 30:ncol(polits_df)]),
  imp_oc = lgb_train(dt, as.numeric(polits_df$`Openness to Change` > polits_df$Conservation), params),
  imp_st = lgb_train(data = dt, label = as.numeric(polits_df$`Self-Trancendence` > polits_df$`Self-Enhancement`), params),
  #'
  #' Візуалізація відкритості до змін і збереження
  #'
  viz_oc = polits_df %>% 
    mutate(open = ifelse(`Openness to Change` > Conservation, "Більша відкритість до змін", "Більша консервативність")) %>% 
    select(open, imp_oc %>% 
             as_tibble() %>% 
             filter(Feature %in% (t_test_summary %>% filter(`p.value` == 0 & depend == "op_co") %>% pull(variable))) %>% 
             dplyr::slice(1:9) %>% pull(Feature)) %>% 
    tidyr::pivot_longer(cols = 2:10) %>% 
    group_by(name) %>% 
    mutate(value = ntile(value, 30)) %>% 
    ungroup() %>% 
    mutate(name = paste0(name %>% stringr::str_split("_") %>% sapply(function(.).[[1]]), " (",
                         name %>% stringr::str_split("_") %>% sapply(function(.).[[length(.)]]),")")) %>% 
    ggplot(aes(value, group = open, fill = open, color = NULL)) +
    scale_fill_manual(values = c("#e41a1c", "#4daf4a")) +
    geom_density(alpha = 0.8, position = "fill") +
    ggtitle(label = "Основні фактори впливу на пару відкритість до змін-збереження") +
    theme_ipsum() +
    facet_wrap(~name, scales = "free") +
    xlab("") +
    theme(legend.position = "bottom", legend.title = element_blank()),
  #'
  #' Візуалізація самоперевершення і саморозвитку
  #'
  viz_st = polits_df %>% 
    mutate(open = ifelse(`Self-Trancendence` > `Self-Enhancement`, "Більше самоперевершення", "Більший саморозвиток")) %>% 
    select(open, imp_st %>% 
             as_tibble() %>% 
             filter(Feature %in% (t_test_summary %>% filter(`p.value` == 0 & depend == "st_se") %>% pull(variable))) %>% 
             dplyr::slice(1:9) %>% pull(Feature)) %>% 
    tidyr::pivot_longer(cols = 2:10) %>% 
    group_by(name) %>% 
    mutate(value = ntile(value, 30)) %>% 
    ungroup() %>% 
    mutate(name = paste0(name %>% stringr::str_split("_") %>% sapply(function(.).[[1]]), " (",
                         name %>% stringr::str_split("_") %>% sapply(function(.).[[length(.)]]),")")) %>% 
    ggplot(aes(value, group = open, fill = open, color = NULL)) +
    scale_fill_manual(values = c("#e41a1c", "#4daf4a")) +
    geom_density(alpha = 0.8, position = "fill") +
    ggtitle(label = "Основні фактори впливу на пару самоперевершення-саморозвиток") +
    theme_ipsum(base_family = "PT Sans") +
    facet_wrap(~name, scales = "free") +
    xlab("") +
    theme(legend.position = "bottom", legend.title = element_blank()),
  report = rmarkdown::render(
    knitr_in("masters_thesis.Rmd"),
    output_file = file_out("masters_thesis.docx"),
    quiet = TRUE
  )
)

config <- drake_config(plan)
vis_drake_graph(config)

#clean()

make(plan, jobs = 4, parallelism = "future")

