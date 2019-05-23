recoder <- function(variable) {
  #'
  #' Функція для перекодування порядкової шкали в псевдометричну
  #'
  case_when(
    variable == "Very much like me" ~ 5,
    variable == "Like me" ~ 4,
    variable == "Somewhat like me" ~ 3,
    variable == "A little like me" ~ 2,
    variable == "Not like me" ~ 1,
    variable == "Not like me at all" ~ 0
  )
}

shwartz_4 <- function(ess) {
  #'
  #' Узагальнення цінностей Шварца до 4-х
  #'
  ess <- ess %>% mutate(
    impsafe = recoder(impsafe), ipstrgv = recoder(ipstrgv), ipfrule = recoder(ipfrule),
    ipbhprp = recoder(ipbhprp), ipmodst = recoder(ipmodst), imptrad = recoder(imptrad),
    iphlppl = recoder(iphlppl), iplylfr = recoder(iplylfr), ipeqopt = recoder(ipeqopt),
    ipudrst = recoder(ipudrst), impenv  = recoder(impenv),  impdiff = recoder(impdiff),
    ipadvnt = recoder(ipadvnt), ipgdtim = recoder(ipgdtim), impfun  = recoder(impfun),
    ipshabt = recoder(ipshabt), ipsuces = recoder(ipsuces), imprich = recoder(imprich),
    iprspot = recoder(iprspot), ipcrtiv = recoder(ipcrtiv), impfree = recoder(impfree)
  )
  coef <- ess %>% mutate(m = (impsafe + ipstrgv + ipfrule + ipbhprp + ipmodst + imptrad + 
                                iphlppl + iplylfr + ipeqopt + ipudrst + impenv + impdiff + 
                                ipadvnt + ipgdtim + impfun + ipshabt + ipsuces + imprich +
                                iprspot + ipcrtiv + impfree) / 21) %>% pull("m")
  ess %>% mutate(
    `Self-Enhancement` = (imprich + iprspot + ipshabt + ipsuces) / 4 - coef,
    Conservation = (impsafe + ipstrgv + ipfrule + ipbhprp + ipmodst + imptrad) / 6 - coef,
    `Self-Trancendence` = (iphlppl + iplylfr + ipeqopt + ipudrst + impenv) / 5 - coef,
    `Openness to Change` = (ipcrtiv + impfree + impdiff + ipadvnt + ipgdtim + impfun) / 6 - coef
  )
}

shwartz_10 <- function(ess) {
  #'
  #' Узагальнення цінностей Шварца до 10-ти
  #'
  coef <- ess %>% mutate(m = (impsafe + ipstrgv + ipfrule + ipbhprp + ipmodst + imptrad + 
                                iphlppl + iplylfr + ipeqopt + ipudrst + impenv + impdiff + 
                                ipadvnt + ipgdtim + impfun + ipshabt + ipsuces + imprich +
                                iprspot + ipcrtiv + impfree)/21) %>% pull("m")
  ess %>% mutate(
    security = (impsafe + ipstrgv)/2 - coef,
    conformity = (ipfrule + ipbhprp)/2 - coef,
    tradition = (ipmodst + imptrad)/2 - coef,
    benevolence = (iphlppl + iplylfr)/2 - coef,
    universalism = (ipeqopt + ipudrst + impenv)/3 - coef,
    self_direction = (ipcrtiv + impfree)/2 - coef,
    stimulation = (impdiff + ipadvnt)/2 - coef,
    hedonism = (ipgdtim + impfun)/2 - coef,
    achievement = (ipshabt + ipsuces)/2- coef,
    power = (imprich + iprspot)/2 - coef
  )
}

unite <- function(spdf) {
  #'
  #' Об'єднання регіонів у шейп-файлах
  #'
  cat(spdf$NAME_0[1], "\n")
  pol <- unionSpatialPolygons(spdf, spdf$NAME_1)
  pol2 <- rmapshaper::ms_simplify(pol, keep = 0.25)
  for (i in 1:length(pol2@polygons)) pol2@polygons[[i]]@ID <- pol@polygons[[i]]@ID
  df <- data.frame(
    id = getSpPPolygonsIDSlots(pol2)
  )
  row.names(df) <- getSpPPolygonsIDSlots(pol2)
  SpatialPolygonsDataFrame(pol2, df)
}


ess_region <- function(ess) {
  #'
  #' Уніфікація назв європейських регіонів
  #'
  ess %>% mutate(
  region = case_when(
    cntry == "Denmark" & region == "Københavns og Frederiksberg Kommune" ~ "Hovedstaden",
    cntry == "Denmark" & region == "Københavns Amt" ~ "Hovedstaden",
    cntry == "Denmark" & region == "Bornholms Amt" ~ "Hovedstaden",
    cntry == "Denmark" & region == "Roskilde Amt" ~ "Sjælland",
    cntry == "Denmark" & region == "Frederiksborg Amt" ~ "Hovedstaden",
    cntry == "Denmark" & region == "Vestsjællands Amt" ~ "Sjælland",
    cntry == "Denmark" & region == "Storstrøms Amt" ~ "Sjælland",
    cntry == "Denmark" & region == "Fyns Amt" ~ "Syddanmark",
    cntry == "Denmark" & region == "Vejle Amt" ~ "Syddanmark",
    cntry == "Denmark" & region == "Sønderjyllands Amt" ~ "Syddanmark",
    cntry == "Denmark" & region == "Århus Amt" ~ "Nordjylland",
    cntry == "Denmark" & region == "Ribe Amt" ~ "Syddanmark",
    cntry == "Denmark" & region == "Ringkøbing Amt" ~ "Midtjylland",
    cntry == "Denmark" & region == "Viborg Amt" ~ "Midtjylland",
    cntry == "Denmark" & region == "Nordjyllands Amt" ~ "Nordjylland",
    cntry == "Finland" & region == "Mid Finland" ~ "Oulu",
    cntry == "Finland" & region == "Southern Finland and Åland" ~ "Southern Finland",
    cntry == "Finland" & region == "Northern Finland" ~ "Lapland",
    cntry == "Finland" & region == "Uusimaa" ~ "Southern Finland",
    cntry == "Finland" & region == "Eastern" ~ "Eastern Finland",
    cntry == "Finland" & region == "Oulu" ~ "Oulu",
    cntry == "Finland" & region == "Southern Finland" ~ "Southern Finland",
    cntry == "Finland" & region == "Lapland" ~ "Lapland",
    cntry == "Finland" & region == "Eastern Finland" ~ "Eastern Finland",
    cntry == "Finland" & region == "Finland" ~ "Southern Finland",
    cntry == "Finland" & region == "Varsinais-Suomi" ~ "Southern Finland",
    cntry == "Finland" & region == "Pirkanmaa" ~ "Western Finland",
    cntry == "Finland" & region == "Etelä-Pohjanmaa" ~ "Eastern Finland",
    cntry == "Finland" & region == "Päijät-Häme" ~ "Southern Finland",
    cntry == "Finland" & region == "Itä-Uusimaa" ~ "Southern Finland",
    cntry == "Finland" & region == "Lappi" ~ "Lapland",
    cntry == "Finland" & region == "Satakunta" ~ "Western Finland",
    cntry == "Finland" & region == "Kanta-Häme" ~ "Southern Finland",
    cntry == "Finland" & region == "Pohjois-Pohjanmaa" ~ "Eastern Finland",
    cntry == "Finland" & region == "Kymenlaakso" ~ "Southern Finland",
    cntry == "Finland" & region == "Keski-Suomi" ~ "Western Finland",
    cntry == "Finland" & region == "Etelä-Savo" ~ "Eastern Finland",
    cntry == "Finland" & region == "Kainuu" ~ "Oulu",
    cntry == "Finland" & region == "Pohjois-Savo" ~ "Eastern Finland",
    cntry == "Finland" & region == "Pohjois-Karjala" ~ "Eastern Finland",
    cntry == "Finland" & region == "Pohjanmaa" ~ "Western Finland",
    cntry == "Finland" & region == "Etelä-Karjala" ~ "Southern Finland",
    cntry == "Finland" & region == "Keski-Pohjanmaa" ~ "Western Finland",
    cntry == "Finland" & region == "Helsinki-Uusimaa" ~ "Southern Finland",
    cntry == "Finland" & region == "Åland" ~ "Southern Finland",
    cntry == "United Kingdom" & region == "London" ~ "England",
    cntry == "United Kingdom" & region == "Scotland" ~ "Scotland",
    cntry == "United Kingdom" & region == "North West" ~ "England",
    cntry == "United Kingdom" & region == "Wales" ~ "Wales",
    cntry == "United Kingdom" & region == "Yorkshire and The Humber" ~ "England",
    cntry == "United Kingdom" & region == "North East" ~ "England",
    cntry == "United Kingdom" & region == "West Midlands" ~ "England",
    cntry == "United Kingdom" & region == "East Midlands" ~ "England",
    cntry == "United Kingdom" & region == "East of England" ~ "England",
    cntry == "United Kingdom" & region == "South East" ~ "England",
    cntry == "United Kingdom" & region == "South West" ~ "England",
    cntry == "United Kingdom" & region == "Northern Ireland" ~ "Northern Ireland",
    cntry == "Greece" & region == "Attiki" ~ "Attica",
    cntry == "Greece" & region == "Kentriki Makedonia" ~ "Macedonia and Thrace",
    cntry == "Greece" & region == "Anatoliki Makedonia, Thraki" ~ "Macedonia and Thrace",
    cntry == "Greece" & region == "Thessalia" ~ "Thessaly and Central Greece",
    cntry == "Greece" & region == "Sterea Ellada" ~ "Thessaly and Central Greece",
    cntry == "Greece" & region == "Peloponnisos" ~ "Peloponnese, Western Greece and the Ionian Islands",
    cntry == "Greece" & region == "Kriti" ~ "Crete",
    cntry == "Greece" & region == "Ipeiros" ~ "Epirus and Western Macedonia",
    cntry == "Greece" & region == "Dytiki Makedonia" ~ "Epirus and Western Macedonia",
    cntry == "Greece" & region == "Dytiki Ellada" ~ "Peloponnese, Western Greece and the Ionian Islands",
    cntry == "Greece" & region == "Ionia Nissia" ~ "Peloponnese, Western Greece and the Ionian Islands",
    cntry == "Greece" & region == "Notio Agaio" ~ "Aegean",
    cntry == "Greece" & region == "Voreio Agaio" ~ "Aegean",
    cntry == "Netherlands" & region == "Groot-Amsterdam" ~ "Noord-Holland",
    cntry == "Netherlands" & region == "Agglomeratie Haarlem" ~ "Noord-Holland",
    cntry == "Netherlands" & region == "Het Gooi en Vechtstreek" ~ "Noord-Holland",
    cntry == "Netherlands" & region == "Flevoland" ~ "Nord Netherlands",
    cntry == "Netherlands" & region == "Zaanstreek" ~ "Noord-Holland",
    cntry == "Netherlands" & region == "Kop van Noord-Holland" ~ "Friesland",
    cntry == "Netherlands" & region == "Alkmaar en Omgeving" ~ "Noord-Holland",
    cntry == "Netherlands" & region == "Alkmaar en omgeving" ~ "Noord-Holland",
    cntry == "Netherlands" & region == "Ijmond" ~ "Noord-Holland",
    cntry == "Netherlands" & region == "IJmond" ~ "Noord-Holland",
    cntry == "Netherlands" & region == "Agglomeratie Leiden en Bollenstreek" ~ "Zuid-Holland",
    cntry == "Netherlands" & region == "Agglomeratie  S-Gravenhage" ~ "Zuid-Holland",
    cntry == "Netherlands" & region == "Agglomeratie`s-Gravenhage" ~ "Zuid-Holland",
    cntry == "Netherlands" & region == "Delft en Westland" ~ "Zuid-Holland",
    cntry == "Netherlands" & region == "Oost-Zuid-Holland" ~ "Zuid-Holland",
    cntry == "Netherlands" & region == "Groot-Rijnmond" ~ "Zuid-Holland",
    cntry == "Netherlands" & region == "Zuidoost-Zuid-Holland" ~ "Zuid-Holland",
    cntry == "Netherlands" & region == "Utrecht" ~ "Utrecht",
    cntry == "Netherlands" & region == "Veluwe" ~ "Gelderland",
    cntry == "Netherlands" & region == "Gelderland" ~ "Gelderland",
    cntry == "Netherlands" & region == "Zuidwest-Gelderland" ~ "Gelderland",
    cntry == "Netherlands" & region == "Midden-Noord-Brabant" ~ "Noord-Brabant",
    cntry == "Netherlands" & region == "Overig Zeeland" ~ "Zuid-Holland",
    cntry == "Netherlands" & region == "Zeeuwsch-Vlaanderen" ~ "Zuid-Holland",
    cntry == "Netherlands" & region == "Zeeland" ~ "Zuid-Holland",
    cntry == "Netherlands" & region == "West-Noord-Brabant" ~ "Noord-Brabant",
    cntry == "Netherlands" & region == "Zuidoost-Noord-Brabant" ~ "Noord-Brabant",
    cntry == "Netherlands" & region == "Noordoost-Noord-Brabant" ~ "Noord-Brabant",
    cntry == "Netherlands" & region == "Noord-Brabant" ~ "Noord-Brabant",
    cntry == "Netherlands" & region == "Noord-Limburg" ~ "Limburg",
    cntry == "Netherlands" & region == "Midden-Limburg" ~ "Limburg",
    cntry == "Netherlands" & region == "Zuid-Limburg" ~ "Limburg",
    cntry == "Netherlands" & region == "Limburg (NL)" ~ "Limburg",
    cntry == "Netherlands" & region == "Arnhem\\Nijmegen" ~ "Gelderland",
    cntry == "Netherlands" & region == "Achterhoek" ~ "Gelderland",
    cntry == "Netherlands" & region == "Zuidwest-Overijssel" ~ "Overijssel",
    cntry == "Netherlands" & region == "Twente" ~ "Overijssel",
    cntry == "Netherlands" & region == "Noord-Ooverijssel" ~ "Overijssel",
    cntry == "Netherlands" & region == "Noord-Overijssel" ~ "Overijssel",
    cntry == "Netherlands" & region == "Overijssel" ~ "Overijssel",
    cntry == "Netherlands" & region == "Zuidoost-Drenthe" ~ "Nord Netherlands",
    cntry == "Netherlands" & region == "Zuidwest-Drenthe" ~ "Nord Netherlands",
    cntry == "Netherlands" & region == "Noord-Drenthe" ~ "Nord Netherlands",
    cntry == "Netherlands" & region == "Drenthe" ~ "Nord Netherlands",
    cntry == "Netherlands" & region == "Zuidoost-Friesland" ~ "Nord Netherlands",
    cntry == "Netherlands" & region == "Noord-Friesland" ~ "Nord Netherlands",
    cntry == "Netherlands" & region == "Zuidwest-Friesland" ~ "Nord Netherlands",
    cntry == "Netherlands" & region == "Friesland (NL)" ~ "Nord Netherlands",
    cntry == "Netherlands" & region == "Overig Groningen" ~ "Nord Netherlands",
    cntry == "Netherlands" & region == "Oost-Groningen" ~ "Nord Netherlands",
    cntry == "Netherlands" & region == "Delfzijl  en Omgeving" ~ "Nord Netherlands",
    cntry == "Netherlands" & region == "Delfzijl en omgeving" ~ "Nord Netherlands",
    cntry == "Netherlands" & region == "Groningen" ~ "Nord Netherlands",
    cntry == "Netherlands" & region == "Alkmaar en omgeving" ~ "Noord-Holland",
    cntry == "Netherlands" & region == "Noord-Holland" ~ "Noord-Holland",
    cntry == "Netherlands" & region == "IJmond" ~ "Noord-Holland",
    cntry == "Netherlands" & region == "Agglomeratie`s-Gravenhage" ~ "Zuid-Holland",
    cntry == "Netherlands" & region == "Zuid-Holland" ~ "Zuid-Holland",
    cntry == "Netherlands" & region == "Noord-Overijssel" ~ "Overijssel",
    cntry == "Netherlands" & region == "Delfzijl en omgeving" ~ "Nord Netherlands",
    cntry == "Slovakia" & region == "Bratislavský kraj" ~ "Bratislava Reg.",
    cntry == "Slovakia" & region == "Trnavský kraj" ~ "Trnava Reg.",
    cntry == "Slovakia" & region == "Trenèiansky kraj" ~ "Trencin Reg.",
    cntry == "Slovakia" & region == "Nitriansky kraj" ~ "Nitra Reg.",
    cntry == "Slovakia" & region == "Žilinský kraj" ~ "Zilina Reg.",
    cntry == "Slovakia" & region == "Košický kraj" ~ "Kosice Reg.",
    cntry == "Slovakia" & region == "Banskobystrický kraj" ~ "Banska Bystrica Reg.",
    cntry == "Slovakia" & region == "Prešovský kraj" ~ "Presov Reg.",
    cntry == "Sweden" & region == "Västra Götalands län" ~ "Västsverige",
    cntry == "Sweden" & region == "Stockholms län" ~ "Stockholm",
    cntry == "Sweden" & region == "Kalmar län" ~ "Småland och Öarna",
    cntry == "Sweden" & region == "Hallands län" ~ "Västsverige",
    cntry == "Sweden" & region == "Skåne län" ~ "Sydsverige",
    cntry == "Sweden" & region == "Östergötlands län" ~ "Östra Mellansverige",
    cntry == "Sweden" & region == "Västernorrlands län" ~ "Mellemsta Norrland",
    cntry == "Sweden" & region == "Västmanlands län" ~ "Östra Mellansverige",
    cntry == "Sweden" & region == "Norrbottens län" ~ "Övre Norrland",
    cntry == "Sweden" & region == "Uppsala län" ~ "Östra Mellansverige",
    cntry == "Sweden" & region == "Kronobergs län" ~ "Småland och Öarna",
    cntry == "Sweden" & region == "Gävleborgs län" ~ "Norra Mellansverige",
    cntry == "Sweden" & region == "Södermanlands län" ~ "Östra Mellansverige",
    cntry == "Sweden" & region == "Västerbottens län" ~ "Övre Norrland",
    cntry == "Sweden" & region == "Dalarnas län" ~ "Norra Mellansverige",
    cntry == "Sweden" & region == "Örebro län" ~ "Östra Mellansverige",
    cntry == "Sweden" & region == "Blekinge län" ~ "Sydsverige",
    cntry == "Sweden" & region == "Jönköpings län" ~ "Småland och Öarna",
    cntry == "Sweden" & region == "Värmlands län" ~ "Norra Mellansverige",
    cntry == "Sweden" & region == "Jämtlands län" ~ "Mellemsta Norrland",
    cntry == "Sweden" & region == "Gotlands län" ~ "Småland och Öarna",
    cntry == "Italy" & region == "Provincia autonoma di Trento" ~ "Veneto",
    cntry == "Italy" & region == "Emilia Romagna" ~ "Emilia-Romagna-Toscana",
    cntry == "Italy" & region == "Friulia Venezia Giulia" ~ "Veneto",
    cntry == "Italy" & region == "Puglia" ~ "South Italy",
    cntry == "Italy" & region == "Sicilia" ~ "Sicily-Sardegna",
    cntry == "Italy" & region == "Provincia autonoma di Bolzano" ~ "Veneto",
    cntry == "Italy" & region == "Veneto" ~ "Veneto",
    cntry == "Italy" & region == "Sardegna" ~ "Sicily-Sardegna",
    cntry == "Italy" & region == "Liguria" ~ "Piemonte",
    cntry == "Italy" & region == "Piemonte" ~ "Piemonte",
    cntry == "Italy" & region == "Toscana" ~ "Emilia-Romagna-Toscana",
    cntry == "Italy" & region == "Lazio" ~ "Centre Italy",
    cntry == "Italy" & region == "Abruzzo" ~ "Centre Italy",
    cntry == "Italy" & region == "Marche" ~ "Centre Italy",
    cntry == "Italy" & region == "Umbria" ~ "Centre Italy",
    cntry == "Italy" & region == "Apulia" ~ "South Italy",
    cntry == "Italy" & region == "Basilicata" ~ "South Italy",
    cntry == "Italy" & region == "Calabria" ~ "South Italy",
    cntry == "Poland" & region == "Warminsko-mazurskie" ~ "Warmińsko-Mazurskie",
    cntry == "Poland" & region == "Dolnoslaskie" ~ "Dolnośląskie",
    cntry == "Poland" & region == "Lodzkie" ~ "Łódzkie",
    cntry == "Poland" & region == "Kujawsko-pomorskie" ~ "Kujawsko-Pomorskie",
    cntry == "Poland" & region == "Malopolskie" ~ "Małopolskie",
    cntry == "Poland" & region == "Slaskie" ~ "Śląskie",
    cntry == "Poland" & region == "Swietokrzyskie" ~ "Świętokrzyskie",
    cntry == "Ukraine" & region == "Crimea, Autonomy Republic" ~ "South Ukraine",
    cntry == "Ukraine" & region == "Ivano-Frankivska oblast" ~ "West Ukraine",
    cntry == "Ukraine" & region == "Rivenska oblast" ~ "West Ukraine",
    cntry == "Ukraine" & region == "Poltavska oblast" ~ "Center Ukraine",
    cntry == "Ukraine" & region == "Sumska oblast" ~ "Nord Ukraine",
    cntry == "Ukraine" & region == "Chernigivska oblast" ~ "Nord Ukraine",
    cntry == "Ukraine" & region == "Chernovytska oblast" ~ "West Ukraine",
    cntry == "Ukraine" & region == "Kharkivska oblast" ~ "East Ukraine",
    cntry == "Ukraine" & region == "Zhytomyrska oblast" ~ "Nord Ukraine",
    cntry == "Ukraine" & region == "Kirovogradska oblast" ~ "Center Ukraine",
    cntry == "Ukraine" & region == "Cherkasska oblast" ~ "Center Ukraine",
    cntry == "Ukraine" & region == "Volynska oblast" ~ "West Ukraine",
    cntry == "Ukraine" & region == "Mykolaivska oblast" ~ "South Ukraine",
    cntry == "Ukraine" & region == "Khersonska oblast" ~ "South Ukraine",
    cntry == "Ukraine" & region == "Zaporizska oblast" ~ "South Ukraine",
    cntry == "Ukraine" & region == "Dnipropetrovska oblast" ~ "Center Ukraine",
    cntry == "Ukraine" & region == "Lvivska oblast" ~ "West Ukraine",
    cntry == "Ukraine" & region == "Luganska oblast" ~ "East Ukraine",
    cntry == "Ukraine" & region == "Donetska oblast" ~ "East Ukraine",
    cntry == "Ukraine" & region == "Zakarpatska oblast" ~ "West Ukraine",
    cntry == "Ukraine" & region == "Khmelnitska oblast" ~ "West Ukraine",
    cntry == "Ukraine" & region == "Kyivska oblast" ~ "Nord Ukraine",
    cntry == "Ukraine" & region == "Kyiv city" ~ "Nord Ukraine",
    cntry == "Ukraine" & region == "Odesska oblast" ~ "South Ukraine",
    cntry == "Ukraine" & region == "Vynnytska oblast" ~ "Center Ukraine",
    cntry == "Ukraine" & region == "Ternopilska oblast" ~ "West Ukraine",
    cntry == "Albania" & region == "Shkodër" ~ "Nord Albania",
    cntry == "Albania" & region == "Kukës" ~ "Nord Albania",
    cntry == "Albania" & region == "Lezhë" ~ "Nord Albania",
    cntry == "Albania" & region == "Dibër" ~ "Nord Albania",
    cntry == "Albania" & region == "Durrës" ~ "Nord Albania",
    cntry == "Albania" & region == "Tiranë" ~ "Nord Albania",
    cntry == "Albania" & region == "Berat" ~ "South Albania",
    cntry == "Albania" & region == "Elbasan" ~ "South Albania",
    cntry == "Albania" & region == "Fier" ~ "South Albania",
    cntry == "Albania" & region == "Gjirokastër" ~ "South Albania",
    cntry == "Albania" & region == "Korçë" ~ "South Albania",
    cntry == "Albania" & region == "Vlorë" ~ "South Albania",
    cntry == "Lithuania" & region == "Kauno apskritis" ~ "Kauno",
    cntry == "Lithuania" & region == "Telðiø apskritis" ~ "West Lithuania",
    cntry == "Lithuania" & region == "Klaipëdos apskritis" ~ "West Lithuania",
    cntry == "Lithuania" & region == "Marijampolës apskritis" ~ "South Lithuania",
    cntry == "Lithuania" & region == "Vilniaus apskritis" ~ "Vilniaus",
    cntry == "Lithuania" & region == "Utenos apskritis" ~ "Nord Lithuania",
    cntry == "Lithuania" & region == "Panevëþio apskritis" ~ "Nord Lithuania",
    cntry == "Lithuania" & region == "Alytaus apskritis" ~ "South Lithuania",
    cntry == "Lithuania" & region == "Ðiauliø apskritis" ~ "Nord Lithuania",
    cntry == "Lithuania" & region == "Tauragës apskritis" ~ "West Lithuania",
    cntry == "Lithuania" & region == "Klaipedos apskritis" ~ "West Lithuania",
    cntry == "Lithuania" & region == "Panevežio apskritis" ~ "Nord Lithuania",
    cntry == "Lithuania" & region == "Taurages apskritis" ~ "West Lithuania",
    cntry == "Lithuania" & region == "Telšiu apskritis" ~ "West Lithuania",
    cntry == "Lithuania" & region == "Šiauliu apskritis" ~ "Nord Lithuania",
    cntry == "Lithuania" & region == "Marijampoles apskritis" ~ "South Lithuania",
    cntry == "Latvia" & region == "Kurzeme" ~ "Nord-West Latvia",
    cntry == "Latvia" & region == "Latgale" ~ "South-East Latvia",
    cntry == "Latvia" & region == "Riga" ~ "Nord-West Latvia",
    cntry == "Latvia" & region == "Vidzeme" ~ "South-East Latvia",
    cntry == "Latvia" & region == "Zemgale" ~ "South-East Latvia",
    cntry == "Bulgaria" & region == "Sofia (region)" ~ "South Western",
    cntry == "Bulgaria" & region == "Sofia (capital)" ~ "South Western",
    cntry == "Bulgaria" & region == "Bourgas" ~ "South Eastern",
    cntry == "Bulgaria" & region == "Vratca" ~ "North Western",
    cntry == "Bulgaria" & region == "Kurdjali" ~ "South Central",
    cntry == "Bulgaria" & region == "Kustendil" ~ "South Western",
    cntry == "Bulgaria" & region == "Lovetch" ~ "North Western",
    cntry == "Bulgaria" & region == "Pazardjik" ~ "South Central",
    cntry == "Bulgaria" & region == "Rouse" ~ "North Central",
    cntry == "Bulgaria" & region == "Smolian" ~ "South Central",
    cntry == "Bulgaria" & region == "Sofia-region" ~ "South Western",
    cntry == "Bulgaria" & region == "Shoumen" ~ "North Eastern",
    cntry == "Bulgaria" & region == "Iambol" ~ "South Eastern",
    cntry == "Turkey" & region == "Western Black Sea" ~ "Black Sea",
    cntry == "Turkey" & region == "Eastern Black Sea" ~ "Black Sea",
    cntry == "Turkey" & region == "North Eastern Anatolia" ~ "Anatolia",
    cntry == "Turkey" & region == "Central Anatolia" ~ "Anatolia",
    cntry == "Turkey" & region == "Western Anatolia" ~ "Anatolia",
    cntry == "Turkey" & region == "Eastern Marmara" ~ "Marmara",
    cntry == "Turkey" & region == "Western Marmara" ~ "Marmara",
    cntry == "Turkey" & region == "South east" ~ "East",
    cntry == "Ireland" & region == "South East" ~ "South-Ireland",
    cntry == "Ireland" & region == "Mid East" ~ "South-Ireland",
    cntry == "Ireland" & region == "Mid West" ~ "South-Ireland",
    cntry == "Ireland" & region == "South-West" ~ "South-Ireland",
    cntry == "Kosovo" & region == "Gjakova" ~ "South Kosovo",
    cntry == "Kosovo" & region == "Gjilan" ~ "South Kosovo",
    cntry == "Kosovo" & region == "Prizren" ~ "South Kosovo",
    cntry == "Kosovo" & region == "Ferizaj" ~ "South Kosovo",
    cntry == "Kosovo" & region == "Peja" ~ "Nord Kosovo",
    cntry == "Kosovo" & region == "Mitrovica" ~ "Nord Kosovo",
    cntry == "Kosovo" & region == "Prishtina" ~ "Nord Kosovo",
    cntry == "Slovenia" & region == "Gorenjska" ~ "Gorenjska",
    cntry == "Slovenia" & region == "Goriska" ~ "Goriška",
    cntry == "Slovenia" & region == "Goriška" ~ "Goriška",
    cntry == "Slovenia" & region == "Jugovzhodna Slovenija" ~ "Jugovzhodna Slovenija",
    cntry == "Slovenia" & region == "Koroska" ~ "Koroška",
    cntry == "Slovenia" & region == "Koroška" ~ "Koroška",
    cntry == "Slovenia" & region == "Notranjsko-kraska" ~ "Notranjsko-kraška",
    cntry == "Slovenia" & region == "Notranjsko-kraška" ~ "Notranjsko-kraška",
    cntry == "Slovenia" & region == "Obalno-kraska" ~ "Obalno-kraška",
    cntry == "Slovenia" & region == "Obalno-kraška" ~ "Obalno-kraška",
    cntry == "Slovenia" & region == "Osrednjeslovenska" ~ "Osrednjeslovenska",
    cntry == "Slovenia" & region == "Podravska" ~ "Podravska",
    cntry == "Slovenia" & region == "Pomurska" ~ "Pomurska",
    cntry == "Slovenia" & region == "Savinjska" ~ "Savinjska",
    cntry == "Slovenia" & region == "Spodnjeposavska" ~ "Spodnjeposavska",
    cntry == "Slovenia" & region == "Zasavska" ~ "Zasavska",
    T ~ region
  )
)
}

imoran <- function(regions_gSimplify){
  #'
  #' Розрахунок І Морана
  #'
  w <- poly2nb(regions_gSimplify, paste(regions_gSimplify$id, 1:length(regions_gSimplify)))
  
  nm <- sapply(w, function(x) all(x!=0))
  
  w <- poly2nb(regions_gSimplify[nm,], paste(regions_gSimplify$id[nm], 1:sum(nm==T)))
  
  ww <-  nb2listw(w, style='B')
  
  a <- bind_rows(
    moran.test(regions_gSimplify[nm,]$`Openness to Change`, ww) %>% broom::tidy(),
    moran.test(regions_gSimplify[nm,]$`Self-Trancendence`, ww) %>% broom::tidy(),
    moran.test(regions_gSimplify[nm,]$Conservation, ww) %>% broom::tidy(),
    moran.test(regions_gSimplify[nm,]$`Self-Enhancement`, ww) %>% broom::tidy())
  a$value <- c("Openness to Change", "Self-Trancendence", "Conservation", "Self-Enhancement")
  a
}

norm_tbl <- function(tbl) {
  #'
  #' Нормалізація матриці зі значеннями цінностей
  #'
  tbl[6:15] <- tbl[6:15] %>% 
    as.matrix() %>% 
    BBmisc::normalize("range", range = c(0, 5))
  tbl
}

modify_A <- function(A, idx, tbl) {
  #'
  #' Нормалізація матриці зі значеннями цінностей
  #'
  diag(A) <- 1
  A <- A[idx, idx]
  colnames(A) <- rownames(A) <- tbl$region
  A
}

t_all <- function(var, polits_df) {
  #'
  #' Функція для розрахунку t Стьюдента для двох груп: 
  #' - групи тих, хто за цінностями є більш відкритим до змін
  #' - групи тих, хто за цінностями є більш консервативним
  #'
  polits_df %>% 
    mutate(
      open = as.numeric(`Openness to Change` > Conservation)
    ) %>% 
    list() %>% 
    map(.f = ~t.test(as.formula(paste0(var," ~ open")), data = .)) %>% 
    magrittr::extract2(1) %>% 
    broom::tidy() %>% 
    mutate(variable = var) %>% 
    dplyr::select(c(11,1:10)) %>% 
    mutate(depend = "op_co") %>% 
    bind_rows(
      polits_df %>% 
        mutate(
          st_se = as.numeric(`Self-Trancendence` > `Self-Enhancement`)
        ) %>% 
        list() %>% 
        map(.f = ~t.test(as.formula(paste0(var," ~ st_se")), data = .)) %>% 
        magrittr::extract2(1) %>% 
        broom::tidy() %>% 
        mutate(variable = var) %>% 
        dplyr::select(c(11,1:10)) %>% 
        mutate(depend = "st_se")
    )
}

anova_all <- function(var, polits_df) {
  #'
  #' Функція для розрахунку ANOVA
  #'
  polits_df %>% 
    list() %>% 
    map(.f = ~anova(lm(as.formula(paste0("`Openness to Change` ~ ", var)), data = .))) %>% 
    magrittr::extract2(1) %>% 
    broom::tidy() %>% 
    dplyr::slice(1) %>% 
    mutate(variable = var) %>% 
    dplyr::select(c(7,3:6))
}

lgb_train <- function(data, label, params) {
  #'
  #' Функція для розрахунку градієнтного бустингу
  #'
  lgb_oc = lgb.Dataset(data = data, label = label)
  lgb_cv_oc = lgb.cv(params = params, lgb_oc, nrounds = 5000, early_stopping_rounds = 50, nfold = 5)
  lgb_train_oc = lgb.train(params = params, lgb_oc, nrounds = lgb_cv_oc$best_iter)
  lgb.importance(lgb_train_oc)
}

polit_residual <- function(polits_df) {
  #'
  #' Функція для усунення впливу факторів віку та країни респондента
  #' усуває лінійний вплив цих двох змінних на інші змінні
  #'
  for (i in 30:ncol(polits_df)){
    r <- c()
    cat(i, "\n")
    fit <- lm(polits_df[[i]] ~ polits_df$agea*polits_df$cntry)
    if(summary(fit)$adj.r.squared > 0.1 & summary(fit)$adj.r.squared < 0.5) {
      polits_df[[i]] <- fit %>% resid() %>% unname()
    }
    if(summary(fit)$adj.r.squared >= 0.5) {
      r <- c(r, i)
    }
  }
  polits_df %>% select(-r)
}

