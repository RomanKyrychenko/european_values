library(drake)

trans <- function(dpi_stata) {
  dpi_stata[dpi_stata$countryname == "Croatia" & dpi_stata$year <= 1990, 2:114] <- dpi_stata[dpi_stata$countryname == "Yugoslavia" & dpi_stata$year <= 1990, 2:114]
  dpi_stata[dpi_stata$countryname == "Slovenia" & dpi_stata$year <= 1991, 2:114] <- dpi_stata[dpi_stata$countryname == "Yugoslavia" & dpi_stata$year <= 1991, 2:114]
  dpi_stata[dpi_stata$countryname == "Ukraine" & dpi_stata$year <= 1991, 2:114] <- dpi_stata[dpi_stata$countryname == "Soviet Union" & dpi_stata$year <= 1991, 2:114]
  dpi_stata[dpi_stata$countryname == "Estonia" & dpi_stata$year <= 1991, 2:114] <- dpi_stata[dpi_stata$countryname == "Soviet Union" & dpi_stata$year <= 1991, 2:114]
  dpi_stata[dpi_stata$countryname == "Lithuania" & dpi_stata$year <= 1991, 2:114] <- dpi_stata[dpi_stata$countryname == "Soviet Union" & dpi_stata$year <= 1991, 2:114]
  dpi_stata[dpi_stata$countryname == "Latvia" & dpi_stata$year <= 1991, 2:114] <- dpi_stata[dpi_stata$countryname == "Soviet Union" & dpi_stata$year <= 1991, 2:114]
  dpi_stata[dpi_stata$countryname == "Slovakia" & dpi_stata$year <= 1992, 2:114] <- dpi_stata[dpi_stata$countryname == "Czech Rep." & dpi_stata$year <= 1992, 2:114]
  dpi_stata[dpi_stata$countryname == "GDR" & dpi_stata$year >= 1992, 2:114] <- dpi_stata[dpi_stata$countryname == "FRG/Germany" & dpi_stata$year >= 1992, 2:114]
  dpi_stata[dpi_stata$countryname == "Soviet Union" & dpi_stata$year <= 1991, 1] <- "Russia"
  
  dpi_stata <- dpi_stata %>% filter(countryname != "Yugoslavia")
  
  dpi_stata <- dpi_stata %>% mutate(
    system =   haven::as_factor(system),
    execrlc =  haven::as_factor(execrlc),
    execrel =  haven::as_factor(execrel),
    gov1rlc =  haven::as_factor(gov1rlc),
    gov1rel =  haven::as_factor(gov1rel),
    gov2rlc =  haven::as_factor(gov2rlc),
    gov2rel =  haven::as_factor(gov2rel),
    gov3rlc =  haven::as_factor(gov3rlc),
    gov3rel =  haven::as_factor(gov3rel),
    opp1rlc =  haven::as_factor(opp1rlc),
    opp1rel =  haven::as_factor(opp1rel),
    housesys = haven::as_factor(housesys),
    sensys =   haven::as_factor(sensys),
    muni =     haven::as_factor(muni),
    state =    haven::as_factor(state)
  )
  
  dpi_stata <- dpi_stata %>% mutate_if(is.numeric,~ ifelse(. == -999, NA, .))
}

ess_region <- function(ess) {
  ess$region <- ess %>%
    dplyr::select(starts_with("region")) %>%
    as.matrix() %>%
    apply(1, function(x) x[!is.na(x)][1])
  ess
}

shwartz <- function(ess) {
  coef <- ess %>% mutate(m = (impsafe + ipstrgv + ipfrule + ipbhprp + ipmodst + imptrad + iphlppl + iplylfr + ipeqopt +
                                ipudrst + impenv + impdiff + ipadvnt + ipgdtim + impfun + ipshabt + ipsuces + imprich +
                                iprspot + ipcrtiv + impfree) / 21) %>% pull("m")
  ess %>% mutate(
    `Self-Enhancement` = (imprich + iprspot + ipshabt + ipsuces) / 4 - coef,
    Conservation = (impsafe + ipstrgv + ipfrule + ipbhprp + ipmodst + imptrad) / 6 - coef,
    `Self-Trancendence` = (iphlppl + iplylfr + ipeqopt + ipudrst + impenv) / 5 - coef,
    `Openness to Change` = (ipcrtiv + impfree + impdiff + ipadvnt + ipgdtim + impfun) / 6 - coef
  )
}


plan <- drake_plan(
  raw_data = haven::read_stata("Database DPI2017/DPI2017.dta") %>% 
    filter(
      countryname %in% c(
        "Albania", "Austria", "Belgium", "Bulgaria", "Switzerland", "Czech Rep.",
        "Cyprus", "FRG/Germany", "Denmark", "Spain", "Estonia", "Finland", "France",
        "UK", "Greece", "Croatia", "Hungary", "Ireland", "Iceland", "Israel", "Italy",
        "Lithuania", "Luxembourg", "Latvia",  "Netherlands", "Norway", "Slovenia",
        "Poland", "Portugal", "Romania", "Russia", "Slovakia", "Sweden", "Ukraine", "Turkey",
        "Soviet Union", "Yugoslavia", "GDR"
      )
    ) %>% select(-multpl, -select, -execme, -gov1me, -gov2me, -gov3me, -ifs, -opp1me, -opp2me, -opp3me, -nonchief),
  data = raw_data %>% trans,
  p = skimr::skim(data),
  re = recipe(yrsoffc ~ ., data = data) %>% 
    step_dummy(system, execrlc, execrel, gov1rlc, gov1rel, gov2rlc, 
               gov2rel, gov3rlc, gov3rel, opp1rlc, opp1rel, housesys, 
               sensys, muni, state, one_hot = T) %>% 
    prep(training = data, retain = TRUE) %>% 
    juice() %>% mutate(
      countryname = as.character(countryname)
    ),
  ess = purrr::map_dfr(list.files(path = "~/social_data_analysis/", pattern = ".sav", full.names = T), ~ haven::read_sav(.) %>% select(
    essround, agea, cntry, impsafe, ipbhprp, iphlppl, ipudrst, ipadvnt, ipshabt, iprspot, ipstrgv,
    ipmodst, iplylfr, impenv, ipgdtim, ipsuces, ipcrtiv, ipfrule, imptrad,
    ipeqopt, impdiff, impfun, imprich, impfree, starts_with("region"), brncntr) %>% 
      mutate(cntry = haven::as_factor(cntry)) %>% 
      mutate_at(vars(starts_with("region")), funs(as.character(haven::as_factor(.))))) %>% 
    mutate(
      cntry = as.character(cntry),
      agea = as.numeric(as.character(agea)),
      round_year = case_when(
        essround == 1 ~ 2002, essround == 2 ~ 2004,
        essround == 3 ~ 2006, essround == 4 ~ 2008,
        essround == 5 ~ 2010, essround == 6 ~ 2012,
        essround == 7 ~ 2014, essround == 8 ~ 2016
      ),
      birth_date = round_year - agea
    ) %>% filter(!is.na(birth_date) & brncntr == 1 & ((round_year - agea + 6) < 1975)),
  ess_1 = ess %>% ess_region(),
  ess_2 = shwartz(ess_1) %>% dplyr::select(
    round_year, agea, cntry, region, impsafe, ipbhprp, iphlppl, ipudrst, ipadvnt, ipshabt, iprspot, ipstrgv,
    ipmodst, iplylfr, impenv, ipgdtim, ipsuces, ipcrtiv, ipfrule, imptrad,
    ipeqopt, impdiff, impfun, imprich, impfree, `Self-Enhancement`, Conservation,
    `Self-Trancendence`, `Openness to Change`
  ) %>% mutate(
    cntry = ifelse(cntry == "Czech Republic", "Czech Rep.", cntry),
    cntry = ifelse(cntry == "Germany", "FRG/Germany", cntry),
    cntry = ifelse(region %in% c("Berlin", "Brandenburg", "Mecklenburg-Vorpommern", "Sachsen", "Sachsen-Anhalt", "Thüringen"), "GDR", cntry),
    cntry = ifelse(cntry == "United Kingdom", "UK", cntry),
    cntry = ifelse(cntry == "Russian Federation", "Russia", cntry)
  ) %>% filter(cntry != "Kosovo")
)

config <- drake_config(plan)
vis_drake_graph(config)

make(plan)

readd(p)

readd(ess_2)

require(ggplot2)

ggplot(readd(re), aes(opp3vote)) + geom_density()

outdated(config)


