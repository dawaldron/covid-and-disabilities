library(here)
library(data.table)
library(magrittr)


dt_cps <- fread('cps_00163.csv.gz')

dt_cps <- dt_cps %>%
  .[AGE >= 18
    & YEAR >= 2008,
    .(YEAR,
      MONTH,
      WTFINL,
      AGE,
      SEX,
      DIFFANY,
      DIFFREM,
      DIFFANY,
      DIFFHEAR,
      DIFFEYE,
      DIFFPHYS,
      DIFFREM,
      DIFFMOB,
      DIFFCARE)]

dt_cps[, agegrp := '18 to 34']
dt_cps[AGE >= 35, agegrp := '35 to 54']
dt_cps[AGE >= 55, agegrp := '55+']

dt_cps[, sex := 'Men']
dt_cps[SEX == 2, sex := 'Women']

dt_cps.sum <- dt_cps %>%
  .[,
    .(total = sum(WTFINL),
      `Any difficulty`  = sum(WTFINL * (DIFFANY  == 2)) / sum(WTFINL),
      `Hearing difficulty` = sum(WTFINL * (DIFFHEAR == 2)) / sum(WTFINL),
      `Vision difficulty`  = sum(WTFINL * (DIFFEYE  == 2)) / sum(WTFINL),
      `Physical difficulty` = sum(WTFINL * (DIFFPHYS == 2)) / sum(WTFINL),
      `Difficulty remembering`  = sum(WTFINL * (DIFFREM  == 2)) / sum(WTFINL),
      `Disability limiting mobility`  = sum(WTFINL * (DIFFMOB  == 2)) / sum(WTFINL),
      `Personal care limitation` = sum(WTFINL * (DIFFCARE == 2)) / sum(WTFINL)),
    .(YEAR,
      MONTH,
      sex,
      agegrp)] %>%
  melt(id.var = c('YEAR', 'MONTH', 'sex', 'agegrp', 'total'),
       variable.factor = FALSE, variable.name = 'disabilityType',
       value.name = 'percent')

fwrite(dt_cps.sum, 'covid-disability-by-age.csv')

dt_cps.chart1 <- dt_cps %>%
  .[agegrp %in% c('18 to 34', '35 to 54'),
    .(total = sum(WTFINL),
      `Any difficulty`  = sum(WTFINL * (DIFFANY  == 2)) / sum(WTFINL),
      `Difficulty hearing, deaf` = sum(WTFINL * (DIFFHEAR == 2)) / sum(WTFINL),
      `Difficulty seeing, blind`  = sum(WTFINL * (DIFFEYE  == 2)) / sum(WTFINL),
      `Difficulty walking or climbing stairs` = sum(WTFINL * (DIFFPHYS == 2)) / sum(WTFINL),
      `Difficulty remembering, concentrating`  = sum(WTFINL * (DIFFREM  == 2)) / sum(WTFINL),
      `Difficulty doing errands`  = sum(WTFINL * (DIFFMOB  == 2)) / sum(WTFINL),
      `Difficulty dressing or bathing` = sum(WTFINL * (DIFFCARE == 2)) / sum(WTFINL)),
    .(YEAR,
      MONTH)] %>%
  melt(id.var = c('YEAR', 'MONTH', 'total'),
       variable.factor = FALSE, variable.name = 'disabilityType',
       value.name = 'percent') %>%
  .[, date := as.Date(paste0(YEAR, '-', MONTH, '-01'))] %>%
  .[!is.na(percent)]

dt_cps.chart1[, baseline := mean(ifelse(YEAR %in% 2019, percent, NA), na.rm = TRUE) , disabilityType]
dt_cps.chart1[, percentDiff := percent - baseline]

getLine <- function(x) {
  fit <- loess(percentDiff ~ as.numeric(date),
               dt_cps.chart1[disabilityType == x],
               span = 0.2)
  dt <- data.table(line = fit$fitted)
  return(dt)
}

dt_cps.chart1.f <- lapply(dt_cps.chart1[!duplicated(disabilityType), disabilityType], getLine) %>%
  rbindlist() %>%
  cbind(dt_cps.chart1, .)

fwrite(dt_cps.chart1.f, 'chart1.csv')

dt_cps.chart2 <- dt_cps %>%
  .[,
    .(total = sum(WTFINL),
      `Any difficulty`  = sum(WTFINL * (DIFFANY  == 2)) / sum(WTFINL),
      `Difficulty hearing, deaf` = sum(WTFINL * (DIFFHEAR == 2)) / sum(WTFINL),
      `Difficulty seeing, blind`  = sum(WTFINL * (DIFFEYE  == 2)) / sum(WTFINL),
      `Difficulty walking or climbing stairs` = sum(WTFINL * (DIFFPHYS == 2)) / sum(WTFINL),
      `Difficulty remembering, concentrating`  = sum(WTFINL * (DIFFREM  == 2)) / sum(WTFINL),
      `Difficulty doing errands`  = sum(WTFINL * (DIFFMOB  == 2)) / sum(WTFINL),
      `Difficulty dressing or bathing` = sum(WTFINL * (DIFFCARE == 2)) / sum(WTFINL)),
    .(YEAR,
      MONTH,
      sex,
      agegrp)] %>%
  melt(id.var = c('YEAR', 'MONTH', 'sex', 'agegrp', 'total'),
       variable.factor = FALSE, variable.name = 'disabilityType',
       value.name = 'percent') %>%
  .[, date := as.Date(paste0(YEAR, '-', MONTH, '-01'))] %>%
  .[!is.na(percent) & disabilityType == 'Difficulty remembering, concentrating'] %>%
  .[order(sex, agegrp, date)]

dt_cps.chart2[, baseline := mean(ifelse(YEAR %in% 2019, percent, NA), na.rm = TRUE), .(sex, agegrp)]
dt_cps.chart2[, percentDiff := percent - baseline]

getLine2 <- function(x) {
  print(dt_cps.chart2[paste(sex, agegrp) == x])
  fit <- loess(percentDiff ~ as.numeric(date),
               dt_cps.chart2[paste(sex, agegrp) == x],
               span = 0.2)
  dt <- data.table(line = fit$fitted)
  return(dt)
}

dt_cps.chart2.f <- lapply(dt_cps.chart2[!duplicated(cbind(sex, agegrp)), paste(sex, agegrp)], getLine2) %>%
  rbindlist() %>%
  cbind(dt_cps.chart2, .)

fwrite(dt_cps.chart2.f, 'chart2.csv')

dt_cps.chart3 <- dt_cps %>%
  .[agegrp %in% c('18 to 34', '35 to 54'),
    .(total = sum(WTFINL),
      `Any difficulty`  = sum(WTFINL * (DIFFANY  == 2)) / sum(WTFINL),
      `Difficulty hearing, deaf` = sum(WTFINL * (DIFFHEAR == 2)) / sum(WTFINL),
      `Difficulty seeing, blind`  = sum(WTFINL * (DIFFEYE  == 2)) / sum(WTFINL),
      `Difficulty walking or climbing stairs` = sum(WTFINL * (DIFFPHYS == 2)) / sum(WTFINL),
      `Difficulty remembering, concentrating`  = sum(WTFINL * (DIFFREM  == 2)) / sum(WTFINL),
      `Difficulty doing errands`  = sum(WTFINL * (DIFFMOB  == 2)) / sum(WTFINL),
      `Difficulty dressing or bathing` = sum(WTFINL * (DIFFCARE == 2)) / sum(WTFINL)),
    .(YEAR,
      MONTH)] %>%
  melt(id.var = c('YEAR', 'MONTH', 'total'),
       variable.factor = FALSE, variable.name = 'disabilityType',
       value.name = 'percent') %>%
  .[, date := as.Date(paste0(YEAR, '-', MONTH, '-01'))] %>%
  .[!is.na(percent) & disabilityType == 'Difficulty remembering, concentrating']

dt_cps.chart3[, baseline := mean(ifelse(YEAR %in% 2019, percent, NA), na.rm = TRUE) , disabilityType]
dt_cps.chart3[, percentDiff := percent - baseline]

fit <- loess(percent ~ as.numeric(date),
             dt_cps.chart3,
             span = 0.2)
dt_cps.chart3[, line := fit$fitted]

fwrite(dt_cps.chart3, 'chart3.csv')

servr::httd()


bitmap <- rsvg::rsvg('logo.svg', width = 560)
png::writePNG(bitmap, "logo.png", dpi = 400)

bitmap <- rsvg::rsvg('chart1.svg', css = "style.css", width = 1700)
png::writePNG(bitmap, "chart1.png", dpi = 400)

bitmap <- rsvg::rsvg('chart2.svg', css = "style.css", width = 1700)
png::writePNG(bitmap, "chart2.png", dpi = 400)

bitmap <- rsvg::rsvg('chart3.svg', css = "style.css", width = 1700)
png::writePNG(bitmap, "chart3.png", dpi = 400)
