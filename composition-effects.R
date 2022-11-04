library(here)
library(data.table)
library(magrittr)

# data from IPUMS-CPS (cps.impus.org)
dt_cps <- fread('cps_00163.csv.gz')

dt_cps <- dt_cps %>%
  .[AGE >= 18
    & YEAR >= 2008,
    .(YEAR,
      MONTH,
      WTFINL,
      AGE,
      SEX,
      EMPSTAT,
      DIFFANY,
      DIFFREM,
      DIFFHEAR,
      DIFFEYE,
      DIFFPHYS,
      DIFFREM,
      DIFFMOB,
      DIFFCARE)]

# by quarter
dt_cps[, period := paste0(YEAR, 'Q', (floor((MONTH - 1) / 3) + 1))]

# age group
dt_cps[, agegrp := '18 to 24']
dt_cps[AGE >= 25, agegrp := '25 to 34']
dt_cps[AGE >= 35, agegrp := '35 to 44']
dt_cps[AGE >= 45, agegrp := '45 to 54']
dt_cps[AGE >= 55, agegrp := '55 to 64']
dt_cps[AGE >= 65, agegrp := '65 to 74']
dt_cps[AGE >= 75, agegrp := '75 to 84']
dt_cps[AGE >= 85, agegrp := '85 and older']

#sex
dt_cps[, sex := 'Women']
dt_cps[SEX == 1, sex := 'Men']

# disabled status
dt_cps[, disabled := 'Not disabled']
dt_cps[DIFFANY == 2, disabled := 'Disabled']


# disability type, ordered from highest employment rate to lowest, so that more
# severe type overwrites less severe
dt_cps[, difftype := 'Not disabled']
dt_cps[DIFFHEAR == 2, difftype := 'Disabled - hearing']
dt_cps[DIFFEYE == 2, difftype := 'Disabled - seeing']
dt_cps[DIFFREM == 2, difftype := 'Disabled - cognitive']
dt_cps[DIFFPHYS == 2, difftype := 'Disabled - walking']
dt_cps[DIFFMOB == 2, difftype := 'Disabled - errands']
dt_cps[DIFFCARE == 2, difftype := 'Disabled - personal care']


# stratify by age group, sex and disability type
dt_sum2 <- dt_cps %>%
  .[period %in% c('2019Q3','2022Q3')
    & AGE %in% 18:64,
    .(pop = sum(WTFINL) / 3,
      epop = sum(WTFINL * (EMPSTAT %in% 10:12)) / sum(WTFINL)),
    .(period,
      sex,
      agegrp,
      disabled,
      difftype)]

# calculate 2019 employment rates for each cell
dt_sum2[, epop2019 := max(ifelse(period == '2019Q3', epop, 0)), .(sex, agegrp, disabled, difftype)]

# apply current epop rates and 2019 rates to population in each cell
dt_sum3 <- dt_sum2 %>%
  .[,
    .(pop = sum(pop),
      workers = sum(epop * pop),
      nonworkers = sum(pop) - sum(epop * pop),
      epop = sum(epop * pop) / sum(pop),
      workers2019 = sum(epop2019 * pop),
      nonworkers2019 = sum(pop) - sum(epop2019 * pop),
      epop2019 = sum(epop2019 * pop) / sum(pop)),
    .(period,
      disabled)]


# write
fwrite(dt_sum3, 'disab-epop.csv')
