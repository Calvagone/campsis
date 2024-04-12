library(dplyr)
library(tidyr)
library(foreign)

url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/DEMO_J.XPT"
tf <- tempfile()
download.file(url, tf, mode="wb", quiet=TRUE)
demo <- read.xport(tf)

url <- "https://wwwn.cdc.gov/Nchs/Nhanes/2017-2018/BMX_J.XPT"
tf <- tempfile()
download.file(url, tf, mode="wb", quiet=TRUE)
bmx <- read.xport(tf)

nhanes.demo <- demo %>%
  select(BS_ID=SEQN, SEX=RIAGENDR, AGE=RIDAGEYR) %>% 
  mutate(SEXC=case_when(SEX=='1' ~ 'male',
                        SEX=='2' ~ 'female',
                        SEX=='.' ~'missing'))
nhanes.body <- bmx %>%
  select(BS_ID=SEQN, BW=BMXWT, BMI=BMXBMI, HT=BMXHT)

# Join both tables and keep only complete cases
nhanes <- left_join(nhanes.demo, nhanes.body) %>%
  tidyr::drop_na()

# For now, don't use the SEXC column since Bootstrap in Campsis expects numeric columns
nhanes <- nhanes %>%
  select(-SEXC)

usethis::use_data(nhanes, overwrite=TRUE)
