# data_df <- 
#     read_excel("input_data/explore_data_avident.xlsx") %>% 
#     select(1,5:6,11,13,15)

# library(readxl)
library(httr)
library(magrittr)
library(dplyr)
library(lubridate)
Sys.setenv(TZ="Europe/Oslo")

# Lagre filen i Jottacloud
# slaa paa deling og kopier lenke
# aapne lenke inkognito, lukk forhaandsvisningsbildet
# Hoyreklikk download-knapp oppe til hoyre og lagre denne linken
# Denne linken kan benyttes til nedlasting (skal staa "get" inne i linken)
# Benytt folgende 
# url <- "https://www.jottacloud.com/opin/io/downloadPublic/get-44992800/@84d030e6b71749809bf49fc10f0466f4"
# data_df = tempfile(fileext = ".xlsx")
# download.file(url, destfile=data_df, mode='wb')
# data_df <- read_excel(data_df)

# data_df <- read_excel("input_data/explore_data_avident.xlsx")
inputlist <- c("input_data/explore_data_anonym_export.xlsx", "input_data/export_22_mars19_red.xlsx")
data_df <- readxl::read_excel(inputlist[2])
rm(inputlist)

# data_df$SAMPLE_DATE <- date(dmy_hm(data_df$SAMPLED_DATE))
data_df$CLIN_TRL_VISIT <-
    data_df$CLIN_TRL_VISIT %>% 
    toupper()

data_df$SAMPLED_DATE <- lubridate::date(data_df$SAMPLED_DATE)

# data_df$LOGIN_BY <- factor(data_df$LOGIN_BY)
# data_df$SAMPLE_TYPE <- factor(data_df$SAMPLE_TYPE)
# data_df$STORAGE_LOC_NO <- as.integer(data_df$STORAGE_LOC_NO)
# data_df$CLIN_TRL_SUBJECT_ID <- factor(data_df$CLIN_TRL_SUBJECT_ID)
# data_df$T_AGE <- as.integer(data_df$T_AGE)
# data_df$T_SOP <- factor(data_df$T_SOP)

data_df$CLIN_TRL_VISIT <-
    replace(data_df$CLIN_TRL_VISIT, 
            which(data_df$CLIN_TRL_VISIT == "AKRO LONG"), 
            values="LONG")
