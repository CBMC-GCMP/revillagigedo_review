
# Load dependencies -------------------------------------------------------

library(aws.s3)
library(tidyverse)
library(readxl)
library(writexl)

source("R/00-retrieve_db_from_s3.R")

# Load data ---------------------------------------------------------------

#LTEM database from s3
ltem.s3 <- readRDS("data/s3/promares_pnr_historic_updated.RDS")

#PCU databse from s3
pcu.s3 <- readxl::read_excel("data/s3/pristine_seas_pnr_pcu_updated.xlsx")



#Load new data:

#LTEM cleaned database
ltem.new <- readRDS("path/to/file/promares_pnr_ltem.RDS")

#PCU cleaned database
pcu.new <- readxl::read_excel("path/to/file/pristine_seas_pnr_pcu.xlsx")


# Bind new rows -----------------------------------------------------------

#Local updated files can be retrieved from "data/updates/" folder

 rbind(ltem.s3, ltem.new) |> 
  saveRDS("data/updates/promares_pnr_update.RDS")

 rbind(pcu.s3, pcu.new) |> 
  writexl::write_xlsx("data/updates/pristine_seas_pnr_pcu_update.xlsx")


# Save the updates to Revillagigedo's s3 bucket: --------------------------

#LTEM
put_object(file= "data/updates/promares_pnr_update.RDS",
           object = ltem,
           bucket = bucket)
#PCU
put_object(file= "data/updates/pristine_seas_pnr_pcu_update.xlsx",
           object = pcu,
           bucket = bucket)


# END ---------------------------------------------------------------------


