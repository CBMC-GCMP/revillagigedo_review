
# Load dependencies -------------------------------------------------------

library(aws.s3)
library(aws.signature)


# Retrieve AWS credentials ------------------------------------------------

.creds <- locate_credentials(file = ".aws/credentials.txt")

# Configure bucket data ---------------------------------------------------
dir.create("data/s3", showWarnings = F)
dir.create("data/updates", showWarnings = F)
#Path to the desired s3 bucket
bucket <- "datalake-cbmc-revillagigedo"


#Define the paths to both LTEM and PCU databases
ltem <- "databases/updates/promares_pnr_historic_updated.RDS"
pcu <- "databases/updates/pristine_seas_pnr_pcu_updated.xlsx"



# Download databases to local files ---------------------------------------

#LTEM db
save_object(file = "data/s3/promares_pnr_historic_updated.RDS", 
           object = ltem, 
           bucket = bucket) 

#PCU db
save_object(file = "data/s3/pristine_seas_pnr_pcu_updated.xlsx", 
            object = pcu, 
            bucket = bucket) 



# END ---------------------------------------------------------------------




