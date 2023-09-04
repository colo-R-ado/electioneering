###############################################################################
# 02-colorado-demographics.R                                                  #
# Colorado shapefile mapping for ZCTAs, state counties, county voting         #
# districts, county tracts and county block groups                            #
###############################################################################

# Housekeeping ----------------------------------------------------------------

library(dplyr) # Data wrangling
library(leaflet) # Mapping
library(mapview) # Map exporting
library(osfr) # Data files
library(purrr) # Data wrangling
library(sf) # Shapefiles
library(tidycensus) # Census data
library(tigris) # Shapefiles

# OSF personal access token: https://osf.io/m5qyk/
pat <- keyring::key_get("OSF")
osf_auth(token = pat)

# FBI crime data API key
fbi <- keyring::key_get("FBI.gov")

# Import Colorado color palette
source("./scripts/colorado_cols.R")
col_pal <- as.list(colorado_cols())

# Import data -----------------------------------------------------------------

# Compare 2010 to 2020 census
acs10 <- load_variables(dataset = "acs5", year = 2010)
acs20 <- load_variables(dataset = "acs5", year = 2020)

acs10c <- distinct(acs10, concept)
acs20c <- distinct(acs20, concept)

inner_join(acs10c, acs20c) %>%
  mutate(
    chunk_id = ceiling(seq_along(concept) / 50) # 50 rows in each column
  ) %>%
  mutate(
    obs = seq_along(concept),
    .by = chunk_id
  ) %>%
  pivot_wider(
    id_cols = obs,
    names_from = chunk_id,
    values_from = concept
  ) %>%
  select(
    -obs
  ) %>%
  mutate(
    across(everything(), ~ replace_na(.x, ""))
  ) %>%
  as.matrix() %>%
  unname() %>%
  knitr::kable(
    "simple"
  )

# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------  -------------------------------------------------------------------------------------------------------------------------------------  ----------------------------------------------------------------------------------------------------------------------------------------  ------------------------------------------------------------------------------------------------------------------------------------  ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  --------------------------------------------------------------------------------------------------------------------------------  --------------------------------------------------------------------------------------------------------------------------------------------------------------------  ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  ----------------------------------------------------------------------------------------------------------------------------------------------------------------------  -----------------------------------------------------------------------------------------  -------------------------------------------------------------------------------------------------------------  -------------------------------------------------------------------------------------------------------------------------------------------  ------------------------------------------------------------------------------------------------------------------------------
# SEX BY AGE (WHITE ALONE)                                                                                                                                                   PLACE OF BIRTH (WHITE ALONE) IN PUERTO RICO                                                                                            GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY CITIZENSHIP STATUS FOR CURRENT RESIDENCE IN PUERTO RICO                                         SEX OF WORKERS BY PLACE OF WORK--STATE AND COUNTY LEVEL                                                                               MEANS OF TRANSPORTATION TO WORK BY CITIZENSHIP STATUS FOR WORKPLACE GEOGRAPHY                                                                                                                                 HOUSEHOLD TYPE BY RELATIVES AND NONRELATIVES FOR POPULATION IN HOUSEHOLDS (WHITE ALONE)                                           WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY MARITAL STATUS (WHITE ALONE, NOT HISPANIC OR LATINO)                                                    POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF RELATED CHILDREN (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)                    AGE BY VETERAN STATUS BY EMPLOYMENT STATUS FOR THE CIVILIAN POPULATION 18 TO 64 YEARS                                                                                   TENURE BY FAMILIES AND PRESENCE OF OWN CHILDREN                                            PLUMBING FACILITIES BY OCCUPANTS PER ROOM BY YEAR STRUCTURE BUILT                                              MEDIAN MONTHLY HOUSING COSTS (DOLLARS)                                                                                                       SEX BY AGE BY VETERAN STATUS FOR THE CIVILIAN POPULATION 18 YEARS AND OVER (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE) 
# SEX BY AGE (BLACK OR AFRICAN AMERICAN ALONE)                                                                                                                               PLACE OF BIRTH (WHITE ALONE) IN THE UNITED STATES                                                                                      GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY CITIZENSHIP STATUS FOR CURRENT RESIDENCE IN THE UNITED STATES                                   SEX OF WORKERS BY PLACE OF WORK--PLACE LEVEL                                                                                          MEANS OF TRANSPORTATION TO WORK BY LANGUAGE SPOKEN AT HOME AND ABILITY TO SPEAK ENGLISH FOR WORKPLACE GEOGRAPHY                                                                                               HOUSEHOLD TYPE BY RELATIVES AND NONRELATIVES FOR POPULATION IN HOUSEHOLDS (BLACK OR AFRICAN AMERICAN ALONE)                       WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY MARITAL STATUS (HISPANIC OR LATINO)                                                                     POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF RELATED CHILDREN (AMERICAN INDIAN AND ALASKA NATIVE ALONE)                        RECEIPT OF FOOD STAMPS/SNAP IN THE PAST 12 MONTHS BY PRESENCE OF PEOPLE 60 YEARS AND OVER FOR HOUSEHOLDS                                                                TENURE BY EDUCATIONAL ATTAINMENT OF HOUSEHOLDER                                            KITCHEN FACILITIES FOR ALL HOUSING UNITS                                                                       TENURE BY HOUSING COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS                                                            SEX BY AGE BY VETERAN STATUS FOR THE CIVILIAN POPULATION 18 YEARS AND OVER (SOME OTHER RACE ALONE)                            
# SEX BY AGE (AMERICAN INDIAN AND ALASKA NATIVE ALONE)                                                                                                                       PLACE OF BIRTH (BLACK OR AFRICAN AMERICAN ALONE) IN PUERTO RICO                                                                        GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY MARITAL STATUS FOR CURRENT RESIDENCE IN PUERTO RICO                                             SEX OF WORKERS BY PLACE OF WORK--MINOR CIVIL DIVISION LEVEL FOR 12 SELECTED STATES (CT, ME, MA, MI, MN, NH, NJ, NY, PA, RI, VT, WI)   MEANS OF TRANSPORTATION TO WORK BY POVERTY STATUS IN THE PAST 12 MONTHS FOR WORKPLACE GEOGRAPHY                                                                                                               HOUSEHOLD TYPE BY RELATIVES AND NONRELATIVES FOR POPULATION IN HOUSEHOLDS (AMERICAN INDIAN AND ALASKA NATIVE ALONE)               WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY MARITAL STATUS AND AGE                                                                                  POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF RELATED CHILDREN (ASIAN ALONE HOUSEHOLDER)                                        RECEIPT OF FOOD STAMPS/SNAP IN THE PAST 12 MONTHS BY PRESENCE OF CHILDREN UNDER 18 YEARS BY HOUSEHOLD TYPE FOR HOUSEHOLDS                                               OCCUPANTS PER ROOM (WHITE ALONE HOUSEHOLDER)                                               KITCHEN FACILITIES FOR OCCUPIED HOUSING UNITS                                                                  MEDIAN VALUE BY YEAR STRUCTURE BUILT                                                                                                         SEX BY AGE BY VETERAN STATUS FOR THE CIVILIAN POPULATION 18 YEARS AND OVER (TWO OR MORE RACES)                                
# SEX BY AGE (ASIAN ALONE)                                                                                                                                                   PLACE OF BIRTH (BLACK OR AFRICAN AMERICAN ALONE) IN THE UNITED STATES                                                                  GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY MARITAL STATUS FOR CURRENT RESIDENCE IN THE UNITED STATES                                       SEX OF WORKERS BY TRAVEL TIME TO WORK                                                                                                 MEANS OF TRANSPORTATION TO WORK BY OCCUPATION FOR WORKPLACE GEOGRAPHY                                                                                                                                         HOUSEHOLD TYPE BY RELATIVES AND NONRELATIVES FOR POPULATION IN HOUSEHOLDS (ASIAN ALONE)                                           WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY MARITAL STATUS AND NATIVITY                                                                             POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF RELATED CHILDREN (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)   RECEIPT OF FOOD STAMPS/SNAP IN THE PAST 12 MONTHS BY POVERTY STATUS IN THE PAST 12 MONTHS FOR HOUSEHOLDS                                                                OCCUPANTS PER ROOM (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)                           TENURE BY KITCHEN FACILITIES                                                                                   AGGREGATE VALUE (DOLLARS) BY YEAR STRUCTURE BUILT                                                                                            SEX BY AGE BY VETERAN STATUS FOR THE CIVILIAN POPULATION 18 YEARS AND OVER (WHITE ALONE, NOT HISPANIC OR LATINO)              
# SEX BY AGE (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)                                                                                                              PLACE OF BIRTH (AMERICAN INDIAN AND ALASKA NATIVE ALONE) IN PUERTO RICO                                                                GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY EDUCATIONAL ATTAINMENT FOR CURRENT RESIDENCE IN PUERTO RICO                                     AGGREGATE TRAVEL TIME TO WORK (IN MINUTES) OF WORKERS BY SEX                                                                          MEANS OF TRANSPORTATION TO WORK BY INDUSTRY FOR WORKPLACE GEOGRAPHY                                                                                                                                           HOUSEHOLD TYPE BY RELATIVES AND NONRELATIVES FOR POPULATION IN HOUSEHOLDS (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)      WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY MARITAL STATUS AND POVERTY STATUS IN THE PAST 12 MONTHS                                                 POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF RELATED CHILDREN (SOME OTHER RACE ALONE HOUSEHOLDER)                              RECEIPT OF FOOD STAMPS/SNAP IN THE PAST 12 MONTHS BY RACE OF HOUSEHOLDER (WHITE ALONE)                                                                                  OCCUPANTS PER ROOM (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)                   KITCHEN FACILITIES BY MEALS INCLUDED IN RENT                                                                   MEDIAN VALUE BY YEAR HOUSEHOLDER MOVED INTO UNIT                                                                                             SEX BY AGE BY VETERAN STATUS FOR THE CIVILIAN POPULATION 18 YEARS AND OVER (HISPANIC OR LATINO)                               
# SEX BY AGE (SOME OTHER RACE ALONE)                                                                                                                                         PLACE OF BIRTH (AMERICAN INDIAN AND ALASKA NATIVE ALONE) IN THE UNITED STATES                                                          GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY EDUCATIONAL ATTAINMENT FOR CURRENT RESIDENCE IN THE UNITED STATES                               SEX OF WORKERS BY VEHICLES AVAILABLE                                                                                                  MEANS OF TRANSPORTATION TO WORK BY CLASS OF WORKER FOR WORKPLACE GEOGRAPHY                                                                                                                                    HOUSEHOLD TYPE BY RELATIVES AND NONRELATIVES FOR POPULATION IN HOUSEHOLDS (SOME OTHER RACE ALONE)                                 WOMEN 16 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY MARITAL STATUS AND LABOR FORCE STATUS                                                                   POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF RELATED CHILDREN (TWO OR MORE RACES HOUSEHOLDER)                                  RECEIPT OF FOOD STAMPS/SNAP IN THE PAST 12 MONTHS BY RACE OF HOUSEHOLDER (BLACK OR AFRICAN AMERICAN ALONE)                                                              OCCUPANTS PER ROOM (ASIAN ALONE HOUSEHOLDER)                                               AGE OF HOUSEHOLDER BY MEALS INCLUDED IN RENT                                                                   AGGREGATE VALUE (DOLLARS) BY YEAR HOUSEHOLDER MOVED INTO UNIT                                                                                SEX BY AGE BY EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER (WHITE ALONE)                                            
# SEX BY AGE (TWO OR MORE RACES)                                                                                                                                             PLACE OF BIRTH (ASIAN ALONE) IN PUERTO RICO                                                                                            GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY POVERTY STATUS IN THE PAST 12 MONTHS FOR CURRENT RESIDENCE IN PUERTO RICO                       AGGREGATE NUMBER OF VEHICLES (CAR, TRUCK, OR VAN) USED IN COMMUTING BY WORKERS 16 YEARS AND OVER BY SEX                               MEANS OF TRANSPORTATION TO WORK BY TRAVEL TIME TO WORK FOR WORKPLACE GEOGRAPHY                                                                                                                                HOUSEHOLD TYPE BY RELATIVES AND NONRELATIVES FOR POPULATION IN HOUSEHOLDS (TWO OR MORE RACES)                                     WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY MARITAL STATUS AND EDUCATIONAL ATTAINMENT                                                               POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF RELATED CHILDREN (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)                RECEIPT OF FOOD STAMPS/SNAP IN THE PAST 12 MONTHS BY RACE OF HOUSEHOLDER (AMERICAN INDIAN AND ALASKA NATIVE ALONE)                                                      OCCUPANTS PER ROOM (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)          CONTRACT RENT                                                                                                  MEDIAN GROSS RENT BY YEAR STRUCTURE BUILT                                                                                                    SEX BY AGE BY EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER (BLACK OR AFRICAN AMERICAN ALONE)                        
# SEX BY AGE (WHITE ALONE, NOT HISPANIC OR LATINO)                                                                                                                           PLACE OF BIRTH (ASIAN ALONE) IN THE UNITED STATES                                                                                      GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY POVERTY STATUS IN THE PAST 12 MONTHS FOR CURRENT RESIDENCE IN THE UNITED STATES                 PLACE OF WORK FOR WORKERS 16 YEARS AND OVER--METROPOLITAN STATISTICAL AREA LEVEL                                                      AGGREGATE TRAVEL TIME TO WORK (IN MINUTES) OF WORKERS BY MEANS OF TRANSPORTATION TO WORK FOR WORKPLACE GEOGRAPHY                                                                                              HOUSEHOLD TYPE BY RELATIVES AND NONRELATIVES FOR POPULATION IN HOUSEHOLDS (WHITE ALONE, NOT HISPANIC OR LATINO)                   WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY MARITAL STATUS AND RECEIPT OF PUBLIC ASSISTANCE INCOME IN THE PAST 12 MONTHS                            POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF RELATED CHILDREN (HISPANIC OR LATINO)                                             RECEIPT OF FOOD STAMPS/SNAP IN THE PAST 12 MONTHS BY RACE OF HOUSEHOLDER (ASIAN ALONE)                                                                                  OCCUPANTS PER ROOM (SOME OTHER RACE ALONE HOUSEHOLDER)                                     LOWER CONTRACT RENT QUARTILE (DOLLARS)                                                                         AGGREGATE GROSS RENT (DOLLARS) BY YEAR STRUCTURE BUILT                                                                                       SEX BY AGE BY EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER (AMERICAN INDIAN AND ALASKA NATIVE ALONE)                
# SEX BY AGE (HISPANIC OR LATINO)                                                                                                                                            PLACE OF BIRTH (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE) IN PUERTO RICO                                                       GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY TENURE FOR CURRENT RESIDENCE IN PUERTO RICO                                                     PLACE OF WORK FOR WORKERS 16 YEARS AND OVER--MICROPOLITAN STATISTICAL AREA LEVEL                                                      MEANS OF TRANSPORTATION TO WORK BY TENURE FOR WORKPLACE GEOGRAPHY                                                                                                                                             HOUSEHOLD TYPE BY RELATIVES AND NONRELATIVES FOR POPULATION IN HOUSEHOLDS (HISPANIC OR LATINO)                                    WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY AGE                                                                                                     POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF RELATED CHILDREN                                                                  RECEIPT OF FOOD STAMPS/SNAP IN THE PAST 12 MONTHS BY RACE OF HOUSEHOLDER (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)                                             OCCUPANTS PER ROOM (TWO OR MORE RACES HOUSEHOLDER)                                         MEDIAN CONTRACT RENT (DOLLARS)                                                                                 MEDIAN GROSS RENT BY YEAR HOUSEHOLDER MOVED INTO UNIT                                                                                        SEX BY AGE BY EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER (ASIAN ALONE)                                            
# SEX BY AGE                                                                                                                                                                 PLACE OF BIRTH (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE) IN THE UNITED STATES                                                 GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY TENURE FOR CURRENT RESIDENCE IN THE UNITED STATES                                               PLACE OF WORK FOR WORKERS 16 YEARS AND OVER--NOT METROPOLITAN OR MICROPOLITAN STATISTICAL AREA LEVEL                                  MEANS OF TRANSPORTATION TO WORK BY VEHICLES AVAILABLE FOR WORKPLACE GEOGRAPHY                                                                                                                                 HOUSEHOLD TYPE BY RELATIVES AND NONRELATIVES FOR POPULATION IN HOUSEHOLDS                                                         SCHOOL ENROLLMENT BY LEVEL OF SCHOOL FOR THE POPULATION 3 YEARS AND OVER                                                                                              AGGREGATE INCOME DEFICIT (DOLLARS) IN THE PAST 12 MONTHS FOR FAMILIES BY FAMILY TYPE                                                                                                                       RECEIPT OF FOOD STAMPS/SNAP IN THE PAST 12 MONTHS BY RACE OF HOUSEHOLDER (SOME OTHER RACE ALONE)                                                                        OCCUPANTS PER ROOM (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)                       UPPER CONTRACT RENT QUARTILE (DOLLARS)                                                                         AGGREGATE GROSS RENT (DOLLARS) BY YEAR HOUSEHOLDER MOVED INTO UNIT                                                                           SEX BY AGE BY EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)       
# MEDIAN AGE BY SEX (WHITE ALONE)                                                                                                                                            PLACE OF BIRTH (SOME OTHER RACE ALONE) IN PUERTO RICO                                                                                  MOVERS BETWEEN REGIONS IN THE UNITED STATES                                                                                               MEANS OF TRANSPORTATION TO WORK BY AGE                                                                                                MEANS OF TRANSPORTATION TO WORK FOR WORKPLACE GEOGRAPHY                                                                                                                                                       FAMILY TYPE BY PRESENCE AND AGE OF OWN CHILDREN UNDER 18 YEARS                                                                    SEX BY SCHOOL ENROLLMENT BY LEVEL OF SCHOOL BY TYPE OF SCHOOL FOR THE POPULATION 3 YEARS AND OVER                                                                     POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY HOUSEHOLD TYPE BY NUMBER OF RELATED CHILDREN UNDER 18 YEARS                                                                                            RECEIPT OF FOOD STAMPS/SNAP IN THE PAST 12 MONTHS BY RACE OF HOUSEHOLDER (TWO OR MORE RACES)                                                                            OCCUPANTS PER ROOM (HISPANIC OR LATINO HOUSEHOLDER)                                        AGGREGATE CONTRACT RENT (DOLLARS)                                                                              TENURE BY HOUSEHOLD TYPE AND PRESENCE AND AGE OF OWN CHILDREN                                                                                SEX BY AGE BY EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER (SOME OTHER RACE ALONE)                                  
# MEDIAN AGE BY SEX (BLACK OR AFRICAN AMERICAN ALONE)                                                                                                                        PLACE OF BIRTH (SOME OTHER RACE ALONE) IN THE UNITED STATES                                                                            GEOGRAPHICAL MOBILITY IN THE PAST YEAR FOR CURRENT RESIDENCE--MICROPOLITAN STATISTICAL AREA LEVEL IN PUERTO RICO                          MEDIAN AGE BY MEANS OF TRANSPORTATION TO WORK                                                                                         TRAVEL TIME TO WORK FOR WORKPLACE GEOGRAPHY                                                                                                                                                                   FAMILY TYPE BY PRESENCE AND AGE OF RELATED CHILDREN UNDER 18 YEARS                                                                SEX BY SCHOOL ENROLLMENT BY TYPE OF SCHOOL BY AGE FOR THE POPULATION 3 YEARS AND OVER                                                                                 POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY HOUSEHOLD TYPE BY NUMBER OF PERSONS IN FAMILY                                                                                                          RECEIPT OF FOOD STAMPS/SNAP IN THE PAST 12 MONTHS BY RACE OF HOUSEHOLDER (WHITE ALONE, NOT HISPANIC OR LATINO)                                                          TENURE BY OCCUPANTS PER ROOM                                                               RENT ASKED                                                                                                     TENURE BY HOUSEHOLD SIZE BY AGE OF HOUSEHOLDER                                                                                               SEX BY AGE BY EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER (TWO OR MORE RACES)                                      
# MEDIAN AGE BY SEX (ASIAN ALONE)                                                                                                                                            PLACE OF BIRTH (TWO OR MORE RACES) IN PUERTO RICO                                                                                      GEOGRAPHICAL MOBILITY IN THE PAST YEAR FOR CURRENT RESIDENCE--NOT METROPOLITAN OR MICROPOLITAN STATISTICAL AREA LEVEL IN PUERTO RICO      MEANS OF TRANSPORTATION TO WORK (WHITE ALONE)                                                                                         POPULATION UNDER 18 YEARS BY AGE                                                                                                                                                                              HOUSEHOLDS BY PRESENCE OF PEOPLE UNDER 18 YEARS BY HOUSEHOLD TYPE                                                                 SEX BY COLLEGE OR GRADUATE SCHOOL ENROLLMENT BY TYPE OF SCHOOL BY AGE FOR THE POPULATION 15 YEARS AND OVER                                                            POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY HOUSEHOLD TYPE BY NUMBER OF WORKERS IN FAMILY                                                                                                          RECEIPT OF FOOD STAMPS/SNAP IN THE PAST 12 MONTHS BY RACE OF HOUSEHOLDER (HISPANIC OR LATINO)                                                                           TENURE BY AGE OF HOUSEHOLDER BY OCCUPANTS PER ROOM                                         AGGREGATE RENT ASKED (DOLLARS)                                                                                 TENURE BY HOUSE HEATING FUEL                                                                                                                 SEX BY AGE BY EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER (WHITE ALONE, NOT HISPANIC OR LATINO)                    
# MEDIAN AGE BY SEX (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)                                                                                                       PLACE OF BIRTH (TWO OR MORE RACES) IN THE UNITED STATES                                                                                GEOGRAPHICAL MOBILITY IN THE PAST YEAR FOR CURRENT RESIDENCE--STATE, COUNTY AND PLACE LEVEL IN PUERTO RICO                                MEANS OF TRANSPORTATION TO WORK (BLACK OR AFRICAN AMERICAN ALONE)                                                                     OWN CHILDREN UNDER 18 YEARS BY FAMILY TYPE AND AGE                                                                                                                                                            HOUSEHOLDS BY PRESENCE OF PEOPLE 60 YEARS AND OVER BY HOUSEHOLD TYPE                                                              SEX BY SCHOOL ENROLLMENT BY EDUCATIONAL ATTAINMENT BY EMPLOYMENT STATUS FOR THE POPULATION 16 TO 19 YEARS                                                             POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY SOCIAL SECURITY INCOME BY SUPPLEMENTAL SECURITY INCOME (SSI) AND CASH PUBLIC ASSISTANCE INCOME                                          RECEIPT OF FOOD STAMPS/SNAP IN THE PAST 12 MONTHS BY FAMILY TYPE BY NUMBER OF WORKERS IN FAMILY IN THE PAST 12 MONTHS                                                   TENURE BY PLUMBING FACILITIES BY OCCUPANTS PER ROOM                                        GROSS RENT                                                                                                     TENURE BY SELECTED PHYSICAL AND FINANCIAL CONDITIONS                                                                                         SEX BY AGE BY EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER (HISPANIC OR LATINO)                                     
# MEDIAN AGE BY SEX (SOME OTHER RACE ALONE)                                                                                                                                  PLACE OF BIRTH (WHITE ALONE, NOT HISPANIC OR LATINO) IN PUERTO RICO                                                                    GEOGRAPHICAL MOBILITY IN THE PAST YEAR FOR CURRENT RESIDENCE--STATE, COUNTY AND PLACE LEVEL IN THE UNITED STATES                          MEANS OF TRANSPORTATION TO WORK (AMERICAN INDIAN AND ALASKA NATIVE ALONE)                                                             HOUSEHOLD TYPE FOR CHILDREN UNDER 18 YEARS IN HOUSEHOLDS (EXCLUDING HOUSEHOLDERS, SPOUSES, AND UNMARRIED PARTNERS)                                                                                            HOUSEHOLDS BY PRESENCE OF PEOPLE 65 YEARS AND OVER, HOUSEHOLD SIZE AND HOUSEHOLD TYPE                                             POVERTY STATUS IN THE PAST 12 MONTHS BY SCHOOL ENROLLMENT BY LEVEL OF SCHOOL FOR THE POPULATION 3 YEARS AND OVER                                                      POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY WORK EXPERIENCE OF HOUSEHOLDER AND SPOUSE                                                                                               SEX BY AGE BY EMPLOYMENT STATUS FOR THE POPULATION 16 YEARS AND OVER                                                                                                    ROOMS                                                                                      MEDIAN GROSS RENT (DOLLARS)                                                                                    TENURE BY HOUSEHOLD SIZE BY UNITS IN STRUCTURE                                                                                               SEX BY OCCUPATION FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER (WHITE ALONE)                                        
# MEDIAN AGE BY SEX (TWO OR MORE RACES)                                                                                                                                      PLACE OF BIRTH (WHITE ALONE, NOT HISPANIC OR LATINO) IN THE UNITED STATES                                                              GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY AGE FOR RESIDENCE 1 YEAR AGO IN PUERTO RICO                                                     MEANS OF TRANSPORTATION TO WORK (ASIAN ALONE)                                                                                         RELATIONSHIP TO HOUSEHOLDER FOR CHILDREN UNDER 18 YEARS IN HOUSEHOLDS                                                                                                                                         NONFAMILY HOUSEHOLDS BY SEX OF HOUSEHOLDER BY LIVING ALONE BY AGE OF HOUSEHOLDER                                                  SEX BY AGE BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 18 YEARS AND OVER                                                                                             POVERTY STATUS IN THE PAST 12 MONTHS BY HOUSEHOLD TYPE BY AGE OF HOUSEHOLDER                                                                                                                               PRESENCE OF OWN CHILDREN UNDER 18 YEARS BY AGE OF OWN CHILDREN UNDER 18 YEARS BY EMPLOYMENT STATUS FOR FEMALES 20 TO 64 YEARS                                           MEDIAN NUMBER OF ROOMS                                                                     AGGREGATE GROSS RENT (DOLLARS)                                                                                 TENURE BY AGE OF HOUSEHOLDER BY UNITS IN STRUCTURE                                                                                           SEX BY OCCUPATION FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER (BLACK OR AFRICAN AMERICAN ALONE)                    
# MEDIAN AGE BY SEX (WHITE ALONE, NOT HISPANIC OR LATINO)                                                                                                                    PLACE OF BIRTH (HISPANIC OR LATINO) IN PUERTO RICO                                                                                     GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY AGE FOR RESIDENCE 1 YEAR AGO IN THE UNITED STATES                                               MEANS OF TRANSPORTATION TO WORK (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)                                                    RECEIPT OF SUPPLEMENTAL SECURITY INCOME (SSI), CASH PUBLIC ASSISTANCE INCOME, OR FOOD STAMPS/SNAP IN THE PAST 12 MONTHS BY HOUSEHOLD TYPE FOR CHILDREN UNDER 18 YEARS IN HOUSEHOLDS                           HOUSEHOLD TYPE BY UNITS IN STRUCTURE                                                                                              SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER                                                                                                    POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY HOUSEHOLD TYPE BY EDUCATIONAL ATTAINMENT OF HOUSEHOLDER                                                                                                EDUCATIONAL ATTAINMENT BY EMPLOYMENT STATUS FOR THE POPULATION 25 TO 64 YEARS                                                                                           AGGREGATE NUMBER OF ROOMS                                                                  AGGREGATE GROSS RENT (DOLLARS) BY UNITS IN STRUCTURE                                                           TENURE BY AGE OF HOUSEHOLDER BY YEAR STRUCTURE BUILT                                                                                         SEX BY OCCUPATION FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER (AMERICAN INDIAN AND ALASKA NATIVE ALONE)            
# MEDIAN AGE BY SEX (HISPANIC OR LATINO)                                                                                                                                     PLACE OF BIRTH (HISPANIC OR LATINO) IN THE UNITED STATES                                                                               MEDIAN AGE BY GEOGRAPHICAL MOBILITY IN THE PAST YEAR FOR RESIDENCE 1 YEAR AGO IN PUERTO RICO                                              MEANS OF TRANSPORTATION TO WORK (SOME OTHER RACE ALONE)                                                                               HOUSEHOLD TYPE (INCLUDING LIVING ALONE) BY RELATIONSHIP                                                                                                                                                       SUBFAMILY TYPE BY PRESENCE OF OWN CHILDREN UNDER 18 YEARS                                                                         LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER                                                                               POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY HOUSEHOLD TYPE BY TENURE                                                                                                                               PRESENCE OF OWN CHILDREN UNDER 18 YEARS BY FAMILY TYPE BY EMPLOYMENT STATUS                                                                                             TENURE BY ROOMS                                                                            AGGREGATE GROSS RENT (DOLLARS) BY MEALS INCLUDED IN RENT                                                       TENURE BY YEAR STRUCTURE BUILT BY UNITS IN STRUCTURE                                                                                         SEX BY OCCUPATION FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER (ASIAN ALONE)                                        
# MEDIAN AGE BY SEX                                                                                                                                                          PLACE OF BIRTH BY LANGUAGE SPOKEN AT HOME AND ABILITY TO SPEAK ENGLISH IN PUERTO RICO                                                  MEDIAN AGE BY GEOGRAPHICAL MOBILITY IN THE PAST YEAR FOR RESIDENCE 1 YEAR AGO IN THE UNITED STATES                                        MEANS OF TRANSPORTATION TO WORK (TWO OR MORE RACES)                                                                                   RELATIONSHIP BY HOUSEHOLD TYPE (INCLUDING LIVING ALONE) FOR THE POPULATION 65 YEARS AND OVER                                                                                                                  POPULATION IN SUBFAMILIES BY SUBFAMILY TYPE BY RELATIONSHIP                                                                       AGE BY LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER                                                                        POVERTY STATUS IN THE PAST 12 MONTHS BY AGE (WHITE ALONE)                                                                                                                                                  AGE OF OWN CHILDREN UNDER 18 YEARS IN FAMILIES AND SUBFAMILIES BY LIVING ARRANGEMENTS BY EMPLOYMENT STATUS OF PARENTS                                                   MEDIAN NUMBER OF ROOMS BY TENURE                                                           BEDROOMS BY GROSS RENT                                                                                         TENURE BY AGE OF HOUSEHOLDER BY YEAR HOUSEHOLDER MOVED INTO UNIT                                                                             SEX BY OCCUPATION FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)   
# TOTAL POPULATION                                                                                                                                                           PLACE OF BIRTH BY LANGUAGE SPOKEN AT HOME AND ABILITY TO SPEAK ENGLISH IN THE UNITED STATES                                            GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY SEX FOR RESIDENCE 1 YEAR AGO IN PUERTO RICO                                                     MEANS OF TRANSPORTATION TO WORK (WHITE ALONE, NOT HISPANIC OR LATINO)                                                                 GRANDCHILDREN UNDER 18 YEARS LIVING WITH A GRANDPARENT HOUSEHOLDER BY AGE OF GRANDCHILD                                                                                                                       HOUSEHOLDS BY PRESENCE OF NONRELATIVES                                                                                            NATIVITY BY LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER (WHITE ALONE)                                                     POVERTY STATUS IN THE PAST 12 MONTHS BY AGE (BLACK OR AFRICAN AMERICAN ALONE)                                                                                                                              PRESENCE OF OWN CHILDREN UNDER 18 YEARS BY FAMILY TYPE BY NUMBER OF WORKERS IN FAMILY IN THE PAST 12 MONTHS                                                             AGGREGATE NUMBER OF ROOMS BY TENURE                                                        INCLUSION OF UTILITIES IN RENT                                                                                 TENURE BY YEAR HOUSEHOLDER MOVED INTO UNIT BY UNITS IN STRUCTURE                                                                             SEX BY OCCUPATION FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER (SOME OTHER RACE ALONE)                              
# RACE                                                                                                                                                                       PLACE OF BIRTH BY MARITAL STATUS IN PUERTO RICO                                                                                        GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY SEX FOR RESIDENCE 1 YEAR AGO IN THE UNITED STATES                                               MEANS OF TRANSPORTATION TO WORK (HISPANIC OR LATINO)                                                                                  GRANDCHILDREN UNDER 18 YEARS LIVING WITH A GRANDPARENT HOUSEHOLDER BY GRANDPARENT RESPONSIBILITY AND PRESENCE OF PARENT                                                                                       HOUSEHOLD TYPE BY HOUSEHOLD SIZE                                                                                                  NATIVITY BY LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER (BLACK OR AFRICAN AMERICAN ALONE)                                 POVERTY STATUS IN THE PAST 12 MONTHS BY AGE (AMERICAN INDIAN AND ALASKA NATIVE ALONE)                                                                                                                      PRESENCE OF OWN CHILDREN UNDER 18 YEARS IN MARRIED-COUPLE FAMILIES BY WORK EXPERIENCE OF HOUSEHOLDER AND SPOUSE                                                         UNITS IN STRUCTURE                                                                         GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS                                           GROUP QUARTERS POPULATION                                                                                                                    SEX BY OCCUPATION FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER (TWO OR MORE RACES)                                  
# AMERICAN INDIAN AND ALASKA NATIVE ALONE FOR SELECTED TRIBAL GROUPINGS                                                                                                      PLACE OF BIRTH BY MARITAL STATUS IN THE UNITED STATES                                                                                  GEOGRAPHICAL MOBILITY IN THE PAST YEAR (WHITE ALONE) FOR RESIDENCE 1 YEAR AGO IN PUERTO RICO                                              MEANS OF TRANSPORTATION TO WORK BY CITIZENSHIP STATUS                                                                                 MEDIAN FAMILY INCOME FOR FAMILIES WITH GRANDPARENT HOUSEHOLDERS AND/OR SPOUSES LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN AND PRESENCE OF PARENT OF GRANDCHILDREN   SEX BY MARITAL STATUS FOR THE POPULATION 15 YEARS AND OVER                                                                        NATIVITY BY LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER (AMERICAN INDIAN AND ALASKA NATIVE ALONE)                         POVERTY STATUS IN THE PAST 12 MONTHS BY AGE (ASIAN ALONE)                                                                                                                                                  MEDIAN AGE BY SEX FOR WORKERS 16 TO 64 YEARS                                                                                                                            TOTAL POPULATION IN OCCUPIED HOUSING UNITS BY TENURE BY YEAR HOUSEHOLDER MOVED INTO UNIT   MEDIAN GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS (DOLLARS)                          UNWEIGHTED HOUSING UNIT SAMPLE                                                                                                               SEX BY OCCUPATION FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER (WHITE ALONE, NOT HISPANIC OR LATINO)                
# ASIAN ALONE BY SELECTED GROUPS                                                                                                                                             PLACE OF BIRTH BY EDUCATIONAL ATTAINMENT IN PUERTO RICO                                                                                GEOGRAPHICAL MOBILITY IN THE PAST YEAR (WHITE ALONE) FOR RESIDENCE 1 YEAR AGO IN THE UNITED STATES                                        MEANS OF TRANSPORTATION TO WORK BY LANGUAGE SPOKEN AT HOME AND ABILITY TO SPEAK ENGLISH                                               GRANDPARENTS LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN BY LENGTH OF TIME RESPONSIBLE FOR OWN GRANDCHILDREN FOR THE POPULATION 30 YEARS AND OVER                    SEX BY MARITAL STATUS FOR THE POPULATION 15 YEARS AND OVER (WHITE ALONE)                                                          NATIVITY BY LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER (ASIAN ALONE)                                                     POVERTY STATUS IN THE PAST 12 MONTHS BY AGE (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)                                                                                                             AGGREGATE USUAL HOURS WORKED IN THE PAST 12 MONTHS BY SEX FOR WORKERS 16 TO 64 YEARS                                                                                    MORTGAGE STATUS BY AGE OF HOUSEHOLDER                                                      AGE OF HOUSEHOLDER BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS                     UNWEIGHTED GROUP QUARTERS POPULATION SAMPLE                                                                                                  SEX BY OCCUPATION FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER (HISPANIC OR LATINO)                                 
# NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE BY SELECTED GROUPS                                                                                                        PLACE OF BIRTH BY EDUCATIONAL ATTAINMENT IN THE UNITED STATES                                                                          GEOGRAPHICAL MOBILITY IN THE PAST YEAR (BLACK OR AFRICAN AMERICAN ALONE) FOR RESIDENCE 1 YEAR AGO IN PUERTO RICO                          MEANS OF TRANSPORTATION TO WORK BY POVERTY STATUS IN THE PAST 12 MONTHS                                                               GRANDPARENTS LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN AND AGE OF GRANDPARENT (WHITE ALONE)                                                                        SEX BY MARITAL STATUS FOR THE POPULATION 15 YEARS AND OVER (BLACK OR AFRICAN AMERICAN ALONE)                                      NATIVITY BY LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)                POVERTY STATUS IN THE PAST 12 MONTHS BY AGE (SOME OTHER RACE ALONE)                                                                                                                                        MEAN USUAL HOURS WORKED IN THE PAST 12 MONTHS FOR WORKERS 16 TO 64 YEARS                                                                                                UNITS IN STRUCTURE (WHITE ALONE HOUSEHOLDER)                                               HOUSEHOLD INCOME BY GROSS RENT AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS                       HOUSING UNIT COVERAGE RATE                                                                                                                   SEX BY OCCUPATION FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER                                                      
# WHITE ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES                                                                                                                 PLACE OF BIRTH BY POVERTY STATUS IN THE PAST 12 MONTHS IN PUERTO RICO                                                                  GEOGRAPHICAL MOBILITY IN THE PAST YEAR (BLACK OR AFRICAN AMERICAN ALONE) FOR RESIDENCE 1 YEAR AGO IN THE UNITED STATES                    MEANS OF TRANSPORTATION TO WORK BY OCCUPATION                                                                                         GRANDPARENTS LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN AND AGE OF GRANDPARENT (BLACK OR AFRICAN AMERICAN ALONE)                                                    SEX BY MARITAL STATUS FOR THE POPULATION 15 YEARS AND OVER (AMERICAN INDIAN AND ALASKA NATIVE ALONE)                              NATIVITY BY LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER (SOME OTHER RACE ALONE)                                           POVERTY STATUS IN THE PAST 12 MONTHS BY AGE (TWO OR MORE RACES)                                                                                                                                            SEX BY WORK STATUS IN THE PAST 12 MONTHS BY USUAL HOURS WORKED PER WEEK IN THE PAST 12 MONTHS BY WEEKS WORKED IN THE PAST 12 MONTHS FOR THE POPULATION 16 TO 64 YEARS   UNITS IN STRUCTURE (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)                           VALUE                                                                                                          TOTAL POPULATION COVERAGE RATE BY SEX                                                                                                        SEX BY OCCUPATION FOR THE FULL-TIME, YEAR-ROUND CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER                                
# BLACK OR AFRICAN AMERICAN ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES                                                                                             PLACE OF BIRTH BY POVERTY STATUS IN THE PAST 12 MONTHS IN THE UNITED STATES                                                            GEOGRAPHICAL MOBILITY IN THE PAST YEAR (AMERICAN INDIAN AND ALASKA NATIVE ALONE) FOR RESIDENCE 1 YEAR AGO IN PUERTO RICO                  MEANS OF TRANSPORTATION TO WORK BY INDUSTRY                                                                                           GRANDPARENTS LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN AND AGE OF GRANDPARENT (AMERICAN INDIAN AND ALASKA NATIVE ALONE)                                            SEX BY MARITAL STATUS FOR THE POPULATION 15 YEARS AND OVER (ASIAN ALONE)                                                          NATIVITY BY LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER (TWO OR MORE RACES)                                               POVERTY STATUS IN THE PAST 12 MONTHS BY AGE (WHITE ALONE, NOT HISPANIC OR LATINO)                                                                                                                          SEX BY CLASS OF WORKER FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER                                                                                           UNITS IN STRUCTURE (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)                   LOWER VALUE QUARTILE (DOLLARS)                                                                                 TOTAL POPULATION COVERAGE RATE BY WEIGHTING RACE AND HISPANIC OR LATINO GROUPS                                                               SEX BY INDUSTRY FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER                                                        
# AMERICAN INDIAN AND ALASKA NATIVE ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES                                                                                     GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY AGE FOR CURRENT RESIDENCE IN PUERTO RICO                                                     GEOGRAPHICAL MOBILITY IN THE PAST YEAR (AMERICAN INDIAN AND ALASKA NATIVE ALONE) FOR RESIDENCE 1 YEAR AGO IN THE UNITED STATES            MEANS OF TRANSPORTATION TO WORK BY CLASS OF WORKER                                                                                    GRANDPARENTS LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN AND AGE OF GRANDPARENT (ASIAN ALONE)                                                                        SEX BY MARITAL STATUS FOR THE POPULATION 15 YEARS AND OVER (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)                     NATIVITY BY LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER (WHITE ALONE, NOT HISPANIC OR LATINO)                             POVERTY STATUS IN THE PAST 12 MONTHS BY AGE (HISPANIC OR LATINO)                                                                                                                                           SEX BY CLASS OF WORKER FOR THE FULL-TIME, YEAR-ROUND CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER                                                                     UNITS IN STRUCTURE (ASIAN ALONE HOUSEHOLDER)                                               MEDIAN VALUE (DOLLARS)                                                                                         GROUP QUARTERS POPULATION COVERAGE RATE                                                                                                      SEX BY INDUSTRY FOR THE FULL-TIME, YEAR-ROUND CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER                                  
# ASIAN ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES                                                                                                                 GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY AGE FOR CURRENT RESIDENCE IN THE UNITED STATES                                               GEOGRAPHICAL MOBILITY IN THE PAST YEAR (ASIAN ALONE) FOR RESIDENCE 1 YEAR AGO IN PUERTO RICO                                              MEANS OF TRANSPORTATION TO WORK BY PLACE OF WORK--STATE AND COUNTY LEVEL                                                              GRANDPARENTS LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN AND AGE OF GRANDPARENT (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)                                   SEX BY MARITAL STATUS FOR THE POPULATION 15 YEARS AND OVER (SOME OTHER RACE ALONE)                                                NATIVITY BY LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER (HISPANIC OR LATINO)                                              POVERTY STATUS OF INDIVIDUALS IN THE PAST 12 MONTHS BY LIVING ARRANGEMENT                                                                                                                                  DETAILED OCCUPATION FOR THE FULL-TIME, YEAR-ROUND CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER                                                                        UNITS IN STRUCTURE (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)          UPPER VALUE QUARTILE (DOLLARS)                                                                                 HOUSING UNIT RESPONSE AND NONRESPONSE RATES WITH REASONS FOR NONINTERVIEWS                                                                   INDUSTRY BY OCCUPATION FOR THE CIVILIAN  EMPLOYED POPULATION 16 YEARS AND OVER                                                
# NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES                                                                            MEDIAN AGE BY GEOGRAPHICAL MOBILITY IN THE PAST YEAR FOR CURRENT RESIDENCE IN PUERTO RICO                                              GEOGRAPHICAL MOBILITY IN THE PAST YEAR (ASIAN ALONE) FOR RESIDENCE 1 YEAR AGO IN THE UNITED STATES                                        AGGREGATE TRAVEL TIME TO WORK (IN MINUTES) OF WORKERS BY PLACE OF WORK--STATE AND COUNTY LEVEL                                        GRANDPARENTS LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN AND AGE OF GRANDPARENT (SOME OTHER RACE ALONE)                                                              SEX BY MARITAL STATUS FOR THE POPULATION 15 YEARS AND OVER (TWO OR MORE RACES)                                                    NATIVITY BY LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER                                                                   RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF RELATED CHILDREN                                                DETAILED OCCUPATION FOR THE FULL-TIME, YEAR-ROUND CIVILIAN EMPLOYED MALE POPULATION 16 YEARS AND OVER                                                                   UNITS IN STRUCTURE (SOME OTHER RACE ALONE HOUSEHOLDER)                                     AGGREGATE VALUE (DOLLARS) BY AGE OF HOUSEHOLDER                                                                GROUP QUARTERS POPULATION RESPONSE AND NONRESPONSE RATES WITH REASONS FOR NONINTERVIEWS                                                      OCCUPATION BY CLASS OF WORKER FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER                                          
# SOME OTHER RACE ALONE OR IN COMBINATION WITH ONE OR MORE OTHER RACES                                                                                                       MEDIAN AGE BY GEOGRAPHICAL MOBILITY IN THE PAST YEAR FOR CURRENT RESIDENCE IN THE UNITED STATES                                        GEOGRAPHICAL MOBILITY IN THE PAST YEAR (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE) FOR RESIDENCE 1 YEAR AGO IN PUERTO RICO         AGGREGATE TRAVEL TIME TO WORK (IN MINUTES) OF WORKERS BY TRAVEL TIME TO WORK                                                          GRANDPARENTS LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN AND AGE OF GRANDPARENT (TWO OR MORE RACES)                                                                  SEX BY MARITAL STATUS FOR THE POPULATION 15 YEARS AND OVER (WHITE ALONE, NOT HISPANIC OR LATINO)                                  LANGUAGE SPOKEN AT HOME BY ABILITY TO SPEAK ENGLISH FOR THE POPULATION 5 YEARS AND OVER (HISPANIC OR LATINO)                                                          POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY HOUSEHOLD TYPE BY NUMBER OF OWN CHILDREN UNDER 18 YEARS                                                                                                DETAILED OCCUPATION FOR THE FULL-TIME, YEAR-ROUND CIVILIAN EMPLOYED FEMALE POPULATION 16 YEARS AND OVER                                                                 UNITS IN STRUCTURE (TWO OR MORE RACES HOUSEHOLDER)                                         AGGREGATE VALUE (DOLLARS) BY UNITS IN STRUCTURE                                                                DETAILED RACE                                                                                                                                INDUSTRY BY CLASS OF WORKER FOR THE CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER                                            
# HISPANIC OR LATINO ORIGIN BY SPECIFIC ORIGIN                                                                                                                               GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY SEX FOR CURRENT RESIDENCE IN PUERTO RICO                                                     GEOGRAPHICAL MOBILITY IN THE PAST YEAR (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE) FOR RESIDENCE 1 YEAR AGO IN THE UNITED STATES   MEANS OF TRANSPORTATION TO WORK BY TENURE                                                                                             GRANDPARENTS LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN AND AGE OF GRANDPARENT (WHITE ALONE, NOT HISPANIC OR LATINO)                                                SEX BY MARITAL STATUS FOR THE POPULATION 15 YEARS AND OVER (HISPANIC OR LATINO)                                                   AGE BY LANGUAGE SPOKEN AT HOME FOR THE POPULATION 5 YEARS AND OVER                                                                                                    AGE BY RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS                                                                                                                                              HOUSING UNITS                                                                                                                                                           UNITS IN STRUCTURE (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)                       MORTGAGE STATUS                                                                                                GEOGRAPHICAL MOBILITY IN THE PAST YEAR FOR CURRENT RESIDENCE--METROPOLITAN STATISTICAL AREA LEVEL IN PUERTO RICO                             HOUSEHOLD INCOME BY SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS                    
# HISPANIC OR LATINO ORIGIN BY RACE                                                                                                                                          GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY SEX FOR CURRENT RESIDENCE IN THE UNITED STATES                                               GEOGRAPHICAL MOBILITY IN THE PAST YEAR (SOME OTHER RACE ALONE) FOR RESIDENCE 1 YEAR AGO IN PUERTO RICO                                    MEANS OF TRANSPORTATION TO WORK BY VEHICLES AVAILABLE                                                                                 GRANDPARENTS LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN AND AGE OF GRANDPARENT (HISPANIC OR LATINO)                                                                 SEX BY MARITAL STATUS BY AGE FOR THE POPULATION 15 YEARS AND OVER                                                                 POVERTY STATUS IN THE PAST 12 MONTHS BY AGE BY LANGUAGE SPOKEN AT HOME FOR THE POPULATION 5 YEARS AND OVER                                                            POVERTY STATUS IN THE PAST 12 MONTHS BY NATIVITY                                                                                                                                                           OCCUPANCY STATUS                                                                                                                                                        UNITS IN STRUCTURE (HISPANIC OR LATINO HOUSEHOLDER)                                        AGGREGATE VALUE (DOLLARS) BY MORTGAGE STATUS                                                                   GEOGRAPHICAL MOBILITY IN THE PAST YEAR FOR CURRENT RESIDENCE--METROPOLITAN STATISTICAL AREA LEVEL IN THE UNITED STATES                                                                                                                                                     
# HISPANIC OR LATINO ORIGIN                                                                                                                                                  GEOGRAPHICAL MOBILITY IN THE PAST YEAR (WHITE ALONE) FOR CURRENT RESIDENCE IN PUERTO RICO                                              GEOGRAPHICAL MOBILITY IN THE PAST YEAR (SOME OTHER RACE ALONE) FOR RESIDENCE 1 YEAR AGO IN THE UNITED STATES                              HOUSEHOLD SIZE BY VEHICLES AVAILABLE                                                                                                  GRANDPARENTS LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN BY PRESENCE OF PARENT OF GRANDCHILDREN AND AGE OF GRANDPARENT                                               MARITAL STATUS BY SEX BY LABOR FORCE PARTICIPATION                                                                                EDUCATIONAL ATTAINMENT AND EMPLOYMENT STATUS BY LANGUAGE SPOKEN AT HOME FOR THE POPULATION 25 YEARS AND OVER                                                          RATIO OF INCOME TO POVERTY LEVEL OF FAMILIES IN THE PAST 12 MONTHS                                                                                                                                         TENURE (WHITE ALONE HOUSEHOLDER)                                                                                                                                        TENURE BY UNITS IN STRUCTURE                                                               MEDIAN VALUE (DOLLARS) FOR MOBILE HOMES                                                                        GEOGRAPHICAL MOBILITY IN THE PAST YEAR FOR CURRENT RESIDENCE--MICROPOLITAN STATISTICAL AREA LEVEL IN THE UNITED STATES                                                                                                                                                     
# PEOPLE REPORTING SINGLE ANCESTRY                                                                                                                                           GEOGRAPHICAL MOBILITY IN THE PAST YEAR (WHITE ALONE) FOR CURRENT RESIDENCE IN THE UNITED STATES                                        GEOGRAPHICAL MOBILITY IN THE PAST YEAR (TWO OR MORE RACES) FOR RESIDENCE 1 YEAR AGO IN PUERTO RICO                                        HOUSEHOLD SIZE BY NUMBER OF WORKERS IN HOUSEHOLD                                                                                      NATIVITY BY GRANDPARENTS RESPONSIBLE FOR OWN GRANDCHILDREN UNDER 18 YEARS BY AGE OF GRANDPARENT                                                                                                               MEDIAN AGE AT FIRST MARRIAGE (WHITE ALONE)                                                                                        POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE (WHITE ALONE)                                                                                                      EARNINGS IN THE PAST 12 MONTHS FOR HOUSEHOLDS                                                                                                                                                              TENURE (BLACK OR AFRICAN AMERICAN ALONE HOUSEHOLDER)                                                                                                                    TOTAL POPULATION IN OCCUPIED HOUSING UNITS BY TENURE BY UNITS IN STRUCTURE                 PRICE ASKED                                                                                                    GEOGRAPHICAL MOBILITY IN THE PAST YEAR FOR CURRENT RESIDENCE--NOT METROPOLITAN OR MICROPOLITAN STATISTICAL AREA LEVEL IN THE UNITED STATES                                                                                                                                 
# PEOPLE REPORTING MULTIPLE ANCESTRY                                                                                                                                         GEOGRAPHICAL MOBILITY IN THE PAST YEAR (BLACK OR AFRICAN AMERICAN ALONE) FOR CURRENT RESIDENCE IN PUERTO RICO                          GEOGRAPHICAL MOBILITY IN THE PAST YEAR (TWO OR MORE RACES) FOR RESIDENCE 1 YEAR AGO IN THE UNITED STATES                                  NUMBER OF WORKERS IN HOUSEHOLD BY VEHICLES AVAILABLE                                                                                  LANGUAGE AND ABILITY TO SPEAK ENGLISH OF GRANDPARENTS LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN AND AGE OF GRANDPARENT                                             MEDIAN AGE AT FIRST MARRIAGE (BLACK OR AFRICAN AMERICAN ALONE)                                                                    POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE (BLACK OR AFRICAN AMERICAN ALONE)                                                                                  WAGE OR SALARY INCOME IN THE PAST 12 MONTHS FOR HOUSEHOLDS                                                                                                                                                 TENURE (AMERICAN INDIAN AND ALASKA NATIVE ALONE HOUSEHOLDER)                                                                                                            YEAR STRUCTURE BUILT                                                                       AGGREGATE PRICE ASKED (DOLLARS)                                                                                MEANS OF TRANSPORTATION TO WORK BY TRAVEL TIME TO WORK                                                                                                                                                                                                                     
# PEOPLE REPORTING ANCESTRY                                                                                                                                                  GEOGRAPHICAL MOBILITY IN THE PAST YEAR (BLACK OR AFRICAN AMERICAN ALONE) FOR CURRENT RESIDENCE IN THE UNITED STATES                    GEOGRAPHICAL MOBILITY IN THE PAST YEAR (WHITE ALONE, NOT HISPANIC OR LATINO) FOR RESIDENCE 1 YEAR AGO IN PUERTO RICO                      MEANS OF TRANSPORTATION TO WORK                                                                                                       SEX OF GRANDPARENTS LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN AND AGE OF GRANDPARENT                                                                               MEDIAN AGE AT FIRST MARRIAGE (AMERICAN INDIAN AND ALASKA NATIVE ALONE)                                                            POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE (AMERICAN INDIAN AND ALASKA NATIVE ALONE)                                                                          SELF-EMPLOYMENT INCOME IN THE PAST 12 MONTHS FOR HOUSEHOLDS                                                                                                                                                TENURE (ASIAN ALONE HOUSEHOLDER)                                                                                                                                        MEDIAN YEAR STRUCTURE BUILT                                                                MORTGAGE STATUS AND SELECTED MONTHLY OWNER COSTS                                                               AGGREGATE TRAVEL TIME TO WORK (IN MINUTES) OF WORKERS BY MEANS OF TRANSPORTATION TO WORK                                                                                                                                                                                   
# ANCESTRY                                                                                                                                                                   GEOGRAPHICAL MOBILITY IN THE PAST YEAR (AMERICAN INDIAN AND ALASKA NATIVE ALONE) FOR CURRENT RESIDENCE IN PUERTO RICO                  GEOGRAPHICAL MOBILITY IN THE PAST YEAR (WHITE ALONE, NOT HISPANIC OR LATINO) FOR RESIDENCE 1 YEAR AGO IN THE UNITED STATES                TRAVEL TIME TO WORK                                                                                                                   MARITAL STATUS BY GRANDPARENTS LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN AND AGE OF GRANDPARENT                                                                    MEDIAN AGE AT FIRST MARRIAGE (ASIAN ALONE)                                                                                        POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE (ASIAN ALONE)                                                                                                      INTEREST, DIVIDENDS, OR NET RENTAL INCOME IN THE PAST 12 MONTHS FOR HOUSEHOLDS                                                                                                                             TENURE (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE HOUSEHOLDER)                                                                                                   TENURE BY YEAR STRUCTURE BUILT                                                             MEDIAN SELECTED MONTHLY OWNER COSTS (DOLLARS) BY MORTGAGE STATUS                                               SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER (WHITE ALONE)                                                                                                                                                                                           
# PLACE OF BIRTH FOR THE FOREIGN-BORN POPULATION IN PUERTO RICO                                                                                                              GEOGRAPHICAL MOBILITY IN THE PAST YEAR (AMERICAN INDIAN AND ALASKA NATIVE ALONE) FOR CURRENT RESIDENCE IN THE UNITED STATES            GEOGRAPHICAL MOBILITY IN THE PAST YEAR (HISPANIC OR LATINO) FOR RESIDENCE 1 YEAR AGO IN PUERTO RICO                                       SEX OF WORKERS BY MEANS OF TRANSPORTATION TO WORK FOR WORKPLACE GEOGRAPHY                                                             EMPLOYMENT STATUS OF GRANDPARENTS LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN AND AGE OF GRANDPARENT                                                                 MEDIAN AGE AT FIRST MARRIAGE (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)                                                   POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)                                                                 SOCIAL SECURITY INCOME IN THE PAST 12 MONTHS FOR HOUSEHOLDS                                                                                                                                                TENURE (SOME OTHER RACE ALONE HOUSEHOLDER)                                                                                                                              MEDIAN YEAR STRUCTURE BUILT BY TENURE                                                      AGGREGATE SELECTED MONTHLY OWNER COSTS (DOLLARS) BY MORTGAGE STATUS                                            SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER (BLACK OR AFRICAN AMERICAN ALONE)                                                                                                                                                                       
# PLACE OF BIRTH FOR THE FOREIGN-BORN POPULATION IN THE UNITED STATES                                                                                                        GEOGRAPHICAL MOBILITY IN THE PAST YEAR (ASIAN ALONE) FOR CURRENT RESIDENCE IN PUERTO RICO                                              GEOGRAPHICAL MOBILITY IN THE PAST YEAR (HISPANIC OR LATINO) FOR RESIDENCE 1 YEAR AGO IN THE UNITED STATES                                 SEX OF WORKERS BY TRAVEL TIME TO WORK FOR WORKPLACE GEOGRAPHY                                                                         POVERTY STATUS IN THE PAST 12 MONTHS OF GRANDPARENTS LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN AND AGE OF GRANDPARENT                                              MEDIAN AGE AT FIRST MARRIAGE (SOME OTHER RACE ALONE)                                                                              POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE (SOME OTHER RACE ALONE)                                                                                            SUPPLEMENTAL SECURITY INCOME (SSI) IN THE PAST 12 MONTHS FOR HOUSEHOLDS                                                                                                                                    TENURE (TWO OR MORE RACES HOUSEHOLDER)                                                                                                                                  TENURE BY YEAR HOUSEHOLDER MOVED INTO UNIT                                                 MORTGAGE STATUS BY AGGREGATE REAL ESTATE TAXES PAID (DOLLARS)                                                  SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER (AMERICAN INDIAN AND ALASKA NATIVE ALONE)                                                                                                                                                               
# PLACE OF BIRTH BY YEAR OF ENTRY BY CITIZENSHIP STATUS FOR THE FOREIGN-BORN POPULATION                                                                                      GEOGRAPHICAL MOBILITY IN THE PAST YEAR (ASIAN ALONE) FOR CURRENT RESIDENCE IN THE UNITED STATES                                        GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY CITIZENSHIP STATUS FOR RESIDENCE 1 YEAR AGO IN PUERTO RICO                                      MEANS OF TRANSPORTATION TO WORK BY AGE FOR WORKPLACE GEOGRAPHY                                                                        HOUSEHOLDS WITH GRANDPARENTS LIVING WITH OWN GRANDCHILDREN UNDER 18 YEARS BY RESPONSIBILITY FOR OWN GRANDCHILDREN AND PRESENCE OF PARENT OF GRANDCHILDREN                                                     MEDIAN AGE AT FIRST MARRIAGE (TWO OR MORE RACES)                                                                                  POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE (TWO OR MORE RACES)                                                                                                PUBLIC ASSISTANCE INCOME IN THE PAST 12 MONTHS FOR HOUSEHOLDS                                                                                                                                              TENURE (WHITE ALONE, NOT HISPANIC OR LATINO HOUSEHOLDER)                                                                                                                MEDIAN YEAR HOUSEHOLDER MOVED INTO UNIT BY TENURE                                          MORTGAGE STATUS BY SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS      SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER (ASIAN ALONE)                                                                                                                                                                                           
# SEX BY PLACE OF BIRTH BY YEAR OF ENTRY FOR THE FOREIGN-BORN POPULATION                                                                                                     GEOGRAPHICAL MOBILITY IN THE PAST YEAR (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE) FOR CURRENT RESIDENCE IN PUERTO RICO         GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY CITIZENSHIP STATUS FOR RESIDENCE 1 YEAR AGO IN THE UNITED STATES                                MEDIAN AGE BY MEANS OF TRANSPORTATION TO WORK FOR WORKPLACE GEOGRAPHY                                                                 HOUSEHOLD TYPE (INCLUDING LIVING ALONE) (WHITE ALONE)                                                                                                                                                         MEDIAN AGE AT FIRST MARRIAGE (WHITE ALONE, NOT HISPANIC OR LATINO)                                                                POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE (WHITE ALONE, NOT HISPANIC OR LATINO)                                                                              PUBLIC ASSISTANCE INCOME OR FOOD STAMPS/SNAP IN THE PAST 12 MONTHS FOR HOUSEHOLDS                                                                                                                          TENURE (HISPANIC OR LATINO HOUSEHOLDER)                                                                                                                                 HOUSE HEATING FUEL                                                                         MEDIAN SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS                  SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)                                                                                                                                                      
# AGE AND NATIVITY OF OWN CHILDREN UNDER 18 YEARS IN FAMILIES AND SUBFAMILIES BY NUMBER AND NATIVITY OF PARENTS                                                              GEOGRAPHICAL MOBILITY IN THE PAST YEAR (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE) FOR CURRENT RESIDENCE IN THE UNITED STATES   GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY MARITAL STATUS FOR RESIDENCE 1 YEAR AGO IN PUERTO RICO                                          MEANS OF TRANSPORTATION TO WORK FOR WORKPLACE GEOGRAPHY (WHITE ALONE)                                                                 HOUSEHOLD TYPE (INCLUDING LIVING ALONE) (BLACK OR AFRICAN AMERICAN ALONE)                                                                                                                                     MEDIAN AGE AT FIRST MARRIAGE (HISPANIC OR LATINO)                                                                                 POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE (HISPANIC OR LATINO)                                                                                               RETIREMENT INCOME IN THE PAST 12 MONTHS FOR HOUSEHOLDS                                                                                                                                                     TENURE                                                                                                                                                                  BEDROOMS                                                                                   AGE OF HOUSEHOLDER BY SELECTED MONTHLY OWNER COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS   SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER (SOME OTHER RACE ALONE)                                                                                                                                                                                 
# RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS BY NATIVITY OF CHILDREN UNDER 18 YEARS IN FAMILIES AND SUBFAMILIES BY LIVING ARRANGEMENTS AND NATIVITY OF PARENTS   GEOGRAPHICAL MOBILITY IN THE PAST YEAR (SOME OTHER RACE ALONE) FOR CURRENT RESIDENCE IN PUERTO RICO                                    GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY MARITAL STATUS FOR RESIDENCE 1 YEAR AGO IN THE UNITED STATES                                    MEANS OF TRANSPORTATION TO WORK FOR WORKPLACE GEOGRAPHY (BLACK OR AFRICAN AMERICAN ALONE)                                             HOUSEHOLD TYPE (INCLUDING LIVING ALONE) (AMERICAN INDIAN AND ALASKA NATIVE ALONE)                                                                                                                             MEDIAN AGE AT FIRST MARRIAGE                                                                                                      POVERTY STATUS IN THE PAST 12 MONTHS BY SEX BY AGE                                                                                                                    OTHER TYPES OF INCOME IN THE PAST 12 MONTHS FOR HOUSEHOLDS                                                                                                                                                 VACANCY STATUS                                                                                                                                                          TENURE BY BEDROOMS                                                                         SELECTED MONTHLY OWNER COSTS                                                                                   SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER (TWO OR MORE RACES)                                                                                                                                                                                     
# NATIVITY IN THE UNITED STATES                                                                                                                                              GEOGRAPHICAL MOBILITY IN THE PAST YEAR (SOME OTHER RACE ALONE) FOR CURRENT RESIDENCE IN THE UNITED STATES                              GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY EDUCATIONAL ATTAINMENT FOR RESIDENCE 1 YEAR AGO IN PUERTO RICO                                  MEANS OF TRANSPORTATION TO WORK FOR WORKPLACE GEOGRAPHY (AMERICAN INDIAN AND ALASKA NATIVE ALONE)                                     HOUSEHOLD TYPE (INCLUDING LIVING ALONE) (ASIAN ALONE)                                                                                                                                                         WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY MARITAL STATUS (WHITE ALONE)                                        POVERTY STATUS IN THE PAST 12 MONTHS OF INDIVIDUALS BY SEX BY EDUCATIONAL ATTAINMENT                                                                                  HOUSEHOLD INCOME QUINTILE UPPER LIMITS                                                                                                                                                                     VACANT - CURRENT RESIDENCE ELSEWHERE                                                                                                                                    TENURE BY TELEPHONE SERVICE AVAILABLE BY AGE OF HOUSEHOLDER                                MORTGAGE STATUS BY VALUE                                                                                       SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER (WHITE ALONE, NOT HISPANIC OR LATINO)                                                                                                                                                                   
# PLACE OF BIRTH BY AGE IN PUERTO RICO                                                                                                                                       GEOGRAPHICAL MOBILITY IN THE PAST YEAR (TWO OR MORE RACES) FOR CURRENT RESIDENCE IN PUERTO RICO                                        GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY EDUCATIONAL ATTAINMENT FOR RESIDENCE 1 YEAR AGO IN THE UNITED STATES                            MEANS OF TRANSPORTATION TO WORK FOR WORKPLACE GEOGRAPHY (ASIAN ALONE)                                                                 HOUSEHOLD TYPE (INCLUDING LIVING ALONE) (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)                                                                                                                    WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY MARITAL STATUS (BLACK OR AFRICAN AMERICAN ALONE)                    POVERTY STATUS IN THE PAST 12 MONTHS OF INDIVIDUALS BY SEX BY WORK EXPERIENCE                                                                                         MEAN HOUSEHOLD INCOME OF QUINTILES                                                                                                                                                                         RACE OF HOUSEHOLDER                                                                                                                                                     TENURE BY VEHICLES AVAILABLE                                                               MORTGAGE STATUS BY MEDIAN VALUE (DOLLARS)                                                                      SEX BY EDUCATIONAL ATTAINMENT FOR THE POPULATION 25 YEARS AND OVER (HISPANIC OR LATINO)                                                                                                                                                                                    
# PLACE OF BIRTH BY AGE IN THE UNITED STATES                                                                                                                                 GEOGRAPHICAL MOBILITY IN THE PAST YEAR (TWO OR MORE RACES) FOR CURRENT RESIDENCE IN THE UNITED STATES                                  GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY POVERTY STATUS IN THE PAST 12 MONTHS FOR RESIDENCE 1 YEAR AGO IN PUERTO RICO                    MEANS OF TRANSPORTATION TO WORK FOR WORKPLACE GEOGRAPHY (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)                            HOUSEHOLD TYPE (INCLUDING LIVING ALONE) (SOME OTHER RACE ALONE)                                                                                                                                               WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY MARITAL STATUS (AMERICAN INDIAN AND ALASKA NATIVE ALONE)            POVERTY STATUS IN THE PAST 12 MONTHS OF INDIVIDUALS BY SEX BY EMPLOYMENT STATUS                                                                                       SHARES OF AGGREGATE HOUSEHOLD INCOME BY QUINTILE                                                                                                                                                           TENURE BY AGE OF HOUSEHOLDER                                                                                                                                            TENURE BY VEHICLES AVAILABLE BY AGE OF HOUSEHOLDER                                         MORTGAGE STATUS BY RATIO OF VALUE TO HOUSEHOLD INCOME IN THE PAST 12 MONTHS                                    RATIO OF INCOME TO POVERTY LEVEL IN THE PAST 12 MONTHS                                                                                                                                                                                                                     
# MEDIAN AGE BY PLACE OF BIRTH IN PUERTO RICO                                                                                                                                GEOGRAPHICAL MOBILITY IN THE PAST YEAR (WHITE ALONE, NOT HISPANIC OR LATINO) FOR CURRENT RESIDENCE IN PUERTO RICO                      GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY POVERTY STATUS IN THE PAST 12 MONTHS FOR RESIDENCE 1 YEAR AGO IN THE UNITED STATES              MEANS OF TRANSPORTATION TO WORK FOR WORKPLACE GEOGRAPHY (SOME OTHER RACE ALONE)                                                       HOUSEHOLD TYPE (INCLUDING LIVING ALONE) (TWO OR MORE RACES)                                                                                                                                                   WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY MARITAL STATUS (ASIAN ALONE)                                        POVERTY STATUS IN THE PAST 12 MONTHS OF RELATED CHILDREN UNDER 18 YEARS BY FAMILY TYPE BY AGE OF RELATED CHILDREN UNDER 18 YEARS                                      GINI INDEX OF INCOME INEQUALITY                                                                                                                                                                            TOTAL POPULATION IN OCCUPIED HOUSING UNITS BY TENURE                                                                                                                    AGGREGATE NUMBER OF VEHICLES AVAILABLE BY TENURE                                           MORTGAGE STATUS BY MONTHLY HOUSING COSTS AS A PERCENTAGE OF HOUSEHOLD INCOME IN THE PAST 12 MONTHS             SEX BY AGE BY VETERAN STATUS FOR THE CIVILIAN POPULATION 18 YEARS AND OVER (WHITE ALONE)                                                                                                                                                                                   
# MEDIAN AGE BY PLACE OF BIRTH IN THE UNITED STATES                                                                                                                          GEOGRAPHICAL MOBILITY IN THE PAST YEAR (WHITE ALONE, NOT HISPANIC OR LATINO) FOR CURRENT RESIDENCE IN THE UNITED STATES                GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY TENURE FOR RESIDENCE 1 YEAR AGO IN PUERTO RICO                                                  MEANS OF TRANSPORTATION TO WORK FOR WORKPLACE GEOGRAPHY (TWO OR MORE RACES)                                                           HOUSEHOLD TYPE (INCLUDING LIVING ALONE) (WHITE ALONE, NOT HISPANIC OR LATINO)                                                                                                                                 WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY MARITAL STATUS (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE)   POVERTY STATUS IN THE PAST 12 MONTHS OF UNRELATED INDIVIDUALS 15 YEARS AND OVER BY SEX BY AGE                                                                         SEX BY AGE BY VETERAN STATUS FOR THE CIVILIAN POPULATION 18 YEARS AND OVER                                                                                                                                 TENURE BY HOUSEHOLD SIZE                                                                                                                                                PLUMBING FACILITIES FOR ALL HOUSING UNITS                                                  MORTGAGE STATUS BY REAL ESTATE TAXES PAID                                                                      SEX BY AGE BY VETERAN STATUS FOR THE CIVILIAN POPULATION 18 YEARS AND OVER (BLACK OR AFRICAN AMERICAN ALONE)                                                                                                                                                               
# PLACE OF BIRTH BY SEX IN PUERTO RICO                                                                                                                                       GEOGRAPHICAL MOBILITY IN THE PAST YEAR (HISPANIC OR LATINO) FOR CURRENT RESIDENCE IN PUERTO RICO                                       GEOGRAPHICAL MOBILITY IN THE PAST YEAR BY TENURE FOR RESIDENCE 1 YEAR AGO IN THE UNITED STATES                                            MEANS OF TRANSPORTATION TO WORK FOR WORKPLACE GEOGRAPHY (WHITE ALONE, NOT HISPANIC OR LATINO)                                         HOUSEHOLD TYPE (INCLUDING LIVING ALONE) (HISPANIC OR LATINO)                                                                                                                                                  WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY MARITAL STATUS (SOME OTHER RACE ALONE)                              POVERTY STATUS BY WORK EXPERIENCE OF UNRELATED INDIVIDUALS BY HOUSEHOLDER STATUS                                                                                      PERIOD OF MILITARY SERVICE FOR CIVILIAN VETERANS 18 YEARS AND OVER                                                                                                                                         AVERAGE HOUSEHOLD SIZE OF OCCUPIED HOUSING UNITS BY TENURE                                                                                                              PLUMBING FACILITIES FOR OCCUPIED HOUSING UNITS                                             MORTGAGE STATUS BY MEDIAN REAL ESTATE TAXES PAID (DOLLARS)                                                     SEX BY AGE BY VETERAN STATUS FOR THE CIVILIAN POPULATION 18 YEARS AND OVER (AMERICAN INDIAN AND ALASKA NATIVE ALONE)                                                                                                                                                       
# PLACE OF BIRTH BY SEX IN THE UNITED STATES                                                                                                                                 GEOGRAPHICAL MOBILITY IN THE PAST YEAR (HISPANIC OR LATINO) FOR CURRENT RESIDENCE IN THE UNITED STATES                                 SEX OF WORKERS BY MEANS OF TRANSPORTATION TO WORK                                                                                         MEANS OF TRANSPORTATION TO WORK FOR WORKPLACE GEOGRAPHY (HISPANIC OR LATINO)                                                          HOUSEHOLD TYPE (INCLUDING LIVING ALONE)                                                                                                                                                                       WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY MARITAL STATUS (TWO OR MORE RACES)                                  POVERTY STATUS IN THE PAST 12 MONTHS OF FAMILIES BY FAMILY TYPE BY PRESENCE OF RELATED CHILDREN UNDER 18 YEARS BY AGE OF RELATED CHILDREN (WHITE ALONE HOUSEHOLDER)   VETERAN STATUS BY EDUCATIONAL ATTAINMENT FOR THE CIVILIAN POPULATION 25 YEARS AND OVER                                                                                                                     TENURE BY HOUSEHOLD TYPE (INCLUDING LIVING ALONE) AND AGE OF HOUSEHOLDER                                                                                                TENURE BY PLUMBING FACILITIES                                                              MONTHLY HOUSING COSTS                                                                                          SEX BY AGE BY VETERAN STATUS FOR THE CIVILIAN POPULATION 18 YEARS AND OVER (ASIAN ALONE)                                                                                                                                                                                   
# -------------------------------------------------------------------------------------------------------------------------------------------------------------------------  -------------------------------------------------------------------------------------------------------------------------------------  ----------------------------------------------------------------------------------------------------------------------------------------  ------------------------------------------------------------------------------------------------------------------------------------  ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  --------------------------------------------------------------------------------------------------------------------------------  --------------------------------------------------------------------------------------------------------------------------------------------------------------------  ---------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------  ----------------------------------------------------------------------------------------------------------------------------------------------------------------------  -----------------------------------------------------------------------------------------  -------------------------------------------------------------------------------------------------------------  -------------------------------------------------------------------------------------------------------------------------------------------  ------------------------------------------------------------------------------------------------------------------------------

# TOTAL POPULATION
# RACE
# MEDIAN AGE BY SEX
# WOMEN 15 TO 50 YEARS WHO HAD A BIRTH IN THE PAST 12 MONTHS BY MARITAL STATUS AND AGE
# GEOGRAPHICAL MOBILITY IN THE PAST YEAR FOR CURRENT RESIDENCE--STATE, COUNTY AND PLACE LEVEL IN THE UNITED STATES
# MEDIAN AGE BY GEOGRAPHICAL MOBILITY IN THE PAST YEAR FOR RESIDENCE 1 YEAR AGO IN THE UNITED STATES
# DETAILED OCCUPATION FOR THE FULL-TIME, YEAR-ROUND CIVILIAN EMPLOYED POPULATION 16 YEARS AND OVER
# POPULATION UNDER 18 YEARS BY AGE


# B07204_011 Estimate!!Total!!Different house in United States 1 year ago!!Elsewhere!!Different county!!Different state
# B09002_001 Estimate!!Total                                                                            OWN CHILDREN UNDER 18 YEARS BY FAMILY TYPE AND AGE
# B09002_002 Estimate!!Total!!In married-couple families                                                OWN CHILDREN UNDER 18 YEARS BY FAMILY TYPE AND AGE

vars <- c(
  "total_pop" = "B01003_001",
  "white_pop" = "B02001_002",
  "median_age" = "B01002_001",
  "moved_state" = "B07204_011",
  "median_age_moved_state" = "B07402_005",
  
  "have_children" = "B09002_001",
  "have_children_married" = "B09002_002",
  "female_never_married" = "B12002_096",
  
  "female_ba" = "B15002_032",
  "female_ma" = "B15002_033",
  "female_phd" = "B15002_035",
  
  "management" = "C24060_002",
  "service" = "C24060_003",
  "sales" = "C24060_004",
  "construction" = "C24060_005",
  "transportation" = "C24060_006",
  
  "veteran" = "C21001A_001"
)

acs10 <- get_acs(
  geography = "state", 
  variables = vars, 
  year = 2010
)
acs10$yr <- 2010

acs20 <- get_acs(
  geography = "state", 
  variables = vars, 
  year = 2020
)
acs20$yr <- 2020

acs <- rbind(acs10, acs20)
acs <- select(acs, -moe)

acs <- acs %>%
  pivot_wider(
    id_cols = c(GEOID, NAME, yr),
    names_from = variable,
    values_from = estimate
  ) %>%
  rename(
    !!vars
  ) %>%
  inner_join(
    select(tigris::states(), GEOID, ALAND),
    by = "GEOID"
  ) %>%
  mutate(
    ALAND = ALAND / 2.59e6, # Convert to square miles
    density = total_pop / ALAND,
    pct_white = white_pop / total_pop,
    pct_moved_state = moved_state / total_pop,
    pct_have_children = have_children / total_pop,
    pct_have_children_married = have_children_married / have_children,
    pct_female_never_married = female_never_married / total_pop,
    pct_female_educated = (female_ba + female_ma + female_phd) / total_pop,
    pct_veteran = veteran / total_pop,
    pct_management = management / total_pop,
    pct_service = service / total_pop,
    pct_sales = sales / total_pop,
    pct_construction = construction / total_pop,
    pct_transportation = transportation / total_pop
  ) %>%
  select(
    geoid = GEOID,
    state = NAME,
    yr,
    median_age,
    density,
    contains("pct")
  ) %>%
  filter(
    !state %in% "Puerto Rico"
  )

# Crime ---------------------------------------------------

"
https://cde.ucr.cjis.gov/LATEST/webapp/#/pages/docApi
"

crime_call <- function(state, start_year, end_year, api_key) {
  glue::glue(
    "https://api.usa.gov/crime/fbi/cde/estimate/state/",
    "{state}?from={start_year}&to={end_year}&API_KEY={fbi}"
  )
}

state_abbr <- tigris::states()$STUSPS

params <- data.frame(
  state = state_abbr,
  start_year = 2010,
  end_year = 2020,
  api_key = fbi
)

oo <- params %>%
  rowwise() %>%
  group_split() %>%
  map(
    ~ as.list(.x)
  ) %>%
  map(
    ~ rlang::exec(crime_call, !!!.x)
  ) %>%
  map(
    ~ GET(.x),
    .progress = TRUE
  ) %>%
  map(
    ~ fromJSON(rawToChar(.x$content))
  )

boo <- bind_rows(oo)

boo <- boo %>%
  mutate(
    across(population:last_col(), ~ as.numeric(.x)),
    across(violent_crime:last_col(), ~ .x * 100000 / population),
    rape = coalesce(rape_legacy, rape_revised)
  ) %>%
  inner_join(
    select(tigris::states(), STUSPS, GEOID),
    by = c("state_abbr" = "STUSPS")
  ) %>%
  data.frame() %>%
  select(
    -c(state_id, rape_legacy, rape_revised, geometry, state_abbr)
  )

cacs <- inner_join(acs, boo, by = c("geoid" = "GEOID", "yr" = "year"))

clus <- cacs %>%
  filter(
    yr == 2020
  ) %>%
  select(
    median_age:last_col(),
    -population
  ) %>%
  scale() %>%
  data.frame()

cts <- clValid(
  clus, 
  nClust = 2:15
)

cacs %>%
  filter(
    yr == 2020
  ) %>%
  mutate(
    cluster = cutree(cts@clusterObjs$hierarchical, k = 15)
  ) %>%
  group_by(
    cluster
  ) %>%
  group_split()






census10 <- census10 %>%
  pivot_wider(
    id_cols = c(GEOID, NAME),
    names_from = variable,
    names_glue = "{variable}_2010",
    values_from = estimate
  ) %>%
  mutate(
    pct_childbirth_2010 = B13002_002_2010 / B01003_001_2010
  )
census20 <- census20 %>%
  pivot_wider(
    id_cols = c(GEOID, NAME),
    names_from = variable,
    names_glue = "{variable}_2020",
    values_from = estimate
  ) %>%
  mutate(
    pct_childbirth_2020 = B13002_002_2020 / B01003_001_2020
  )

census_comp <- inner_join(census10, census20, by = c("GEOID", "NAME"))
census_comp <- census_comp %>%
  pivot_longer(
    cols = !c(GEOID, NAME)
  ) %>%
  mutate(
    yr = stringr::str_extract(name, "[^_]*$"),
    name = gsub("[^_]*$", "", name)
  ) %>%
  pivot_wider(
    names_from = name,
    values_from = value
  ) %>%
  mutate(
    dod = diff(B01003_001_),
    .by = GEOID
  ) %>%
  arrange(
    yr,
    -pct_childbirth_
  ) %>%
  mutate(
    NAME = factor(NAME, unique(NAME))
  )

# Census data ---------------------------------------------

variables <- list(
  "B01001_026", # Female population
  "B01002_001", # Median age
  "B01003_001", # Total population
  "B02001_002", # White population
  "B07201_007", # Moved from different metro
  "B09002_001", # Children present
  "B12001_012", # Never-married females
  "B19301_001", # Per capita income prior 12m
  "B15011_001", # Total bachelor's degrees
  "B15011_021", # Female bachelor's degrees
  "B21001_002", # Veteran
  "C15010_002", # Science and engineering field of study
  "C24030_017", # Professional services: Male
  "C24030_044" # Professional services: Female
)

# State -------------------------------

states <- states()

query_api <- function(x) {
  get_acs(
    geography = "state",
    variables = x,
    geometry = FALSE,
    year = 2021
  )
}
sdemos <- lapply(variables, query_api)
sdemos <- bind_rows(sdemos)

sdemos <- sdemos %>%
  pivot_wider(
    id_cols = NAME,
    names_from = variable,
    values_from = estimate
  ) %>%
  mutate(
    total_pop = B01003_001,
    median_age = B01002_001,
    pct_female = B01001_026 / total_pop,
    pct_white = B02001_002 / total_pop,
    pct_moved = B07201_007 / total_pop,
    pct_children = B09002_001 / total_pop,
    pct_unmarried_female = B12001_012 / total_pop,
    per_capita_income = B19301_001,
    pct_ba = B15011_001 / total_pop,
    pct_ba_female = B15011_021 / total_pop,
    pct_vet = B21001_002 / total_pop,
    pct_hard_major = C15010_002 / total_pop,
    pct_professional = (C24030_017 + C24030_044) / total_pop
  ) %>%
  select(
    NAME,
    total_pop:last_col()
  ) %>%
  filter(
    !NAME %in% "Puerto Rico"
  )

sdemos <- left_join(sdemos, data.frame(states[, c("NAME", "ALAND")]))
sdemos$density <- sdemos$total_pop / sdemos$ALAND
sdemos <- select(sdemos, -ALAND, -geometry)

clus <- select(data.frame(sdemos), median_age:last_col())

pca <- psych::principal(
  scale(clus), 
  rotate = 'varimax', 
  nfactors = length(clus) - 1, 
  scores = T
)

loads <- data.frame(unclass(pca$loadings))
loads <- loads %>% select(sort(colnames(.)))
loads$category <- rownames(loads)
loads <- reshape2::melt(loads)

loads <- loads %>%
  arrange(
    desc(category)
  ) %>%
  mutate(
    category = factor(category, unique(category))
  )

ggplot() +
  geom_tile(
    data = loads,
    aes(
      x = variable,
      y = category,
      fill = value
    )
  ) +
  scale_fill_gradient2(
    low = 'firebrick1',
    mid = 'white',
    high = 'dodgerblue1',
    midpoint = 0
  ) +
  labs(
    x = NULL,
    y = NULL,
    fill = 'PCA\nLoading'
  ) +
  theme_minimal(base_size = 20)

clus <- data.frame(sdemos, pca$scores)

cts <- clValid(
  scale(clus), 
  nClust = 2:15,
  clMethods = "hierarchical",
  validation = "stability"
)

sdemos$cluster <- cutree(cts@clusterObjs$hierarchical, k = 5)


# County ------------------------------

# Demographics

# Economics

# Education

# Income



"
Shapefile hierarchy:

Counties
 |-- Voting Districts
 |-- Tracts
      |-- Block Groups
ZCTAs
"

statefp <- 08 # Colorado

cts <- counties(state = statefp)
vtd <- voting_districts(state = statefp)
trx <- tracts(state = statefp)
bgr <- block_groups(state = statefp)
zip <- zctas(state = statefp, year = 2010)

sfh <- lst(cts, vtd, trx, bgr, zip)
sfh <- map(sfh, ~ st_transform(.x, "+proj=longlat +datum=WGS84"))

# El Paso County ------------------------------------------

elp <- filter(sfh[["cts"]], NAME == "El Paso")

elp <- sfh %>%
  map(
    ~ st_intersection(.x, elp),
    .progress = TRUE
  ) %>%
  map(
    ~ st_make_valid(.x) # Fix shapefile oddities e.g. duplicate vertices
  ) %>%
  map(
    ~ mutate(.x, overlap = as.numeric(st_area(.x)))
  ) %>%
  map(
    ~ filter(.x, overlap > 0)
  ) %>%
  map(
    ~ mutate(.x, geometry = st_collection_extract(geometry))
  ) %>%
  map(
    ~ st_make_valid(.x)
  )

m01 <- leaflet() %>%
  addProviderTiles(
    providers$Stamen.Toner
  ) %>%
  addPolygons(
    data = elp[["zip"]],
    color = "grey50",
    weight = 1.5,
    fillOpacity = .3,
    group = "ZCTAs"
  ) %>%
  addPolygons(
    data = elp[["bgr"]],
    color = col_pal[["plate green"]],
    weight = 1.5,
    fillOpacity = .3,
    group = "Block Groups"
  ) %>%
  addPolygons(
    data = elp[["trx"]],
    color = col_pal[["flag red"]],
    weight = 1.5,
    fillOpacity = .3,
    group = "Tracts"
  ) %>%
  addPolygons(
    data = elp[["vtd"]],
    color = col_pal[["rockies purple"]],
    weight = 1.5,
    fillOpacity = .3,
    group = "Voting Districts"
  ) %>%
  addPolygons(
    data = elp[["cts"]],
    color = col_pal[["nuggets yellow"]],
    weight = 1.5,
    fillOpacity = .3,
    group = "Counties"
  ) %>%
  addLayersControl(
    overlayGroups = c(
      "ZCTAs",
      "Block Groups", 
      "Tracts", 
      "Voting Districts", 
      "Counties"
    ),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  hideGroup(
    c("ZCTAs", "Block Groups", "Tracts", "Voting Districts")
  )

mapshot(
  m01, 
  file = "./output/m01-county.png", 
  remove_controls = c("zoomControl")
)

m02 <- m01 %>% 
  hideGroup(
    "Counties"
  ) %>% 
  showGroup(
    "Tracts"
  )
mapshot(
  m02, 
  file = "./output/m02-tracts.png", 
  remove_controls = c("zoomControl")
)

m03 <- m01 %>% 
  hideGroup(
    "Counties"
  ) %>% 
  showGroup(
    "Block Groups"
  )
mapshot(
  m03, 
  file = "./output/m03-block-groups.png", 
  remove_controls = c("zoomControl")
)

m04 <- m01 %>% 
  hideGroup(
    "Counties"
  ) %>% 
  showGroup(
    "Voting Districts"
  )
mapshot(
  m04, 
  file = "./output/m04-voting-districts.png", 
  remove_controls = c("zoomControl")
)

m05 <- m01 %>% 
  hideGroup(
    "Counties"
  ) %>% 
  showGroup(
    "ZCTAs"
  )
mapshot(
  m05, 
  file = "./output/m05-zips.png", 
  remove_controls = c("zoomControl")
)

# Full Shape Overlaps -------------------------------------

b2v <- sfh[["bgr"]] %>%
  group_by(
    GEOID
  ) %>%
  group_split() %>%
  map(
    # Both block groups and voting districts are subsets within same county
    ~ lst(bgr = .x, vtd = filter(sfh[["vtd"]], COUNTYFP20 %in% .x$COUNTYFP))
  ) %>%
  map(
    # Retain only the voting district ID and geometry
    ~ lst(bgr = .x$bgr, vtd = select(.x$vtd, VTDST20, geometry))
  ) %>%
  map(
    # Retain overlap between block group and voting district geometries
    ~ tryCatch({
      st_intersection(.x$bgr, .x$vtd)
    },
      error = function(e) NA
    ),
    .progress = TRUE
  ) %>%
  keep(
    # Strip out stragglers
    is.data.frame
  ) %>%
  compact() %>%
  map(
    # Standardize mixed geometries
    ~ st_collection_extract(.x)
  ) %>%
  map(
    # Clean up any degenerate edges
    ~ st_make_valid(.x)
  ) %>%
  map(
    ~ mutate(.x, area = as.numeric(st_area(.x)))
  ) %>%
  map(
    # Some overlap is only along boundary lines
    ~ filter(.x, area > 0)
  ) %>%
  map(
    ~ st_collection_extract(.x) 
  )

# Illustrative voting district
zoom <- b2v %>%
  bind_rows() %>%
  filter(
    VTDST20 == "041316"
  )

m06 <- leaflet() %>%
  addProviderTiles(
    providers$Stamen.Toner
  ) %>%
  addPolygons(
    data = filter(sfh[["vtd"]], VTDST20 %in% zoom$VTDST20),
    color = col_pal[["rockies purple"]],
    popup = ~ VTDST20,
    group = "Voting Districts"
  ) %>%
  addPolygons(
    data = filter(sfh[["bgr"]], GEOID %in% zoom$GEOID),
    color = col_pal[["plate green"]],
    popup = ~ GEOID,
    group = "Block Groups"
  ) %>%
    addPolygons(
    data = zoom,
    color = col_pal[["broncos orange"]],
    group = "Overlap"
  ) %>%
  addLayersControl(
    overlayGroups = c(
      "Voting Districts",
      "Block Groups",
      "Overlap"
    ),
    options = layersControlOptions(collapsed = FALSE)
  )

m07 <- m06 %>% 
  hideGroup(
    c("Block Groups", "Overlap")
  )
mapshot(
  m07, 
  file = "./output/m07-example-voting-districts.png", 
  remove_controls = c("zoomControl")
)

m08 <- m06 %>% 
  hideGroup(
    c("Voting Districts", "Overlap")
  )
mapshot(
  m08, 
  file = "./output/m08-example-block-groups.png", 
  remove_controls = c("zoomControl")
)

m09 <- m06 %>% 
  hideGroup(
    c("Block Groups", "Voting Districts")
  )
mapshot(
  m09, 
  file = "./output/m09-example-overlap.png", 
  remove_controls = c("zoomControl")
)

# Income Data ---------------------------------------------

income <- get_acs(
  geography = "block group",
  variables = "B19301_001", # Per capita income prior 12m
  state = "CO",
  geometry = FALSE,
  year = 2020
)

# Map
elpincome <- sfh[["bgr"]] %>%
  filter(
    COUNTYFP %in% "041"
  ) %>%
  left_join(
    income,
    by = "GEOID"
  ) %>%
  mutate(
    median_income = median(estimate, na.rm = TRUE), # Impute median
    estimate = ifelse(is.na(estimate), median_income, estimate)
  )

pal_income <- colorNumeric("Greens", domain = elpincome$estimate)

m10 <- leaflet() %>%
  addProviderTiles(
    providers$Stamen.Toner
  ) %>%
  addPolygons(
    data = elpincome,
    color = ~ pal_income(estimate),
    weight = 1.5
  ) %>%
  addLegend(
    "bottomright",
    data = elpincome,
    pal = pal_income, 
    values = ~ estimate,
    title = "Per Capita Income",
    labFormat = labelFormat(prefix = "$"),
    opacity = 1
  )
mapshot(
  m10, 
  file = "./output/m10-el-paso-income.png"
)

# 2020 Election -------------------------------------------

edata <- osf_retrieve_node("m5qyk")
edata <- osf_ls_files(edata)

e20 <- edata[edata$name == "co_2020", ]
e20 <- osf_retrieve_file(e20$id)
e20 <- osf_download(e20, tempdir(), conflicts = "overwrite")
e20 <- read_sf(e20$local_path)
e20 <- st_zm(e20, drop = T, what = "ZM")
e20 <- st_transform(e20, "+proj=longlat +datum=WGS84")

e20 <- e20 %>%
  select(
    STATEFP:PRECINCT,
    contains("PRE")
  ) %>%
  mutate(
    total_votes = rowSums(pick(where(is.numeric)), na.rm = T),
    pct_gop = G20PRERTRU / total_votes
  ) %>%
  data.frame() %>%
  select(
    VTDST20 = VTDST, # For join
    pct_gop
  )

# Map
elpe20 <- sfh[["vtd"]] %>%
  filter(
    COUNTYFP20 %in% front_range
  ) %>%
  left_join(
    e20,
    by = "VTDST20"
  )

pal_gop <- colorNumeric(c("blue", "white", "red"), domain = elpe20$pct_gop)

m11 <- leaflet() %>%
  addProviderTiles(
    providers$Stamen.Toner
  ) %>%
  addPolygons(
    data = elpe20,
    color = ~ pal_gop(pct_gop),
    weight = 1.5
  ) %>%
  addLegend(
    "bottomright",
    data = elpe20,
    pal = pal_gop, 
    values = ~ pct_gop,
    title = "% GOP President (2020)",
    labFormat = scales::percent,
    opacity = 1
  )

mapshot(
  m11, 
  file = "./output/m11-el-paso-e20.png"
)

# Income/Election Join ------------------------------------

# Highlight Front Range corridor
front_range <- c(
  "001", # Adams
  "005", # Arapahoe
  "014", # Broomfield
  "031", # Denver
  "013", # Boulder
  "035", # Douglas
  "041", # El Paso
  "059", # Jefferson
  "069", # Larimer
  "101", # Pueblo
  "123" # Weld
)

inc_e20 <- b2v %>%
  bind_rows() %>%
  filter(
    COUNTYFP %in% front_range
  ) %>%
  left_join(
    income,
    by = "GEOID"
  ) %>%
  arrange(
    VTDST20
  ) %>%
  mutate(
    pct_area = area / sum(area),
    wincome = round(weighted.mean(estimate, pct_area), 0),
    .by = VTDST20
  ) %>%
  mutate(
    wincome_pct = ecdf(wincome)(wincome),
    .by = COUNTYFP
  ) %>%
  left_join(
    e20,
    by = "VTDST20"
  ) %>%
  relocate(
    geometry,
    .after = last_col()
  )

# Illustrative table
inc_e20 %>%
  data.frame() %>%
  filter(
    VTDST20 %in% zoom$VTDST20
  ) %>%
  mutate(
    estimate = scales::dollar(estimate),
    pct_area = scales::percent(pct_area, accuracy = 1),
    wincome = scales::dollar(wincome),
    pct_gop = scales::percent(pct_gop, accuracy = 1)
  ) %>%
  select(
    STATEFP,
    COUNTYFP,
    VTDST20,
    GEOID,
    NAME,
    area,
    pct_area,
    estimate,
    wincome,
    pct_gop
  ) %>%
  write.csv(
    file = "./output/illustrative-vtd.csv",
    row.names = FALSE
  )

# Income v % GOP by County
ingopcty <- inc_e20 %>%
  data.frame() %>%
  select(
    COUNTYFP,
    VTDST20,
    wincome,
    wincome_pct,
    pct_gop
  ) %>%
  distinct() %>%
  inner_join(
    distinct(sfh[["cts"]], COUNTYFP, NAME),
    by = "COUNTYFP"
  )

p01 <- ggplot() +
  geom_point(
    data = ingopcty,
    aes(
      x = wincome,
      y = pct_gop
    ),
    size = 4
  ) +
  geom_smooth(
    data = ingopcty,
    aes(
      x = wincome,
      y = pct_gop
    ),
    size = 1.5,
    color = "green"
  ) +
  scale_x_continuous(
    labels = scales::dollar_format(scale_cut = scales::cut_short_scale())
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  facet_wrap(
    ~ NAME,
    scales = "free_x"
  ) +
  labs(
    x = "Per Capita Income",
    y = "% GOP"
  ) +
  theme_minimal(
    base_size = 24
  )

ggsave(
  "./output/p01.png",
  width = 22,
  height = 18,
  dpi = 300
)

p02 <- ggplot() +
  geom_point(
    data = ingopcty,
    aes(
      x = wincome_pct,
      y = pct_gop
    ),
    size = 4
  ) +
  geom_smooth(
    data = ingopcty,
    aes(
      x = wincome_pct,
      y = pct_gop
    ),
    size = 1.5,
    color = col_pal[["broncos orange"]]
  ) +
  scale_x_continuous(
    labels = scales::percent_format()
  ) +
  scale_y_continuous(
    labels = scales::percent_format()
  ) +
  facet_wrap(
    ~ NAME,
    scales = "free_x"
  ) +
  labs(
    x = "Intra-County Per Capita Income Percentile",
    y = "% GOP"
  ) +
  theme_minimal(
    base_size = 24
  )

ggsave(
  "./output/p02.png",
  width = 22,
  height = 18,
  dpi = 300
)

# Push geofile to OSF ---------------------------------------------------------

saveRDS(b2v, file = paste0(tempdir(), "/b2v.rds"))

osf_upload(osf_retrieve_node("m5qyk"), paste0(tempdir(), "/b2v.rds"))