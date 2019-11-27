install.packages("data.table")
install.packages("g.data")

# data.table processes data faster than data.frame
library(data.table)
library(stringr)
library(dplyr)
library(g.data)
library(sqldf)

# comparison between data frame load and data table load 
# system.time(df <- read.csv("FacebookAds_012018_052018.csv"))
system.time(dt <- fread("FacebookAds_Raw.csv"))

dt <- fread("FacebookAds_Raw.csv")


# check # col and rows
dim(dt)


# Clean rows with empty or NA campaing name
dt <- dt[!(is.na(dt$`CampaignName`) | dt$`CampaignName`==''),]

dt_order <- dt[order(`CampaignName`)]

-----------------------------------------------#understand data#---------------------------------------------------------
#total number of missing and null values
sum(is.na(dt))
sum(is.null(dt))
# na by columns 
colSums(is.na(dt))

# replace NA in Cost per unique click wiht 0

#option 1
dt <-dt[is.na(`CostPerUniqueLinkClick`), `CostPerUniqueLinkClick` := 0]
dt <-dt[is.na(`PostShares`), `PostShares` :=0]
dt <-dt[is.na(`Reach`), `Reach` :=0]
dt <-dt[is.na(`AmountSpent`), `AmountSpent` :=0]
dt <-dt[is.na(`PostReactions`), `PostReactions` :=0]
dt <-dt[is.na(`UniqueCTR_all`), `UniqueCTR_all` :=0]
dt <-dt[is.na(`UniqueLinkClicks`), `UniqueLinkClicks` :=0]

colSums(is.na(dt))


dt$AdNameSubject <- str_extract(dt$AdName, "Male|Female|Group|Couple|Feet|Hands|Food|Still|Doctor")
dt$AdNameDescription <- str_extract(dt$AdName, "With Condition|Medical|Illustration|Photoshop|Nature|Home|Technology|Active")
dt$AdNameEmotion <- str_extract(dt$AdName, "Happy|Relaxed|Support|Sad|Serious|Pain|Tired|Pensive|Neutral|Active")


dt$CampaignNameCleaned <- str_extract(dt$CampaignName,"Activase|Adcetris|Alphanate|Aubagio|Benra|Bilinta|Contrave|CTCA|Dermira|Dupixent|Elagolix|
                                                    Fasenra|Fasenra|Gilenya|Gocorvi|Harvoni|Humira - PSA|Humira AS|Humira CD|Humira PS|Humira RA|
                                                    Humira RA|Humira UC|Imfinzi|Jardiance|Kisqali|Latrtuvo|Latuda|Mavyret|Mydayis|Ninlaro|Nucala|
                                                    Nuedexta|Ocrelizumab|Ofev|Orencia RA|Otezla|Promacta|Ribociclib|Rothman|Ruconest|Soliqua Havas|
                                                    Spinraza|Taltz|Tecfidera|Tez|Toujeo|Toujeo|Trintellix|Truvada|Tymlos|Tysabri|UPMC Adult|
                                                    UPMC Pediatric|Vagisil|Verzenio|Victoza|Walgreens|Xiidra|Xtandi|Benra|Humira Pso|Nuedexta|Brilinta|
                                                    Humira - PsA|Dexcom|PsA- Humira|Shire|Spinraza|UPMC Pediatric|Humira - PsA|Spiriva|Vyavnse")
# clean up duplicate names for the same campagin

dt$CampaignNameCleaned = sub("Humira - PsA", "Humira PSA", dt$CampaignNameCleaned)
dt$CampaignNameCleaned = sub("Humira PS", "Humira PSO", dt$CampaignNameCleaned)
dt$CampaignNameCleaned = sub("Humira Pso", "Humira PSO", dt$CampaignNameCleaned)
dt$CampaignNameCleaned = sub("PsA- Humira", "Humira PSA", dt$CampaignNameCleaned)
dt$CampaignNameCleaned = sub("Orencia RA", "Orencia", dt$CampaignNameCleaned)
dt$CampaignNameCleaned = sub("Benra", "Fasenra", dt$CampaignNameCleaned)
dt$CampaignNameCleaned = sub("Ocrelizumab", "Ocrevus", dt$CampaignNameCleaned)
dt$CampaignNameCleaned = sub("Ribociclib", "Kisqali", dt$CampaignNameCleaned)
dt$CampaignNameCleaned = sub("Humira PSOA", "Humira PSA", dt$CampaignNameCleaned)




dt$CampaignYear <- str_extract(dt$CampaignName,"2017|2018")


# clean conditions

dt$Condition <- str_extract(dt$CampaignName, "Diabetes|Ankylosing Spondylitis|Crohns Disease|Psoriasis|Bipolar Disorder|
                            Multiple Myeloma|PBA|MS|Stroke|Hodgkin Lymphoma|Hemophilia|Heart Attack|Weight Loss|
                            Atopic Dermatitis|Endometriosis|Asthma|Parkinson's|Psoriatic Arthritis|
                            Rheumatoid Arthritis|Ulcerative Colitis|NSCLC|Lunage Cancer|T2D|Breast Cancer|
                            Hep C|Multiple Myeloma|IPF|ITP|HAE|HIV|Dry Eye|NSCLC|Lunage Cancer|T2D|Breast Cancer|Hep C|
                            IPF|ITP|HAE|Cystic Fibrosis|Prostate Cancer|T1D|PsA|SMA|Hepatitis C|ADHD-|Atopic Dermatitis|
                            Rheumatoid Arthritis|BC|Osteoperosis")

# Fix typos in Condition 
dt$Condition = sub("ADHD-", "ADHD", dt$Condition)
dt$Condition = sub("Hep C", "Hepatitis C", dt$Condition)
dt$Condition = sub("Lunage Cancer", "Lung Cancer", dt$Condition)
dt$Condition = sub("BC", "Breast Cancer", dt$Condition)


# convert NA to None
dt$Condition[is.na(dt$Condition)] <- "None"

# Pull condition from elsewhere
dt$Condition <- ifelse(dt$CampaignName == "2017 Aubagio - Switcher" & dt$Condition == "None" , "MS", dt$Condition)
dt$Condition <- ifelse(dt$CampaignName == "2017 Latuda - Bipolar Disorder" & dt$Condition == "None" , "Bipolar Disorder", dt$Condition)

dt$Condition <- ifelse(dt$CampaignName == "2018 CCRM - Brand Pages"  & dt$Condition == "None", "Infertility", dt$Condition)
dt$Condition <- ifelse(dt$CampaignName == "UPMC Adults"  & dt$Condition == "None", "Hospital", dt$Condition)
dt$Condition <- ifelse(dt$CampaignName == "UPMC Pediatric"  & dt$Condition == "None", "Hospital", dt$Condition)
dt$Condition <- ifelse(dt$CampaignName == "UPMC Pediatric"  & dt$Condition == "None", "Hospital", dt$Condition)
dt$Condition <- ifelse(dt$CampaignName == "CTCA Brand Page"  & dt$Condition == "None", "Prostate Cancer", dt$Condition)
dt$Condition <- ifelse(dt$CampaignName == "2018 Ofev Branded - IPF"  & dt$Condition == "None", "IPF", dt$Condition)
dt$Condition <- ifelse(dt$CampaignName == "Ofev 2018"  & dt$Condition == "None", "IPF", dt$Condition)
dt$Condition <- ifelse(dt$CampaignName == "2018 Walgreens Oncology - Videos"  & dt$Condition == "None", "Oncology", dt$Condition)
dt$Condition <- ifelse(dt$CampaignName == "2018 Walgreens Oncology Brand Pages"  & dt$Condition == "None", "Oncology", dt$Condition)
dt$Condition <- ifelse(dt$CampaignName == "2018 Humira RA - Rheumatoid Arthritis"  & dt$Condition == "None", "Rheumatoid Arthritis", dt$Condition)
dt$Condition <- ifelse(dt$CampaignName == "2018 Humira RA - Rheumatoid Arthritis - Test"  & dt$Condition == "None", "Rheumatoid Arthritis", dt$Condition)
dt$Condition <- ifelse(dt$CampaignName == "2018 Patisiran - Amyloidosis"  & dt$Condition == "None", "Amyloidosis", dt$Condition)
dt$Condition <- ifelse(dt$CampaignName == "2018 Spiriva - COPD"  & dt$Condition == "None", "COPD ", dt$Condition)
dt$Condition <- ifelse(dt$CampaignName == "2018 Vyvanse Branded"  & dt$Condition == "None", "ADHD", dt$Condition)
dt$Condition <- ifelse(dt$CampaignName == "2018 Mydayis - ADHD CPM"  & dt$Condition == "None", "ADHD", dt$Condition)
dt$Condition <- ifelse(dt$CampaignName == "2018 Trintellix - Depression"  & dt$Condition == "None", "Depression", dt$Condition)
dt$Condition <- ifelse(dt$CampaignName == "Orencia RA - CPM"  & dt$Condition == "None", "Rheumatoid Arthritis", dt$Condition)
dt$Condition <- ifelse(dt$CampaignName == "Otezla"  & dt$Condition == "None", "Psoriasis", dt$Condition)




# delete rows with not needed campaign information 

dt<-dt[!(dt$CampaignName=="2018 Imfinzi Reach Audience Extension" ),]
dt<-dt[!(dt$CampaignName=="Newsletter Lead Gen Campaign Test" ),]
dt<-dt[!(dt$CampaignName=="Well Tested Test Vidoe Views" ),]
dt<-dt[!(dt$CampaignName=="2018 HCA CareNow - Austin" ),]
dt<-dt[!(dt$CampaignName=="2018 HCA CareNow - Dallas - Forth Worth" ),]
dt<-dt[!(dt$CampaignName=="2018 HCA CareNow - Houston" ),]
dt<-dt[!(dt$CampaignName=="2018 HCA CareNow - Kansas City" ),]
dt<-dt[!(dt$CampaignName=="2018 HCA CareNow - Las Vegas" ),]
dt<-dt[!(dt$CampaignName=="2018 HCA CareNow - Nashville" ),]
dt<-dt[!(dt$CampaignName=="2018 HCA CareNow - Northern Virginia" ),]
dt<-dt[!(dt$CampaignName=="2018 HCA CareNow - San Jose" ),]
dt<-dt[!(dt$CampaignName=="2018 HCA CareNow - Southern California" ),]
dt<-dt[!(dt$CampaignName=="2018 HCA CareNow - Treasure Coast" ),]
dt<-dt[!(dt$CampaignName=="MMAP:863P:CCRM (facebook) Posts Link Clicks" ),]



# Audience (retargeting, interest lookalike)

dt$Audience <- str_extract(dt$AdSetName, "Lookalike|lookalike|LAL|Interest|Int|INT|Ret|Retargeting|Retargetting|retargeting|ret")
#fix namings
dt$Audience = sub("LAL", "Lookalike", dt$Audience)
dt$Audience = sub("Int", "Interest", dt$Audience)
dt$Audience = sub("Ret", "Retargeting", dt$Audience)
dt$Audience = sub("Retargetting", "Retargeting", dt$Audience)
dt$Audience = sub("ret", "Retargeting", dt$Audience)
dt$Audience = sub("Interesterest", "Interest", dt$Audience)
dt$Audience = sub("Retargetingargeting", "Interest", dt$Audience)
dt$Audience = sub("INT", "Interest", dt$Audience)
dt$Audience = sub("lookalike", "Lookalike", dt$Audience)
dt$Audience[is.na(dt$Condition)] <- "Retargeting"




# Audience type
dt$AudienceType <- str_extract(dt$AdSetName, "Eng|FBF|NewsL")
dt$AudienceType = sub("Eng", "Engagement", dt$AudienceType)
dt$AudienceType = sub("NewsL", "Newsletters", dt$AudienceType)
dt$AudienceType = sub("FBF", "Facebook followers", dt$AudienceType)







# AdType
dt$AdType <- str_extract(dt$AdSetName, "Vid|Car|Carousel|Dyn|Dynamic") 

dt$AdType = sub("Car", "Carousel", dt$AdType)
dt$AdType = sub("Vid", "Video", dt$AdType)
dt$AdType = sub("Dyn", "Dynamic", dt$AdType)

dt$AdType[is.na(dt$AdType)] <- "Standard"



write.csv(dt,"FacebookAdCleaned_v8.csv")




 #check for distinctive /uniue 
sqldf("select distinct (AdSetName) from dt")
sqldf("select distinct (AdType) from dt")
sqldf("select distinct (Audience) from dt")

check <- sqldf("select distinct (Condition) from dt")


  



