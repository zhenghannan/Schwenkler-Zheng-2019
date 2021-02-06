Sys.setenv(JAVA_HOME="C://Program Files/Java/jdk1.8.0_191/") #change this to where you install Java's jdk
options(java.parameters = "-Xmx13312m") #maximum memory you allow R to use for Java
options(java.parameters = "-Xms512m") #minimum memory you allow R to use for Java

library(rJava)
library(XML)
library(NLP)
library(RWeka)
library(readtext)
library(magrittr)
library(stringi)
library(coreNLP)
library(cleanNLP)
library(markdown)
library(dplyr)
library(stringr)
library(knitr)
library(readr)
library(data.table)

############# Step 1 ##############
cnlp_init_corenlp(language = "en", anno_level = 2, mem = "12g") 
#load NLP algorithm, need to run this every time you re-open R for NER

ReutersData = read_csv("FullData.csv",trim_ws = FALSE)
ReutersData = data.frame(ReutersData)
#read in news text data as data frame, in which the first column is always article ids (titles or filenames), 
#the second column is always content in text.

data_2006 <- subset(ReutersData, date<20070000 & date > 20060000)
data_2007 <- subset(ReutersData, date<20080000 & date > 20070000)
data_2008 <- subset(ReutersData, date<20090000 & date > 20080000)
data_2009 <- subset(ReutersData, date<20100000 & date > 20090000)
data_2010 <- subset(ReutersData, date<20110000 & date > 20100000)
data_2011 <- subset(ReutersData, date<20120000 & date > 20110000)
data_2012 <- subset(ReutersData, date<20130000 & date > 20120000)
data_2013 <- subset(ReutersData, date<20140000 & date > 20130000)
#you want to divide the whole news dataset into pieces to avoid computer breaking down

annotated_2006 = cnlp_annotate(data_2006[,3:4],as_string=TRUE,backend = "coreNLP")
annotated_2007 = cnlp_annotate(data_2007[,3:4],as_string=TRUE,backend = "coreNLP")
annotated_2008 = cnlp_annotate(data_2008[,3:4],as_string=TRUE,backend = "coreNLP")
annotated_2009 = cnlp_annotate(data_2009[,3:4],as_string=TRUE,backend = "coreNLP")
annotated_2010 = cnlp_annotate(data_2010[,3:4],as_string=TRUE,backend = "coreNLP")
annotated_2011 = cnlp_annotate(data_2011[,3:4],as_string=TRUE,backend = "coreNLP")
annotated_2012 = cnlp_annotate(data_2012[,3:4],as_string=TRUE,backend = "coreNLP")
annotated_2013 = cnlp_annotate(data_2013[,3:4],as_string=TRUE,backend = "coreNLP")
#annotate text; you may also want to store workspace at this point for future use

entity_2006 = annotated_2006$entity
entity_2007 = annotated_2007$entity
entity_2008 = annotated_2008$entity
entity_2009 = annotated_2009$entity
entity_2010 = annotated_2010$entity
entity_2011 = annotated_2011$entity
entity_2012 = annotated_2012$entity
entity_2013 = annotated_2013$entity
entity_full = rbind(entity_2006,entity_2007,entity_2008,entity_2009,entity_2010,entity_2011,entity_2012,entity_2013)
rm(list=setdiff(ls(), "entity_full"))
#extract entities and delete other variables to save working space; we only need entities here







############# Step 2 ##############
cnlp_init_corenlp(language = "en", anno_level = 0, mem = "4g")
#reload NLP algorithm; "anno_level = 0" means the NLP algorithm only tokenize strings, 
#which is what we need in tokenizing entity results to clean them
entity_full = filter(entity_full,entity_type == "ORGANIZATION") #we only care about "Organization" here
entity_full = data.table(entity_full)
entity_full$year = as.numeric(str_sub(entity_full$id,-8,-5))
entity_full$row_order = c(1:length(entity_full$id))

org = read_csv("OrgList.csv")
keywords = read_csv("keywords.csv",col_names = FALSE)
#these two are useful when filtering out non-firm organizations later

#start Cleaning
data_full = c()
for (y in c(2006:2013)){
  #for efficiency, we want to just deal with unique entity results (weighted by frequency)
  data = filter(entity_full,year==y) %>%
    group_by(entity) %>%
    summarise (weight = n())
  data = data[order(data$weight,decreasing = TRUE),]
  #filter entity names that only show 5 times
  data = filter(data,weight>=5)
  data = data.table(data)
  data[,entity_normalized := entity]
  data$entity_normalized = gsub("-"," ",data$entity_normalized)
  #remove special symbol '.'
  data <- data[,entity_normalized := gsub('\\.com','',entity_normalized)]
  data <- data[,entity_normalized := gsub('\\.Com','',entity_normalized)]
  data <- data[,entity_normalized := gsub('\\.COM','',entity_normalized)]
  data <- data[,entity_normalized := gsub("[^[:alnum:][:blank:]&']",'',entity_normalized)]
  #remove "& Co" "corp" "corporation" as suffixes
  data <- data[,entity_normalized := paste0(entity_normalized,"_")]
  data <- data[,entity_normalized := gsub(' & co_','',entity_normalized)]
  data <- data[,entity_normalized := gsub(' & Co_','',entity_normalized)]
  data <- data[,entity_normalized := gsub(' & CO_','',entity_normalized)]
  data <- data[,entity_normalized := gsub(' corporation_','',entity_normalized)]
  data <- data[,entity_normalized := gsub(' Corporation_','',entity_normalized)]
  data <- data[,entity_normalized := gsub(' CORPORATION_','',entity_normalized)]
  data <- data[,entity_normalized := gsub('_','',entity_normalized)]
  
  #tokenize entity names
  data[, entity_id := c(1:length(entity)) ]
  data_token = cnlp_annotate(data[,c("entity_id","entity_normalized")],as_string=TRUE)$token
  data_token$word = tolower(data_token$word)
  #remove the tokens of business types
  suffix = c("co","co.","inc","inc.","ag","ag.","ltd","ltd.","lp","lp.","llc","llc.",
             "pllc","pllc.","llp","llp.","plc","plc.","ltd/plc","ltd/plc.","corp","corp.",
             "ab","ab.","cos","cos.","cia","cia.","sa","sa.","reuters","reuter","based","rrb")
  data_token = filter(data_token, !(word %in% suffix))
  #remove other irrelevant noises
  data_token = filter(data_token, upos != "." & upos !="" )
  #now paste the cleaned tokens as names accordingly
  data_token <- data.table(data_token)
  data_token_tmp <- data_token[, c("id","word")]
  tmp <- data_token_tmp[, lapply(.SD, paste, collapse = "_"), by = id]
  tmp[,word := paste0("_",word,"_") ]
  setnames(tmp, "id","id_str")
  setkey(tmp, "id_str")
  
  data[, id_str := as.character(entity_id)]
  setkey(data, "id_str")
  
  data <- data[tmp]
  data <- data[order(entity_id)]
  data_token = c()
  data_token_tmp = c()
  tmp = c()
  
  data[, entity_normalized := word]
  data[, c("id_str", "word") := NULL]
  
  #remove all the entities referring to non-firm organizations using keywords
  for (i in 1:length(keywords$X1)){
    data = data[!grepl(keywords$X1[i],entity_normalized),]
  }
  
  # MANUAL ADJUSTMENT #
  data$entity_normalized = gsub("_&_","&",data$entity_normalized)
  data$entity_normalized = gsub("&","_&_",data$entity_normalized)
  data$entity_normalized = gsub("_'s_","s_",data$entity_normalized)
  data$entity_normalized[which(data$entity_normalized=="_united_")] = "_united_airlines_"
  data$entity_normalized[which(data$entity_normalized=="_continental_")] = "_united_continental_"
  data$entity_normalized[which(data$entity_normalized=="_delta_")] = "_delta_airlines_"
  data$entity_normalized[which(data$entity_normalized=="_southwest_")] = "_southwest_airlines_"
  data$entity_normalized[which(data$entity_normalized=="_northwest_")] = "_northwest_airlines_"
  data$entity_normalized[which(data$entity_normalized=="_t-mobile_usa_")] = "_t-mobile_"
  data$entity_normalized[which(data$entity_normalized=="_l_3_")] = "_l3_"
  
  #merge entities that refer to the same company; again, we want to just deal with unique entity names
  data_unique = data %>%
    group_by(entity_normalized) %>%
    summarise (n = sum(weight))
  data_unique = data_unique[order(data_unique$n,decreasing = TRUE),]
  data_unique$stem = NA
  data_unique = filter(data_unique,entity_normalized!="_ab_")
  data_unique = filter(data_unique,entity_normalized!="_agriculture_")
  data_unique = filter(data_unique,entity_normalized!="_army_")
  data_unique = filter(data_unique,entity_normalized!="_bank_")
  data_unique = filter(data_unique,entity_normalized!="_board_")
  data_unique = filter(data_unique,entity_normalized!="_company_")
  data_unique = filter(data_unique,entity_normalized!="_energy_")
  data_unique = filter(data_unique,entity_normalized!="_estimate_")
  data_unique = filter(data_unique,entity_normalized!="_europe_")
  data_unique = filter(data_unique,entity_normalized!="_forest_")
  data_unique = filter(data_unique,entity_normalized!="_globe_")
  data_unique = filter(data_unique,entity_normalized!="_group_")
  data_unique = filter(data_unique,entity_normalized!="_healthcare_")
  data_unique = filter(data_unique,entity_normalized!="_lincoln_")
  data_unique = filter(data_unique,entity_normalized!="_jaguar_")
  data_unique = filter(data_unique,entity_normalized!="_navy_")
  data_unique = filter(data_unique,entity_normalized!="_office_")
  data_unique = filter(data_unique,entity_normalized!="_press_")
  data_unique = filter(data_unique,entity_normalized!="_research_")
  data_unique = filter(data_unique,entity_normalized!="_system_")
  data_unique = filter(data_unique,entity_normalized!="_transportation_")
  data_unique = filter(data_unique,entity_normalized!="_treasuries_")
  data_unique = filter(data_unique,entity_normalized!="_tv_")
  data_unique = filter(data_unique,entity_normalized!="_united_states_")
  data_unique = filter(data_unique,entity_normalized!="_usa_")
  
  #when a entity shows more than 5 times, we believe it has potential to be the 'stem' name of a company,
  #then we put it into 'stem candidate"
  for (i in 1:length(data_unique$entity_normalized)){
    if (data_unique[i,]$n>=5){
      data_unique[i,]$stem = data_unique[i,]$entity_normalized
    }
  }
  
  non_na_id = which(!is.na(data_unique$stem))
  na_id = which(is.na(data_unique$stem))
  
  #merge within the 'stem candidate'; remember they're ranked by frequency
  for (i in non_na_id[-length(non_na_id)]){
    for (j in (i+1):non_na_id[length(non_na_id)]){
      if(str_detect(data_unique$stem[i],data_unique$stem[j])|
         str_detect(data_unique$stem[j],data_unique$stem[i])){
        data_unique$stem[j]=data_unique$stem[i]
      }
    }
  }
  
  #merge within the 'stem candidate' and non 'stem candidate'
  data_unique = data.table(data_unique)
  temp <- data_unique[, .(n=sum(n)), by=list(stem)]
  temp <- temp[!is.na(stem)]
  temp = temp[order(temp$n,decreasing = TRUE),]
  
  for (i in na_id){
    for (j in 1:length(temp$stem)){
      if(str_detect(data_unique$entity_normalized[i],temp$stem[j])|
         str_detect(temp$stem[j],data_unique$entity_normalized[i])){
        data_unique$stem[i] = temp$stem[j]
        break
      }
    }
  }
  
  #now, for a long enough non candidate that is not be merged to any candidate, we tend to believe it could be
  #a candidate for some companies that didn't show much in news
  na_id = which(is.na(data_unique$stem))
  for (i in na_id){
    if (str_count(data_unique$entity_normalized[i],'_')>2){
      data_unique$stem[i]=data_unique$entity_normalized[i]
    }
  }
  
  #delete all unmerged non candidate
  data_unique = data_unique[complete.cases(data_unique), ]
  data_unique$n = NULL
  data_unique$stem_ab = data_unique$stem

  # MANUAL ADJUSTMENT #
  data_unique[which(data_unique$stem=='_committee_'),c("stem","stem_ab")] = '_fed_'
  data_unique[which(data_unique$stem=='_ixic_'),c("stem","stem_ab")] = '_nasdaq_'
  data_unique[which(data_unique$stem=='_lehman_'),c("stem","stem_ab")] = '_lehman_brothers_'
  data_unique[which(data_unique$stem=='_bell_'),c("stem","stem_ab")] = '_bell_canada_'
  data_unique[which(data_unique$stem=='_eads_'),c("stem","stem_ab")] = '_airbus_'
  data_unique[which(data_unique$stem=='_wal_mart_'),c("stem","stem_ab")] = '_walmart_'
  data_unique[which(data_unique$stem=='_jp_morgan_'),c("stem","stem_ab")] = '_jpmorgan_'
  data_unique[which(data_unique$stem=='_google_'),c("stem","stem_ab")] = '_googl_'
  data_unique[which(data_unique$stem=='_tnk-bp_'),c("stem","stem_ab")] = '_bp_'
  data_unique[which(data_unique$stem=='_citi_'),c("stem","stem_ab")] = '_citigroup_'
  data_unique[which(data_unique$stem=='_goldman_'),c("stem","stem_ab")] = '_goldman_sachs_'
  data_unique[which(data_unique$stem=='_exxon_'),c("stem","stem_ab")] = '_exxon_mobil_'
  data_unique[which(data_unique$stem=='_cerberus_capital_management_'),c("stem","stem_ab")] = '_cerberus_'
  data_unique[which(data_unique$stem=='_cerberus_capital_'),c("stem","stem_ab")] = '_cerberus_'
  data_unique[grepl('_royal_bank_of_canada_',data_unique$entity_normalized),c("stem","stem_ab")] = '_royal_bank_of_canada_'
  data_unique[which(data_unique$stem=='_us_steel_'),c("stem","stem_ab")] = '_united_states_steel_'
  data_unique[which(data_unique$stem=='_berkshire_'),c("stem","stem_ab")] = '_berkshire_hathaway_'
  
  #unify abbreviations and full names
  ab_transfer = function(string){
    temp = gsub("_general_motors_","_gm_",string)
    temp = gsub("_new_york_stock_exchange_","_nyse_",temp)
    temp = gsub("_federal_reserve_","_fed_",temp)
    temp = gsub("_united_auto_workers_","_uaw_",temp)
    temp = gsub("_american_international_group_","_aig_",temp)
    temp = gsub("_us_securities_and_exchange_commission_","_sec_",temp)
    temp = gsub("_standard_&_poor_","_s_&_p_",temp)
    temp = gsub("_general_electric_","_ge_",temp)
    temp = gsub("_volkswagen_","_vw_",temp)
    temp = gsub("_international_monetary_fund_","_imf_",temp)
    temp = gsub("_european_central_bank_","_ecb_",temp)
    temp = gsub("_european_union_","_eu_",temp)
    temp = gsub("_world_trade_organization_","_wto_",temp)
    temp = gsub("_bank_of_japan_","_boj_",temp)
    temp = gsub("_societe_generale_","_socgen_",temp)
    temp = gsub("_societe_general_","_socgen_",temp)
    temp = gsub("_royal_bank_of_scotland_","_rbs_",temp)
    temp = gsub("_european_stability_mechanism_","_esm_",temp)
    temp = gsub("_hewlett_packard_","_hp_",temp)
    temp = gsub("_federal_deposit_insurance_","_fdic_",temp)
    temp = gsub("_automatic_data_unique_processing_","_adp_",temp)
    temp = gsub("_london_stock_exchange_","_lse_",temp)
    temp = gsub("_institute_supply_management_","_ism_",temp)
    temp = gsub("_japan_airlines_","_jal_",temp)
    temp = gsub("_united_airlines_","_ual_",temp)
    temp = gsub("_swiss_national_bank_","_snb_",temp)
    temp = gsub("_european_financial_stability_facility_","_efsf_",temp)
    temp = gsub("_european_financial_stability_fund_","_efsf_",temp)
    temp = gsub("_international_business_machines_","_ibm_",temp)
    temp = gsub("_british_airways_","_ba_",temp)
    temp = gsub("_organization_petroleum_exporting_countries_","_opec_",temp)
    temp = gsub("_procter_&_gamble_","_p_&_g_",temp)
    temp = gsub("_bank_of_america_","_bofa_",temp)
    temp = gsub("_federal_open_market_committee_","_fomc_",temp)
    temp = gsub("_premier_automotive_group_","_pag_",temp)
    temp = gsub("_royal_bank_of_canada_","_rbc_",temp)
    return(temp)
  }
  ad_transfer = function(string){
    temp = gsub("_gm_","_general_motors_",string)
    temp = gsub("_s_&_p_","_standard_&_poor_",temp)
    temp = gsub("_aig_","_american_international_group_",temp)
    temp = gsub("_ge_","_general_electric_",temp)
    temp = gsub("_vw_","_volkswagen_",temp)
    temp = gsub("_socgen_","_societe_generale_",temp)
    temp = gsub("_rbs_","_royal_bank_of_scotland_",temp)
    #temp = gsub("_hp_","_hewlett_packard_",temp)
    temp = gsub("_hp_","_hp_",temp)
    temp = gsub("_adp_","_automatic_data_unique_processing_",temp)
    temp = gsub("_ism_","_institute_supply_management_",temp)
    temp = gsub("_jal_","_japan_airlines_",temp)
    temp = gsub("_ual_","_united_airlines_",temp)
    temp = gsub("_ibm_","_international_business_machines_",temp)
    temp = gsub("_ba_","_british_airways_",temp)
    temp = gsub("_p_&_g_","_procter_&_gamble_",temp)
    temp = gsub("_bofa_","_bank_of_america_",temp)
    temp = gsub("_pag_","_premier_automotive_group_",temp)
    temp = gsub("_rbc_","_royal_bank_of_canada_",temp)
    temp = gsub("_boj_","_bank_of_japan_",temp)
    return(temp)
  }
  #and filter out non-firms using organization list
  data_unique = filter(data_unique, !(stem %in% org$stem_ab))
  data_unique$stem_ab = ab_transfer(data_unique$stem)
  data_unique = filter(data_unique, !(stem_ab %in% org$stem_ab))
  data_unique$stem = ad_transfer(data_unique$stem_ab)
  
  #now merge our unique entity clean results ('stem') into unique entity names we have before
  data_unique = data.table(data_unique)
  data = data.table(data)
  setkey(data_unique,"entity_normalized")
  setkey(data,"entity_normalized")
  data <- data[data_unique]
  data$year = y
  data_full = rbind(data_full,data)
  data_unique = data %>%
    group_by(stem,stem_ab) %>%
    summarise (weight = sum(weight))
  data_unique = data_unique[order(data_unique$weight,decreasing = TRUE),]
  path = paste0("Reuters_UniqueStem_",y,".csv")
  write_csv(data_unique,path = path)
  data = c()
}

#now mrege our unique entity names back into our full entity data
data_full = data.table(data_full)
setkey(data_full,"year","entity")
setkey(entity_full,"year","entity")
final = entity_full[data_full]
final = final[order(final$row_order),]
final$row_order = NULL
final$entity_id = NULL
final$weight = NULL
write_csv(final,"Reuters_Full_Entity_Result.csv")
