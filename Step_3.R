Sys.setenv(JAVA_HOME="C://Program Files/Java/jdk1.8.0_191/")
options(java.parameters = "-Xmx13312m")
options(java.parameters = "-Xms512m")

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
library(RecordLinkage)

cnlp_init_corenlp(language = "en", anno_level = 0, mem = "4g")
samp_lastyear = data.frame("stem"=NA) #store merge result of last year
samp_lastyear[1,1] = "99999"
final = c()
base_full = read_csv('compustat_crsp_monthly.csv')
#you can read in any dataset that contains company names (more companies the better);
#here we use Compustat/CRSP merged stock price data

for (y in c(2006:2013)){
  variable = c('GVKEY','LPERMNO','LPERMCO','tic','cusip','conml','fic','naics','sic')
  base = filter(base_full,substr(datadate,1,4)==y)[,variable]
  base = base[complete.cases(base),]
  base = filter(base,naics!=525990) 
  base = filter(base,naics!=525910) #these two sectors are for funds
  base = data.table(base)
  setkey(base,"GVKEY")
  base = unique(base)
  
  #Now read in our unique stem database
  path = paste0("Reuters_UniqueStem_",y,".csv")
  samp = read_csv(path)[,c("stem","stem_ab")]
  samp = data.table(samp)
  samp$stem_ab = gsub("_","",samp$stem_ab)
  for (i in variable){
    samp[,i] = NA
  }

  #Now clean official names in our fashion
  base$name_normalized = tolower(base$conml)
  base$ticker_normalized = tolower(base$tic)
  base = base[,name_normalized := gsub('\\.com','',name_normalized)]
  base = base[,name_normalized := gsub('\\.','',name_normalized)]
  base = base[,name_normalized := gsub('\\*','',name_normalized)]
  base <- base[,name_normalized := paste0(name_normalized,"_")]
  base <- base[,name_normalized := gsub(' & co_','',name_normalized)]
  base <- base[,name_normalized := gsub(' corporation_','',name_normalized)]
  base <- base[,name_normalized := gsub('_','',name_normalized)]
  base <- base[,name_normalized := gsub('-',' ',name_normalized)]
  base <- base[,name_normalized := gsub("'",'',name_normalized)]
  
  base[, name_id := c(1:length(conml)) ]
  base_token = cnlp_annotate(base[,c("name_id","name_normalized")],as_string=TRUE)$token
  #Remove the tokens of business types
  suffix = c("co","co.","inc","inc.","ag","ag.","ltd","ltd.","lp","lp.","llc","llc.",
             "pllc","pllc.","llp","llp.","plc","plc.","ltd/plc","ltd/plc.","corp","corp.",
             "ab","ab.","cos","cos.","cia","cia.","sa","sa.","reuters","reuter","based")
  base_token = filter(base_token, !(word %in% suffix))
  #Remove other irrelevant noises
  base_token = filter(base_token, upos != "." & upos !="")
  #Now paste the cleaned tokens as names accordingly
  base_token <- data.table(base_token)
  base_token_tmp <- base_token[, c("id","word")]
  tmp <- base_token_tmp[, lapply(.SD, paste, collapse = "_"), by = id]
  tmp[,word := paste0("_",word,"_") ]
  setnames(tmp, "id","id_str")
  setkey(tmp, "id_str")
  
  base[, id_str := as.character(name_id)]
  setkey(base, "id_str")
  
  base <- base[tmp]
  base <- base[order(name_id)]
  
  base[, name_normalized := word]
  base[, c("id_str", "word") := NULL]
  
  base$ticker_normalized = lapply(base$ticker_normalized,sub,pattern='\\..*',replacement='')
  base$ticker_normalized = unlist(base$ticker_normalized, use.names=FALSE)
  
  #Uniform the standard conversion of some special symbols
  base$name_normalized = gsub("_&_","&",base$name_normalized)
  base$name_normalized = gsub("&","_&_",base$name_normalized)
  
  #Now merge..
  base$stem = NA
  base$name_normalized_2 = gsub("_","",base$name_normalized)
  base$name_normalized_2 = gsub("-","",base$name_normalized_2)
  samp$stem_normalized = gsub("_","",samp$stem)
  samp$stem_normalized = gsub("-","",samp$stem_normalized)
  base_naid = which(is.na(base$stem))
  samp_naid = which(is.na(samp$conml))
  samp = data.frame(samp)
  base = data.frame(base)
  
  #First round, merge the pasted stems that are equal the pasted official names
  for (i in samp_naid){
    for (j in base_naid){
      if (base$name_normalized_2[j]==samp$stem_normalized[i]){
        samp[i,variable] = base[j,variable]
        base$stem[j] = samp$stem[i]
        break
      }
    }
  }
  
  #Second round, merge the stems that are: 1.longer than 4 letthers 2. contained in the official names
  #when multiple matched, choose the most similar one
  for (i in samp_naid){
    l1 = 0
    l2 = 0
    for (j in base_naid){
      if (str_detect(base$name_normalized[j],samp$stem[i])&&str_length(samp$stem[i])>=5){
        l2 = levenshteinSim(base$name_normalized[j],samp$stem[i])
        if (l2>l1){
          samp[i,variable] = base[j,variable]
          base$stem[j] = samp$stem[i]
          l1 = l2
        }
      }
    }
  }
  
  #Third round, merge the stems_ab that are equal to the ticker_normalized
  samp_naid = which(is.na(samp$conml))
  for (i in samp_naid){
    for (j in base_naid){
      if (!is.na(base$ticker_normalized[j])){
        if (base$ticker_normalized[j]==samp$stem_ab[i]){
          samp[i,variable] = base[j,variable]
          base$stem[j] = samp$stem[i]
          break
        }
      }
    }
  }
  
  #Forth round, merge the pasted stems (longer than 5 letters) that are contained in the pasted official names or vice versa
  #when multiple matched, choose the most similar one
  samp_naid = which(is.na(samp$conml))
  for (i in samp_naid){
    l1 = 0
    l2 = 0
    for (j in base_naid){
      if (str_length(samp$stem_normalized[i])>=6 & str_length(base$name_normalized_2[j])>=6){
        if (str_detect(base$name_normalized_2[j],samp$stem_normalized[i])|
            str_detect(samp$stem_normalized[i],base$name_normalized_2[j])){
          l2 = levenshteinSim(base$name_normalized[j],samp$stem[i])
          if (l2>l1){
            samp[i,variable] = base[j,variable]
            base$stem[j] = samp$stem[i]
            l1 = l2 
          }
        }
      }
    }
  }
  
  #Fifth round, if there're unmatched entities that are used to be matched in previous year,
  #we use the last available matched results
  samp_naid = which(is.na(samp$conml))
  for (i in samp_naid){
    for (j in 1:dim(samp_lastyear)[1]){
      if (samp_lastyear$stem[j]==samp$stem[i]){
        samp[i,variable] = samp_lastyear[j,variable]
        break
      }
    }
  }

  samp$stem_normalized = NULL
  samp$stem_temp = NULL
  samp$year = y
  final = rbind(final,samp)
  samp_lastyear = samp[complete.cases(samp),]
}

write_csv(final,"Reuters_MatchList.csv",na = "") #we can manually double check samples of the matching results in this step
final = read_csv("Reuters_MatchList.csv") #after checking we reload matching results

#in the following codes, we merge our (unique) matching results into full entity data
Entity = read_csv("Reuters_Full_Entity_Result.csv")
Entity$row = c(1:dim(Entity)[1])
Entity = data.table(Entity)
final = data.table(final)
setkey(Entity,"stem","year")
setkey(final,"stem","year")
Entity = final[Entity]
Entity = Entity[order(Entity$row),]
Entity$i.stem_ab = NULL
Entity = filter(Entity,!is.na(conml))
Entity$stem_id = NULL
write_csv(Entity,"Reuters_Matched_Entity_Result.csv")