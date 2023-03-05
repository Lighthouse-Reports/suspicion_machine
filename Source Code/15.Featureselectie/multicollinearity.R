#****************************************************************************************************
# DESCRIPTION 
# Gebruik multicollinearity feature selection o.b.v. VIF
#
# FILE HISTORY
# 22-02-2017, Medewerker5; initiele versie.
# 02-07-2018, Medewerker4; migratie naar WOB server
# 03-08-2020, Medewerker2; filter op type aangepast naar nieuwe situatie (controleset zit nu ook in traintest)
# 19-08-2020, Medewerker4, op het einde update feature profiles toegevoegd
#****************************************************************************************************

#--------------------------------------------------------------------------------------------------
# init
#--------------------------------------------------------------------------------------------------
library(feather)
library(tidyverse)
library(stringr)
library(fmsb)


source("D:/DATA/SHARED/Analytics_Uitkeringsfraude/1.Code/10.Functies/init.R")

abt <- read_feather(paste0(abtfolder, "abt_basis_", dtlaad , "_", label, ".feather"))

#*******************************************************************************************
# vif_func() definiëren
#
# Credits:
# https://beckmw.wordpress.com/2013/02/05/collinearity-and-stepwise-vif-selection
#*******************************************************************************************


vif_func<-function(in_frame,thresh=10,trace=T,...){
  
  #require(fmsb)
  
  time0 <- Sys.time()
  
  if(class(in_frame) != 'data.frame') in_frame<-data.frame(in_frame)
  
  #get initial vif value for all comparisons of variables
  vif_init<-NULL
  var_names <- names(in_frame)
  for(val in var_names){
    regressors <- var_names[-which(var_names == val)]
    form <- paste(regressors, collapse = '+')
    form_in <- formula(paste(val, '~', form))
    vif_init<-rbind(vif_init, c(val, VIF(lm(form_in, data = in_frame, ...))))
  }
  vif_max<-max(as.numeric(vif_init[,2]), na.rm = TRUE)
  
  if(vif_max < thresh){
    if(trace==T){ #print output of each iteration
      prmatrix(vif_init,collab=c('var','vif'),rowlab=rep('',nrow(vif_init)),quote=F)
      cat('\n')
      cat(paste('All variables have VIF < ', thresh,', max VIF ',round(vif_max,2), sep=''),'\n\n')
    }
    return(var_names)
  }
  else{
    
    in_dat<-in_frame
    
    #backwards selection of explanatory variables, stops when all VIF values are below 'thresh'
    while(vif_max >= thresh){
      
      vif_vals<-NULL
      var_names <- names(in_dat)
      
      for(val in var_names){
        regressors <- var_names[-which(var_names == val)]
        form <- paste(regressors, collapse = '+')
        form_in <- formula(paste(val, '~', form))
        vif_add<-VIF(lm(form_in, data = in_dat, ...))
        vif_vals<-rbind(vif_vals,c(val,vif_add))
      }
      max_row<-which(vif_vals[,2] == max(as.numeric(vif_vals[,2]), na.rm = TRUE))[1]
      
      vif_max<-as.numeric(vif_vals[max_row,2])
      
      if(vif_max<thresh) break
      
      if(trace==T){ #print output of each iteration
        prmatrix(vif_vals,collab=c('var','vif'),rowlab=rep('',nrow(vif_vals)),quote=F)
        cat('\n')
        cat('removed: ',vif_vals[max_row,1],vif_max,'\n\n')
        cat('Duration of this step: ', difftime(Sys.time(), time0, units = "mins"), ' minutes \n\n')
        time0 <- Sys.time()
        flush.console()
      }
      
      in_dat<-in_dat[,!names(in_dat) %in% vif_vals[max_row,1]]
      
    }
    
    return(names(in_dat))
    
  }
  
}

#*******************************************************************************************
# vif_func() uitvoeren
# alleen op gelabelde data
#*******************************************************************************************

abt_vif <- abt %>%
  filter(type %in% c("metlabel_traintest"))  #RZ 03-08-2020; "metlabel_validatie" verwijderd.

# features na nzv
nzv_features_to_keep <- read.csv(paste0(featureselectiesfolder,"no_nzv_", dtlaad, "_", label,  ".csv"))
abt_vif <- abt_vif[, nzv_features_to_keep$features]

# verwijder character variabelen en melding_om (niet mogelijk voor vif)
features_no_nzv_char <- names(abt_vif[, sapply(abt_vif, is.character)])
abt_vif <- abt_vif %>% dplyr::select(-one_of(c(features_no_nzv_char, "melding_om")))

# verwijder hoge correlatie - scheelt veel tijd
del_corr <- abt_vif %>% 
  cor() %>%
  caret::findCorrelation(cutoff = 0.85)
abt_vif <- abt_vif[ , -del_corr]

# verwijder multicollineaire features
t0 <- Sys.time()
features_no_mc <- vif_func(abt_vif, thresh = 10, trace = TRUE)
t1 <- Sys.time()
t1 - t0

# bepaal de features die we uiteindelijk willen behouden
nzv_mc_features_to_keep <- data.frame(features = names(abt)) %>%
  filter(features %in% unique(c(features_permanent, features_no_nzv_char, features_no_mc)))

#--------------------------------------------------------------------------------------------------
# output
#--------------------------------------------------------------------------------------------------
write_feather(nzv_mc_features_to_keep, 
              paste0(featureselectiesfolder,"no_nzv_no_mc_", dtlaad, "_", label, ".feather"))
write.csv(nzv_mc_features_to_keep, 
          paste0(featureselectiesfolder,"no_nzv_no_mc_", dtlaad, "_", label, ".csv"), row.names = FALSE)


#--------------------------------------------------------------------------------------------------
# Profile van alle features bijwerken
#--------------------------------------------------------------------------------------------------
update_feature_profiles(abt, stap = "mc", input = nzv_mc_features_to_keep)

# Maken rapportage. LET OP: Deze overschrijft het rapport dat in abt.R gemaakt is (het is een update)
# NB: dit duurt even (10-15 min). Met feat_range kun je eventueel een beperktere set features aangeven.
rmarkdown::render(paste0(rapportfolder, "controlerapport_features.Rmd"), 
                  params=list(feat_range = c(5:length(abt))),  # eerste 4 niet (id, type, persoon_id, target_per)
                  output_dir = paste0(rapportfolder, label, "/"),
                  output_file = paste0(rapportfolder, label, "/controlerapport_features.html"))

