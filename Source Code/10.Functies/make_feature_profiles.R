#--------------------------------------------------------------------------------------------------
# BESCHRIJVING 
# Profiel maken voor traintest en scoring
#
# WIJZIGINGSHISTORIE
# 24-08-2016, Medewerker3, initiele versie (make_dd.R)
# 20-07-2020, Medewerker4, aangepast voor toepassing na featurebuilding
# 19-08-2020, Medewerker4, uitgebreid met resultaat van featureselecties
# 01-10-2020, Medewerker4, uitgebreid met histogram en density plot (maar nog inactief)
#--------------------------------------------------------------------------------------------------

update_feature_profiles <- function(abt, stap, input = NULL) {
  
  if (stap == "init") {
    
    feature_profiles <- tibble(feature = colnames(abt),
                               data_type = sapply(abt, typeof),
                               data_class = sapply(abt, class),
                               keep_nzv = "",  # wordt na featureselectie gevuld
                               keep_mc = "", # wordt na featureselectie gevuld
                               n_traintest = sapply(abt %>% filter(type == "metlabel_traintest"), function (x) sum(!is.na(x))),
                               missing_traintest = sapply(abt %>% filter(type == "metlabel_traintest"), function (x) sum(is.na(x))),
                               unique_traintest = sapply(abt %>% filter(type == "metlabel_traintest"), function (x) length(unique(x))),
                               min_traintest = sapply(abt %>% filter(type == "metlabel_traintest"), function (x) ifelse(class(x) %in% c("numeric", "integer", "Date"), min(x, na.rm = TRUE), NA)),
                               max_traintest = sapply(abt %>% filter(type == "metlabel_traintest"), function (x) ifelse(class(x) %in% c("numeric", "integer", "Date"), max(x, na.rm = TRUE), NA)),
                               mean_traintest = sapply(abt %>% filter(type == "metlabel_traintest"), function (x) ifelse(class(x) %in% c("numeric", "integer", "Date"), mean(x, na.rm = TRUE), NA)),
                               n_scoring = sapply(abt %>% filter(type == "scoring"), function (x) sum(!is.na(x))),
                               missing_scoring = sapply(abt %>% filter(type == "scoring"), function (x) sum(is.na(x))),
                               unique_scoring = sapply(abt %>% filter(type == "scoring"), function (x) length(unique(x))),
                               min_scoring = sapply(abt %>% filter(type == "scoring"), function (x) ifelse(class(x) %in% c("numeric", "integer", "Date"), min(x, na.rm = TRUE), NA)),
                               max_scoring = sapply(abt %>% filter(type == "scoring"), function (x) ifelse(class(x) %in% c("numeric", "integer", "Date"), max(x, na.rm = TRUE), NA)),
                               mean_scoring = sapply(abt %>% filter(type == "scoring"), function (x) ifelse(class(x) %in% c("numeric", "integer", "Date"), mean(x, na.rm = TRUE), NA)),
                               n_controle = sapply(abt %>% filter(type == "metlabel_controle"), function (x) sum(!is.na(x))),
                               missing_controle = sapply(abt %>% filter(type == "metlabel_controle"), function (x) sum(is.na(x))),
                               unique_controle = sapply(abt %>% filter(type == "metlabel_controle"), function (x) length(unique(x))),
                               min_controle = sapply(abt %>% filter(type == "metlabel_controle"), function (x) ifelse(class(x) %in% c("numeric", "integer", "Date"), min(x, na.rm = TRUE), NA)),
                               max_controle = sapply(abt %>% filter(type == "metlabel_controle"), function (x) ifelse(class(x) %in% c("numeric", "integer", "Date"), max(x, na.rm = TRUE), NA)),
                               mean_controle = sapply(abt %>% filter(type == "metlabel_controle"), function (x) ifelse(class(x) %in% c("numeric", "integer", "Date"), mean(x, na.rm = TRUE), NA))
                               # plotjes nog niet actief
                               # het zijn list objecten en die kunnen niet als feather opgeslagen worden 
                               #plot_histogram = ifelse(data_type != "character", map(feature, ~ ggplot(data = abt, aes_string(x = .x)) + geom_histogram(aes(fill = type), position = "dodge")), NA),
                               #plot_density = ifelse(data_type != "character", map(feature, ~ ggplot(data = abt, aes_string(x = .x)) + geom_density(aes(fill = type), alpha = 0.5)), NA)
                               )

    if (!dir.exists(paste0(rapportfolder, label))) {dir.create(paste0(rapportfolder, label))}    
    feature_profiles %>% write_feather(paste0(rapportfolder, label, "/", "abt_feature_profiles.feather"))
    
    cat("Feature profiles gemaakt")
    
    # Opnemen in global environment
    feature_profiles <<- feature_profiles
    
  } else if (stap == "nzv") {
    
    input <- input %>% rename(feature = features) %>%  mutate(keep = 1L)
    
    feature_profiles <- read_feather(paste0(rapportfolder, label, "/", "abt_feature_profiles.feather")) %>% 
      left_join(input, by = "feature") %>% 
      mutate(keep_nzv = if_else(is.na(keep), "drop_nzv", "KEEP_NZV")) %>% 
      select(-keep)
    
    feature_profiles %>% write_feather(paste0(rapportfolder, label, "/", "abt_feature_profiles.feather"))
    
    print("Keep/drop near zero variance aan feature profiles toegevoegd")
    
    # Opnemen in global environment
    feature_profiles <<- feature_profiles
    
  } else if (stap == "mc") {

    input <- input %>% rename(feature = features) %>%  mutate(keep = 1L)
    
    feature_profiles <- read_feather(paste0(rapportfolder, label, "/", "abt_feature_profiles.feather")) %>% 
      left_join(input, by = "feature") %>% 
      mutate(keep_mc = if_else(is.na(keep), if_else(keep_nzv == "drop_nzv", "", "drop_mc"), "KEEP_MC")) %>%   # drop_mc kan alleen bij keep_nzv
      select(-keep)

    feature_profiles %>% write_feather(paste0(rapportfolder, label, "/", "abt_feature_profiles.feather"))
    
    print("Keep/drop multicollineariteit aan feature profiles toegevoegd")
    
    # Opnemen in global environment
    feature_profiles <<- feature_profiles
    
  } else {
    
    print("Geen bewerkingen uitgevoerd")
    
  }
}

