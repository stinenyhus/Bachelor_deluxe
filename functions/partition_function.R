##Danish partitioning
partition_dk <- function(demo, features, hold_size = 0.2, seed = 2021){
  demo = demo
  features = features
  set.seed(seed)
  n = round((nrow(demo) * hold_size)/2/2,0)
  features$ID <- as.numeric(str_extract(features$ID, "[0-9]+"))
  male_asd <- demo %>% 
    filter(Gender == "Male") %>% filter(Diagnosis == "ASD") %>% sample_n(n)
  male_td <- demo %>% 
    filter(Gender == "Male") %>% filter(Diagnosis == "TD") %>% sample_n(n)
  female_asd <- demo %>% 
    filter(Gender == "Female") %>% filter(Diagnosis == "ASD") %>% sample_n(n)
  female_td <- demo %>% 
    filter(Gender == "Female") %>% filter(Diagnosis == "TD") %>% sample_n(n)
  hold_out_1 <- rbind(male_asd,male_td, female_asd, female_td)
  hold_out_2 <- features[features$ID %in% hold_out_1$ID,]
  train <- features[!(features$ID %in% hold_out_1$ID),]
  return(list(hold_out_2,train))
}
#

##US partitioning 
partition_us <- function(demo, features, hold_size = 0.2, seed = 2021){
  demo = demo
  features = features
  set.seed(seed)
  total_n = round(nrow(demo) * hold_size,0)
  female_hold <- demo %>% filter(Gender == "Female")
  male_n = total_n - nrow(female_hold)
  male_asd <- demo %>% 
    filter(Gender == "Male") %>% filter(Diagnosis == "ASD") %>% sample_n((male_n/2))
  male_td <- demo %>% 
    filter(Gender == "Male") %>% filter(Diagnosis == "TD") %>% sample_n((male_n/2))
  hold_out_1 <- rbind(female_hold, male_asd, male_td)
  hold_out_2 <- features[features$ID %in% hold_out_1$ID,]
  train <- features[!(features$ID %in% hold_out_1$ID),]
  return(list(hold_out_2,train))
}
#











