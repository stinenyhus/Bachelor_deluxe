combined_data <- function(data, demo){
  demo = demo %>% select(c(ID, Diagnosis, Gender))
  data = data %>% left_join(demo, by = "ID") %>%
    mutate(
      Diagnosis = as.factor(Diagnosis),
      Gender = as.factor(Gender)#changing diagnosis column to factor
    ) 
  return(data)
}