combined_data <- function(data, demo){
  demo = demo
  data = data %>% left_join(demo, by = "ID") %>%
    mutate(
      Diagnosis = as.factor(Diagnosis),
      Gender = as.factor(Gender)#changing diagnosis column to factor
    )  %>%
    select( #we will not use any of the descriptive columns in the analysis and remove them from the dataset
      -c(X1,
         frameTime,
         language,
         country,
         feature_set,
         Age,
         AdosCreativity,
         AdosSocial,
         AdosStereotyped,
         AdosCommunication,
         TIQ,
         PIQ,
         VIQ,
         SRS,
         PPVT,
         ParentalEducation,
         CARS,
         Leiter
      ))
  return(data)
}