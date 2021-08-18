library(pacman)
p_load(ggplot2, tidyverse, wesanderson, reshape2, RColorBrewer)

prepare_file <- function(filename){
  file = read_csv(paste(getwd(), "/class_reports/", filename, sep = ""))
  colnames(file)[1] <- "Metric"
  colnames(file)[5] <- "Macro_avg"
  colnames(file)[6] <- "Weighted_avg"
  file$ID <- rep(paste(regmatches(filename, gregexpr("[a-z]*", filename))[[1]][1],
                       regmatches(filename, gregexpr("[a-z]*", filename))[[1]][3],
                       regmatches(filename, gregexpr("[a-z]*", filename))[[1]][9],
                       regmatches(filename, gregexpr("[a-z]*", filename))[[1]][11],
                       sep = " "),4)
  
  return(file)
}


file_list <- list.files(path = "~/Uni/Bachelor/bachelor_2_0/Bachelor_deluxe/class_reports", 
                    pattern = "*.csv",
                    full.names = F)

files <- file_list %>% map_df(prepare_file)

#Train and tested on self
files$ID <- ifelse(files$ID == "dk triangles dk triangles", "M1a", files$ID)
files$ID <- ifelse(files$ID == "dk stories dk stories", "M1b", files$ID)
files$ID <- ifelse(files$ID == "us stories us stories", "M1c", files$ID)

#Cross-task
files$ID <- ifelse(files$ID == "dk stories dk triangles", "M2b", files$ID)
files$ID <- ifelse(files$ID == "dk triangles dk stories", "M2a", files$ID)

#Cross-language
files$ID <- ifelse(files$ID == "us stories dk stories", "M3b", files$ID)
files$ID <- ifelse(files$ID == "dk stories us stories", "M3a", files$ID)

# M1a 100 (50/50) 
# M1b 18 (10/8)
# M1c 60 (32/28)
# M2a 97 (50/47)
# M2b 513 (272/241)
# M3a 309 (187/122)
# M3b 97 (50/47)


pre_recall <- function(dataframe, title){
  #### Preparing dataframe ####
  data = dataframe %>% 
    filter(Metric == "precision" | Metric == "recall") %>% 
    select(-c(accuracy, Macro_avg, Weighted_avg)) %>% 
    melt(id.vars = c("Metric", "ID"), variable.name = "Diagnosis")

  data$total_n <- 0
  data$total_n <- ifelse(data$ID == "M1a", 100, data$total_n)
  data$total_n <- ifelse(data$ID == "M1b", 18, data$total_n)
  data$total_n <- ifelse(data$ID == "M1c", 60, data$total_n)
  data$total_n <- ifelse(data$ID == "M2a", 97, data$total_n)
  data$total_n <- ifelse(data$ID == "M2b", 513, data$total_n)
  data$total_n <- ifelse(data$ID == "M3a", 309, data$total_n)
  data$total_n <- ifelse(data$ID == "M3b", 97, data$total_n)
  data$conf = sqrt(data$value*(1-data$value)/data$total_n)
  
  #### Doing the plot ####
    plot = data %>%
      ggplot(aes(Metric,value, color = Diagnosis))+
      geom_point(size = 3)+
      geom_errorbar(aes(ymin = value - conf, ymax = value + conf), width = 0.3)+
      labs(title = title, y = "Value")+
      scale_color_manual(values= c("#238B45", "#2171B5"))+
      theme_light()+
      facet_wrap(~ID, nrow= 2)+
      theme(text=element_text(family="serif", size = 20),
            strip.text.x = element_text(size = 15),
            axis.text.x = element_text(size = 15, angle = 10))
  return(plot)
}

pre_recall(files, title = "Precision and recall for all seven models")

