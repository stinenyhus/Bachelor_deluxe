id_wrangling <- function(feature, demo, other, language){
  if (language == "dk"){
      demo <- demo %>% filter(language == "dk")
      
      feature <- feature %>% 
        mutate(ID = as.character(str_extract(ID, "[0-9]*")))
      
      demo <- demo %>% subset(ID != "103"& ID != "104"& ID !="105"& ID !="111"& ID !="114")
    
      #Clean other data
      other <- other %>% 
        mutate(
          ID = as.character(ID),  #Making ID character
          ID_letter = str_extract(ID, "[A-Z]+"), #extracting letters from ID name
          ID_number = str_extract(ID, "[0-9]*")  #extracting numbers from ID name
        )
      other$ID_letter <- ifelse(is.na(other$ID_letter), "", 
                                          other$ID_letter)
      other <- other %>% 
        unite(ID, ID_letter:ID_number, sep = "") 
  }
  
  if (language == "us"){
    demo <- demo %>% filter(language == "us")
    feature <- feature %>% 
      mutate(
        ID = as.character(ID),  #Making ID character
        ID_letter = str_extract(ID, "[A-Z]+"), #extracting letters from ID name
        ID_number = str_extract(ID, "[0-9]*"))  #extracting numbers from ID name
    
    feature$ID_letter <- ifelse(is.na(feature$ID_letter), "", feature$ID_letter)
    
    feature <- feature %>% 
      unite(ID, ID_letter:ID_number, sep = "") 
    
    #Clean other
    other <- other %>% 
      mutate(ID = as.character(str_extract(ID, "[0-9]*")))
    
    demo <- demo %>% subset(ID != "103"& ID != "104"& ID !="105"& ID !="111"& ID !="114")
  }
  feature <- feature %>% select(-c(name, trial, frameTime, X1))
  other <- other %>% select(-c(name, trial, frameTime, X1))
  other$ID <- as.character(other$ID)
  return(list(feature, other, demo))
}