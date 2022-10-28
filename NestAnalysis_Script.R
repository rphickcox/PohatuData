library(dplyr)
library(lubridate)

nest <- read.csv("C:/Users/rphic/Desktop/Penguins PhD/Pohatu/Monitoring/Nest checks backup.csv")

nest2 <- nest %>%
  mutate(DateTime2 = dmy(DateTime),
         Eggs = as.numeric(Eggs), 
         Chicks = as.numeric(Chicks)) %>%
  arrange(Nest.ID, DateTime2) %>%
  group_by(Nest.ID) %>%
  summarise(nvisits = n(), 
            first = min(DateTime2, na.rm = TRUE), 
            last = max(DateTime2, na.rm = TRUE), 
            eggs = max(Eggs, na.rm = TRUE), 
            chicks = max(Chicks, na.rm = TRUE), 
            chick1 = paste(Chick.1.status, collapse = " "), 
            chick2 = paste(Chick.2.status, collapse = " "), 
            status = paste(Status, collapse = " ")) %>%
  mutate(eggs = ifelse(chicks == 2, 2, eggs)) %>%
filter(chicks != -Inf)            
            
nest3 <- nest %>%
  filter(Status == "Breeding") %>%
  mutate(DateTime2 = dmy(DateTime),
         Eggs = as.numeric(Eggs), 
         Chicks = as.numeric(Chicks)) %>%
  arrange(Nest.ID, DateTime2) %>%
  group_by(Nest.ID) %>%
  summarise(nvisits = n(), 
            first = min(DateTime2, na.rm = TRUE), 
            last = max(DateTime2, na.rm = TRUE), 
            eggs = max(Eggs, na.rm = TRUE), 
            chicks = max(Chicks, na.rm = TRUE), 
            chick1 = paste(Chick.1.status, collapse = " "), 
            chick2 = paste(Chick.2.status, collapse = " "), 
            status = paste(Status, collapse = " ")) %>%
  mutate(eggs = ifelse(chicks == 2, 2, eggs)) %>%
  filter(chicks != -Inf)            

