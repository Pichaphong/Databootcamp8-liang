##student behavior
## update thesefile

library(tidyverse)
library(patchwork)
library(readxl)
library(readr)

##view many column
rstudioapi::writeRStudioPreference("data_viewer_max_columns", 1000L)

##import
stu <- read_excel("8203 Behavior.xlsx",1)
str(stu)
View(stu)

#aggreagrate
stu <- stu %>%
  filter(COURSE_CODE %in% c("AN8203-V07" , "AN8203-V08" ,"AN8203-V09"),
         LEARNING_PROGRESS >= 0)

View(stu)

##นับรวมโรงเรียน
count_stu_by_school <- stu %>% 
  group_by(SALE_SCHOOL)%>%
  summarise(number=n()) %>%
  arrange(desc(number)) %>%
  head(20)
View(count_stu_by_school)


count_stu <- stu %>% 
  
  group_by(SALE_SCHOOL) %>%
  summarise(number=n()) %>% 
  arrange(desc(number))
View(count_stu)





##แยกตาม รร.ตาม Version 
stu_num_ver <- stu %>%
  inner_join(count_stu_by_school) %>%
  group_by(SALE_SCHOOL,COURSE_CODE) %>%
  summarise(number=n())
View(stu_num_ver)


write.csv(stu_num_ver ,"school8203.csv",row.names = FALSE) 

stu_num_eng <- read_csv("school8203_EN.csv")

##graph of 20 school revenue
ggplot(stu_num_eng, aes(x = COURSE_CODE , y = number , fill = COURSE_CODE )) + 
  geom_col() + 
  facet_wrap(~SALE_SCHOOL)+
  theme_minimal()+
  theme(strip.text = element_text(face = "bold"))+
  geom_text(aes(label = number,fontface="bold"), vjust = 1.5, colour = "white")+
  labs(title = "Number of student",
       subtitle = "Top20 compare with Course code",
       x = "",
       y = "Learning Progress",
  )


