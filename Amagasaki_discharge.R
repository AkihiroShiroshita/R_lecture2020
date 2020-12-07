#Cleaning discharge summary
#ICU User Guide(http://userguide.icu-project.org/strings/regexp)
#Not using the csv file
amagasaki_1 <- read_excel("Amagasaki_1.xlsx")
amagasaki_1 <- amagasaki_1 %>% 
  rename(Id = 研究ID,
         Number = 入院番号,
         Part = サマリー区分,
         Contents = 内容)
amagasaki_1 %>% glimpse()
#Detecting HOT users
which(str_detect(amagasaki_1$Contents, "HOT"))
which(str_detect(amagasaki_1$Contents, "在宅酸素"))
amagasaki_1 <- amagasaki_1 %>% 
  mutate(Hot = if_else((str_detect(amagasaki_1$Contents, "HOT")|
                         str_detect(amagasaki_1$Contents, "在宅酸素"))&(Part==1|Part==2|Part==3|Part==4|Part==5), 1, 0))
#Detecting COPD Stage
which(str_detect(amagasaki_1$Contents, "Stage"))
which(str_detect(amagasaki_1$Contents, "ステージ"))
which(str_detect(amagasaki_1$Contents, "期"))
amagasaki_1 <- amagasaki_1 %>% 
  mutate(Stage = str_extract(amagasaki_1$Contents, ".期")) 

