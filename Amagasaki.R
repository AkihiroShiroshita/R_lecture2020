##Amagasaki
getwd() 
setwd("/cloud/project")
#Read data sheets
amagasaki_1 <- read_excel("Amagasaki_2.xlsx",
                          na = c(""),
                          sheet = "Š³ÒƒŠƒXƒg_DPC",
                          skip = 4)

colnames(amagasaki_1)
#Cleaning of datasheet 2
amagasaki_1 <- amagasaki_1 %>% 
  select("Œ¤‹†ID",
         "”N—î",
         "«•Ê",
         "“ü‰@“ú",
         "‘Ş‰@“ú",
         "...10",
         "“ü‰@\r\nJCS",
         "“ü‰@ADL") %>% 
  rename(Id = "Œ¤‹†ID",
         Age = "”N—î",
         Sex = "«•Ê",
         Adm = "“ü‰@“ú",
         Ent = "‘Ş‰@“ú",
         Prognosis = "...10",
         Ams = "“ü‰@\r\nJCS",
         Adl = "“ü‰@ADL")
amagasaki_1$tADL <- sapply(strsplit(amagasaki_1$Adl, ''), function(x) sum(as.numeric(x)))
#Cleaning of datasheet 3
amagasaki_2 <- read_excel("Amagasaki_2.xlsx",
                          na = c(""),
                          sheet = "“ü‰@ƒoƒCƒ^ƒ‹ƒTƒCƒ“",
                          skip = 2)

colnames(amagasaki_2)
amagasaki_2 <- amagasaki_2 %>% 
  rename(Id = "Œ¤‹†ID",
         Adm = "“ü‰@“ú",
         Care = "ƒPƒA–¼Ì",
         Value = "Œ‹‰Ê’l")
amagasaki_2 <- amagasaki_2 %>% 
  arrange(Id, Adm, Care) %>% 
  group_by(Id, Adm, Care) %>% 
  filter(row_number()==1)
amagasaki_2 <- amagasaki_2 %>% 
  select(Id, Adm, Care, Value)  
amagasaki_2 <- amagasaki_2 %>% 
  pivot_wider(names_from = "Care", values_from = "Value")
amagasaki_2 %>% glimpse()
colnames(amagasaki_2)
amagasaki_2 <- amagasaki_2 %>% 
  rename(Sbp = "ûkŠúŒŒˆ³(mmHg)",
         Dbp = "Šg’£ŠúŒŒˆ³(mmHg)",
         Hr = "–¬””(‰ñ/•ª)",
         Rr = "ŒÄ‹z”(‰ñ/•ª)") %>% 
  select(Sbp, Dbp, Hr, Rr)
#Cleaning of datasheet 4
amagasaki_3 <- read_excel("Amagasaki_2.xlsx",
                          na = c(""),
                          sheet = "ŒŸ¸",
                          skip = 2)

colnames(amagasaki_3)
amagasaki_3 <- amagasaki_3 %>% 
  rename(Id = "Œ¤‹†ID",
         Adm = "“ü‰@“ú",
         Lab = "ŒŸ¸€–Ú–¼",
         Value = "Œ‹‰Ê’l")
amagasaki_3 <- amagasaki_3 %>% 
  arrange(Id, Adm, Lab) %>% 
  group_by(Id, Adm, Lab) %>% 
  filter(row_number()==1)
amagasaki_3 <- amagasaki_3 %>% 
  select(Id, Adm, Lab, Value)  
amagasaki_3 <- amagasaki_3 %>% 
  pivot_wider(names_from = "Lab", values_from = "Value")
amagasaki_3 %>% glimpse()
colnames(amagasaki_3)
#Cleaning of datasheet 5
amagasaki_4 <- read_excel("Amagasaki_2.xlsx",
                          na = c(""),
                          sheet = "–òÜ",
                          skip = 2)
amagasaki_4 %>% glimpse()
colnames(amagasaki_4)
amagasaki_4 <- amagasaki_4 %>% 
  rename(Id = "Œ¤‹†ID",
         Adm = "“ü‰@“ú",
         Do = "À{“ú",
         Drug = "–òÜ–¼")
amagasaki_4 <- amagasaki_4 %>% 
  mutate(Adm = ymd(Adm),
         Do = ymd(Do))
amagasaki_4 <- amagasaki_4 %>% 
  arrange(Id, Adm, Drug, Do) %>% 
  filter(Adm == Do||Adm == Do + 1) %>% 
  group_by(Id, Adm, Drug) %>% 
  filter(row_number()==1)
amagasaki_4 <- amagasaki_4 %>% 
  select(Id, Adm, Drug) 
unique(amagasaki_4$Drug)
amagasaki_4 <- amagasaki_4 %>% 
  pivot_wider(names_from = "Drug", values_from = "Drug")
amagasaki_4 <- amagasaki_4 %>%
  mutate_all(.funs = ~ if_else(is.na(.), "0", "1")) %>% 
  mutate_all(.funs = ~ as.numeric(.))
amagasaki_4 %>% glimpse()
colnames(amagasaki_4)
amagasaki_4$narrow <- if_else(amagasaki_4[3] == 1|
                                amagasaki_4[9] == 1|
                                amagasaki_4[10] ==1|
                                amagasaki_4[16] ==1|
                                amagasaki_4[17] ==1|
                                amagasaki_4[19] ==1|
                                amagasaki_4[21] ==1, 1, 0)
amagasaki_4$broad <- if_else(amagasaki_4[6] == 1|
                                amagasaki_4[8] == 1|
                                amagasaki_4[12] ==1|
                                amagasaki_4[20] ==1|
                                amagasaki_4[24] ==1|
                                amagasaki_4[27] ==1, 1, 0)
amagasaki_4$steroid <- if_else(amagasaki_4[4] == 1|
                                amagasaki_4[5] == 1|
                                amagasaki_4[7] ==1|
                                amagasaki_4[11] ==1|
                                amagasaki_4[13] ==1|
                                amagasaki_4[14] ==1|
                                amagasaki_4[15] ==1|
                                amagasaki_4[18] ==1|
                                amagasaki_4[22] ==1|
                                amagasaki_4[23] ==1|
                                amagasaki_4[25] ==1|
                                amagasaki_4[26] ==1, 1, 0)
amagasaki_4 <- amagasaki_4 %>% 
  select(Id, Adm, narrow, broad, steroid)

  
