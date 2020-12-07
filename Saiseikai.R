##Saiseikai
getwd() 
setwd("/cloud/project")
#Read datasheets
saiseikai_1 <- read_excel("Saiseikai_1.xlsx",
                  na = c("-", "000"),
                  sheet = "編集用",
                  skip = 0)
saiseikai_1 %>% glimpse()
saiseikai_2 <- read_excel("Saiseikai_2.xlsx",
                          na = c(""),
                          sheet = "目視",
                          skip = 0)
saiseikai_2 %>% glimpse()
colnames(saiseikai_1)
colnames(saiseikai_2)
#Select variables for the main analysis
saiseikai_1 <- saiseikai_1 %>% 
  select("研究用ID",
         "入院日",
         "性別",
         "退院日",
         "転帰",
         "入院時ADL") 
saiseikai_2 <- saiseikai_2 %>% 
  select("研究用ID",
         "年齢",
         "入院年月日",
         "Stage",
         "HOT(有無)",
         "HOT(L)",
         "収縮期血圧",
         "拡張期血圧",
         "脈拍",
         "呼吸数",
         "酸素",
         "意識障害",
         "PaCO2")  
#Rename variables
saiseikai_1 <- saiseikai_1 %>% 
  rename(Id = "研究用ID",
         Adm = "入院日")
saiseikai_2 <- saiseikai_2 %>% 
  rename(Id = "研究用ID",
         Adm = "入院年月日",
         Hot = "HOT(有無)",
         HOT_l = "HOT(L)")
#Connecting dataset
df_s <- left_join(saiseikai_1, saiseikai_2, by=c("Id", "Adm"))
df_s %>% glimpse()
#converting from "character" to "numeric"
df_s <- df_s %>% 
  mutate(Sex = if_else(性別 == "男", 0, 1)) %>%   
  mutate(Prognosis = if_else(転帰 == "死亡", 1, 0)) %>% 
  mutate(Hot = if_else(Hot == "有", 1, 0)) %>% 
  mutate(Ams = if_else(意識障害 == "有", 1, 0)) 
df_s <- df_s %>%
  mutate_at(vars(HOT_l), ~replace_na(., 0))
df_s <- df_s %>% 
  mutate(Stage = case_when(Stage == "Ⅰ" ~ 1,
                           Stage == "Ⅱ" ~ 2,
                           Stage == "Ⅲ" ~ 3,
                           Stage == "Ⅳ" ~ 4))
#Manipulating "time scale"
class(df_s$Adm)
class(df_s$退院日)
df_s <- df_s %>% 
  mutate(Adm = ymd(Adm),
         Ent = ymd(退院日))
class(df_s$Adm)
class(df_s$Ent)
df_s <- df_s %>% 
  mutate(Los = (Ent - Adm) + 1)
df_s %>% glimpse()
#Manipulating "ADL"
df_s <- df_s %>% 
  rename(Adl = "入院時ADL")
df_s$tADL <- sapply(strsplit(df_s$Adl, ''), function(x) sum(as.numeric(x)))
#Rename variables ver2
df_s <- df_s %>% 
  rename(Age = "年齢",
         Sbp = "収縮期血圧",
         Dbp = "拡張期血圧",
         Hr = "脈拍",
         Rr = "呼吸数",
         Oxygen = "酸素") %>%   
  select(Id, 
         Stage, 
         Sbp, 
         Dbp, 
         Hr, 
         Rr, 
         Oxygen, 
         Ams, 
         Hot, 
         HOT_l, 
         PaCO2, 
         Sex, 
         Prognosis, 
         Los, 
         tADL)
df_s %>% glimpse()
#Visualize data
vis_miss(df_s)
miss_var_summary(df_s)
ggplot_shiny(data = df_s)
  