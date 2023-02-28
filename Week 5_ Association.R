setwd("~/asdos aed/minggu ke 5")


# library
# library for association measurement =================================
if(!require(rcompanion)){install.packages("rcompanion")}

if(!require(vcd)){install.packages("vcd")}
if(!require(psych)){install.packages("psych")}

if(!require(DescTools)){install.packages("DescTools")}
if(!require(epitools)){install.packages("epitools")}
if(!require(coin)){install.packages("coin")}
if(!require(polycor)){install.packages("polycor")}

# library to make contingency table =================================
if(!require(reshape2)){install.packages("reshape2")}
if(!require(dplyr)){install.packages("dplyr")}
if(!require(maditr)){install.packages("maditr")}
if(!require(tidyverse)){install.packages("tidyverse")}


packages = c('rcompanion','polycor','coin', 'vcd', 'psych','tidyverse', 'DescTools', 'epitools', 'reshape2', 'dplyr', 'maditr')
lapply(packages, library, character.only = TRUE)

df=read.csv("StudentsPerformance.csv",header=TRUE,sep=",",check.names = FALSE)
sum(is.na(df))
#df=na.omit(df)
#reading data =================================
df=read.csv("StudentsPerformance.csv",header = TRUE,check.names = FALSE)
View(df)
str(df)
#cleaning data =================================
unique(df$`parental level of education`)
df_rename <- df %>%
  mutate(`parental level of education` = recode(`parental level of education`, 
                         `bachelor's degree` = 'bachelor', 
                         `some college` = "college",
                         `master's degree`= "master",
                         `associate's degree`="associate",
                         `high school`="high school",
                         `some high school`="high school"
                          ))
df_rename$`parental level of education` <- factor(df_rename$`parental level of education`,
                     order=TRUE,
                     levels=c("high school","college","associate",'bachelor',"master"))
unique(df$`test preparation course`)
#summary(df_rename)

df_clean <- within(df_rename, {
  `parental level of education` <- factor(df_rename$`parental level of education`,
                                          order=TRUE,
                                          levels=c("high school","college","associate",'bachelor',"master"))
  gender <- factor(gender)
  `race/ethnicity` <- factor(`race/ethnicity`)
  lunch <- factor(lunch)
  `test preparation course` <- factor(`test preparation course`,
                                      order=TRUE,
                                      levels = c("none","completed"))
})
head(df_clean)

summary(df_clean)
#metode nominal data dikotomus =================================
df_dikotomus <- df_clean %>%
                            filter(`race/ethnicity`%in% c('group C','group D'))%>%
                            group_by(gender,`race/ethnicity`) %>%
                            summarise(jumlah = n())
df_dikotomus                          
#
df_dikotomus_1 = dcast(df_dikotomus ,gender~`race/ethnicity`, value.var='jumlah')
df_dikotomus_1
data_matrix = data.matrix(df_dikotomus_1[ ,2:3])
data_matrix 



data_tabulasi= df_clean %>%
                           filter(`race/ethnicity`%in% c('group C','group D'))
#data_tabulasi= df_clean[df_clean$`race/ethnicity`=="group C" |df_clean$`race/ethnicity`=="group D",]
data_xtabs = xtabs(~data_tabulasi$gender+factor(data_tabulasi$`race/ethnicity`,levels = c("group C","group D")))
data_xtabs 

# dari library psych
phi(data_xtabs, digits = 4)

# dari library psych
phi(data_matrix, digits = 4)


cramerV(data_matrix, digits = 4)

ggplot(df_dikotomus, aes(fill=`race/ethnicity`, y=jumlah, x=gender)) + 
  geom_bar(position="stack", stat="identity") +
  ggtitle("Studying 4 species..") +
  xlab("")


#metode nomina data non dikotomus =================================

df_non_dikotomus <- df_clean %>%
                                group_by(gender,`race/ethnicity`) %>%
                                summarise(jumlah = n())
df_non_dikotomus_1 = dcast(df_non_dikotomus ,gender~`race/ethnicity`, value.var='jumlah')
df_non_dikotomus_1
data_matrix_1 = data.matrix(df_non_dikotomus_1[ ,2:6])
data_matrix_1
cramerV(data_matrix_1, digits = 4)
data_xtabs_1 = xtabs(~df_clean$gender+factor(df_clean$`race/ethnicity`))

assocstats(data_xtabs_1)
#metode data ordinal dengan data ordinal =================================

spearman<-cor.test(as.numeric(df_clean$`parental level of education`),as.numeric(df_clean$`test preparation course`, method="spearman"))
spearman
kendall<-cor.test(as.numeric(df_clean$`parental level of education`),as.numeric(df_clean$`test preparation course`, method="kendall"))
kendall           

#pearson correlation ==================
plot(df_clean$`math score`,df_clean$`writing score`, col='#184E77', pch=19,
     main = "Scatterplot of math score vs writing score",
     xlab = "math score",
     ylab = "Writing score",
     cex.axis = 1)
mtext(paste0('Korelasi = ', round(cor(df_clean$`math score`,df_clean$`writing score`), digits = 3)), adj = 1, line = -10)
#in mtext adj is the coordinate for x axis between 0 and 1
#and line for the y axis
library(psych)
pairs.panels(df_clean[ ,c(6:8)], 
             method = 'pearson', # correlation method
             hist.col = '#34A0A4',
             density = TRUE,  # show density plots
             ellipses = TRUE, # show correlation ellipses
             main = 'Pair plot of math,reading,and writing score')

#biserial rank correlation
polyserial(as.numeric(df_clean$gender),as.numeric(df_clean$`parental level of education`))

