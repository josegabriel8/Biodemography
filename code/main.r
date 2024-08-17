###Biodemography final proyect

#A Comparative Analysis of Life Tables: 
#Examining Demographic Disparities between Black and White Populations 
#in the USA (1980-2020)



library(readr)
library(dplyr)
e0 <- read_csv("C:/Users/jose-/Desktop/Prioyecto bio/e0.csv")

lt.usa <- read_csv("C:/Users/jose-/Desktop/Prioyecto bio/USA.csv")

#################################
#cargar y estructurar data
###################################

#life expectancy

USAe <- e0 %>% 
  mutate(
    ok = case_when(
      (Year1 >= 2007 & (Ethnicity %in% c("E230", "E240"))) | 
        Year1 >= 2000 & Year1 <= 2006 & (Ethnicity %in% c("E030", "E110")) ~ "ok",
         Year1 <= 1999 & (Ethnicity %in% c("E030", "E110")) ~ "ok1",
         TRUE ~ "no"
        )
    ) %>% 
      filter(
        Version == 1, 
        Ethnicity %in% c("E230", "E240", "E030", "E110"),
        TypeLT == 1,
        ok == "ok"
      ) %>% 
      select(Ethnicity, Year1, Year3, Sex, `e(x)`, `e(x)Orig`)


filter_and_arrange <- function(data, e_values, sex_value) {
  data %>% 
    filter(Ethnicity %in% e_values, Sex == sex_value) %>% 
    arrange(Year1)
}

e_blackf <- filter_and_arrange(USAe, c("E240", "E030"), 2)
e_blackm <- filter_and_arrange(USAe, c("E240", "E030"), 1)
e_whitef <- filter_and_arrange(USAe, c("E230", "E110"), 2)
e_whitem <- filter_and_arrange(USAe, c("E230", "E110"), 1)

#
rm(USAe)
rm(e0)
#

combined_datae <- bind_rows(
  mutate(e_blackf, Group = "Black Female"),
  mutate(e_blackm, Group = "Black Male"),
  mutate(e_whitef, Group = "White Female"),
  mutate(e_whitem, Group = "White Male")
)

#Graph
library(ggplot2)

ggplot(combined_datae, aes(x = Year1, y = `e(x)`, color = Group)) +
  geom_point(size = 2) +
  geom_line() +
  labs(title = "Life Expectancy at birth, USA 2000-2020",
       x = "Year",
       y = "e(0)") +
  theme_minimal()



#####################################################################
##lifetables
#####################################################################
USAlf<- lt.usa %>% 
  mutate(ok = ifelse(Year1>=2007 & (Ethnicity == "E230" | Ethnicity == "E240" ), 
                     "ok",
                     ifelse(Year1>=2000 & Year1<=2006 & (Ethnicity == "E030" | Ethnicity == "E110"),
                            "ok",
                            ifelse(Year1<=1999 & (Ethnicity == "E030" | Ethnicity == "E110"),
                                   "ok1","no"))))  %>% 
  filter(Version==1, 
         Ethnicity == "E230" | Ethnicity == "E240" | Ethnicity == "E030" | Ethnicity == "E110",
         TypeLT == 1,
         ok == "ok") 

#individuales
USAlf_blackm <- filter_and_arrange(USAlf, c("E240", "E030"), 1)
USAlf_blackf <- filter_and_arrange(USAlf, c("E240", "E030"), 2)
USAlf_whitem <- filter_and_arrange(USAlf, c("E230", "E110"), 1)
USAlf_whitef <- filter_and_arrange(USAlf, c("E230", "E110"), 2)

################men and women toguether
library(dplyr)
usblack <- data.frame(Age = USAlf_blackm$Age, Year1 = USAlf_blackm$Year1,
                     male.lx=USAlf_blackm$`l(x)`, male.qx= USAlf_blackm$`q(x)`,
                     female.lx= USAlf_blackf$`l(x)`, female.qx= USAlf_blackf$`q(x)`) %>% 
  mutate(`l(x)`= round(male.lx*0.5+female.lx*0.5,0), 
         `q(x)`=male.qx*0.5+female.qx*0.5)

uswhite <- data.frame(Age = USAlf_whitem$Age, Year1 = USAlf_whitem$Year1,
                      male.lx=USAlf_whitem$`l(x)`, male.qx= USAlf_whitem$`q(x)`,
                      female.lx= USAlf_whitef$`l(x)`, female.qx= USAlf_whitef$`q(x)`) %>% 
  mutate(`l(x)`= round(male.lx*0.5+female.lx*0.5,0), 
         `q(x)`=male.qx*0.5+female.qx*0.5)




##############################
##transform into a lifetable object
##############################

library(demography)

# Function to convert lifetables to demogdata object
convert_to_demogdata <- function(dataset_name,name_) {
  ages <- unique(dataset_name$Age)
  years <- unique(dataset_name$Year1)
  
  # Create the 'pop' matrix
  pop_matrix <- matrix(0, nrow = length(ages), ncol = length(years))
  
  # Fill in the matrix with the corresponding population exposure values
  for (i in 1:length(ages)) {
    for (j in 1:length(years)) {
      subset_data <- dataset_name[dataset_name$Age == ages[i] & dataset_name$Year1 == years[j], ]
      pop_matrix[i, j] <- sum(subset_data$`l(x)`)  
    }
  }
  
  # Create the 'data' matrix
  data_matrix <- matrix(0, nrow = length(ages), ncol = length(years))
  
  # Fill in the matrix with the corresponding mortality count values
  for (i in 1:length(ages)) {
    for (j in 1:length(years)) {
      subset_data <- dataset_name[dataset_name$Age == ages[i] & dataset_name$Year1 == years[j], ]
      data_matrix[i, j] <- sum(subset_data$`q(x)`)  
    }
  }
  
  # Use the demogdata function
  demog_object <- demogdata(
    data = data_matrix,
    pop = pop_matrix,
    ages = ages,
    years = years,
    type = "mortality",
    label = "USA",
    name = name_  
  )
  
  return(demog_object)
}

# Call the function for each dataset
usblackm <- convert_to_demogdata(USAlf_blackm,name_="Black Male")
usblackf <- convert_to_demogdata(USAlf_blackf,name_="Black Female")
uswhitem <- convert_to_demogdata(USAlf_whitem,name_="White Male")
uswhitef <- convert_to_demogdata(USAlf_whitef,name_="White Female")
black <- convert_to_demogdata(usblack,name_="Black")
White <- convert_to_demogdata(uswhite,name_="White")



#remove innecesary tables
rm(lt.usa)
rm(USAlf)
rm(USAlf_blackf, USAlf_blackm, 
   USAlf_whitef, USAlf_whitem, uswhite, usblack)
#







######################################################################
#results
######################################################################

#piramid
library(pyramid)

males<- usblackm$pop$`Black Male`[,21]
females  <- usblackf$pop$`Black Female`[,21]
ages<-usblackm$age
data1 <- data.frame(males,females,ages)
pyramid(data1,Llab="Males",Rlab="Females",Clab="",Laxis=seq(0,100000,len=5),
        AxisFM="d", AxisBM=",", Csize=0.8, Cstep=10,
        main="Age pyramid USA black population 2020")

Males_percentage <- round((USAdata1$USAmales1 / total_population) * 100, digits=2)
Females_percentage <- round((USAdata1$USAfemales1 / total_population) * 100, digits=2)
USAdata2 <-data.frame(Males_percentage,Females_percentage,USAages1)



pyramid(USAdata2, Llab = "Males", Rlab = "Females", Clab = "Ages",
        Laxis = seq(0, 1, len = 4),  # Adjust the range of the x-axis to be percentages
        AxisFM = "g", AxisBM = ",", AxisBI = 2, Csize = 0.5, Cstep = 20, Cgap = 0.15,
        main = "Age pyramid USA 2021")

#######################
#age structure

Black<- c(0.26,0.23,0.26,0.12,0.13)
White<- c(0.19,0.19,0.25,0.15,0.22)
AgeR<- c("0-18yrs","19-34yrs","35-54yrs","55-64yrs","65+yrs")

Agestructure <- data.frame(Black,White, AgeR)

library(tidyr)
Agestructure_long <- gather(Agestructure, key = "Ethnicity", value = "Proportion", Black, White)

age_group_colors <- c("0-18yrs" = "#4682B4", "19-34yrs" = "#6495ED", "35-54yrs" = "#87CEEB", "55-64yrs" = "#ADD8E6", "65+yrs" = "#B0C4DE")

Agestructure_long$AgeR <- factor(Agestructure_long$AgeR, levels = rev(unique(Agestructure_long$AgeR)))


ggplot(Agestructure_long, aes(x = Ethnicity, y = Proportion, fill = AgeR)) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = scales::percent(Proportion)),
            check_overlap = F,
            color = "white",
            position = position_stack(vjust = 0.5),
            size = 5,
            stat = "identity") +
  labs(title = "Proportion of Black and White Population by Age Group 2021",
       x = "Ethnicity",
       y = "Proportion") +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  scale_fill_manual(values = age_group_colors)+
  theme_minimal()+
  theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
        axis.title.x = element_blank())




# death rates 

par(mfrow = c(1, 1))
plot(usblackf, 
     series=names(usblackf$rate),year=2019:2019, main="black Female")

plot(usblackm, series=names(usblackm$rate),year=2019:2019, main="black Male")
plot(uswhitef, series=names(uswhitef$rate),year=2019:2019, main="White Female")
plot(uswhitef, series=names(uswhitef$rate),year=2019:2019, main="white Male")
par(mfrow = c(1, 1))
plot(usblackf, series = names(usblackf$rate), year = 2019:2019, 
     main = "Death Rates by etnia - 2019", col="red")
lines(log(usblackm$rate$`Black Male`[,20]), col = "red", lty=2)
lines(log(uswhitef$rate$`White Female`[,20]), col = "blue")
lines(log(uswhitem$rate$`White Male`[,20]), col = "blue", lty=2)

legend("topleft", legend = c("Black Female", "Black Male", "White Female", "White Male"), 
       col = c("red", "red", "blue", "blue"), lty = c(1,2,1,2))

#calculate life tables to gey lx

calculate_lifetable <- function(dataset) {
  lt <- lifetable(dataset, series = names(dataset$rate), years = dataset$year, ages =
                    dataset$age, max.age = min(110, max(dataset$age)), type = c("period"))
  return(lt)
}

# Usage
usblackf.lt <- calculate_lifetable(usblackf)
usblackm.lt <- calculate_lifetable(usblackm)
uswhitef.lt <- calculate_lifetable(uswhitef)
uswhitem.lt <- calculate_lifetable(uswhitem)

whitelf <- calculate_lifetable(White)

blacklf <- calculate_lifetable(black)




lx_blackf <- usblackf.lt$lx[,which(usblackf.lt$year == 2019)]
lx_ublackm<- usblackm.lt$lx[,which(usblackm.lt$year == 2019)]
lx_whitef<-  uswhitef.lt$lx[,which(uswhitef.lt$year == 2019)]
lx_uwhitem<- uswhitem.lt$lx[,which(uswhitem.lt$year == 2019)]



#survivors

plot (lx_blackf, col="red", xlab ="age", ylab ="lx",
       type ="l", lty =1 , main = " Survivors USA 2019 by ethnicity")
lines(lx_ublackm, col = "red", lty=2)
lines(lx_whitef, col = "blue", lty=1)
lines(lx_uwhitem, col = "blue", lty=2)
legend("bottomleft", legend = c("Black Female", "Black Male", "White Female", "White Male"), 
       col = c("red", "red", "blue", "blue"), lty = c(1,2,1,2))


#######calculating deaths


usblackf.lt$dx[,which(usblackf.lt$year == 2019)]
usblackm.lt$dx[,which(usblackm.lt$year == 2019)]
uswhitef.lt$dx[,which(uswhitef.lt$year == 2019)]
uswhitem.lt$dx[,which(uswhitem.lt$year == 2019)]


plot (usblackf.lt$dx[,which(usblackf.lt$year == 2019)],
      ylim =c(0 , 0.05), col="red", xlab ="age", ylab ="dx",
      type ="l", lty =1 , main = " Deaths USA 2019 by ethnicity")
lines(usblackm.lt$dx[,which(usblackm.lt$year == 2019)],
      col = "red", lty=2)
lines(uswhitef.lt$dx[,which(uswhitef.lt$year == 2019)],
      col = "blue", lty=1)
lines(uswhitem.lt$dx[,which(uswhitem.lt$year == 2019)],
      col = "blue", lty=2)
legend("topleft", legend = c("Black Female", "Black Male", "White Female", "White Male"), 
       col = c("red", "red", "blue", "blue"), lty = c(1,2,1,2))

#####life expectancy decomposition

##i get the % data from anither separate file in excel

library(knitr)

# Display the table using kable
kable(data, caption = "Percentage by Age Group")

library(ggplot2)


data <- data.frame(
  Age_Group = c("0", "1-10", "11-20", "21-30", "31-40", "41-50", "51-60", "61-70", "71-80", "81-90", "91-99", "100+"),
  Percentage = c(9.6308, 1.6199, 6.1489, 9.3098, 9.0000, 11.8232, 16.4823, 22.5590, 12.5637, 2.3233, -1.4608, 0.0120)
)

# Creating a bar plot using ggplot2
bar_plot <- ggplot(data, aes(x = factor(Age_Group, levels = unique(Age_Group)), y = Percentage, fill = "lightblue")) +
  geom_bar(stat = "identity") +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), vjust = -0.5, color = "black", size = 3) +
  labs(title = "Decomposition of e(0) differences by age: 
       white vs black population 2019", x = "Age Group", y = "%e(0)") +
  theme_minimal()

# Display the bar plot
print(bar_plot)

#########################
######################

#Hacer el modelo para todos, 

#lee carter model

library(demography)

black.LC <- lca(black, series=names(black$rate),
                 years=2000:2019)


#
White.LC <- lca(White, series=names(White$rate),
                years=2000:2019)


?lca


plot(black.LC$bx)
plot(black)
summary(black.LC)
plot(White.LC$bx)
plot(White)
summary(White.LC$bx)



#Fitted values and residuals for agefrom lee-carter model 



#black
RES=residuals(black.LC, "residuals")

plot(rep(RES$y,length(RES$x)),(RES$z), xlab = "age", ylab
     = "residuals", main = "Residuals by ages from LC model")

plot(rep(RES$x,length(RES$y)),(RES$z), xlab = "years", ylab
     = "residuals", main = "Residuals by years from LC model")

plot(residuals(black.LC),type="image")

#white

RES2=residuals(White.LC, "residuals")

plot(rep(RES2$y,length(RES2$x)),(RES2$z), xlab = "age", ylab
     = "residuals", main = "Residuals by ages from LC model")

plot(rep(RES2$x,length(RES2$y)),(RES2$z), xlab = "years", ylab
     = "residuals", main = "Residuals by years from LC model")

plot(residuals(White.LC),type="image")










#forecast

plot(forecast(black.LC, h = 20)$kt,)
plot(forecast(White.LC, h=10)$kt,)


blackforecast<-forecast(black.LC, h = 20)
whiteforcast<-forecast(White.LC, h = 20)


m$kt.f

par(mfrow = c(1, 2))


plot(black, series=names(black$rate), main="black")
plot(fitted(black.LC), main =
       "black Fitted")


#proyected life table

black_2020_2035.lt <-
  lifetable(forecast(black.LC,h=15))

plot(black_2020_2035.lt)


black_2035.lt <- lifetable(forecast(black.LC), years = 2035)
black_2035.lt
#
white_2010_2035.lt <-
  lifetable(forecast(White.LC,h=15))

plot(white_2010_2035.lt)


white_2035.lt <- lifetable(forecast(White.LC), years = 2035)
white_2035.lt


#just lo life expectancy

e_0_2020_2040b <- life.expectancy(forecast(black.LC,h=40),
                                 type = c("period"))



e_0_2020_2040w <- life.expectancy(forecast(White.LC,h=40),
                                 type = c("period"))

plot(e_0_2020_2040b)
plot(e_0_2020_2040w)

#joining
e_0b <- life.expectancy(black, series =
                         names(black$rate), years = 2000:2019,
                       type = c("period"))
e_0w <- life.expectancy(White, series =
                          names(White$rate), years = 2000:2019,
                        type = c("period"))


 e_0_2000_2040b <- c(e_0b, e_0_2020_2040b)
 e_0_2000_2040w <- c(e_0w, e_0_2020_2040w)
 
 years <- seq(2000, 2049, 1)

 
 plot(years, e_0_2000_2040b, type = "l", col = "red", main = "Past and Forecast Life Expectancy. USA 2000-2035",
      xlab = "Year", ylab = "Life Expectancy", ylim = c(70,80))
  lines(years, e_0_2000_2040w, col = "blue")
  abline(v = 2020, col = "black", lty = 2)
 legend("bottomright", legend = c("Black", "White"), col = c("red", "blue"), lty = 1, cex = 0.8)

 #############ggplot
 library(ggplot2)
 

 
 df1 <- data.frame(years = years,
                  e_0_2000_2040b = e_0_2000_2040b,
                  e_0_2000_2040w = e_0_2000_2040w)
 
 
 
 ggplot(df1, aes(x = years)) +
   geom_path(aes(y = e_0_2000_2040b, linetype = ifelse(years <= 2020, "dashed", "solid"), color = "Black Population"), size = 1) +
   geom_path(aes(y = e_0_2000_2040w, linetype = ifelse(years <= 2020, "dashed", "solid"), color = "White Population"), size = 1) +
   geom_vline(xintercept = 2020, linetype = "dashed", color = "black") +
   ylim(c(70, 80)) +
   labs(title = "Past and Forecast Life Expectancy. USA 2000-2040",
        x = "Year", y = "Life Expectancy") +
   theme_minimal() +
   guides(linetype = FALSE)+
   scale_color_manual(name = "Population",
                      values = c("Black Population" = "red", "White Population" = "blue")) +
   theme(legend.position = c(0.8, 0.2))
 
 
########Comparing also Kt
 ########################
 
 black_Kt <- black.LC$ax
 white_Kt <- White.LC$ax
 
 # Step 2: Create a time series plot
 Age <- 1:101
 plot(Age, black_Kt, type = "l", col = "red", lty = 1, ylim = range(black_Kt, white_Kt),
      xlab = "Age", ylab = "ax Value", main = "Lee-Carter Model - ax Values Comparison")
 lines(Age, white_Kt, col = "blue", lty = 2)
 legend("bottomright", legend = c("Black Population", "White Population"), col = c("red", "blue"), lty = 1:2, cex = 0.8) 

 ######in ggplot2
 library(ggplot2)
 
 data <- data.frame(Year = rep(years, 2),
                    Kt = c(black_Kt, white_Kt),
                    Population = rep(c("Black Population", "White Population"), each = length(years)))
 
 # Create ggplot
 ggplot(data, aes(x = Year, y = Kt, color = Population, linetype = Population)) +
   geom_line() +
   labs(x = "Year", y = "Kt Value", title = "Lee-Carter Model - Kt Values Comparison 2000-2019") +
   scale_color_manual(values = c("red", "blue")) +
   scale_linetype_manual(values = c(1, 2)) +
   theme_minimal() +
   theme(legend.position = c(0.8, 0.8))
 
 
 #############
