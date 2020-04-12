
#Author: Cristian Marius-Valentin

library(tsm)
library(forecast)
library(strucchange)
library(astsa)
library("car")
library("ggplot2")
library(nlmrt)

#######################################################
#Importam datele infectati

cazuri_covid_list <- c(read.csv("C:\\Users\\crist\\Desktop\\Coronavirus\\Data\\covid19_confirmed_global.csv",
                        colClasses = c("NULL", NA, "NULL", "NULL", "NULL", "NULL")))
cazuri_covid <- as.numeric(as.character(unlist(cazuri_covid_list)))
(cazuri_covid)


nr_zile_primul_caz <- c(read.csv("C:\\Users\\crist\\Desktop\\Coronavirus\\Data\\covid19_confirmed_global.csv",
                                 colClasses = c("NULL", "NULL", NA, "NULL", "NULL", "NULL")))
zile <- as.numeric(as.character(unlist(nr_zile_primul_caz)))
(zile)

cazuri_zi_plot <- data.frame(nr_zile_primul_caz, cazuri_covid)
plot(cazuri_zi_plot)

#######################################################
#Importam datele vindecati

cazuri_covid_list_vindecati <- c(read.csv("C:\\Users\\crist\\Desktop\\Coronavirus\\Data\\covid19_confirmed_global.csv",
                                colClasses = c("NULL", "NULL", "NULL", "NULL", "NULL", NA)))

cazuri_covid_vindecati <- as.numeric(as.character(unlist(cazuri_covid_list_vindecati)))
(cazuri_covid_vindecati)

cazuri_zi_vindecati_plot <- data.frame(nr_zile_primul_caz, cazuri_covid_vindecati)
plot(cazuri_zi_vindecati_plot)

#######################################################
#Importam datele decedati

cazuri_covid_list_decedati <- c(read.csv("C:\\Users\\crist\\Desktop\\Coronavirus\\Data\\covid19_confirmed_global.csv",
                                          colClasses = c("NULL", "NULL", "NULL", "NULL", NA , "NULL")))

cazuri_covid_decedati <- as.numeric(as.character(unlist(cazuri_covid_list_decedati)))
(cazuri_covid_decedati)

cazuri_zi_decedati_plot <- data.frame(nr_zile_primul_caz, cazuri_covid_decedati)
plot(cazuri_zi_decedati_plot)

#######################################################
#Modelul logistic I
#Y <- c/(1 + exp(-(x-b)/a))
#Y sau y(t) <- Numarul de cazuri la momentul t
#c <- Limita pe care o poate atinge Y
#a <- Parametrul ce denota cresterea, in cazul nostru rata de infectare (<1 logaritmic, =1 liniar, >1 exponential)
#b <- Ziua cu cel mai mare numar de infectari.
#t <- Timp

model <- nls(cazuri_covid ~ c/(1 + exp(-(zile-80)/a)),
             start = list(c=14000,a=3),
             data = cazuri_zi_plot)

summary(model)

c <- coef(model)[1]
a <- coef(model)[2]

#x <- c(min(zile):max(zile))
#y <- c/(1 + exp(-(x-b)/a))


#######################################################
#Predictia numarului oamenilor vindecati


model_vindecati <- nls(cazuri_covid_vindecati ~ c_vindecati/(1 + exp(-(zile- 99)/a_vindecati)),
                       start = list(c_vindecati = 10000 ,a_vindecati = 5),
                       data = cazuri_zi_vindecati_plot)  
summary(model_vindecati)

c_vindecati <- coef(model_vindecati)[1]
#b_vindecati <- coef(model_vindecati)[3]
a_vindecati <- coef(model_vindecati)[2]


#######################################################
#Predictia numarului oamenilor decedati

model_decedati <- nls(cazuri_covid_decedati ~ c_decedati/(1 + exp(-(zile-91)/a_decedati)),
                       start = list(c_decedati = 5000 ,a_decedati = 3),
                       data = cazuri_zi_decedati_plot)  
summary(model_decedati)

c_decedati <- coef(model_decedati)[1]
b_decedati <- coef(model_decedati)[3]
a_decedati <- coef(model_decedati)[2]


#######################################################
#Graficul modelelor

date_ox <- as.Date(c(00000:00130), origin = "2020-01-23")
date_ox_cazuri_covid <- as.Date(c(00000:00078), origin = "2020-01-23")



model_prezicere <- predict(model, data.frame(zile = c(0:130)))
model_prezicere_vindecati <- predict(model_vindecati, data.frame(zile = c(0:130)))
model_prezicere_decedati <- predict(model_decedati, data.frame(zile = c(0:130)))
plot(date_ox,model_prezicere, type="l", col="green", lwd=4,lty = 5, xlab="Timp", ylab="Numar infectati", main="Evolutie Covid-19")
  lines(date_ox_cazuri_covid,cazuri_covid, col="red", lwd=3)
  lines(date_ox,model_prezicere_vindecati, col="blue", lwd=3, lty=4)
  lines(date_ox,model_prezicere_decedati, col="black",lwd = 3, lty=4)
  legend("topleft",
         c("Real","Predictie infectari","Predictie vindecati","Predictie decedati"),
         fill=c("red","green","blue","black"))
  
  
#######################################################
#Modelul logistic II

  
Y <- c/(1 + (1-a) * c^(-(b*t)))

#Y sau y(t) <- numarul de cazuri la momentul t
#c <- limita pe care o poate atinge Y
#a <- distantare sociala. Apartine [0:1] (0% - nici un fel de restrictie si distantare sociala, 100% nici un fel de interactiune interumana)
#b <- Parametrul ce denota cresterea, in cazul notru rata de infectare (<1 logaritmic, =1 liniar, >1 exponential)
#t <- timp


#model_caz <- nlxb(cazuri_covid~c/(1 + a*exp(-b * zile)),
#                 start = c(c=1200,b=2.345,a=0.123))

#distantare <- nls()


model <- nls(cazuri_covid ~ c/(1 + exp(-(b*zile)/a)),
             start = list(c=10000,b=10,a=1),
             data = cazuri_zi_plot)


summary(model)

c <- coef(model)[1]
b <- coef(model)[2]
a <- coef(model)[3]

x <- c(min(zile):max(zile))
y <- c/(exp(-b*zile))



prediction <- data.frame(x,y)
plot.ts(prediction)
model_prezicere <- predict(model, data.frame(zile = c(0:80)))
plot(model_prezicere)


#######################################################
#Testarea modelului pe date reale din China

cazuri_covid_list_china <- c(read.csv("C:\\Users\\crist\\Desktop\\Coronavirus\\Data\\covid19_confirmed_China.csv",
                                colClasses = c( NA, "NULL", "NULL")))

cazuri_china<- as.numeric(as.character(unlist(cazuri_covid_list_china)))
(cazuri_china)


nr_zile_primul_caz_china <- c(read.csv("C:\\Users\\crist\\Desktop\\Coronavirus\\Data\\covid19_confirmed_China.csv",
                                 colClasses = c("NULL","NULL", NA)))
zile_china <- as.numeric(as.character(unlist(nr_zile_primul_caz_china)))
(zile_china)


cazuri_zi_plot <- data.frame(nr_zile_primul_caz_china, cazuri_covid_list_china)
plot(cazuri_zi_plot)

baza_cazuri_china <- window(cazuri_china, start = 0, end = 23)
baza_zi_china <- window(zile_china, start = 0, end = 23)
china_window <- data.frame(baza_zi_china,baza_cazuri_china)

plot(baza_zi_china,baza_cazuri_china)


model <- nls(baza_cazuri_china~c/(1 + exp(-(baza_zi_china-b)/a)),
             start = list(c=100000, b=23,a=5))
summary(model)

c <- coef(model)[1]
b <- coef(model)[2]
a <- coef(model)[3]

x <- c(0:80)
y <- c/(1 + exp(-(x-b)/a))

china_plot <- data.frame(x,y)
plot(china_plot, type="l", col="green", lwd=5, xlab="Timp", ylab="Numar infectati", main="Evolutie Covid-19")
lines(cazuri_zi_plot, col="red", lwd=2)
lines(china_window, col="blue", lwd=5, lty=4)
legend("bottomright",
       c("Predictie","Real","Datele folosite pentru predictie"),
       fill=c("green","red","blue"))
