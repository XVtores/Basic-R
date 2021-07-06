
install.packages(c("tidyverse","dslabs"))  ## instalar más de un paquete, los nombres de packetes entre comillas
installed.packages() ## ver todos los paquetes instalados


## ejercicios de operaciones

a<-2
b<--1
c<--4
ls()# lista los objetos
sol_1<-(-b + sqrt(b^2 - 4*a*c))/(2*a)
print(sol_1)
sol_2<-(-b - sqrt(b^2 - 4*a*c) ) / ( 2*a )
print(sol_2)

ls() #all variables saved in workspace
rm() ##borra objetos determinados
rm(list = ls())## borra todos los objetos

help("log") ## función de ayuda
?log
args(log)  ## vistazo a los argumentos

#ejemplo
log(8, base = 2) ## cambiando default
log(8,2) ## R sabe el lugar de los argumentos
log(base=2, x=8) ## al colocar los nombres de los argumentos, no importa el orden

data() ## lista los datasets
co2 ## ejemplo de objeto dataset

class()#determinar el tipo de objeto
class(co2)  #objeto tipo serie temporal

#ejemplo data.frame

library(dslabs) ## carga librería con los datos del libro
data("murders") ## carga el dataset que buscamos
class(murders)
str(murders) ## estructura del objeto
head(murders)
names(murders)
pop<-murders$population 
length(pop)
class(pop)


#Example of factor
region<-murders$region
class(region)
levels(region)
table(murders$region)
value<-murders$total
region<-reorder(region,value, FUN = sum) #reordenar un factor dado un estadístico
levels(region)

## funcion concatenar - vector  asignación de nombres

temp<-c(35, 88, 42, 84, 81, 30)
city<-c("Beijing", "Lagos", "Paris", "Rio de Janeiro", "San Juan", "Toronto")
names(temp)<-city #asignar nombres al verctor
temp[1:3]
temp[c("Paris","San Juan")]

# secuancias

12:73
seq(1,99,2)
a <- seq(1, 10, 0.5)
class(a)  # numeric
a <- seq(1, 10)
class(a)

x <- c("1", "3", "5")
class(x) # interger
x<-as.numeric(x)
class(x)

#  ordenar

library(dslabs) 
data("murders") 

pop<-murders$population
pops<-sort(pop) #ordena de menor a mayor
pops[1]  ## menor población
ind<-order(pop)  #indices de menor a mayor
ind[1] ## índice de la menor población
which.min(pop) ## forma directa
murders$state[51]  ## state with the smallest population

sort(murders$total)  # ordena los valores, sin embargo no me da los estados que tienen cada valor
index<-order(murders$total)
murders$state[index]
max(murders$total)
i_max<-which.max(murders$total) #índice en el que se encuentra el máximo
i_max
murders$state[i_max]



# Ejercicio ordenar un data frame

rank_hom<-rank(murders$total)
ind<-order(murders$total)
my_df<-data.frame(Estado=murders$state, Ranking=rank_hom)
my_df2<-data.frame(Estado=murders$state[ind], Ranking=rank_hom[ind])


data("na_example")
str(na_example)
mean(na_example) ## obtenemos NAs
ind<-is.na(na_example)  ## obtenemos los indices de los NA
sum(ind)
mean(na_example[!ind])  # promedio sin los indices NA


#ejercicio 
name <- c("Mandi", "Amy", "Nicole", "Olivia")
distance <- c(0.8, 3.1, 2.8, 4.0)
time <- c(10, 30, 40, 50)
time_h<-time/60  ## tiempo en horas
time_h
speed<-distance/time_h
record<-data.frame(nombre=name, distancia=distance, tiempo=time_h, vel=speed)

#Ejercicios 2.14

library(dslabs)
data(murders)
str(murders)
murder_rate<-murders$total/murders$population*100000
murders$state[which.min(murder_rate)] #Estado con mínima tasa
murders$state[which.max(murder_rate)] #Estado con la máxima tasa
low<-murder_rate<1
which(low) #los índices que cumplen la condición previa
sum(low) # núero de estados de cumplen esta condición
murders$state[low]
zone<-murders$region=="Northeast"
index<-low & zone
murders$state[index] # estados que cumplen las dos condiciones

mean(murder_rate)
murders$state[murder_rate<mean(murder_rate)]
sum(murder_rate<mean(murder_rate))

ind<-match(c("AK", "MI", "IA"), murders$abb) #Índices que tienen las abreviaciones
murders$state[ind]


ab1<-c("MA", "ME", "MI", "MO", "MU")
sol<-!ab1%in%murders$abb  #indice del que no forma parte
which(sol)




## ********************Uso de comandos dplyr

data("co2")
str(co2)
data("ChickWeight")

library(dplyr)
library(dslabs)
data("murders")

murders<-mutate(murders, rate=total/population*100000) #mutate to add a column
filter(murders, rate<=0.71) #filter for rows
new_table <- select(murders, state, region, rate) # select for columns
filter(new_table, rate<=0.71)

murders<-mutate(murders, population_in_millions=population/10^6)
head(murders)
murders<-mutate(murders, ranking=rank(-rate)) #the minus sign gives you the ranks from highest to lowest

select(murders, state, population) %>% head()

filter(murders, ranking <=5) # ranking de las 5 más altas tasas

no_florida <- filter(murders, state != "Florida") ## todos excepto florida

no_south<-filter(murders, region!="South") # todos excepto la región South
nrow(no_south)

murders_nw<-filter(murders, region %in% c("Northeast", "West"))
nrow(murders_nw)

my_states<-filter(murders_nw, rate<1)
my_states<-select(my_states, state, rate, ranking)

## ejercicio de dply
data("murders")
str(murders)
my_states<-murders %>% mutate(rate=total/population*100000, rank= rank(-rate))%>% filter(rate<1 & region%in% c("Northeast","West"))%>% select(state, rate, rank) 
class(my_states)

# diferencia entre tasa nacional y tasa promedio
murders <- murders %>% mutate(rate = total/population*100000)
summarize(murders, mean(rate)) # Promedio de las tasas de los estados
us_murder_rate <- murders %>% summarize(rate = sum(total) / sum(population) * 100000) # tasa nacional
us_murder_rate


#plots
population_in_millions<-murders$population/10^6
total_gun_murders<-murders$total
plot(population_in_millions,total_gun_murders)
with(murders, plot(population, total))
with(murders, plot(log10(population), log10(total))) #alternativa with


hist(murders$rate)
with(murders, hist(total / population * 100000))
murders$state[which.max(murders$rate)]
boxplot(rate~region, data = murders)
with(murders, boxplot(rate~region))
x <- matrix(1:120, 12, 10)
image(x)

#*****************************

library(dslabs)
data(heights)
options(digits = 3)    # report 3 significant digits for all answers
str(heights)
avg<-mean(heights$height)
ind<-heights$height>avg
sum(ind)
indsex<-heights$sex=="Female"
indfinal<-ind&indsex
sum(indfinal)

mean(heights$sex=="Female")

min<-which.min(heights$height)
heights$height[min]
sort(heights$height)

match(50,heights$height)

heights$sex[1032]

maximo<-max(heights$height)
minimo<-min(heights$height)
x<-50:82
sol<-match(x, heights$height) # vector de índices
!x%in%heights$height ## vector lógico de los que no están
sum(!x%in%heights$height)

heights<-mutate(heights, heigh_cm=height*2.54)
heights$heigh_cm[18]
mean(heights$heigh_cm)

female<-filter(heights,sex=="Female")

data<-filter(heights,sex=="Female")%>%select(heigh_cm)
mean(data$heigh_cm)

data("olive")
str(olive)
head(olive)
with(olive, plot(palmitic~palmitoleic))
with(olive, hist(eicosenoic))
with(olive, boxplot(palmitic~region))


new<-ifelse(heights$sex=="Female",1,2)
sum(new)

new<-ifelse(heights$height>72,heights$height,0)
mean(new)

#función de pulgadas a pies
inches_to_ft<-function(x){
  x/12
}
feet<-inches_to_ft(heights$height)
consulta<-feet<5
sum(consulta)


char_len <- nchar(murders$state)
head(char_len)
new_names<-ifelse(nchar(murders$state)>8, murders$abb,murders$state)

sum_n<-function(n){
  x<-1:n
  y<-sum(x)
  y
}

# Write a function compute_s_n with argument n that for any given n computes the sum of 1 + 2^2 + ...+ n^2
compute_s_n<-function(n){
  x<-1:n
  y<-x^2
  sum(y)
}

# Report the value of the sum when n=10
compute_s_n(10)



for(i in 1:25){
  s_n[i]<-compute_s_n(i)
} 


#ejercicio de dply y summarize
library(dplyr)
library(dslabs)
data(heights)

# media y desviación estándar

s <- heights %>% 
  filter(sex == "Female") %>%
  summarize(average = mean(height), standard_deviation = sd(height))

s 
class(s)  ##data frame
s$average
s$standard_deviation


# estadísticos de orden
heights %>% 
  filter(sex == "Female") %>%
  summarize(median = median(height), minimum = min(height), 
            maximum = max(height))

heights %>%
  filter(sex == "Male") %>%
  summarize(range = quantile(height, c(0, 0.5, 1))) ## genera un error porque las funciones dentro de summarize solo pueden generar un valor
           
# group by
heights %>%
  group_by(sex) %>%
  summarize(average = mean(height), standard_deviation = sd(height))


## Ejercicio
library(dplyr)
library(NHANES)
data(NHANES)
## modify the code we wrote for previous exercise.
ref_avg <- NHANES %>%
  filter(AgeDecade == " 20-29" & Gender == "female") %>%
  summarize(ref_avg = mean(BPSysAve, na.rm = TRUE), 
            standard_deviation = sd(BPSysAve, na.rm=TRUE))%>%.$ref_avg

#mínimo y máximo
NHANES %>%
  filter(AgeDecade == " 20-29"& Gender == "female") %>%summarize(minbp = min(BPSysAve, na.rm = TRUE), maxbp = max(BPSysAve, na.rm=TRUE))

#group by
NHANES %>%
  filter(Gender == "female") %>%group_by(AgeDecade)%>% summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm=TRUE))

#group by por dos categorias
NHANES %>%
  group_by(AgeDecade, Gender)%>% summarize(average = mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm=TRUE))


# arrange by average
NHANES%>%filter(Gender=="male" & AgeDecade ==" 40-49")%>%group_by(Race1)%>%summarize(average=mean(BPSysAve, na.rm = TRUE), standard_deviation = sd(BPSysAve, na.rm=TRUE))%>%arrange(average)