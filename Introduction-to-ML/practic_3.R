library("lmtest")
library("rlms")
library("dplyr")
library("GGally")
library(car)
library(sandwich)

data <- read.csv("C:\\Users\\Admin\\Documents\\r14i_os26b.csv")
glimpse(data)

data_main = select(data, jj13.2, jh5, j_marst, j_educ, j_age, status, jj6.2, jj6, jj1.1.2, jj23, jj24)
data_main

# В data_main - набор данных, где записаны параметры из задачи по порядку:
# jj13.2 - зарплата; jh5 - пол; j_marst - семейное положение; j_educ - наличие высшего обращования;
# j_age - возраст; status - тип населенного пункта; jj6.2 - длительность рабочей недели;
# jj6 - есть ли подчиненные; jj1.1.2 - удовлетворенность работе; jj23 - государство(совладелец или владелец);
# jj24 - иностранное фирмы и частники(совладельцы или владельцы);

#----------------------------------------Семейное положение(wed)------------------------------------------------------------

data_main["wed"] = data_main$j_marst

# Переменная wed1 имеет значение 1 в случае, если респондент женат, 0 – в противном случае

data_main["wed1"] = lapply(data_main["wed"], as.character) # Приведение данных к одному и тому же типу
data_main$wed1 = 0 # В противном случае
data_main$wed1[which(data_main$wed=='2')] <- 1 # Ответ : Состоите в зарегистрированном браке
data_main$wed1[which(data_main$wed=='6')] <- 1 # Ответ : Официально зарегистрированы, но вместе не проживают
data_main$wed1 = as.numeric(data_main$wed1)

# wed2 = 1, если респондент разведён или вдовец

data_main["wed2"] = lapply(data_main["wed"], as.character)
data_main$wed2 = 0 # По умолчанию
data_main$wed2[which(data_main$wed=='4')] <- 1 # Ответ : Разведены и в браке не состоите
data_main$wed2[which(data_main$wed=='5')] <- 1 # Ответ : Вдовец(вдова)
data_main$wed2 = as.numeric(data_main$wed2)

# wed3 = 1, если респондент никогда не состоял в браке

data_main["wed3"]=lapply(data_main["wed"], as.character)
data_main$wed3 = 0 # По умолчанию
data_main$wed3[which(data_main$wed=='1')] <- 1 # Ответ : Никогда в браке не состояли
data_main$wed3 = as.numeric(data_main$wed3)

#------------------------------------Проверка мультиколлинеарности(wed)-----------------------------------------------------

model0 = lm(jj13.2~wed1+wed2+wed3, data_main)
model0
vif(model0) # По коэффициенту вздутия дисперсии видно, что мультиколлинерности нет (все коэф. меньше 3)

#-------------------------------------------------Пол(sex)------------------------------------------------------------------

# Из параметра пол сделайте переменную sex, имеющую значение 1 для мужчин и равную 0 для женщин.

data_main["sex"] = data_main$jh5
data_main$sex[which(data_main$sex=='2')] <- 0
data_main$sex[which(data_main$sex=='1')] <- 1
data_main$sex = as.numeric(data_main$sex)

#------------------------------------Тип населенного пункта(city status)----------------------------------------------------

# Из параметра, отвечающего типу населённого пункта, создайте одну дамми-переменную city_status со значением 1 для города
# или областного центра, 0 – в противоположном случае.

data_main["city_status"] = data_main$status
data_main$city_status = 0 # По умолчанию
data_main$city_status[which(data_main$status=='1')] <- 1 # Областной центр
data_main$city_status[which(data_main$status=='2')] <- 1 # Город
data_main$city_status = as.numeric(data_main$city_status)

#------------------------------------Полное высшее образование(higher_educ)------------------------------------------------------------

# Введите один параметр higher_educ, характеризующий наличие полного высшего образования.

data_main["higher_educ"] = data_main$j_educ
data_main$higher_educ = 0 # По умолчанию
data_main$higher_educ[which(data_main$j_educ=='21')] <- 1 # Диплом о высшем образовании
data_main$higher_educ[which(data_main$j_educ=='22')] <- 1 # Аспирантура и т.п. без диплома
data_main$higher_educ[which(data_main$j_educ=='23')] <- 1 # Аспирантура и т.п. с дипломом
data_main$higher_educ = as.numeric(data_main$higher_educ)

#------------------------------------Нормализация факторных переменных-------------------------------------------------------

# Преобразование заработной платы

data_main$jj13.2[which(data_main$jj13.2>99999990)] <- NA # Исключение респондентов, затруднившихся к ответу
data_main = na.omit(data_main)
salary = data_main$jj13.2
mean(salary) # Средняя заработная плата
data_main["salary"] = (salary - mean(salary)) / sqrt(var(salary))
#data_main["salary"]

# Преобразование возраста

age = data_main$j_age
data_main["age"] = (age - mean(age)) / sqrt(var(age))
mean(age) # Средний возраст
#data_minimal["age"]

# Преобразование длительности рабочей недели

data_main$jj6.2[which(data_main$jj6.2>99999990)] <- NA # Исключение респондентов, затруднившихся к ответу
data_main = na.omit(data_main)
duration = data_main$jj6.2
mean(duration) # Средняя длительность рабочей недели
data_main["duration"] = (duration - mean(duration)) / sqrt(var(duration))
#data_main["duration"]

#----------------------------Нормализация факторных переменных(не из минимального набора данных)---------------------------

# Есть ли подчиненные?
data_main["employees"] = data_main$jj6
data_main$employees = 0 # По умолчанию
data_main$employees[which(data_main$jj6=='1')] <- 1 # Есть подчиненные
data_main$employees = as.numeric(data_main$employees)
#data_main$employees

# Удовлетворенность работе
data_main["satisfaction"] = data_main$jj1.1.2
data_main$satisfaction = 0 # По умолчанию
data_main$satisfaction[which(data_main$jj1.1.2=='1')] <- 1 # Полностью удовлетворены
data_main$satisfaction[which(data_main$jj1.1.2=='2')] <- 1 # Частично удовлетворены
data_main$satisfaction = as.numeric(data_main$satisfaction)

# Государство(совладелец или владелец)
data_main["state_owner"] = data_main$jj23
data_main$state_owner = 0 # По умолчанию
data_main$state_owner[which(data_main$jj23=='1')] <- 1 # Да
data_main$state_owner = as.numeric(data_main$state_owner)

# Иностранное фирмы и частники(совладельцы или владельцы)
data_main["foreign_owner"] = data_main$jj24
data_main$foreign_owner = 0 # По умолчанию
data_main$foreign_owner[which(data_main$jj24=='1')] <- 1 # Да
data_main$foreign_owner = as.numeric(data_main$foreign_owner)

#------------------------------Построение линейной регрессии зарплаты на остальные параметры-------------------------------

model_main = lm(salary~wed1+wed2+wed3+duration+age+sex+city_status+higher_educ+satisfaction+employees+foreign_owner+state_owner, data_main)  
summary(model_main)

# Наша модель имеет довольно высокий коэффициент детерминации(R^2) = 0.2324. Хотя вся зарплата зависит от наших регрессоров
# прямым образом, так как кол-во звездочек у всех регрессоров высокое.

vif(model_main)

# Значения коэффициента вздутия дисперсии небольшие, значит можно сказать, что наши регрессоры линейно независимые
# друг от друга

#--------------------------------------------Введение функций от регрессоров------------------------------------------------

model_main = lm(salary~wed1+wed2+wed3+duration+age+sex+city_status+higher_educ+satisfaction+employees+foreign_owner+state_owner, data_main)
summary(model_main)

# -----------Вводим логарифмы от регрессоров

# Логарифмы следует вводить только от вещественных переменных, так как смысла вводить их в дамми-переменные нет
# Проверим насколько сильно отклоняются вещественные переменные от среднего и добавим данное значение к регрессорам

min(data_main$age) # min(age) = -2.023109
min(data_main$duration) # min(duration) = -3.32842

# Добавим значение 5 к каждому регрессору и возьмем от них логарифмы

# Проверим на линейную зависимость исходного регрессора от его логарифма

summary(lm((age+5)~I(log(age+5)), data_main)) # R^2 ~ 0.9872
summary(lm((duration+5)~I(log(duration+5)), data_main)) # R^2 ~ 0.9502

# R^2 высокий в обоих случаях, поэтому исходные регрессоры нужно заменить логарифмами

# Зависимость salary от вещественных переменных(age, duration) c введением логарифмов

summary(lm(salary~wed1+wed2+wed3+duration+I(log(age+5))+sex+city_status+satisfaction+higher_educ+employees+foreign_owner+state_owner, data_main)) # R^2 ~ 0.2311
summary(lm(salary~wed1+wed2+wed3+I(log(duration+5))+age+sex+city_status+satisfaction+employees+higher_educ+foreign_owner+state_owner, data_main)) # R^2 ~ 0.2324 
summary(lm(salary~wed1+wed2+wed3+I(log(duration+5))+I(log(age+5))+sex+city_status+satisfaction+employees+higher_educ+foreign_owner+state_owner, data_main)) # R^2 ~ 0.2312

# Мы видим, что R^2 при добавлении регрессоров становится немного ниже или равен исходной модели, поэтому добавлять логарифмы в нашу модель не имеет смысла

# -------------------------------------Вводим степени от регрессоров (от 0.1 до 2 с шагом 0.1)

# Степени следует вводить только от вещественных переменных, так как смысла вводить их в дамми-переменные нет
# Добавим значение 5 к каждому регрессору и возведем их в степени от 0.1 до 2 с шагом 0.1

# Степень 0.1
model_degree_01 = lm(salary~wed1+wed2+wed3+I((duration+5)^0.1)+I((age+5)^0.1)+sex+higher_educ+city_status+satisfaction+employees+foreign_owner+state_owner, data_main)
summary(model_degree_01) # R^2 ~ 0.2314
vif(model_degree_01) # Коэффициенты VIF хорошие

# Степень 0.2
model_degree_02 = lm(salary~wed1+wed2+wed3+I((duration+5)^0.2)+I((age+5)^0.2)+sex+higher_educ+city_status+satisfaction+employees+foreign_owner+state_owner, data_main)
summary(model_degree_02) # R^2 ~ 0.2315
vif(model_degree_02) # Коэффициенты VIF хорошие

# Степень 0.3
model_degree_03 = lm(salary~wed1+wed2+wed3+I((duration+5)^0.3)+I((age+5)^0.3)+sex+higher_educ+city_status+satisfaction+employees+foreign_owner+state_owner, data_main)
summary(model_degree_03) # R^2 ~ 0.2317
vif(model_degree_03) # Коэффициенты VIF хорошие

# Степень 0.4
model_degree_04 = lm(salary~wed1+wed2+wed3+I((duration+5)^0.4)+I((age+5)^0.4)+sex+higher_educ+city_status+satisfaction+employees+foreign_owner+state_owner, data_main)
summary(model_degree_04) # R^2 ~ 0.2318 
vif(model_degree_04) # Коэффициенты VIF хорошие

# Степень 0.5
model_degree_05 = lm(salary~wed1+wed2+wed3+I((duration+5)^0.5)+I((age+5)^0.5)+sex+higher_educ+city_status+satisfaction+employees+foreign_owner+state_owner, data_main)
summary(model_degree_05) # R^2 ~ 0.2319
vif(model_degree_05) # Коэффициенты VIF хорошие

# Степень 0.6
model_degree_06 = lm(salary~wed1+wed2+wed3+I((duration+5)^0.6)+I((age+5)^0.6)+sex+city_status+higher_educ+satisfaction+employees+foreign_owner+state_owner, data_main)
summary(model_degree_06) # R^2 ~ 0.232
vif(model_degree_06) # Коэффициенты VIF хорошие

# Степень 0.7
model_degree_07 = lm(salary~wed1+wed2+wed3+I((duration+5)^0.7)+I((age+5)^0.7)+sex+higher_educ+city_status+satisfaction+employees+foreign_owner+state_owner, data_main)
summary(model_degree_07) # R^2 ~ 0.2321
vif(model_degree_07) # Коэффициенты VIF хорошие

# Степень 0.8
model_degree_08 = lm(salary~wed1+wed2+wed3+I((duration+5)^0.8)+I((age+5)^0.8)+sex+higher_educ+city_status+satisfaction+employees+foreign_owner+state_owner, data_main)
summary(model_degree_08) # R^2 ~ 0.2322
vif(model_degree_08) # Коэффициенты VIF хорошие

# Степень 0.9
model_degree_09 = lm(salary~wed1+wed2+wed3+I((duration+5)^0.9)+I((age+5)^0.9)+sex+higher_educ+city_status+satisfaction+employees+foreign_owner+state_owner, data_main)
summary(model_degree_09) # R^2 ~ 0.2323
vif(model_degree_09) # Коэффициенты VIF хорошие

# Степень 1 тоже самое, что и исходная модель, поэтому пропускаем

# Степень 1.1
model_degree_11 = lm(salary~wed1+wed2+wed3+I((duration+5)^1.1)+I((age+5)^1.1)+sex+higher_educ+city_status+satisfaction+employees+foreign_owner+state_owner, data_main)
summary(model_degree_11) # R^2 ~ 0.2324
vif(model_degree_11) # Коэффициенты VIF хорошие

# Степень 1.2
model_degree_12 = lm(salary~wed1+wed2+wed3+I((duration+5)^1.2)+I((age+5)^1.2)+sex+higher_educ+city_status+satisfaction+employees+foreign_owner+state_owner, data_main)
summary(model_degree_12) # R^2 ~ 0.2325
vif(model_degree_12) # Коэффициенты VIF хорошие

# Степень 1.3
model_degree_13 = lm(salary~wed1+wed2+wed3+I((duration+5)^1.3)+I((age+5)^1.3)+sex+higher_educ+city_status+satisfaction+employees+foreign_owner+state_owner, data_main)
summary(model_degree_13) # R^2 ~ 0.2325
vif(model_degree_13) # Коэффициенты VIF хорошие

# Степень 1.4
model_degree_14 = lm(salary~wed1+wed2+wed3+I((duration+5)^1.4)+I((age+5)^1.4)+sex+higher_educ+city_status+satisfaction+employees+foreign_owner+state_owner, data_main)
summary(model_degree_14) # R^2 ~ 0.2325
vif(model_degree_14) # Коэффициенты VIF хорошие

# Степень 1.5
model_degree_15 = lm(salary~wed1+wed2+wed3+I((duration+5)^1.5)+I((age+5)^1.5)+sex+higher_educ+city_status+satisfaction+employees+foreign_owner+state_owner, data_main)
summary(model_degree_15) # R^2 ~ 0.2326
vif(model_degree_15) # Коэффициенты VIF хорошие

# Степень 1.6
model_degree_16 = lm(salary~wed1+wed2+wed3+I((duration+5)^1.6)+I((age+5)^1.6)+sex+higher_educ+city_status+satisfaction+employees+foreign_owner+state_owner, data_main)
summary(model_degree_16) # R^2 ~ 0.2326
vif(model_degree_16) # Коэффициенты VIF хорошие

# Степень 1.7
model_degree_17 = lm(salary~wed1+wed2+wed3+I((duration+5)^1.7)+I((age+5)^1.7)+sex+higher_educ+city_status+satisfaction+employees+foreign_owner+state_owner, data_main)
summary(model_degree_17) # R^2 ~ 0.2326
vif(model_degree_17) # Коэффициенты VIF хорошие

# Степень 1.8
model_degree_18 = lm(salary~wed1+wed2+wed3+I((duration+5)^1.8)+I((age+5)^1.8)+sex+higher_educ+city_status+satisfaction+employees+foreign_owner+state_owner, data_main)
summary(model_degree_18) # R^2 ~ 0.2326
vif(model_degree_18) # Коэффициенты VIF хорошие

# Степень 1.9
model_degree_19 = lm(salary~wed1+wed2+wed3+I((duration+5)^1.9)+I((age+5)^1.9)+sex+higher_educ+city_status+satisfaction+employees+foreign_owner+state_owner, data_main)
summary(model_degree_19) # R^2 ~ 0.2326
vif(model_degree_19) # Коэффициенты VIF хорошие

# Степень 2
model_degree_20 = lm(salary~wed1+wed2+wed3+I((duration+5)^2)+I((age+5)^2)+sex+higher_educ+city_status+satisfaction+employees+foreign_owner+state_owner, data_main)
summary(model_degree_20) # R^2 ~ 0.2326
vif(model_degree_20) # Коэффициенты VIF хорошие

# Проанализировав ввод степеней в модель, мы приходим к выводу, что зависимость становится лучше при росте степени.
# Коэффициент VIF у всех моделей хорошие, поэтому приходим к выводу, что линейной зависимости между регрессорами нет.
# Самая лучшая модель по коэффициенту детерминации R^2 model_degree_20.

#----------------------Ввод произведения вещественных переменных

summary(lm(salary~wed1+wed2+wed3+I(duration*age)+sex+city_status+higher_educ+satisfaction+employees+foreign_owner+state_owner, data_main))
vif(lm(salary~wed1+wed2+wed3+I(duration*age)+sex+city_status+satisfaction+employees+foreign_owner+state_owner, data_main))

# В модели при введении произведения регрессоров коэффициент детерминации R^2 стал ниже, при этом коэффиенты вздутия дисперсии хорошие.

# !!!!!!!!!!!!!

# Лучшей моделью стала model_degree_20. Эта модель имеет самый высокий коэффициент детерминации R^2 и множество звездочек (значимость регрессоров хорошая).

model_ = lm(salary~wed1+wed2+wed3+I((duration+5)^2)+I((age+5)^2)+sex+higher_educ+city_status+satisfaction+employees+foreign_owner+state_owner, data_main)
summary(model_)

# Попробуем исключить wed1, wed2 из нашей модели

model_corrected = lm(salary~wed3+I((duration+5)^2)+I((age+5)^2)+sex+higher_educ+city_status+satisfaction+employees+foreign_owner+state_owner, data_main)

# R^2 упал незначительно, а максимальное кол-во звездочек теперь у всех регрессоров, значит wed1, wed2 можно исключить из нашей модели.

# model_corrected - модель, с которой будем работать в дальнейшем

#--------------------------------------Вывод о том, какие индивиды получают наибольшую зарплату------------------------------------------------------------

summary(model_corrected)

# Из команды summary(model_corrected) мы видим, что коэффициент регрессора:
# wed3 - отрицательный 
# I((duration + 5)^2) - положительный 
# I((age + 5)^2) - отрицательный
# sex - положительный
# higher_educ - положительный
# city_status - положительный
# satisfaction - положительный
# employees - положительный
# foreign_owner - положительный
# state_owner - отрицательный

# Вывод о том, какие индивиды получают большую зарплату: большую зарплату получают в большинстве своём мужчины с продолжительной рабочей неделей, имеющие высшее 
# образование, проживающие в городе, удовлетворённые своей заработной платой, имеющие подчиненных. При этом, если иностранные фирмы и частники являются 
# совладельцами или владельцами Вашего предприятия, то это положительно сказывается на уровне зааботной платы. Возраст не влияют на уровень заработанной платы, а если
# государство являются совладельцами или владельцами Вашего предприятия, то это отрицательно сказывается на уровне зааботной платы.

#-----------Оцените лучшие модели для подмножества индивидов, указанных в варианте. Сделайте вывод о том, какие индивиды получают наибольшую зарплату.------------

# Женщины не замужем---------------------------------------------------------------------------------------------------
data_woman = subset(data_main, sex==0) # Женщины

data_woman_moment = subset(data_woman, wed1==0) # Женщины не замужем
#data_woman_moment

# Возьмём выбранную мной откорректированную модель зависимости зарплаты от других параметров, и учтём, что находимся в подмножестве 
# женщин не замужем (в этом подмножестве переменные sex и wed1 равны единице).

model_subset1 = lm(salary~wed3+I((duration+5)^2)+I((age+5)^2)+higher_educ+city_status+satisfaction+employees+foreign_owner+state_owner, data_woman_moment)
summary(model_subset1) # R^2 ~ 21%
vif(model_subset1) # Коэффициенты VIF хорошие(невысокие) для всех регрессоров

# Женщины не замужем получают высокую заработную плату, если имеют диплом о высшем образовании, живут в городе, имеют подчиненных и собственником их предприятия
# является иностранная компания или частник. Такой вывод был сделан из коэффициентов регрессоров из команды summary(если высокие и положительные включаем в вывод).

# Женщины, живущие в городе, разведённые---------------------------------------------------------------------------------------------------

data_woman_city = subset(data_woman, city_status==0) # Женщины, живущие в городе

data_woman_city_moment = subset(data_woman_city, wed2==1) # Женщины, живущие в городе, разведённые

# Возьмём выбранную мной откорректированную модель зависимости зарплаты от других параметров, и учтём, что находимся в подмножестве 
# женщин, живущих в городе, разведённых (в этом подмножестве переменные sex, wed2, city_status равны единице).

model_subset1 = lm(salary~I((duration+5)^2)+I((age+5)^2)+higher_educ+satisfaction+employees+state_owner, data_woman_city_moment)
summary(model_subset1) # R^2 ~ 27%
vif(model_subset1) # Коэффициенты VIF хорошие(невысокие) для всех регрессоров

# Разведенные женщины, живущие в городе, получают высокую заработную плату, если имеют диплом о высшем образовании, удовлетворены своей работой и имеют своих подчиненных.
# Такой вывод был сделан из коэффициентов регрессоров из команды summary(если высокие и положительные включаем в вывод).

