#Поленок В.П., Д-Х 124 (Вариант 8)
#Задание 2
#создайте модель множественной линейной регрессии ночных потоков 
#углекислого газа за весенний период 2013 года по данным измерений 
#методом турбулентной пульсации
#Очистим полностью память
rm(list=ls())
#Проверяем рабочую директорию
getwd()
#Указываем рабочую директорию 
setwd("L:/work/mahach")

#Подключим нужные пакеты
library(rnoaa)
library(tidyverse)
library(lubridate)
library(tidyr)
library(stringr)
library(dplyr)
library(tibble)
library(readr)

#1.МАНИПУЛЯЦИИ С ДАННЫМИ
#Загрузим нашу таблицу, пропустив 1 и 3 строчки, а также заменив значения -9999 символом NA
eddypro = read_csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
#Удалим пустую первую строку
eddypro = eddypro[-1,]
eddypro
#Просмотрим сами переменные и для этого воспользуеся функцией glimpse()
glimpse(eddypro)
#Переменная roll содержит только NA, а потому будет только мешать нам при анализе.
#Избавимся от нее с помощью функции select:
eddypro = select(eddypro, -(roll))
eddypro
#Изменение специальных символов в названии стобцов на допустимые для переменных названия
names(eddypro) = names(eddypro) %>% 
  str_replace_all("[!]","_emph_") %>% 
  str_replace_all("[?]","_quest_") %>% 
  str_replace_all("[*]","_star_") %>% 
  str_replace_all("[+]","_plus_") %>%
  str_replace_all("[-]","_minus_") %>%
  str_replace_all("[@]","_at_") %>%
  str_replace_all("[$]","_dollar_") %>%
  str_replace_all("[#]","_hash_") %>%
  str_replace_all("[/]","_div_") %>%
  str_replace_all("[%]","_perc_") %>%
  str_replace_all("[&]","_amp_") %>%
  str_replace_all("[\\^]","_power_") %>%
  str_replace_all("[()]","_") 
glimpse(eddypro)
#Воспользуемся функцией drop_na(), чтобы избавиться от всех строк, где есть хоть одно значение NA
eddypro = drop_na(eddypro)
#Отфильтруем данные за весенний период, а именно, с  1 марта (60 день) по 31 мая (151 день)
eddypro = filter(eddypro, DOY >= 60 & DOY <= 151)
#Отфильтруем данные для нoчного периода
eddypro = filter(eddypro, daytime==FALSE)
#Функция cor работает только с численными данными, поэтому, чтобы перейти к корелляционному анализу 
#Воспользуемся функциями is.numeric(), и sapply()
sapply(eddypro,is.numeric)
#Подставим этот вектор в саму таблицу и получить таблицу состояющую только из интересующих нас колонок
eddypro_numeric = eddypro[,sapply(eddypro,is.numeric) ]
#Получаем таблицу, содержащую все остальные колонки
eddypro_non_numeric = eddypro[,!sapply(eddypro,is.numeric) ]

#Корелляционный анализ
cor_td=cor(eddypro_numeric)
cor_td
#Полученные результаты довольно тяжело интерпретировать т.к. 
#они выдаются в виде матрицы, поэтому преобразуем матрицу в таблицу, выберем интересующий нас столбец, 
#а из него возьмем только те имена строк(переменных) для которых значения коэффициента детерминации 
#было больше 0,1
cor_td = cor(drop_na(eddypro_numeric)) %>% as.data.frame %>% select(co2_flux)
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude
# Собрать все переменные из вектора с именами переменных в одну формулу можно следующим образом:
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep=""))
formula
#Создадим произвольные обучающие выборки с использованием команды sample_n из пакета dplyr
teaching_eddypro = sample_n(eddypro, floor(length(eddypro$co2_flux)*.7))
testing_eddypro = sample_n(eddypro, floor(length(eddypro$co2_flux)*.3))
#Сформируем  непересекающиеся подвыборки
row_numbers = 1:length(eddypro$co2_flux)
teach = sample(row_numbers, floor(length(eddypro$co2_flux)*.7))
test = row_numbers[-teach]
teaching_tbl = eddypro_numeric[teach,]
testing_tbl = eddypro_numeric[test,]

#2. ПОСТРОЕНИЕ МОДЕЛЕЙ
#Модель 1 по обучающей выборки с учетом всех переменных, отраженных в переменной formula
mod1 = lm(formula , data = teaching_tbl)
names(mod1)
#Посмотрим информацию о модели
summary(mod1)
#Посмотрим коэффициенты
coef(mod1)
#Выведем остатки
resid(mod1)
#доверительный интервал
confint(mod1)
#дисперсионный анализ
anova(mod1)
#Посмотрим графики модели:
plot(mod1)

#Модель 2 - создадим модель и добавим в неё переменные, полученные при помощии функции anova() с коэффициентом
#значимости меньше 0.01: с пометками "***", "**" и "*"
mod2 = lm(co2_flux ~ rand_err_H + rand_err_LE + co2_flux + rand_err_co2_flux + 
  rand_err_h2o_flux + co2_molar_density + co2_mole_fraction + 
  co2_mixing_ratio + sonic_temperature + air_temperature + 
  air_density + air_molar_volume + es + T_star_ + un_H + un_LE + 
  un_co2_flux + w_spikes + ts_var + w_div_ts_cov + co2 + co2_1, data = teaching_tbl)
#Посмотрим информацию о модели
summary(mod2)
#Посмотрим коэффициенты
coef(mod2)
#Выведем остатки
resid(mod2)
#доверительный интервал
confint(mod2)
#дисперсионный анализ
anova(mod2)
#Сравним модели 2 и 1
anova(mod2, mod1)
#Посмотрим графики модели:
plot(mod2)

#Построим модель 3, и добавим в неё переменные, полученные при помощии функции anova() с коэффииентом
#значимости менше 0.001: с пометками "***" и "**" 
mod3 = lm(co2_flux ~ rand_err_H + rand_err_LE + co2_flux + rand_err_co2_flux + 
               rand_err_h2o_flux + co2_molar_density + co2_mole_fraction + 
               co2_mixing_ratio + sonic_temperature + air_temperature + 
               air_density + air_molar_volume + es + T_star_ + un_H + un_LE + 
               un_co2_flux + w_spikes + ts_var + w_div_ts_cov + co2, data = teaching_tbl)

#Посмотрим информацию о модели
summary(mod3)
#Посмотрим коэффициенты
coef(mod3)
#Выведем остатки
resid(mod3)
#Доверительный интервал
confint(mod3)
#Дисперсионный анализ
anova(mod3)
#Сравним модели 2 и 3
anova(mod3, mod2)
#Посмотрим графики
plot(mod3)


#Построим модель 4и добавим в неё переменные, полученные при помощии функции anova() с пометками "***" 
mod4 = lm(co2_flux ~ rand_err_H + rand_err_LE + co2_flux + rand_err_co2_flux + 
            rand_err_h2o_flux + co2_molar_density + co2_mole_fraction + 
            co2_mixing_ratio + sonic_temperature + air_temperature + 
            air_molar_volume + es + T_star_ + un_H + un_LE + 
            un_co2_flux + co2, data = teaching_tbl)

#Посмотрим информацию о модели
summary(mod4)
#Посмотрим коэффициенты
coef(mod4)
#Выведем остатки
resid(mod4)
#доверительный интервал
confint(mod4)
#дисперсионный анализ
anova(mod4)
# Сравним модели 4 и 3
anova(mod4, mod3)
#Посмотрим графики
plot(mod4)

#3. КОРРЕЛЯЦИОННЫЙ АНАЛИЗ ПЕРЕМЕННОЙ
#Обозначим только те переменные, которые участвуют в корреляционном анализе
cor_teaching_tbl = select(teaching_tbl, rand_err_H, rand_err_LE, co2_flux, rand_err_co2_flux , 
                            rand_err_h2o_flux, co2_molar_density, co2_mole_fraction, 
                            co2_mixing_ratio, sonic_temperature, air_temperature, 
                            air_molar_volume, es, T_star_, un_H, un_LE, 
                            un_co2_flux )


#Получаем таблицу коэффициентов корреляции
cor_eddypro = cor(cor_teaching_tbl) %>% as.data.frame
#Получаем таблицу коэффициентов корреляции
cor_eddypro = cor(cor_teaching_tbl) %>% as.data.frame
#Построение графиков по полученной модели
#Построим график co2_flux от co2_flux, использовав значения, полученные на модели 4, и на основе обучающей выборки
qplot(co2_flux, co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod4, teaching_tbl)))
#График расположен под углом 45 градусов и проходит почти через все точки 
#Построим график h2o_flux от h2o_flux, использовав значения, полученные на модели 4, и на основе тестирующей выборки
qplot(co2_flux, co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod4, teaching_tbl)))
#Для примера выведем несколько графиков зависимостей переменной co2_flux от: sonic_temperature, co2_mixing_ratio, air_molar_volume, 
#un_co2_flux на основе тестирующей модели
qplot(sonic_temperature,co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod4, teaching_tbl)))
qplot(co2_mixing_ratio,co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod4, teaching_tbl)))
qplot(air_molar_volume,co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod4, teaching_tbl)))
qplot(un_co2_flux,co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod4, teaching_tbl)))

                          