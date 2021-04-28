#Поленок В.П., Д-Х 124 (Вариант 9)
#Задание 1
#для региона 5 рассчитайте урожайность пшеницы в 2016 году, взяв для рассчета 
#средние суммы активных температур за предыдущие 3 года, 
#с метеостанций в радиусе не более 250 км

#Очистим полностью память
rm(list=ls())
#Проверяем рабочую директорию
getwd()
#Указываем рабочую директорию 
setwd("L:/work/mahach")

#Подключим нужные пакеты
install.packages(c("rnoaa", "tidyverse", "lubridate"))
library(rnoaa)
library(tidyverse)
library(lubridate)


##1. Скачивание списка метеостанций 
station_data = ghcnd_stations() 
write.csv(station_data,file = "station_data.csv")
station_data = read.csv("station_data.csv")

##2. Формирование списка метеостанций
#После получения списка всех станций, находим список станций 
#ближайших к столице Республики Дагестан,создав таблицу с именем региона и координатами его столицы
mahachkala = data.frame(id = "MAHACHKALA", latitude = 42.98,  longitude = 47.50)
mahachkala_around=meteo_nearby_stations(lat_lon_df = mahachkala, 
                                        station_data = station_data,
                                        limit = 15, var = c("PRCP", "TAVG"),
                                        year_min = 2013, year_max = 2015)

#Для получения всех данных с метеостанции, зная ее идентификатор, используем след. команду
mahachkala_id = mahachkala_around[["MAHACHKALA"]][["id"]][1]
summary(mahachkala_id)

#Для получения таблицы со всеми метеостанциями вокруг Махачкалы 
#необходимо выбрать целиком первый объект из списка
mahachkala_table = mahachkala_around[[1]]
summary(mahachkala_table)

#Отберем метеостанции, находящиеся на расстоянии не более 250 км
mahachkala_stations = mahachkala_table %>% filter(distance < 250) 

#Смотрим, что содержит сформированный список
str(mahachkala_stations)

#Список содержит метеостанций расположенных в радиусе до 250 км 
#от Махачкалы
#Выведем индетификаторы отфильрованных метеостанций 
mahachkala_stations$id

##3. Скачивание погодных данных для наших метеостанций
#Чтобы получить все данные с 1 метеостанции используем команду meteo_tidy_ghcnd
all_mahachkala_data=meteo_tidy_ghcnd(stationid = mahachkala_id)
#Проверяем скаченное
summary(all_mahachkala_data)

#Создадим объект, куда скачаем все данные всех метеостанций
all_mahachkala_meteodata = data.frame()

#Создаем цикл для метеостанций
stations_names=mahachkala_stations$id
stations_names=stations_names[1:15]

for (sname in stations_names)
{ one_meteo=meteo_tidy_ghcnd (stationid = sname,
                            date_min = "2013-01-01",
                            date_max = "2015-12-31")
station_vars=names(one_meteo)
if (!("tavg" %in% station_vars)){
  if(!("tmax"%in% station_vars)){
    next()
  }
  one_meteo = one_meteo %>% mutate(tavg=(tmax+tmin)/2)}

one_meteo = one_meteo %>% select(id,date,tavg)
one_meteo = one_meteo %>% mutate(tavg=tavg/10)

all_mahachkala_meteodata=rbind(all_mahachkala_meteodata, one_meteo)}

#Записываем полученные результаты 
write.csv(all_mahachkala_meteodata,"all_mahachkala_meteodata.csv")

#Считываем данные
#mahachkala_meteodata=read.csv("all_mahachkala_meteodata.csv")
#Проверяем что получилось
str(all_mahachkala_meteodata)

#Добавим год, месяц и день
all_mahachkala_meteodata=all_mahachkala_meteodata %>% mutate(year=year(date), 
                                                         month=month(date), 
                                                         day=day(date))
#Заменим на нули все значения Na и те, что меньше 5 oC (tavg<5) 
all_mahachkala_meteodata[is.na(all_mahachkala_meteodata$tavg),"tavg"] = 0
all_mahachkala_meteodata[all_mahachkala_meteodata$tavg<5, "tavg"] = 0
summary(all_mahachkala_meteodata)

#Сгруппируем метеостанции по id,месяцам и годам и просуммируем температатуру
#по этим группам, затем сгурппируем данные по месяцам и найдем среднее по месяцам 
#для всех метеостанций
group_meteodata =all_mahachkala_meteodata %>% group_by(id,year,month) 
sumT_group_meteodata = group_meteodata %>% summarise(tsum=sum(tavg))
groups_month=sumT_group_meteodata%>%group_by(month)
sumT_month=groups_month%>%summarise(St=mean(tsum))



##Подготовка к расчёту
#Константы для определения урожайности
#коэффициент для экпозиции склона
y = 1.0
#Константы
afi = c(0.00, 0.00, 0.00, 32.11, 26.31, 25.64, 23.20, 18.73, 16.30, 13.83, 0.00, 0.00)
bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
#отношение числа дней i-го месяца, входящих в период вегетации культуры, к общему числу дней в месяце
di = c(0.00, 0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
#Коэффициент использования ФАР посевом
Kf = 300
#Калорийность урожая культуры
Qj = 1600
#Коэффициент "Сумма частей основной и побочной продукции
Lj = 2.2
#Коэффициент "Стандартная влажность культуры"
Ej = 25

##Расчет
#Рассчитаем Fi по месяцам
sumT_month =sumT_month %>% mutate(Fi = afi+bfi*y*St)
#Рассчитаем Yi
sumT_month = sumT_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))
##Расчитываем урожай 
Yield = (sum(sumT_month$Yi)) 
Yield
#Получаем урожай в ц/га
#В итоге урожайность составит 14.97239 ц/га

