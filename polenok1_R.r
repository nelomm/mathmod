#������� �.�., �-� 124 (������� 9)
#������� 1
#��� ������� 5 ����������� ����������� ������� � 2016 ����, ���� ��� �������� 
#������� ����� �������� ���������� �� ���������� 3 ����, 
#� ������������ � ������� �� ����� 250 ��

#������� ��������� ������
rm(list=ls())
#��������� ������� ����������
getwd()
#��������� ������� ���������� 
setwd("L:/work/mahach")

#��������� ������ ������
install.packages(c("rnoaa", "tidyverse", "lubridate"))
library(rnoaa)
library(tidyverse)
library(lubridate)


##1. ���������� ������ ������������ 
station_data = ghcnd_stations() 
write.csv(station_data,file = "station_data.csv")
station_data = read.csv("station_data.csv")

##2. ������������ ������ ������������
#����� ��������� ������ ���� �������, ������� ������ ������� 
#��������� � ������� ���������� ��������,������ ������� � ������ ������� � ������������ ��� �������
mahachkala = data.frame(id = "MAHACHKALA", latitude = 42.98,  longitude = 47.50)
mahachkala_around=meteo_nearby_stations(lat_lon_df = mahachkala, 
                                        station_data = station_data,
                                        limit = 15, var = c("PRCP", "TAVG"),
                                        year_min = 2013, year_max = 2015)

#��� ��������� ���� ������ � ������������, ���� �� �������������, ���������� ����. �������
mahachkala_id = mahachkala_around[["MAHACHKALA"]][["id"]][1]
summary(mahachkala_id)

#��� ��������� ������� �� ����� �������������� ������ ��������� 
#���������� ������� ������� ������ ������ �� ������
mahachkala_table = mahachkala_around[[1]]
summary(mahachkala_table)

#������� ������������, ����������� �� ���������� �� ����� 250 ��
mahachkala_stations = mahachkala_table %>% filter(distance < 250) 

#�������, ��� �������� �������������� ������
str(mahachkala_stations)

#������ �������� ������������ ������������� � ������� �� 250 �� 
#�� ���������
#������� �������������� �������������� ������������ 
mahachkala_stations$id

##3. ���������� �������� ������ ��� ����� ������������
#����� �������� ��� ������ � 1 ������������ ���������� ������� meteo_tidy_ghcnd
all_mahachkala_data=meteo_tidy_ghcnd(stationid = mahachkala_id)
#��������� ���������
summary(all_mahachkala_data)

#�������� ������, ���� ������� ��� ������ ���� ������������
all_mahachkala_meteodata = data.frame()

#������� ���� ��� ������������
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

#���������� ���������� ���������� 
write.csv(all_mahachkala_meteodata,"all_mahachkala_meteodata.csv")

#��������� ������
#mahachkala_meteodata=read.csv("all_mahachkala_meteodata.csv")
#��������� ��� ����������
str(all_mahachkala_meteodata)

#������� ���, ����� � ����
all_mahachkala_meteodata=all_mahachkala_meteodata %>% mutate(year=year(date), 
                                                         month=month(date), 
                                                         day=day(date))
#������� �� ���� ��� �������� Na � ��, ��� ������ 5 oC (tavg<5) 
all_mahachkala_meteodata[is.na(all_mahachkala_meteodata$tavg),"tavg"] = 0
all_mahachkala_meteodata[all_mahachkala_meteodata$tavg<5, "tavg"] = 0
summary(all_mahachkala_meteodata)

#����������� ������������ �� id,������� � ����� � ������������ �������������
#�� ���� �������, ����� ����������� ������ �� ������� � ������ ������� �� ������� 
#��� ���� ������������
group_meteodata =all_mahachkala_meteodata %>% group_by(id,year,month) 
sumT_group_meteodata = group_meteodata %>% summarise(tsum=sum(tavg))
groups_month=sumT_group_meteodata%>%group_by(month)
sumT_month=groups_month%>%summarise(St=mean(tsum))



##���������� � �������
#��������� ��� ����������� �����������
#����������� ��� ��������� ������
y = 1.0
#���������
afi = c(0.00, 0.00, 0.00, 32.11, 26.31, 25.64, 23.20, 18.73, 16.30, 13.83, 0.00, 0.00)
bfi = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03, 8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
#��������� ����� ���� i-�� ������, �������� � ������ ��������� ��������, � ������ ����� ���� � ������
di = c(0.00, 0.00, 0.00, 0.33, 1.00, 1.00, 1.00, 0.32, 0.00, 0.00, 0.00, 0.00)
#����������� ������������� ��� �������
Kf = 300
#������������ ������ ��������
Qj = 1600
#����������� "����� ������ �������� � �������� ���������
Lj = 2.2
#����������� "����������� ��������� ��������"
Ej = 25

##������
#���������� Fi �� �������
sumT_month =sumT_month %>% mutate(Fi = afi+bfi*y*St)
#���������� Yi
sumT_month = sumT_month %>% mutate( Yi = ((Fi*di)*Kf)/(Qj*Lj*(100-Ej)))
##����������� ������ 
Yield = (sum(sumT_month$Yi)) 
Yield
#�������� ������ � �/��
#� ����� ����������� �������� 14.97239 �/��

