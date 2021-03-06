#������� �.�., �-� 124 (������� 8)
#������� 2
#�������� ������ ������������� �������� ��������� ������ ������� 
#����������� ���� �� �������� ������ 2013 ���� �� ������ ��������� 
#������� ������������ ���������
#������� ��������� ������
rm(list=ls())
#��������� ������� ����������
getwd()
#��������� ������� ���������� 
setwd("L:/work/mahach")

#��������� ������ ������
library(rnoaa)
library(tidyverse)
library(lubridate)
library(tidyr)
library(stringr)
library(dplyr)
library(tibble)
library(readr)

#1.����������� � �������
#�������� ���� �������, ��������� 1 � 3 �������, � ����� ������� �������� -9999 �������� NA
eddypro = read_csv("eddypro.csv", skip = 1, na =c("","NA","-9999","-9999.0"), comment=c("["))
#������ ������ ������ ������
eddypro = eddypro[-1,]
eddypro
#���������� ���� ���������� � ��� ����� ������������ �������� glimpse()
glimpse(eddypro)
#���������� roll �������� ������ NA, � ������ ����� ������ ������ ��� ��� �������.
#��������� �� ��� � ������� ������� select:
eddypro = select(eddypro, -(roll))
eddypro
#��������� ����������� �������� � �������� ������� �� ���������� ��� ���������� ��������
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
#������������� �������� drop_na(), ����� ���������� �� ���� �����, ��� ���� ���� ���� �������� NA
eddypro = drop_na(eddypro)
#����������� ������ �� �������� ������, � ������, �  1 ����� (60 ����) �� 31 ��� (151 ����)
eddypro = filter(eddypro, DOY >= 60 & DOY <= 151)
#����������� ������ ��� �o����� �������
eddypro = filter(eddypro, daytime==FALSE)
#������� cor �������� ������ � ���������� �������, �������, ����� ������� � ��������������� ������� 
#������������� ��������� is.numeric(), � sapply()
sapply(eddypro,is.numeric)
#��������� ���� ������ � ���� ������� � �������� ������� ���������� ������ �� ������������ ��� �������
eddypro_numeric = eddypro[,sapply(eddypro,is.numeric) ]
#�������� �������, ���������� ��� ��������� �������
eddypro_non_numeric = eddypro[,!sapply(eddypro,is.numeric) ]

#�������������� ������
cor_td=cor(eddypro_numeric)
cor_td
#���������� ���������� �������� ������ ���������������� �.�. 
#��� �������� � ���� �������, ������� ����������� ������� � �������, ������� ������������ ��� �������, 
#� �� ���� ������� ������ �� ����� �����(����������) ��� ������� �������� ������������ ������������ 
#���� ������ 0,1
cor_td = cor(drop_na(eddypro_numeric)) %>% as.data.frame %>% select(co2_flux)
vars = row.names(cor_td)[cor_td$co2_flux^2 > .1] %>% na.exclude
# ������� ��� ���������� �� ������� � ������� ���������� � ���� ������� ����� ��������� �������:
formula = as.formula(paste("co2_flux~", paste(vars,collapse = "+"), sep=""))
formula
#�������� ������������ ��������� ������� � �������������� ������� sample_n �� ������ dplyr
teaching_eddypro = sample_n(eddypro, floor(length(eddypro$co2_flux)*.7))
testing_eddypro = sample_n(eddypro, floor(length(eddypro$co2_flux)*.3))
#����������  ���������������� ����������
row_numbers = 1:length(eddypro$co2_flux)
teach = sample(row_numbers, floor(length(eddypro$co2_flux)*.7))
test = row_numbers[-teach]
teaching_tbl = eddypro_numeric[teach,]
testing_tbl = eddypro_numeric[test,]

#2. ���������� �������
#������ 1 �� ��������� ������� � ������ ���� ����������, ���������� � ���������� formula
mod1 = lm(formula , data = teaching_tbl)
names(mod1)
#��������� ���������� � ������
summary(mod1)
#��������� ������������
coef(mod1)
#������� �������
resid(mod1)
#������������� ��������
confint(mod1)
#������������� ������
anova(mod1)
#��������� ������� ������:
plot(mod1)

#������ 2 - �������� ������ � ������� � �� ����������, ���������� ��� ������� ������� anova() � �������������
#���������� ������ 0.01: � ��������� "***", "**" � "*"
mod2 = lm(co2_flux ~ rand_err_H + rand_err_LE + co2_flux + rand_err_co2_flux + 
  rand_err_h2o_flux + co2_molar_density + co2_mole_fraction + 
  co2_mixing_ratio + sonic_temperature + air_temperature + 
  air_density + air_molar_volume + es + T_star_ + un_H + un_LE + 
  un_co2_flux + w_spikes + ts_var + w_div_ts_cov + co2 + co2_1, data = teaching_tbl)
#��������� ���������� � ������
summary(mod2)
#��������� ������������
coef(mod2)
#������� �������
resid(mod2)
#������������� ��������
confint(mod2)
#������������� ������
anova(mod2)
#������� ������ 2 � 1
anova(mod2, mod1)
#��������� ������� ������:
plot(mod2)

#�������� ������ 3, � ������� � �� ����������, ���������� ��� ������� ������� anova() � ������������
#���������� ����� 0.001: � ��������� "***" � "**" 
mod3 = lm(co2_flux ~ rand_err_H + rand_err_LE + co2_flux + rand_err_co2_flux + 
               rand_err_h2o_flux + co2_molar_density + co2_mole_fraction + 
               co2_mixing_ratio + sonic_temperature + air_temperature + 
               air_density + air_molar_volume + es + T_star_ + un_H + un_LE + 
               un_co2_flux + w_spikes + ts_var + w_div_ts_cov + co2, data = teaching_tbl)

#��������� ���������� � ������
summary(mod3)
#��������� ������������
coef(mod3)
#������� �������
resid(mod3)
#������������� ��������
confint(mod3)
#������������� ������
anova(mod3)
#������� ������ 2 � 3
anova(mod3, mod2)
#��������� �������
plot(mod3)


#�������� ������ 4� ������� � �� ����������, ���������� ��� ������� ������� anova() � ��������� "***" 
mod4 = lm(co2_flux ~ rand_err_H + rand_err_LE + co2_flux + rand_err_co2_flux + 
            rand_err_h2o_flux + co2_molar_density + co2_mole_fraction + 
            co2_mixing_ratio + sonic_temperature + air_temperature + 
            air_molar_volume + es + T_star_ + un_H + un_LE + 
            un_co2_flux + co2, data = teaching_tbl)

#��������� ���������� � ������
summary(mod4)
#��������� ������������
coef(mod4)
#������� �������
resid(mod4)
#������������� ��������
confint(mod4)
#������������� ������
anova(mod4)
# ������� ������ 4 � 3
anova(mod4, mod3)
#��������� �������
plot(mod4)

#3. �������������� ������ ����������
#��������� ������ �� ����������, ������� ��������� � �������������� �������
cor_teaching_tbl = select(teaching_tbl, rand_err_H, rand_err_LE, co2_flux, rand_err_co2_flux , 
                            rand_err_h2o_flux, co2_molar_density, co2_mole_fraction, 
                            co2_mixing_ratio, sonic_temperature, air_temperature, 
                            air_molar_volume, es, T_star_, un_H, un_LE, 
                            un_co2_flux )


#�������� ������� ������������� ����������
cor_eddypro = cor(cor_teaching_tbl) %>% as.data.frame
#�������� ������� ������������� ����������
cor_eddypro = cor(cor_teaching_tbl) %>% as.data.frame
#���������� �������� �� ���������� ������
#�������� ������ co2_flux �� co2_flux, ����������� ��������, ���������� �� ������ 4, � �� ������ ��������� �������
qplot(co2_flux, co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod4, teaching_tbl)))
#������ ���������� ��� ����� 45 �������� � �������� ����� ����� ��� ����� 
#�������� ������ h2o_flux �� h2o_flux, ����������� ��������, ���������� �� ������ 4, � �� ������ ����������� �������
qplot(co2_flux, co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod4, teaching_tbl)))
#��� ������� ������� ��������� �������� ������������ ���������� co2_flux ��: sonic_temperature, co2_mixing_ratio, air_molar_volume, 
#un_co2_flux �� ������ ����������� ������
qplot(sonic_temperature,co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod4, teaching_tbl)))
qplot(co2_mixing_ratio,co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod4, teaching_tbl)))
qplot(air_molar_volume,co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod4, teaching_tbl)))
qplot(un_co2_flux,co2_flux, data = teaching_tbl) + geom_line(aes(y = predict(mod4, teaching_tbl)))

                          