setwd("C:/Users/Vsevolod/Desktop/MathModeling/1Local")
# ��� ������� 64 ����������� ����������� ������� � 2004 ����,
# ���� ��� �������� ������� ����� �������� ���������� �� ���������� 23 ����,
# � 2 ��������� ������������ �� ��������� ������� di ��������������, ��� ���� ������,
# ����� ������������� ����������� ���� ���� 8 ��������, �� ��������, ��� ����� �� ����� �������� ������ �������� ������,
# � ��������� ���������� 4 ������
# ������ 64 - �������, ���������� �������: 51.5405600, 46.0086100

#����������:
library(tidyverse)
library(rnoaa)
library(lubridate)

# �������� ������� � ������� ��� �������:
af = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bf = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
Kf = 300 #  ����������� ������������� ���
Qj = 1600 # ������������ ������ ��������
Lj = 2.2 #  ����� ������ �������� � �������� ���������
Ej = 25 #   ����������� ��������� ��������

# �������� ������ � �������������:
#station_data = ghcnd_stations()
# �������� � ���� "station_data.�sv".
#write.csv(station_data, "station_data.csv")

# �������� ������ � ������ ��� ���������� ������:
station_data = read.csv("station_data.csv")
# ������� �������� ������� � ���������� ������� �������:
zasratov = data.frame(id = "zasratov", latitude = 51.5405600,  longitude = 46.0086100)
# �������� ����� 2 ��������� ������������ � ������� �� �������������� ����������� �� 1980-2004 ���
#   � �������� � ������.
zasratov_around = meteo_nearby_stations(lat_lon_df = zasratov, station_data = station_data,
                                        limit = 2, var = "TAVG",
                                        year_min = 1980, year_max = 2003)

#������� ������ � 2 ������������
#�������� ������ ������� ���� ������� ������ � ������������
all_saratov_data = data.frame()
#��� ��������� ���� ������ � ������������, ���� �� �������������, ���������� ����
for(i in 1:2){
  #������� ������ �� ������ � ������� �� ��������� ����������
  temp = meteo_tidy_ghcnd(stationid = zasratov_around[["zasratov"]][["id"]][i], date_min="1980-01-1", date_max="2003-12-31")
  #������� ������ �������
  temp = select(temp, id, date, tavg)
  #����������� ���������� ������ � �������������� ������� � �������
  all_saratov_data = rbind(all_saratov_data, temp)
}

#����������� ���� � ����� � ���� � ������� ��������
all_saratov_data = mutate(all_saratov_data, year = year(date), month = month(date), day = day(date))
all_saratov_data_without0 = all_saratov_data #�������� ��������� ������� ��� ���������

#������� �������� ����������� � �������������� ������
all_saratov_data[(all_saratov_data$month < 4),"tavg"] = 0
all_saratov_data[(all_saratov_data$month > 8),"tavg"] = 0
all_saratov_data[(all_saratov_data$month == 4 & all_saratov_data$day <= 15),"tavg"] = 0
all_saratov_data[(all_saratov_data$month == 8 & all_saratov_data$day >= 15),"tavg"] = 0

#����������� �� ����� � ������� 
all_saratov_data = all_saratov_data %>% group_by(month)
all_saratov_data_without0 = all_saratov_data_without0 %>% group_by(month)

#�������� di ��� ������� ������
di = summarize(all_saratov_data, di = length(tavg[tavg>80])/length(tavg))[,-1]

#�������� ����� ���������� ������ 5 �������� � ������ ������ (����� �� 10 � ��� 20, ������ ��� ������ �� 20 ���)
St = summarize(all_saratov_data_without0, St = sum(tavg[tavg>50])/460)[,-1]


#������ ���������� �� �������:
Fi = af + bf * 1.0 * St
yield = sum(Fi*di*Kf/(Qj*Lj*(100-Ej)))
yield
