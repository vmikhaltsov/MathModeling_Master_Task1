setwd("C:/Users/Vsevolod/Desktop/MathModeling/1Local")
# для региона 64 рассчитайте урожайность пшеницы в 2004 году,
# взяв для рассчета средние суммы активных температур за предыдущие 23 года,
# с 2 ближайших метеостанций но рассчитав колонку di самостоятельно, как долю месяца,
# когда среднедневные температуры были выше 8 градусов, но учитывая, что посев не может начаться раньше середины апреля,
# а вегетация составляет 4 месяца
# Регион 64 - Саратов, координаты столицы: 51.5405600, 46.0086100

#Библиотеки:
library(tidyverse)
library(rnoaa)
library(lubridate)

# Создадим векторы с данными для расчета:
af = c(0.00,0.00,0.00,32.11, 26.31,25.64,23.20,18.73,16.30,13.83,0.00,0.00)
bf = c(0.00, 0.00, 0.00, 11.30, 9.26, 9.03,8.16, 6.59, 5.73, 4.87, 0.00, 0.00)
Kf = 300 #  Коэффициент использования ФАР
Qj = 1600 # калорийность урожая культуры
Lj = 2.2 #  сумма частей основной и побочной продукции
Ej = 25 #   стандартная влажность культуры

# Загрузим данные о метеостанциях:
#station_data = ghcnd_stations()
# Сохраним в файл "station_data.сsv".
#write.csv(station_data, "station_data.csv")

# Сохраним данные в вектор для дальнейшей работы:
station_data = read.csv("station_data.csv")
# Зададим название вектора и координаты столицы региона:
zasratov = data.frame(id = "zasratov", latitude = 51.5405600,  longitude = 46.0086100)
# Выполним поиск 2 ближайших метеостанций с данными по среднемесячной температуре за 1980-2004 год
#   и сохраним в вектор.
zasratov_around = meteo_nearby_stations(lat_lon_df = zasratov, station_data = station_data,
                                        limit = 2, var = "TAVG",
                                        year_min = 1980, year_max = 2003)

#Получим данные с 2 метеостанций
#Создадим пустую таблицу куда запишем данные с метеостанций
all_saratov_data = data.frame()
#Для получения всех данных с метеостанции, зная ее идентификатор, используем цикл
for(i in 1:2){
  #Получим данные со стании и запишем во временную переменную
  temp = meteo_tidy_ghcnd(stationid = zasratov_around[["zasratov"]][["id"]][i], date_min="1980-01-1", date_max="2003-12-31")
  #Оставим нужные столбцы
  temp = select(temp, id, date, tavg)
  #Присоединим полученные данные в результирующую таблицу с данными
  all_saratov_data = rbind(all_saratov_data, temp)
}

#преобразуем дату в месяц и день и добавим столбики
all_saratov_data = mutate(all_saratov_data, year = year(date), month = month(date), day = day(date))
all_saratov_data_without0 = all_saratov_data #сохраним резервную таблицу без обнуления

#обнулим значение температуры в невегетативный период
all_saratov_data[(all_saratov_data$month < 4),"tavg"] = 0
all_saratov_data[(all_saratov_data$month > 8),"tavg"] = 0
all_saratov_data[(all_saratov_data$month == 4 & all_saratov_data$day <= 15),"tavg"] = 0
all_saratov_data[(all_saratov_data$month == 8 & all_saratov_data$day >= 15),"tavg"] = 0

#Сгруппируем по годам и месяцам 
all_saratov_data = all_saratov_data %>% group_by(month)
all_saratov_data_without0 = all_saratov_data_without0 %>% group_by(month)

#Вычислим di для каждого месяца
di = summarize(all_saratov_data, di = length(tavg[tavg>80])/length(tavg))[,-1]

#Вычислим сумму температур больше 5 градусов в каждом месяце (делим на 10 и еще 20, потому что данные за 20 лет)
St = summarize(all_saratov_data_without0, St = sum(tavg[tavg>50])/460)[,-1]


#Найдем урожаность по формуле:
Fi = af + bf * 1.0 * St
yield = sum(Fi*di*Kf/(Qj*Lj*(100-Ej)))
yield
