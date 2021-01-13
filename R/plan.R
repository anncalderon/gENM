plan <- drake_plan(
  
  files = list.files(path="data", pattern = "^hf147-1(.*).csv$", 
                     full.names = TRUE),
  
  Data = sapply(files, read.csv, simplify=FALSE) %>% bind_rows(.id = "id") %>% 
    select(latitude, longitude, ant.genus, ant.species) %>% 
    as.data.frame() %>% na.omit() %>% filter(ant.genus=="Aphaenogaster") %>%
    select(-ant.genus),
  
  Data[1822, c("Month", "Day", "Year")]<- c(03,08,1878),
  Data[1823, c("Month", "Day", "Year")]<- c(03,08,1878),
  Data[1824, c("Month", "Day", "Year")]<- c(04,29,1889),
  Data[1825, c("Month", "Day", "Year")]<- c(05,10,1896),
  Data[1826, c("Month", "Day", "Year")]<- c(05,01,1898),
  Data[1827, c("Month", "Day", "Year")]<- c(05,01,1898),
  Data[1828, c("Month", "Day", "Year")]<- c(07,14,1899),
  Data <- transform(Data, Year = as.numeric(Year)),
  
  Data$Year_Bin <- cut(Data$Year, 28),
  data.loess <- loess(Year ~ longitude * latitude, data = Data),       
  xgrid <-  seq(min(Data$longitude), max(Data$longitude), 0.5),
  ygrid <-  seq(min(Data$latitude), max(Data$latitude), 0.5),  
  data.fit <-  expand.grid(longitude = xgrid, latitude = ygrid),  
  mtrx3d <-  predict(data.loess, newdata = data.fit),  
  mtrx.melt <- melt(mtrx3d, id.vars = c('longitude' , 'latitude'), measure.vars =('Year_Bin')),
  names(mtrx.melt) <- c('longitude', 'latitude', 'Year_Bin'),
  mtrx.melt$longitude <- as.numeric(str_sub(mtrx.melt$longitude, str_locate(mtrx.melt$longitude, '=')[1,1] + 1)),  
  mtrx.melt$latitude <- as.numeric(str_sub(mtrx.melt$latitude, str_locate(mtrx.melt$latitude, '=')[1,1] + 1)),
  
  sampling_distribution_year = ggplot(mtrx.melt, aes(x = longitude, y = latitude, z = Year_Bin)) +
    stat_contour() + geom_point(data=Data, aes(color=Year_Bin)) + labs(title='Aphaenogaster ssp. Sampling Distribution by Year'),
  
  aph_hist <- ggplot(Data, aes(x=Year)) +
    geom_histogram(binwidth = 28, fill="darkcyan", col="Black", size=1, alpha=0.65) + 
    theme(panel.background = element_rect(fill="darkseagreen1")) +
    labs(title = "Aphaenogaster Observations", y="Number of Observations"),
   
  BClim = brick("data/YbrevBC_2.5.grd")
  
)
