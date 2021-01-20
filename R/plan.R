plan <- drake_plan(
  
  files = list.files(path="data", pattern = "^hf147-1(.*).csv$", 
                     full.names = TRUE),
  
  Data = sapply(files, read.csv, simplify=FALSE) %>% bind_rows(.id = "id") %>% 
    select(latitude, longitude, ant.genus, ant.species, date) %>% 
    as.data.frame() %>% na.omit() %>% filter(ant.genus=="Aphaenogaster") %>%
    select(-ant.genus) %>% separate(date, c('Month', 'Day', 'Year'),'/'),
  
## Cleaning  
  Data = clean_data(Data),
## Contour Plots
  plot_contour(data),
## Histogram plot  
  aph_hist = ggplot(Data, aes(x=Year)) +
    geom_histogram(binwidth = 28, fill="darkcyan", col="Black", size=1, alpha=0.65) + 
    theme(panel.background = element_rect(fill="darkseagreen1")) +
    labs(title = "Aphaenogaster Observations", y="Number of Observations"),
   
  BClim = brick("data/YbrevBC_2.5.grd")
  
)
