#' clean_data: detect and fix or remove errors in data
#' x = data object
#'

clean_data <- function(x){
    
    x[1822, c("Month", "Day", "Year")]<- c(03,08,1878)
    x[1823, c("Month", "Day", "Year")]<- c(03,08,1878)
    x[1824, c("Month", "Day", "Year")]<- c(04,29,1889)
    x[1825, c("Month", "Day", "Year")]<- c(05,10,1896)
    x[1826, c("Month", "Day", "Year")]<- c(05,01,1898)
    x[1827, c("Month", "Day", "Year")]<- c(05,01,1898)
    x[1828, c("Month", "Day", "Year")]<- c(07,14,1899)
    x <- transform(x, Year = as.numeric(Year))

    return(x)

}

#' subset_species: subsets a dataframe for a species of interest
#' x = "species name"
#'example: picea <- subset_species("picea") stores a dataframe of unique coordinates

subset_species <- function(species=""){
                  subset(Data, ant.species==species) %>% 
                  select(-ant.species) %>% unique()}

#' map_occurrences: creates a distribution map of species presence data
#' x = longitudinal range
#' y = latitudinal range 
#' z = "Graph Title"
#' example: map_occurrences(df = picea, xrange = c(min(picea$longitude), max(picea$longitude)),
#'          yrange = c(min(picea$latitude), max(picea$latitude)),
#'          title = "Picea Presences") returns a map with points
#' source: The Molecular Ecologist          

map_occurrences <- function(df, xrange, yrange, title=""){
  data("stateMapEnv")
  plot(xrange, yrange, mar=par("mar"),xlab="longitude", ylab="latitude",
       xaxt="n", yaxt="n", type="n", main=title)
  rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col="slategray2")
  map("state", xlim=xrange, ylim=yrange, fill=T, col="lavenderblush1", add=T)
  points(df$longitude, df$latitude, col="darkolivegreen4", pch=20, cex=0.5)
  axis(1,las=1)
  axis(2,las=1)
  box()}


#' plotly_map_occurrences: creates a distribution map of species presence data using plotly
#' x = Data frame
#' example: plotly_map_occurrences(df = Data)

plotly_map_occurrences <- function(df){
  plot_ly(df, lat=~latitude, lon=~longitude,  
          marker = list(color = "fuchsia"),
          type = 'scattermapbox',
          hovertext =df[,c('longitude','latitude')]) %>%
    layout(mapbox = list(style = 'open-street-map', zoom =4.5,
                         center = list(lon = -71.81, lat = 43.64)))}
