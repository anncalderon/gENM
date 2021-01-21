plan <- drake_plan(
  
## Munge data
  files = list.files(path="data", pattern = "^hf147-1(.*).csv$", 
                     full.names = TRUE),
  data.raw = load_data(files),
  Data = clean_data(data.raw),
## Contour Plot
  fig.cp = plot_contour(Data),
## Histogram plot  
  ## aph_hist = ggplot(Data, aes(x=Year)) +
  ##   geom_histogram(binwidth = 28, fill="darkcyan", col="Black", size=1, alpha=0.65) + 
  ##   theme(panel.background = element_rect(fill="darkseagreen1")) +
  ##   labs(title = "Aphaenogaster Observations", y="Number of Observations"),

  ## BClim = brick("data/YbrevBC_2.5.grd"),
## Render report
  report = rmarkdown::render(
                          knitr_in("report.Rmd"),
                          output_format = "pdf_document",
                          quiet = TRUE)
)
