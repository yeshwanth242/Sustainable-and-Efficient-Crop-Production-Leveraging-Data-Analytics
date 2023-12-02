library('tidyverse')  
library('data.table')
library("plotly")
library("highcharter")
library('dplyr') 
library('readr') 
library('skimr') # overview
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('purrr') # data wrangling
library('stringr') # string manipulation
library('forcats') # factor manipulation
library('janitor') # data cleaning
library('Tmisc') # data manipulation
library('class') # functions for classification
library('tm')

crops<-fread("D:/College/INT - 500/Code/datasets/India Agriculture Crop Production.csv")
head(crops)
colnames(crops)<-tolower(colnames(crops))
colSums(is.na(crops))
colSums(crops==0)
unique(crops$state)

#Renaming some state names to match with highchart mapping
crops$state[crops$state=="Arunachal Pradesh"]<-"Arunanchal Pradesh"
crops$state[crops$state=="CHANDIGARH"]<-"Chandigarh"
crops$state[crops$state=="Laddak"]<-"Ladakh"
crops$state[crops$state=="Delhi"]<-"NCT of Delhi"
crops$state[crops$state=="Andaman and Nicobar Island"]<-"Andaman and Nicobar"

# Replace "THE DADRA AND NAGAR HAVELI" and "DADRA AND NAGAR HAVELI" with "Dadara and Nagar Havelli"
crops$state <- ifelse(crops$state %in% c("THE DADRA AND NAGAR HAVELI", "DADRA AND NAGAR HAVELI"), 
                      "Dadara and Nagar Havelli", crops$state)

crops$production[is.na(crops$production)]<-0
unique(crops$state)

# Which state has highest production for "whole year"
highest_production_whole_year<-crops %>% 
  select(state, season,production, yield)

highest_production_whole_year<-highest_production_whole_year %>% 
  group_by(state, season) %>%
  filter(season=="Whole Year") %>% 
  summarise(across(everything(), sum, na.rm=TRUE), .groups = 'drop') %>% 
  arrange(desc(production))

hcmap("countries/in/custom/in-all-disputed",
      data=highest_production_whole_year,
      value="production",
      joinBy = c("name", "state"),
      bordercolor="lightgrey",
      borderwidth=0.3) %>% 
  hc_title(text="State-wise Production For Whole Year") %>% 
  hc_colorAxis(type = "logarithmic",stops=color_stops()) 


# Calculation of production for all seasons (graphs and maps)
## Kharif
unique(crops$season) 
kharif<-crops %>% 
  filter(season=="Kharif")

kharif<-kharif %>% 
  select(state, season, production)

kharif<-kharif %>% 
  group_by(state, season) %>% 
  summarise(across(everything(), sum, na.rm=TRUE), .groups='drop')

hcmap("countries/in/custom/in-all-disputed",
      data=kharif,
      value="production",
      joinBy = c("name", "state"),
      bordercolor="black",
      borderwidth=0.3) %>% 
  hc_title(text="Kharif Season Production (start in June and end in October)") %>% 
  hc_colorAxis(type = "logarithmic",
               stops=color_stops()) 

##Kharif top10
top_10_kharif<-kharif %>% 
  arrange(desc(production)) %>% 
  slice(1:10)
highchart() %>% 
  hc_chart(type="column") %>% 
  hc_title(text="Top 10 states With Highest Production in Kharif") %>% 
  hc_xAxis(categories=top_10_kharif$state, crosshair=TRUE) %>% 
  hc_yAxis(min=0, title=list(text="Production")) %>% 
  hc_add_series(data=top_10_kharif$production, name="States")

## Rabi
rabi<-crops %>% 
  filter(season=="Rabi")

rabi<-rabi %>% 
  select(state, season, production)

rabi<-rabi %>% 
  group_by(state, season) %>% 
  summarise(across(everything(), sum, na.rm=TRUE), .groups='drop')

hcmap("countries/in/custom/in-all-disputed",
      data=rabi,
      value="production",
      joinBy = c("name", "state"),
      bordercolor="black",
      borderwidth=0.3) %>% 
  hc_title(text="Rabi Season Production (October to March)") %>% 
  hc_colorAxis(type = "logarithmic",
               stops=color_stops())

##Rabi top10
top_10_rabi<-rabi %>% 
  arrange(desc(production)) %>% 
  slice(1:10)

highchart() %>% 
  hc_chart(type="column") %>% 
  hc_title(text="Top 10 states With Highest Production in Rabi") %>% 
  hc_xAxis(categories=top_10_rabi$state, crosshair=TRUE) %>% 
  hc_yAxis(min=0, title=list(text="Production")) %>% 
  hc_add_series(data=top_10_rabi$production, name="States")

## Autumn
autumn<-crops %>% 
  filter(season=="Autumn")

autumn<-autumn %>% 
  select(state, season, production)

autumn<-autumn %>% 
  group_by(state, season) %>% 
  summarise(across(everything(), sum, na.rm=TRUE), .groups='drop')

hcmap("countries/in/custom/in-all-disputed",
      data=autumn,
      value="production",
      joinBy = c("name", "state"),
      bordercolor="black",
      borderwidth=0.3) %>% 
  hc_title(text="Autumn Season Production") %>% 
  hc_colorAxis(type = "logarithmic",
               stops=color_stops())

##autumn top10
top_10_autumn<-autumn %>% 
  arrange(desc(production)) %>% 
  slice(1:10)

highchart() %>% 
  hc_chart(type="column") %>% 
  hc_title(text="Top 10 states With Highest Production in Autumn") %>% 
  hc_xAxis(categories=top_10_autumn$state, crosshair=TRUE) %>% 
  hc_yAxis(min=0, title=list(text="Production")) %>% 
  hc_add_series(data=top_10_autumn$production, name="States")

## Summer
summer<-crops %>% 
  filter(season=="Summer")

summer<-summer %>% 
  select(state, season, production)

summer<-summer %>% 
  group_by(state, season) %>% 
  summarise(across(everything(), sum, na.rm=TRUE), .groups='drop')

hcmap("countries/in/custom/in-all-disputed",
      data=summer,
      value="production",
      joinBy = c("name", "state"),
      bordercolor="black",
      borderwidth=0.3) %>% 
  hc_title(text="Summer Season Production") %>% 
  hc_colorAxis(type = "logarithmic",
               stops=color_stops())

##summer top10
top_10_summer<-summer %>% 
  arrange(desc(production)) %>% 
  slice(1:10)

highchart() %>% 
  hc_chart(type="column") %>% 
  hc_title(text="Top 10 states With Highest Production in Summer") %>% 
  hc_xAxis(categories=top_10_summer$state, crosshair=TRUE) %>% 
  hc_yAxis(min=0, title=list(text="Production")) %>% 
  hc_add_series(data=top_10_summer$production, name="States")

## Winte
winter<-crops %>% 
  filter(season=="Winter")

winter<-winter %>% 
  select(state, season, production)

winter<-winter %>% 
  group_by(state, season) %>% 
  summarise(across(everything(), sum, na.rm=TRUE), .groups='drop')

hcmap("countries/in/custom/in-all-disputed",
      data=winter,
      value="production",
      joinBy = c("name", "state"),
      bordercolor="black",
      borderwidth=0.3) %>% 
  hc_title(text="Winter Season Production") %>% 
  hc_colorAxis(type = "logarithmic",
               stops=color_stops())

##winter top10
top_10_winter<-winter %>% 
  arrange(desc(production)) %>% 
  slice(1:10)

highchart() %>% 
  hc_chart(type="column") %>% 
  hc_title(text="Top 10 states With Highest Production in Winter") %>% 
  hc_xAxis(categories=top_10_winter$state, crosshair=TRUE) %>% 
  hc_yAxis(min=0, title=list(text="Production")) %>% 
  hc_add_series(data=top_10_winter$production, name="States")

# Which crop has highest production in each stat
all_crops <- crops %>% 
  select(state, crop, production, yield) %>% 
  group_by(state, crop) %>% 
  summarise(across(everything(), sum, na.rm = TRUE), .groups = 'drop')

highest_crop_production_of_each_state <- all_crops %>% 
  group_by(state) %>% 
  filter(production == max(production))

highest_crop_production_of_each_state <- highest_crop_production_of_each_state %>% 
  mutate(crop_production = paste0(crop,":<br>", production))

hcmap("countries/in/custom/in-all-disputed",
      data = highest_crop_production_of_each_state,
      value = "production",
      joinBy = c("name", "state"),
      borderColor = "grey",
      borderWidth = 0.3) %>% 
  hc_title(text = "States with respective highest crop production") %>% 
  hc_colorAxis(
    dataClasses = list(
      list(from = 0, to = 200000, color = "#deffff"),
      list(from = 200000, to = 5200000, color = "#b8e0e4"),
      list(from = 5200001, to = 30000000, color = "#93c1ca"),
      list(from = 30000001, to = 200000000, color = "#70a3b2"),
      list(from = 200000001, to = 1500000000, color = "#4f859b"),
      list(from = 1500000001, to = 26000000000, color = "#2d6884"),
      list(from = 26000000001, to = 129607125000, color = "#004c6d")
    )
  ) %>% 
  hc_tooltip(
    useHTML = TRUE,
    formatter = JS(
      "function(){",
      "  return '<b><u>'+this.point.name+'</u></b><br>'",
      "         +'<span style=\"color:#A020F0\"> '+this.point.crop_production+' </span>';",
      "}"
    )
  ) %>% 
  hc_legend(enabled = FALSE)

# Season Productio
season_productivity<-crops %>% 
  select(season, production) %>% 
  group_by(season) %>% 
  summarise(across(everything(), sum, na.rm=TRUE)) %>% 
  filter(season!="Whole Year")


highchart() %>%
  hc_chart(type = "pie", options3d = list(enabled = TRUE, alpha = 45, beta = 0)) %>%
  hc_plotOptions(pie=list( 
    depth= 45)) %>% 
  hc_title(text = "Season-wise Production") %>%
  hc_add_series(
    name = "Production",
    data = list_parse2(season_productivity),
    size = "60%",
    dataLabels = list(format = "{point.name}: {point.percentage:.1f}%")
  )

# Which year has highest production
year_production<-crops %>% 
  select(year, production) %>% 
  group_by(year) %>% 
  summarise(across(everything(), sum, na.rm=T))

view(year_production)

top_year<-year_production %>% 
  arrange(desc(production)) %>% 
  slice(1:10)

highchart() %>%
  hc_chart(type = "column", options3d = list(enabled = TRUE, alpha = 20, beta = 15)) %>%
  hc_title(text = "Top 10 Years") %>%
  hc_xAxis(categories=top_year$year) %>% 
  hc_add_series(
    name = "Production of Each Year",
    data = top_year$production,
    size = "100%",
    dataLabels = list(format = "{point.name}: {point.percentage:.1f}%")) %>% 
  hc_add_theme(hc_theme_sandsignika())


# Crops production for each season over years
seasonal_production<-crops %>% 
  select(year, season, production) %>% 
  group_by(year, season) %>% 
  summarise(across(everything(), sum, na.rm=T))

new_seasonal_data <- pivot_wider(data = seasonal_production, names_from = season, values_from = production)


## Create function: accumulate_by
accumulate_by = function(dat, var) {
  var <- lazyeval::f_eval(var, dat)
  lvls <- plotly:::getLevels(var)
  dats <- lapply(seq_along(lvls), function(x) {
    cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
  })
  dplyr::bind_rows(dats)
  
}

nsd <- new_seasonal_data %>%
  accumulate_by(~year)

nsd<-ungroup(nsd)

pltly = plot_ly() %>%
  add_trace(
    x = ~ year,
    y = ~ Autumn,
    name = "Autumn",
    frame = ~ frame,
    type = 'scatter',
    mode = 'lines',
    data = nsd,
    opacity = 1.0
  ) %>%
  add_trace(
    x = ~ year,
    y = ~ Kharif,
    name = "Kharif",
    frame = ~ frame,
    type = 'scatter',
    mode = 'lines',
    data = nsd,
    opacity = 1.0) %>% 
  add_trace(
    x = ~ year,
    y = ~ Rabi,
    name = "Rabi",
    frame = ~ frame,
    type = 'scatter',
    mode = 'lines',
    data = nsd,
    opacity = 1.0) %>% 
  add_trace(
    x = ~ year,
    y = ~ Summer,
    name = "Summer",
    frame = ~ frame,
    type = 'scatter',
    mode = 'lines',
    data = nsd,
    opacity = 1.0) %>% 
  add_trace(
    x = ~ year,
    y = ~ Winter,
    name = "Winter",
    frame = ~ frame,
    type = 'scatter',
    mode = 'lines',
    data = nsd,
    opacity = 1.0) %>% 
  layout(title = "Crops Production By Season Over Years") %>%
  animation_opts(frame = 85,
                 transition = 0,
                 redraw = F) %>%
  animation_slider(hide = T) %>%
  animation_button(
    x = 1,
    xanchor = "right",
    y = 0,
    yanchor = "bottom"
  ) %>%
  layout(xaxis = list(title = "Year"),
         yaxis = list(title = "Production"))

pltly










