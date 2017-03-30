#App Dependencies

#Packages required to run the app
library(ggplot2)
library(plyr)
library(shiny)

#Reading to the csv file and correcting labels, formats, etc. 
agridata = read.csv('1934to1937+1943to2015.csv')
agridata$DoP = as.Date(agridata$DoP, format='%d-%b')
agridata$DoH = as.Date(agridata$DoH, format='%d-%b')
agridata = rename(agridata, c('Yield' = 'Yield_in_bushels_per_acre', 'Moisture' = 'Moisture_content', 
               'Erect' = 'Erect_plants',  'RM' = 'Relative_Moisture', 
               'PPA' = 'Plants_per_Acre', 'DoP' = 'Date_of_Planting', 
               'DoH' = 'Date_of_Harvest'))
agridata$IncRate.Exp.Reg = NULL #for now

#To extract the names of the variables
Variable_Extractor <- function(dataset) {
  vars = names(dataset)
  for (i in vars) {
    if (is.factor(dataset[[i]][1]) == TRUE) {
      vars = vars[vars != i]
    }
  }
  return(vars)
}

#To identify min and max values
Identify_Min_Max <- function(dataset, var) { #var must be a string column name
  a = min(dataset[[var]], na.rm = TRUE)
  b = max(dataset[[var]], na.rm = TRUE)
  return(c(a,b))
}

#To extract all field names in that dataset
Field_Name_Extractor <- function(dataset) {
  unique_fields = unique(dataset$Field)
  return(as.character(levels(unique_fields)))
}

#To create a title for the graphs/files
Title_Generator <- function(x, y) {     #x & y are strings; for histograms, enter y as NULL
  if (is.na(y)) {
    title = x
  } else {
    title = paste0(y, '_vs._', x)
  }
  return(title)
}

#To generate the hover text for scatterplots/boxplots and transform it to HTML
HTML_HoverPoint_Text <- function(dataset, point) {
  vars = names(dataset)
  html_text = "<font size='1'>"
  for (i in vars) {
    html_text = paste(html_text, '<b>', i, ': ', '</b>', point[[i]], '<br/>', sep = '')
  }
  html_text = paste(html_text, '</font>', sep = '')
  return(html_text)
}

#To generate the text for hovering over histograms
HTML_HoverHistogram_Text <- function(dataset, index) { #specifically a ggplot_build() dataset
  html_text = "<font size='1'>"
  html_text = paste0(html_text, '<b>Range: </b>', 'From ', format(dataset$xmin[[index]], format="%B %d"), ' to ', 
                     format(dataset$xmax[[index]], format="%B %d"), '</br>',
                     '<b>Count: </b>', dataset$count[[index]])
  html_text = paste0(html_text, '</font>')
}

