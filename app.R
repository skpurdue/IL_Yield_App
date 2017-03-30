#Illinois Yield Trials Interactive Visualizer

# Built and deployed on RStudio's Shiny
# Data collected and compiled by undergraduate researchers 
#  at RDCEP, Computation Institute, The University of Chicago
# Code written during the summer of 2016 by Sydney Purdue
#  Contact at skpurdue@uchicago.edu

#Notes:
# This app currently works with a specific set of data compiled from
# Illinois agricultural records spanning over 100 years. As with anything,
# it is still in progress and will likely be added to periodically. Currently,
# documentation exists in this file for the app functionalities that exist (3/30/2017)

#Call to file with crucial package prerequisites and helper functions
source("/users/Sydney/IL-yield-trials/R_Scripts/AppDependencies.R")

ui <- fluidPage(
  #Formatting
  titlePanel('Illinois Yield Trials'),
  sidebarLayout(position = 'left',
                sidebarPanel(
                  
                  p('Welcome to the Illinois Yield Trial Visualizer, currently for 
                    corn agricultural data. This is still in progress, so please 
                    forgive any bugs or missing components.'),
                  
                  br(),
                  
                  #Graph type is selected
                  selectInput('type', 'Choose type of graph', 
                              c('Choose one' = '','Scatterplot' = 'scatter', 'Histogram' = 'histogram', 'Boxplot' = 'box')),
                  
                  #Dependent on kind of graph, different variables and sliders appear.
                  #Conditional panels must have their conditions written in JavaScript.
                  #Separated into xvar, yvar, and var for ease of working with the histograms.
                  #Need to find a way to do an automated list of variables
                  conditionalPanel(condition = 'input.type == "box" || input.type == "scatter"',
                                   selectInput('xvar', 'Choose x-axis variable', 
                                               c('Choose one' = '', Variable_Extractor(agridata))),
                                   uiOutput('xslider'),
                                   selectInput('yvar', 'Choose y-axis variable', 
                                               c('Choose one' = '', Variable_Extractor(agridata))),
                                   uiOutput('yslider')),
                  
                  #To control box width on boxplots
                  conditionalPanel(condition = 'input.type == "box"', 
                                   numericInput('box_width', 'Choose box width', 
                                                value = 1, min = 1, max = NA)),                 
                  
                  #The variables for histograms
                  conditionalPanel(condition = 'input.type == "histogram"',
                                   selectInput('var', 'Choose variable', 
                                               c('Choose one' = '', Variable_Extractor(agridata))),
                                   uiOutput('histslider'),
                                   numericInput('bin_width', 'Enter binwidth', 
                                                value = 5, min = 0, max = NA)),
                  
                  #To download image of the plot
                  br(),
                  checkboxInput('to_get_to_download', 'Download plot', FALSE),
                  conditionalPanel(condition = 'input.to_get_to_download == true',
                                   selectInput('choose_img_type', 'Choose image type for download',
                                               c('Emulated Post-Script' = 'eps', 'Post-Script' = 'ps',
                                                 'LaTeX (pictex)' = 'tex', 'PDF' = 'pdf', 'JPEG' = 'jpeg', 
                                                 'TIFF' = 'tiff', 'PNG' = 'png', 'BMP' = 'bmp', 'SVG' = 'svg',
                                                 'WMF (Windows only)' = 'wmf'),
                                               selected = 'png'),
                                   downloadButton('actually_download', 'Download'),
                                   br(),
                                   br()),
                  
                  #To freeze the axes at their original scale
                  checkboxInput('freeze', 'Freeze axes', FALSE),
                  conditionalPanel(condition = 'input.freeze == true && input.type != "histogram"',
                                   actionButton('freeze_x', 'Freeze x-axis'),
                                   br(),
                                   br(),
                                   actionButton('freeze_y', 'Freeze y-axis'),
                                   br(),
                                   br()),
                  conditionalPanel(condition = 'input.freeze == true && input.type == "histogram"',
                                   actionButton('freeze_axis', 'Freeze axis'),
                                   br(),
                                   br()),
                  
                  #To get the options for subgrouping, trendlines, whole graph coloring
                  checkboxInput('toggle_options', 'More options', FALSE),
                  
                  conditionalPanel(condition = 'input.toggle_options == true',
                                   actionButton("reset_excluded_points", "Reset excluded points"),
                                   br(),
                                   br(),
                                   p('Use this tool to reset all points that have been excluded 
                                     from the dataset'),
                                   br()),
                  
                  #To get options for subgrouping by variables
                  conditionalPanel(condition = 'input.toggle_options == true && input.color_by_category != true',
                                   checkboxInput('tosubornottosub', 'Create subgroup(s)', FALSE),
                                   p('Use this tool to create up to three colored subgroups to compare
                                     more variables. This cannot be used with the color-by-a-single-variable tool.'),
                                   br()),
                  
                  #Can have up to 3 subgroups
                  conditionalPanel(condition = 'input.toggle_options == true && input.tosubornottosub == true',
                                   selectInput('num_subgroups', 'Choose the number of subgroups you would like to make',
                                               c('Choose one' = '', '1' = 1, '2' = 2, '3' = 3))),
                  
                  #Variable choices for subgroup 1
                  conditionalPanel(condition = 'input.toggle_options == true && input.tosubornottosub == true && input.num_subgroups >= 1', 
                                   selectInput('sub1', 'Select variable', 
                                               c('Choose one' = '', 'Field' = 'Field', Variable_Extractor(agridata)))),
                  
                  #See server below
                  uiOutput('fields1'),
                  uiOutput('slider1condition'),
                  
                  #Variable choices for subgroup 2
                  conditionalPanel(condition = 'input.toggle_options == true && input.tosubornottosub == true && input.num_subgroups >= 2', 
                                   selectInput('sub2', 'Select variable', 
                                               c('Choose one' = '', 'Field' = 'Field', Variable_Extractor(agridata)))),
                  
                  #See server below
                  uiOutput('fields2'),
                  uiOutput('slider2condition'),
                  
                  #Variable choices for subgroup 3
                  conditionalPanel(condition = 'input.toggle_options == true && input.tosubornottosub == true && input.num_subgroups == 3', 
                                   selectInput('sub3', 'Select variable', 
                                               c('Choose one' = '', 'Field' = 'Field', Variable_Extractor(agridata)))),
                  
                  #See server below
                  uiOutput('fields3'),
                  uiOutput('slider3condition'),
                  
                  #To reset excluded points and create a trendline or density curve
                  conditionalPanel(condition = 'input.toggle_options == true',
                                   checkboxInput('trend', 'Create trendline', FALSE),
                                   p('Use this tool to create a trendline (for scatterplots and boxplots) 
                                     or smoothing curve (for historgrams).'),
                                   br()),
                  
                  #Get polynomial degree to plot trendline
                  conditionalPanel(condition = 'input.toggle_options == true && input.trend == true && input.type != "histogram"',
                                   numericInput('poly_input', "Enter degree of polynomial to fit",
                                                value = 1, min = 1, max = 10)),
                  
                  #Display the trendline's confidence interval
                  conditionalPanel(condition = 'input.toggle_options == true && input.trend == true && input.type != "histogram"',
                                   checkboxInput('conf_int', 
                                                 'Display confidence interval with trend line',
                                                 FALSE),
                                   br()),
                  
                  #Color the entire graph by a single variable
                  conditionalPanel(condition = 'input.toggle_options == true && input.tosubornottosub != true',
                                   checkboxInput('color_by_category', 
                                                 'Color whole plot by one variable',
                                                 FALSE),
                                   p('Use this tool to color the entire graph by one variable.
                                     This cannot be used with the subgrouping tool.'),
                                   br()),
                  
                  #Variable choices for coloring by a variable
                  conditionalPanel(condition = 'input.toggle_options == true && input.color_by_category == true',
                                   selectInput('colorvar',
                                               'Choose which variable to use',
                                               c('Choose one' = '', 'Field' = 'Field', Variable_Extractor(agridata))))
                  
                  ),
                
                #To get the plot in the main panel
                #With conditions for clicking/brushing/hovering, etc.
                mainPanel(
                  div(
                    style = "position:relative",
                    plotOutput('plot',
                               click = "clicking",
                               brush = brushOpts(
                                 id = "brushing",
                                 resetOnNew = TRUE),
                               hover = hoverOpts(
                                 id = "hovering", delay = 100, 
                                 delayType = "debounce")),
                    #Output for the hovering panel
                    uiOutput("hover_info"))
                )
                
                  )
  
                  )

server <- function(input, output) {
  
  #SLIDING
  
  #Creating slider for the x-axis for scatter/boxplots
  output$xslider <- renderUI({
    #Call to AppDependencies.R to find min and max values of the variables
    valx <- Identify_Min_Max(agridata, toString(input$xvar))
    #Slider conditional on having selected an option and being the right kind of plot
    conditionalPanel(condition = 'input.xvar != "" && (input.type == "scatter" || input.type == "box")',
                     sliderInput(inputId = 'sliderx', 
                                 label = paste("Select values for ", input$xvar, sep = ''),
                                 min=valx[1], 
                                 max=valx[2], 
                                 value=valx,
                                 sep= ''))
  })
  
  #Creating slider for the y-axis for scatter/boxplots
  output$yslider <- renderUI({
    valy <- Identify_Min_Max(agridata, toString(input$yvar))
    conditionalPanel(condition = 'input.yvar != "" && (input.type == "scatter" || input.type == "box")',
                     sliderInput(inputId = 'slidery', 
                                 label = paste("Select values for ", input$yvar, sep = ''),
                                 min=valy[1], 
                                 max=valy[2], 
                                 value=valy,
                                 sep= ''))
  })
  
  #Creating slider for the axis of the histograms
  output$histslider <- renderUI({
    valhist <- Identify_Min_Max(agridata, toString(input$var))
    conditionalPanel(condition = 'input.var != "" && input.type == "histogram"',
                     sliderInput(inputId = 'sliderhist', 
                                 label = paste("Select values for ", input$var, sep = ''),
                                 min=valhist[1], 
                                 max=valhist[2], 
                                 value=valhist,
                                 sep= ''))
  })
  
  
  #SUBGROUPING
  
  #Creating slider for the subgroup variable by same method as for axis variables
  #Works if subgrouping number is 1 or larger
  #Works if subgrouping variable is not Field
  output$slider1condition <- renderUI ({
    if (input$sub1 != 'Field') {
      subslide1 = Identify_Min_Max(agridata, toString(input$sub1))
      conditionalPanel(condition = 'input.toggle_options == true && input.tosubornottosub == true && input.sub1 != "" && input.num_subgroups >= 1',
                       sliderInput(inputId = 'slider1', 
                                   label = paste("Select values for ", input$sub1, sep = ''),
                                   min=subslide1[1], 
                                   max=subslide1[2], 
                                   value=subslide1,
                                   sep= ''))}
  })
  
  #Creating a drop-down menu for if subgroup variable is Field
  output$fields1 <- renderUI ({
    conditionalPanel(condition = 'input.toggle_options == true && input.tosubornottosub == true && input.sub1 != "" && input.num_subgroups >= 1 && input.sub1 == "Field"',
                     selectInput(inputId = 'field_list1',
                                 label = 'Choose field',
                                 Field_Name_Extractor(agridata)))
  })
  
  #Creating slider for the subgroup variable by same method as for axis variables
  #Works if subgrouping number is 2 or larger
  #Works if subgrouping variable is not Field
  output$slider2condition <- renderUI ({
    if (input$sub2 != 'Field') {
      subslide2 = Identify_Min_Max(agridata, toString(input$sub2))
      conditionalPanel(condition = 'input.toggle_options == true && input.tosubornottosub == true && input.sub2 != "" && input.num_subgroups >= 2',
                       sliderInput(inputId = 'slider2', 
                                   label = paste("Select values for ", input$sub2, sep = ''),
                                   min=subslide2[1], 
                                   max=subslide2[2], 
                                   value=subslide2,
                                   sep= ''))}
    
  })
  
  #Creating a drop-down menu for if subgroup variable is Field
  output$fields2 <- renderUI ({  
    conditionalPanel(condition = 'input.toggle_options == true && input.tosubornottosub == true && input.sub2 != "" && input.num_subgroups >= 2 && input.sub2 == "Field"',
                     selectInput(inputId = 'field_list2',
                                 label = 'Choose field',
                                 Field_Name_Extractor(agridata)))
  })
  
  #Creating slider for the subgroup variable by same method as for axis variables
  #Works if subgrouping number is 3
  #Works if subgrouping variable is not Field
  output$slider3condition <- renderUI ({
    if (input$sub3 != 'Field') {
      subslide3 = Identify_Min_Max(agridata, toString(input$sub3))
      conditionalPanel(condition = 'input.toggle_options == true && input.tosubornottosub == true && input.sub3 != "" && input.num_subgroups == 3',
                       sliderInput(inputId = 'slider3', 
                                   label = paste("Select values for ", input$sub3, sep = ''),
                                   min=subslide3[1], 
                                   max=subslide3[2], 
                                   value=subslide3,
                                   sep= ''))}
  })
  
  #Creating a drop-down menu for if subgroup variable is Field
  output$fields3 <- renderUI ({
    conditionalPanel(condition = 'input.toggle_options == true && input.tosubornottosub == true && input.sub3 != "" && input.num_subgroups >= 3 && input.sub3 == "Field"',
                     selectInput(inputId = 'field_list3',
                                 label = 'Choose field',
                                 Field_Name_Extractor(agridata)))
  })
  
  
  #CLICKING/BRUSHING
  
  #Creating a value that will respond/update as changes are made
  #Specfically making a list as long as the dataset of boolean values (specfically TRUE)
  #to track the datapoints that have been clicked or not
  react_exclude <- reactiveValues(
    keeprows = rep(TRUE, nrow(agridata))
  )
  
  #When clicked, a new column is added to the dataset: "selected_".
  #This column takes booleans TRUE or FALSE (TRUE if clicked).
  #This column is compared with keeprows, and
  #if a row is in keeprows and FALSE in havebeenclicked (so not clicked),
  #then it is kept in keeprows.
  observeEvent(input$clicking, {
    if (input$type != 'histogram') {
      havebeenclicked = nearPoints(agridata, input$clicking, maxpoints = 1, allRows = TRUE)
      react_exclude$keeprows = xor(react_exclude$keeprows, havebeenclicked$selected_)}
  })
  
  #Same as directly above, but with brushed points
  observeEvent(input$brushing, {
    if (input$type != 'histogram') {
      havebeenbrushed = brushedPoints(agridata, input$brushing, allRows = TRUE)
      react_exclude$keeprows = xor(react_exclude$keeprows, havebeenbrushed$selected_)}
  })
  
  #To reset excluded points, keeprows is redeclared
  observeEvent(input$reset_excluded_points, {
    react_exclude$keeprows <- rep(TRUE, nrow(agridata))
  })
  
  #HOVERING
  
  #For histogram hovering: data generation and cleaning from the plot made with ggplot_build
  hist_data <- reactive({
    if (input$type == 'histogram') {
      build_data = ggplot_build(plt())$data[[1]]
      
      #To format dates correctly
      if (input$var == 'Date_of_Planting' | input$var == 'Date_of_Harvest') {
        build_data_xmin = build_data$xmin
        build_data_xmax = build_data$xmax
        build_data$xmin = as.Date(build_data_xmin, origin = '1970-01-01')
        build_data$xmax = as.Date(build_data_xmax, origin = '1970-01-01')
      }
      
      return(build_data)
    }
  })
  
  #Code largely sourced from https://gitlab.com/snippets/16220
  #Many comments are from the developer; those not are signed
  output$hover_info <- renderUI({
    hover = input$hovering
    hover_x = hover$x
    hover_y = hover$y
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # background color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    
    if (input$type != 'histogram') {
      point <- nearPoints(agridata, input$hovering, maxpoints = 1, addDist = TRUE)
      
      if (nrow(point) == 0) {
        return(NULL)}
      
      # actual tooltip created as wellPanel
      wellPanel(
        style = style,
        #Edit to read text provided in AppDependencies.R -SKP
        p(HTML(HTML_HoverPoint_Text(agridata, point))))
    } else if (input$type == 'histogram') {
      #To find the index of the row in hist_data that corresponds with the tooltip -SKP
      found_row_index = min(which(hist_data()$xmax >= hover_x))
      
      if (found_row_index == Inf) {
        return(NULL)
      } else {
        if (hover_y <= hist_data()$count[[found_row_index]]) {
          # actual tooltip created as wellPanel
          wellPanel(
            style = style,
            #Edit to read text provided in AppDependencies.R -SKP
            p(HTML(HTML_HoverHistogram_Text(hist_data(), found_row_index))))
        }
      }
    }
    
  })
  
  
  
  #TITLING
  
  #A reactive to generate a title for the graph/file
  #Responds differently for histograms and scatter/boxplots
  react_title <- reactive({
    if (input$type == 'histogram') {
      Title_Generator(input$var, NA)
    } else {
      Title_Generator(input$xvar, input$yvar)
    }
  })
  
  
  #DOWNLOADING
  
  #Actually does the downloading
  output$actually_download <- downloadHandler(
    #Gets filename from the graph title + the image type chosen
    filename = function() {
      paste(react_title(), input$choose_img_type, sep='.')
    },
    #Saves with ggplot2 function, based on image type chosen
    content = function(file) {
      ggsave(file, device = input$choose_img_type)
    }
  )
  
  
  #AXIS-FREEZING
  
  #Declares axis limits to be overwritten if axes are frozen
  react_lims = reactiveValues(
    xaxis_lims = c(0, 0),
    yaxis_lims = c(0, 0),
    histaxis_lims = c(0, 0)
  )
  
  #If freeze_axis is selected, the histograms axis will not automatically adjust
  #when the slider is changed. It will stay at the previous scale.
  #This will be updated if it is unclicked and reclicked
  observeEvent(input$freeze_axis, {
    react_lims$histaxis_lims = c(input$sliderhist[1], input$sliderhist[2])
  })
  
  #Same as freeze_axis, but on the x-axis for scatterplots and boxplots
  observeEvent(input$freeze_x, {
    react_lims$xaxis_lims = c(input$sliderx[1], input$sliderx[2])
  })
  
  #Same as freeze_axis, but on the y-axis for scatterplots and boxplots
  observeEvent(input$freeze_y, {
    react_lims$yaxis_lims = c(input$slidery[1], input$slidery[2])
  })
  
  
  
  
  #PLOTTING
  
  #A larger plotting function than you could ever possibly want
  #Put into a reactive to get access to the information outside of the plotting
  plt <- reactive({
    
    #Separating rows that're excluded and rows not excluded
    keep = agridata[react_exclude$keeprows, , drop = FALSE]
    remove = agridata[!react_exclude$keeprows, , drop = FALSE]
    
    #Building it all from here
    p <- ggplot()
    
    #To subset keep and remove based off of the sliders
    if (input$type == 'scatter' | input$type == 'box') {
      agridatasub <- subset(keep, keep[[input$xvar]] >= input$sliderx[1] & keep[[input$xvar]] <= input$sliderx[2])
      agridatasub <- subset(agridatasub, agridatasub[[input$yvar]] >= input$slidery[1] & agridatasub[[input$yvar]] <= input$slidery[2])
      agridatasubremove <- subset(remove, remove[[input$xvar]] >= input$sliderx[1] & remove[[input$xvar]] <= input$sliderx[2])
      agridatasubremove <- subset(agridatasubremove, agridatasubremove[[input$yvar]] >= input$slidery[1] & agridatasubremove[[input$yvar]] <= input$slidery[2])
    } else if (input$type == 'histogram') {
      agridatasub <- subset(agridata, agridata[[input$var]] >= input$sliderhist[1] & agridata[[input$var]] <= input$sliderhist[2])}
    
    #To plot the individual plot types of keep and remove
    if (input$type == 'scatter') {
      p <- p + geom_point(data=agridatasub, aes_string(x=input$xvar, y=input$yvar)) +
        geom_point(data=agridatasubremove, aes_string(x=input$xvar, y=input$yvar), 
                   color = 'tomato', alpha = .4, fill = NA)
    } else if (input$type == 'histogram') {
      p <- p + geom_histogram(data=agridatasub, aes_string(x=input$var), binwidth = input$bin_width, 
                              color = 'black', fill = 'gray75')
    } else if (input$type == 'box') {
      p <- p + geom_boxplot(data=agridatasub, aes_string(x=input$xvar, y=input$yvar, 
                                                         group = round_any(agridatasub[[input$xvar]], input$box_width)), 
                            outlier.shape = 1, outlier.colour = 'orange2') +
        geom_point(data=agridatasubremove, aes_string(x=input$xvar, y=input$yvar), 
                   color = 'tomato', fill = NA, alpha = .4)}
    
    #To color whole plot by variable chosen
    if (input$color_by_category == TRUE)  {
      if (input$type == 'scatter') {
        p <- p + geom_point(data=agridatasub, aes_string(x=input$xvar, y=input$yvar, color = input$colorvar))
      } else if (input$type == 'histogram') {
        p <- p + geom_histogram(data=agridatasub, aes_string(x=input$var, color = input$colorvar), 
                                binwidth = input$bin_width)
      } else if (input$type == 'box') {
        p <- p + geom_boxplot(data=agridatasub, aes_string(x=input$xvar, y=input$yvar, 
                                                           group = round_any(agridatasub[[input$xvar]], input$box_width), 
                                                           color = input$colorvar))}
    } 
    
    #To plot a trendline or density line
    if (input$trend == TRUE) {
      if (input$type == 'scatter' | input$type == 'box') {
        if (input$conf_int == FALSE) {
          p <- p + geom_smooth(data = agridatasub, aes_string(x=input$xvar, y=input$yvar),
                               se = FALSE,  method = 'lm', color = 'mediumorchid',
                               formula = y ~ poly(x, input$poly_input)) 
        } else if (input$conf_int == TRUE) {
          p <- p + geom_smooth(data = agridatasub, aes_string(x=input$xvar, y=input$yvar),
                               se = TRUE,  method = 'lm', color = 'mediumorchid',
                               formula = y ~ poly(x, input$poly_input)) 
        }
      } else if (input$type == 'histogram') {
        p <- p + geom_density(data = agridatasub, 
                              aes_string(x=input$var, y=paste(input$bin_width, '..count..', sep = '*')), 
                              color = 'mediumorchid')
      }
    }
    
    #To overplot subgroups
    if (input$tosubornottosub == TRUE) {
      
      if (input$num_subgroups >= 1) {
        if (input$sub1 != 'Field') {
          firstsub <- subset(agridatasub, agridatasub[[input$sub1]] >= input$slider1[1] & agridatasub[[input$sub1]] <= input$slider1[2])
        } else {
          firstsub = subset(agridatasub, agridatasub[[input$sub1]] == input$field_list1)}
        
        if (input$type == 'scatter') {
          p <- p + geom_point(data=firstsub, aes_string(x=input$xvar, y=input$yvar), color='red')
        } else if (input$type == 'histogram') {
          p <- p + geom_histogram(data=firstsub, aes_string(x=input$var), 
                                  binwidth = input$bin_width, fill='red', color='black', 
                                  alpha = .6)
        } else if (input$type == 'box') {
          p <- p + geom_boxplot(data=firstsub, aes_string(x=input$xvar, y=input$yvar, group = round_any(firstsub[[input$xvar]], input$box_width)), 
                                color='red',  outlier.shape = 1, outlier.colour = 'red4')}}
      
      if (input$num_subgroups >= 2) {
        if (input$sub2 != 'Field') {
          secondsub <- subset(agridatasub, agridatasub[[input$sub2]] >= input$slider2[1] & agridatasub[[input$sub2]] <= input$slider2[2])
        } else {
          secondsub = subset(agridatasub, agridatasub[[input$sub2]] == input$field_list2)}
        
        if (input$type == 'scatter') {
          p <- p + geom_point(data=secondsub, aes_string(x=input$xvar, y=input$yvar), color='blue')
        } else if (input$type == 'histogram') {
          p <- p + geom_histogram(data=secondsub, aes_string(x=input$var), 
                                  binwidth = input$bin_width, fill='blue', color='black', 
                                  alpha = .6)
        } else if (input$type == 'box') {
          p <- p + geom_boxplot(data=secondsub, aes_string(x=input$xvar, y=input$yvar, group = round_any(secondsub[[input$xvar]], input$box_width)), 
                                color='blue',  outlier.shape = 1, outlier.colour = 'blue4')}}
      
      if (input$num_subgroups == 3) {
        if (input$sub3 != 'Field') {
          thirdsub <- subset(agridatasub, agridatasub[[input$sub3]] >= input$slider3[1] & agridatasub[[input$sub3]] <= input$slider3[2])
        } else {
          thirdsub = subset(agridatasub, agridatasub[[input$sub3]] == input$field_list3)}
        
        if (input$type == 'scatter') {
          p <- p + geom_point(data=thirdsub, aes_string(x=input$xvar, y=input$yvar), color='green')
        } else if (input$type == 'histogram') {
          p <- p + geom_histogram(data=thirdsub, aes_string(x=input$var), 
                                  binwidth = input$bin_width, fill='green', color='black', 
                                  alpha = .6)
        } else if (input$type == 'box') {
          p <- p + geom_boxplot(data=thirdsub, aes_string(x=input$xvar, y=input$yvar, group = round_any(thirdsub[[input$xvar]], input$box_width)), 
                                color='green', outlier.shape = 1, outlier.colour = 'green4')}}
      
    }
    
    #To freeze axes
    if (input$freeze == TRUE) {
      if (input$freeze_x%%2 == 1) {
        p <- p + coord_cartesian(xlim = react_lims$xaxis_lims)
      } else if (input$freeze_y%%2 == 1) {
        p <- p + coord_cartesian(ylim = react_lims$yaxis_lims)
      }
      if (input$freeze_x%%2 == 1 & input$freeze_y%%2 == 1) {
        p <- p + coord_cartesian(xlim = react_lims$xaxis_lims, ylim = react_lims$yaxis_lims)
      }     
      if (input$freeze_axis%%2 == 1) {
        p <- p + coord_cartesian(xlim = react_lims$histaxis_lims)
      }
      
    }
    
    #To add a title
    p <- p + ggtitle(react_title())
    
    #To generate the plotting code
    return(p)
    
  })
  
  #To actually generate the plot
  output$plot <- renderPlot({
    return(plt())
  })
  
}

shinyApp(ui=ui, server=server)

###   Next Steps:
###   -Supress error messages
###   -Choose by company/hybrid? Self-input
###   -Figure out how to cope with Stand (% vs numbers)
###     -Separate or change one of them to be cooperative to the other
###       - Changing will cause problems with missing PPA's
###   -Eventual weather data handling? Idek
###   -DOCUMENTATION

###   From 8/30/16 Meeting w/ Alison:
###   -Make a map, and display fields for subgrouping

