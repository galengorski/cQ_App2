#===================================================================================#
# NOTES:server for EventPickerApp2.0
#-----------------------------------------------------------------------------------#
# Galen Gorski                                                                      #
# ggorski@ucsc.edu
# 04-30-2020
#-----------------------------------------------------------------------------------#
#===================================================================================#

function(input, output, session) {
  site.data <- reactiveValues(query = NA, number = NA, name = NA, data = NA, subset = NA)
  cq.lims <- reactiveValues(clo = -2.207275, chi = 3.182212, qlo = 1.022451, qhi = 9.172639)

  
  #set reactive values for date limits on low end
  datelims.lo <- reactive({
    if(is.null(input$download_data)){
      as.POSIXct('2008-03-27', format = '%Y-%m-%d')
    }else if(!is.null(input$plot1_date_window)){
      strftime(req(input$plot1_date_window[[1]]), "%Y-%m-%d")
    }else{
      as.POSIXct('1990-03-27', format = '%Y-%m-%d')
    }
  })
  
  #set reactive values for date limits on high end
  datelims.hi <- reactive({
    if(is.null(input$download_data)){
      Sys.Date()
    }else if(!is.null(input$plot1_date_window)){
      strftime(req(input$plot1_date_window[[2]]), "%Y-%m-%d")
    }else{
      Sys.Date()
    }
  })
  
  
  #get the site number
  site.nn <- as.data.frame(readRDS('./Data/sitenamesnumbers.rds'))
  observe(site.data$number <- as.character(site.nn[site.nn$site.names == input$sitename,]$site.numbers))
  #read in initial data to display for summary
  init.site.data <- read.csv('./Data/Initdata.csv', header = T, stringsAsFactors = F)
  
  #read initial plotting data to display
  ipd <- read.csv('./Data/InitSitePlottingData.csv', header = T, stringsAsFactors = F)
  ipd$dateTime <- as.POSIXct(ipd$dateTime, format = '%Y-%m-%d')
  ipd$mo <- strftime(ipd[,'dateTime'], format = '%m')
  isolate(site.data$data <- ipd)
  isolate(site.data$subset <- ipd)
  isolate(site.data$name <- 'North Raccoon River near Sac City, IA')
  
  #action button for query site data
    #this queries the site data and displays a summary of the datasets available
  observeEvent(input$sitename,{
          site.data$query <- whatNWISdata(site = as.character(site.data$number), service = 'dv')
          output$site_name <- renderText(unique(site.data$query[,'station_nm']))
          output$site_data_sum <- renderPrint(site.data$query[,c('parm_cd','stat_cd','begin_date','end_date','count_nu')])
          output$date_range_div <- renderUI({
            dateRangeInput("date_range", label = h5('Enter date'), start = site.data$query[site.data$query[,'parm_cd'] == '99133' & site.data$query[,'stat_cd'] == '00003','begin_date'], 
                           end = max(site.data$query[,'end_date']))
          })
  })

  #produce a box to input a date range for download
  output$date_range_div <- renderUI({
    dateRangeInput('date_range', label = h5('Enter date'), start = '2008-03-27', end = Sys.Date())
  })
  
  #prints a summary of the datasets available
  output$site_data_sum <- renderPrint(init.site.data[,c('parm_cd','stat_cd','begin_date','end_date','count_nu')])
  
  #---------------------------------------------------------------#
  #####DOWNLOAD BUTTON CLICKED######
  #when the download data button is clicked
  observeEvent(input$download_data,{

      site.data$data <- readNWISdata(site = as.character(site.data$number), parameterCd = c(unique(site.data$query[,'parm_cd'])), startDate = input$date_range[1], endDate = input$date_range[2])
      site.data$data[,'mo'] <- strftime(site.data$data[,'dateTime'], format = '%m')
      #if there are zero values that will trip an error with log functions
      z <- site.data$data[!is.na(site.data$data[,input[['second_y_series']]]) & site.data$data[,input[['second_y_series']]] == 0,]
      if(nrow(z) == 0){
      #if there aren't zero values don't do anything
      }else{
      #if there are, then replace them with low values
      site.data$data[!is.na(site.data$data[,input[['second_y_series']]]) & site.data$data[,input[['second_y_series']]] == 0,][input[['second_y_series']]] <- 0.01
      }
      
      #if there are zero values that will trip an error with log functions
      #remove no-flow
      z <- site.data$data[!is.na(site.data$data[,input[['plotting_y']]]) & site.data$data[,input[['plotting_y']]] == 0,]
      if(nrow(z) == 0){
        #if there aren't zero values don't do anything
      }else{
        #if there are, then replace them with low values
        site.data$data[!is.na(site.data$data[,input[['plotting_y']]]) & site.data$data[,input[['plotting_y']]] == 0,][input[['plotting_y']]] <- 0.01
      }
      site.data$subset <- site.data$data
      query <- whatNWISdata(site = as.character(site.data$number), service = 'dv')
      site.data$name <- unique(query[,'station_nm'])
      
      #set cq plot limits
      #concentration
      cq.lims$clo <- log(min(site.data$data[,'X_99133_00003'], na.rm = T))
      cq.lims$chi <- log(max(site.data$data[,'X_99133_00003'], na.rm = T))
      #discharge
      cq.lims$qlo <- log(min(site.data$data[,'X_00060_00003'], na.rm = T))
      cq.lims$qhi <- log(max(site.data$data[,'X_00060_00003'], na.rm = T))
      

    events_original$event.df <- NA
    
    scan_counter$c <- 0

  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####CODE CHECKER######
  #this prints out input to check code
  output$to <- renderText({
   
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####MAP OF SITES######
  #this prints out input to check code
  output$map <- renderLeaflet({
    site.locations <- readRDS('./Data/sitelatlong.rds')
    site.locations.df <- as.data.frame(site.locations)
    
    single.site <- site.locations[site.locations$site == input$sitename,]
    single.site.df <- as.data.frame(single.site)
    
    single.site.loc <- SpatialPointsDataFrame(coords = single.site, data = single.site.df, proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
    
    
    
    
    leaflet(site.locations) %>% addProviderTiles(providers$Esri.WorldTopoMap) %>%
      addCircleMarkers(lng = as.numeric(site.locations$long), lat = as.numeric(site.locations$lat),
                       popup = site.locations$site, radius = 6, color = 'black', fillColor = 'blue', stroke = T, fill = T, weight = 1, opacity = 1, fillOpacity = 0.6) %>%
      addCircleMarkers(lng = as.numeric(single.site$long), lat = as.numeric(single.site$lat),
                       popup = single.site$site, radius = 6, color = 'black', fillColor = 'red', stroke = T, fill = T, weight = 1, opacity = 1, fillOpacity = 1)
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####MAKE A Y-AXIS SELECTION DROPDOWN######
  #an input for what to plot on the y axis of the main plot
  output$y_axis <- renderUI({
    selectInput('plotting_y', 'Select variable to plot', choices = c(colnames(site.data$data)), selected = 'X_00060_00003')
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####MAKE A SECONDARY Y AXIS UPON CHECK MARK CLICK######
  #the default is that the secondary time series is plotted
  output$second_y_axis <- renderUI({
    selectInput("second_y_series",'Select another variable to plot', choices = c(colnames(site.data$data)), selected = 'X_99133_00003')
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####MAKE THE MAIN PLOT######
  #Initial plot before download data is clicked
  output$plot1 <- renderDygraph({
    validate(
      need(input$plotting_y, '')
    )
    if(grepl('99133',input[['plotting_y']])){
      yaxlab <- 'Nitrate (mg/L)'
    }else if(grepl('00060',input[['plotting_y']])){
      yaxlab <- 'Discharge (cfs)'
    }else if(grepl('00010',input[['plotting_y']])){
      yaxlab <- 'Temp (C)'
    }else if(grepl('83554',input[['plotting_y']])){
      yaxlab <- 'Nitrate load (ton/d)'
    }else{
      yaxlab <- input[['plotting_y']]
    }
  
    if(grepl('99133',input[['second_y_series']])){
      yaxlab2 <- 'Nitrate (mg/L)'
    }else if(grepl('00060',input[['second_y_series']])){
      yaxlab2 <- 'Discharge (cfs)'
    }else if(grepl('00010',input[['second_y_series']])){
      yaxlab2 <- 'Temp (C)'
    }else if(grepl('83554',input[['second_y_series']])){
      yaxlab2 <- 'Nitrate load (ton/d)'
    }else{
      yaxlab2 <- input[['second_y_series']]
    }
    
    #intial y series
    initial <- xts(site.data$data[,input[['plotting_y']]], order.by = site.data$data[,'dateTime'])
    #add the secondary y axis if box is checked
    if(input$second_y_check){
      #second y series
      second <- xts(site.data$data[,input[['second_y_series']]], order.by = site.data$data[,'dateTime'])
      #combine them
      both <- cbind(initial, second)
      #make the plot with both series
      dygraph(both, main = paste0('Showing data from ',site.data$name)) %>%
        dySeries('initial', label = yaxlab, color = 'black') %>%
        dyAxis("y", label = yaxlab) %>%
        dySeries('second', label = yaxlab2, color = 'red', axis = 'y2') %>%
        dyAxis("y2", label = yaxlab2) %>%
        dyRangeSelector()
        
    }else{
      #make the plot with one series
      dygraph(initial, main = paste0('Showing data from ',site.data$name)) %>%
        dySeries('V1', label = yaxlab, color = 'black') %>%
        dyAxis("y", label = yaxlab) %>%
        dyRangeSelector()
    }
    
    })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####MAKE AN ACTION BUTTON TO RECALCULATE CQ RELATIONSHIPS BASED ON SLIDER INPUTS######
  observeEvent(input$cq_recalc,{
    site.data.temp <- site.data$data
    site.data$subset <- site.data.temp[site.data.temp[,'dateTime'] > datelims.lo() & site.data.temp[,'dateTime'] < datelims.hi(),]
  })
  #####
  #---------------------------------------------------------------#
    
  #---------------------------------------------------------------#
  #####ALL SEASON CQ PLOTS######
  #c-Q plot for the entire data set
  output$plot2 <- renderPlot({
    ggplot(site.data$subset, aes(x=log(X_00060_00003), y=log(X_99133_00003))) +
     geom_point(shape = 1, col = 'black', na.rm = T) +
     geom_smooth(method = 'lm', na.rm = T)+
     labs(title = 'All seasons', x = 'log(Q)', y = 'log([NO3])', cex = 1.2) +
     theme_classic()+
      xlim(cq.lims$qlo, cq.lims$qhi)+
      ylim(cq.lims$clo, cq.lims$chi)+
     theme(axis.text=element_text(size=20),
           axis.title=element_text(size=20,face="bold"),
           plot.title = element_text(size=22))
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####ALL SEASON CQ TABLE######
  output$allseason.table <- renderTable({
   all.season.ss <- site.data$subset
    
   #linear regression of subset
    lr <- lm(log(all.season.ss$X_99133_00003)~log(all.season.ss$X_00060_00003))
    #calculate slope
    b <- coef(lr)[2] %>%
      as.numeric() %>%
      round(., digits = 2)
    #R2
    r2 <- summary(lr)$r.square %>%
      as.numeric() %>%
      round(., digits = 2)
    #slope significance
    sig <- coefficients(summary(lr))[2,'Pr(>|t|)'] %>%
      as.numeric() 
    #cvq
    cvq <- sd(all.season.ss$X_00060_00003, na.rm = T)/mean(all.season.ss$X_00060_00003, na.rm = T)
    #cvc
    cvc <- sd(all.season.ss$X_99133_00003, na.rm = T)/mean(all.season.ss$X_99133_00003, na.rm = T)
    #cvc/cvq
    cvc.cvq <- round(cvc/cvq, digits = 2) 
    #construct matrix
    allseason.mat <- matrix(c(r2,b,sig,cvc.cvq),ncol = 1)
    rownames(allseason.mat) <- c('R2','b','p-val','CVc/CVq')
    colnames(allseason.mat) <- 'All seasons'
    
    allseason.mat
  },
  spacing = 'xs',
  rownames = TRUE,
  digits = 2)
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####SEASONAL CQ PLOTS######
  #OND c-Q plot
  output$plotOND <- renderPlot({
    #name the reactive subset
    site.data.month.temp <- site.data$subset
    #choose the months of interest
    site.data.ss.mo <- site.data.month.temp[site.data.month.temp$mo == '10' | site.data.month.temp$mo == '11' | site.data.month.temp$mo == '12',]
    
    #make the plot
    ggplot(site.data.ss.mo, aes(x=log(X_00060_00003), y=log(X_99133_00003))) + 
      geom_point(shape = 1, col = '#3B99B1FF', na.rm = T) +
      geom_smooth(method = 'lm', na.rm = T)+
      labs(title = 'Oct, Nov, Dec', x = 'log(Q)', y = 'log([NO3])', cex = 1.2) +
      theme_classic()+
      xlim(cq.lims$qlo, cq.lims$qhi)+
      ylim(cq.lims$clo, cq.lims$chi)+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=18))
  })
  
  #JFM c-Q plot
  output$plotJFM <- renderPlot({
    #name the reactive subset
    site.data.month.temp <- site.data$subset
    #choose the months of interest
    site.data.ss.mo <- site.data.month.temp[site.data.month.temp$mo == '01' | site.data.month.temp$mo == '02' | site.data.month.temp$mo == '03',]
    
    #make the plot
    ggplot(site.data.ss.mo, aes(x=log(X_00060_00003), y=log(X_99133_00003))) + 
      geom_point(shape = 1, col = '#A9C392FF', na.rm = T) +
      geom_smooth(method = 'lm', na.rm = T)+
      labs(title = 'Jan, Feb, Mar', x = 'log(Q)', y = 'log([NO3])', cex = 1.2) +
      theme_classic()+
      xlim(cq.lims$qlo, cq.lims$qhi)+
      ylim(cq.lims$clo, cq.lims$chi)+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=18))
  })
  
  #AMJ c-Q plot
  output$plotAMJ <- renderPlot({
    #name the reactive subset
    site.data.month.temp <- site.data$subset
    #choose the months of interest
    site.data.ss.mo <- site.data.month.temp[site.data.month.temp$mo == '04' | site.data.month.temp$mo == '05' | site.data.month.temp$mo == '06',]
    
    #make the plot
    ggplot(site.data.ss.mo, aes(x=log(X_00060_00003), y=log(X_99133_00003))) + 
      geom_point(shape = 1, col = '#E9AB1CFF', na.rm = T) +
      geom_smooth(method = 'lm', na.rm = T)+
      labs(title = 'Apr, May, Jun', x = 'log(Q)', y = 'log([NO3])', cex = 1.2) +
      theme_classic()+
      xlim(cq.lims$qlo, cq.lims$qhi)+
      ylim(cq.lims$clo, cq.lims$chi)+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=18))
  })
  
  #JAS c-Q plot
  output$plotJAS<- renderPlot({
    #name the reactive subset
    site.data.month.temp <- site.data$subset
    #choose the months of interest
    site.data.ss.mo <- site.data.month.temp[site.data.month.temp$mo == '07' | site.data.month.temp$mo == '08' | site.data.month.temp$mo == '09',]
    
    #make the plot
    ggplot(site.data.ss.mo, aes(x=log(X_00060_00003), y=log(X_99133_00003))) + 
      geom_point(shape = 1, col = '#F5191CFF', na.rm = T) +
      geom_smooth(method = 'lm', na.rm = T)+
      labs(title = 'Jul, Aug, Sep', x = 'log(Q)', y = 'log([NO3])', cex = 1.2) +
      theme_classic()+
      xlim(cq.lims$qlo, cq.lims$qhi)+
      ylim(cq.lims$clo, cq.lims$chi)+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=18))
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####MAKE A SEASONAL CQ TABLE#####
  output$seasonal.table <- renderTable({
    #name the reactive subset
    site.data.month.temp <- site.data$subset
    
    #ond
    site.data.ss.ond <- site.data.month.temp[site.data.month.temp$mo == '10' | site.data.month.temp$mo == '11' | site.data.month.temp$mo == '12',]
    #linear regression of subset
    lr.ond <- lm(log(site.data.ss.ond$X_99133_00003)~log(site.data.ss.ond$X_00060_00003))
    #calculate slope
    b.ond <- coef(lr.ond)[2] %>%
      as.numeric() %>%
      round(., digits = 2)
    #R2
    r2.ond <- summary(lr.ond)$r.square %>%
      as.numeric() %>%
      round(., digits = 2)
    #slope significance
    sig.ond <- coefficients(summary(lr.ond))[2,'Pr(>|t|)'] %>%
      as.numeric() 
    #cvq
    cvq.ond <- sd(site.data.ss.ond$X_00060_00003, na.rm = T)/mean(site.data.ss.ond$X_00060_00003, na.rm = T)
    #cvc
    cvc.ond <- sd(site.data.ss.ond$X_99133_00003, na.rm = T)/mean(site.data.ss.ond$X_99133_00003, na.rm = T)
    #cvc/cvq
    cvc.cvq.ond <- round(cvc.ond/cvq.ond, digits = 2) 
    
    #jfm
    site.data.ss.jfm <- site.data.month.temp[site.data.month.temp$mo == '01' | site.data.month.temp$mo == '02' | site.data.month.temp$mo == '03',]
    #linear regression of subset
    lr.jfm <- lm(log(site.data.ss.jfm$X_99133_00003)~log(site.data.ss.jfm$X_00060_00003))
    #calculate slope
    b.jfm <- coef(lr.jfm)[2] %>%
      as.numeric() %>%
      round(., digits = 2)
    #R2
    r2.jfm <- summary(lr.jfm)$r.square %>%
      as.numeric() %>%
      round(., digits = 2)
    #slope significance
    sig.jfm <- coefficients(summary(lr.jfm))[2,'Pr(>|t|)'] %>%
      as.numeric() 
    #cvq
    cvq.jfm <- sd(site.data.ss.jfm$X_00060_00003, na.rm = T)/mean(site.data.ss.jfm$X_00060_00003, na.rm = T)
    #cvc
    cvc.jfm <- sd(site.data.ss.jfm$X_99133_00003, na.rm = T)/mean(site.data.ss.jfm$X_99133_00003, na.rm = T)
    #cvc/cvq
    cvc.cvq.jfm <- round(cvc.jfm/cvq.jfm, digits = 2) 
    
    #amj
    site.data.ss.amj <- site.data.month.temp[site.data.month.temp$mo == '04' | site.data.month.temp$mo == '05' | site.data.month.temp$mo == '06',]
    #linear regression of subset
    lr.amj <- lm(log(site.data.ss.amj$X_99133_00003)~log(site.data.ss.amj$X_00060_00003))
    #calculate slope
    b.amj <- coef(lr.amj)[2] %>%
      as.numeric() %>%
      round(., digits = 2)
    #R2
    r2.amj <- summary(lr.amj)$r.square %>%
      as.numeric() %>%
      round(., digits = 2)
    #slope significance
    sig.amj <- coefficients(summary(lr.amj))[2,'Pr(>|t|)'] %>%
      as.numeric() 
    #cvq
    cvq.amj <- sd(site.data.ss.amj$X_00060_00003, na.rm = T)/mean(site.data.ss.amj$X_00060_00003, na.rm = T)
    #cvc
    cvc.amj <- sd(site.data.ss.amj$X_99133_00003, na.rm = T)/mean(site.data.ss.amj$X_99133_00003, na.rm = T)
    #cvc/cvq
    cvc.cvq.amj <- round(cvc.amj/cvq.amj, digits = 2) 
    
    #jas
    site.data.ss.jas <- site.data.month.temp[site.data.month.temp$mo == '07' | site.data.month.temp$mo == '08' | site.data.month.temp$mo == '09',]
    #linear regression of subset
    lr.jas <- lm(log(site.data.ss.jas$X_99133_00003)~log(site.data.ss.jas$X_00060_00003))
    #calculate slope
    b.jas <- coef(lr.jas)[2] %>%
      as.numeric() %>%
      round(., digits = 2)
    #R2
    r2.jas <- summary(lr.jas)$r.square %>%
      as.numeric() %>%
      round(., digits = 2)
    #slope significance
    sig.jas <- coefficients(summary(lr.jas))[2,'Pr(>|t|)'] %>%
      as.numeric() 
    #cvq
    cvq.jas <- sd(site.data.ss.jas$X_00060_00003, na.rm = T)/mean(site.data.ss.jas$X_00060_00003, na.rm = T)
    #cvc
    cvc.jas <- sd(site.data.ss.jas$X_99133_00003, na.rm = T)/mean(site.data.ss.jas$X_99133_00003, na.rm = T)
    #cvc/cvq
    cvc.cvq.jas <- round(cvc.jas/cvq.jas, digits = 2) 
    
    #construct matrix
    seasonal.mat <- matrix(c(r2.ond,b.ond,sig.ond,cvc.cvq.ond,
                              r2.jfm,b.jfm,sig.jfm,cvc.cvq.jfm,
                              r2.amj,b.amj,sig.amj,cvc.cvq.amj,
                              r2.jas,b.jas,sig.jas,cvc.cvq.jas),ncol = 4)
    rownames(seasonal.mat) <- c('R2','b','p-val','CVc/CVq')
    colnames(seasonal.mat) <- c('OND','JFM','AMJ','JAS')
    
    seasonal.mat
  },
  spacing = 'xs',
  rownames = TRUE,
  digits = 2)
  #####
  #---------------------------------------------------------------#
  
  ###TAB 2###
  scan_counter <- reactiveValues(c = 0)
  event_scan_counter <- reactiveValues(c = NA)
  events_original <- reactiveValues(event.df = NA, record.peaks.df = NA, event.ls = NA, adjusted.events = NA)
  range_xlim <- reactiveValues(rx_min = NA, rx_max = NA)
  events.removed <- reactiveValues(rm.num = 0)
  #---------------------------------------------------------------#
  #####SCAN FOR EVENTS WHEN EVEN SCAN BUTTON IS CLICKED#####
  observeEvent(input$event_scan,{
    site.data$data <- readNWISdata(site = as.character(site.data$number), startDate = input$date_range[1], endDate = input$date_range[2])
    site.data$data[,'mo'] <- strftime(site.data$data[,'dateTime'], format = '%m')
    
    #if there are zeroes in the events
    z <- site.data$data[!is.na(site.data$data[,input[['second_y_series']]]) & site.data$data[,input[['second_y_series']]] == 0,]
    if(nrow(z) == 0){
      #if there aren't zero values don't do anything
    }else{
      #if there are, then replace them with low values
      site.data$data[!is.na(site.data$data[,input[['second_y_series']]]) & site.data$data[,input[['second_y_series']]] == 0,][input[['second_y_series']]] <- 0.01
    }
    
    df <- site.data$data
    #find.peaks tags the peaks in the data frame and returns a data frame with additional columns that 
    #identify the peaks and their limbs
    df.peaks <- find.peaks(df, 'dateTime', 'X_00060_00003', input$sb_pk_thresh_val, input$sf_pk_thresh_val)
    #events_original$record.peaks.df will be used when the event is adjusted. The event will be redefined 
    #from this reactive dataframe
    events_original$record.peaks.df <- df.peaks
    
    #event scanner returns the events that pass muster in a list
    r.events <- event.scanner(df.peaks, 'dateTime', 'X_00060_00003', input$event_rise_val, input$peak_top_thresh_val, input$event_length_thresh_val)
    
    events_original$event.ls <- r.events
    events_original$adjusted.events <- r.events
    c <- lapply(r.events, function(x) is.null(x)) %>% 
      unlist() %>%
      which(!is.na(.)) %>% 
      as.vector(.)
    first_event <- c[1]
    #use that as a template for a data frame
    events.orig.df <- data.frame(r.events[[first_event]][1,])
    events.orig.df <- events.orig.df[-1,] 
    for(i in 1:length(r.events)){
      if(is.null(nrow(r.events[[i]]))){
        next
      }else{
        if(nrow(r.events[[i]]) == 1){
          next
        }else{
          events.orig.df <- rbind(events.orig.df, r.events[[i]])
        }
      }
    }
    event_scan_counter$c <- 1
    
    #create a month column
    events.orig.df$mo <- strftime(events.orig.df$dateTime, format = '%m')
    
    
    #assign the events to the reactive dataset
    events_original$event.df <- events.orig.df
    
    #add event.index column 
    events_original$event.df[,'event.index'] <<- as.numeric(as.factor(abs(events_original$event.df[,'event.flag'])))
    #don't know if you need to add event.index to the full record
    #record$event.index <- as.numeric(as.factor(abs(record$event.flag)))
    
    df <- site.data$data
    df[,'dateTime'] <- as.POSIXct(strptime(as.character(df[,'dateTime']), format = '%Y-%m-%d'), format = '%Y-%m-%d', tz = 'UTC')     
    #make the xlimits for plot 3
    range_xlim$rx_min <- min(df$dateTime)
    range_xlim$rx_max <- max(df$dateTime)
    
    events.removed$rm.num <- 0
    
    scan_counter$c <- 1
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####MAKE A PLOT OF THE HYDROGRAPH#####
  output$plot3 <- renderDygraph({
    
    #check to make sure that there is an x_plot that has been selected
    validate(
      need('dateTime', '')
    )
    if(grepl('99133',input[['plotting_y']])){
      yaxlab <- 'Nitrate (mg/L)'
    }else if(grepl('00060',input[['plotting_y']])){
      yaxlab <- 'Discharge (cfs)'
    }else if(grepl('00010',input[['plotting_y']])){
      yaxlab <- 'Temp (C)'
    }else if(grepl('83554',input[['plotting_y']])){
      yaxlab <- 'Nitrate load (ton/d)'
    }else{
      yaxlab <- input[['plotting_y']]
    }
    
    if(scan_counter$c == 0){
    p.xts <- xts(site.data$data[,input[['plotting_y']]], order.by = site.data$data[,'dateTime'])
    dygraph(p.xts, main = paste('Showing data from', site.data$name)) %>%
      dySeries('V1', label = yaxlab, color = 'black') %>%
      dyAxis("y", label = yaxlab) %>%
      dyRangeSelector()
    #if you have scanned for events then plot them up
    }else{
      orig.events <- events_original$event.df
      orig.events$dateTime <- as.POSIXlt(orig.events$dateTime, format = '%m/%d/%y', tz = 'UTC')
      p.xts <- xts(site.data$data[,input[['plotting_y']]], order.by = site.data$data[,'dateTime'])
      e.xts <- xts(orig.events[,input[['plotting_y']]], order.by = orig.events$dateTime)
      com.xts <- cbind(p.xts, e.xts)
      
      dygraph(com.xts, main = paste('Showing data from', site.data$name)) %>%
        dySeries('p.xts', label = 'Baseflow', color = 'black') %>%
        dySeries('e.xts', label = 'Stormflow', color = 'dodgerblue') %>%
        dyAxis("y", label = yaxlab) %>%
        dyRangeSelector()
    }
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####MAKE A CQ PLOT OF THE EVENTS#####
  output$stormflow <- renderPlot({
    validate(
      need(!is.na(events_original$event.df), '')
    )
    as.storm <- events_original$event.df
    #make the plot
    ggplot(as.storm, aes(x=log(X_00060_00003), y=log(X_99133_00003))) + 
      geom_point(shape = 1, col = 'dodgerblue', na.rm = T) +
      geom_smooth(method = 'lm', na.rm = T)+
      labs(title = 'All season stormflow', x = 'log(Q)', y = 'log([NO3])', cex = 1.2) +
      theme_classic()+
      xlim(cq.lims$qlo, cq.lims$qhi)+
      ylim(cq.lims$clo, cq.lims$chi)+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=18))
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####MAKE A TABLE OF EVENT STATISTICS#####
  output$stormflowtable <- renderTable({
    validate(
      need(!is.na(events_original$event.df), '')
    )
    as.storm <- events_original$event.df
    
    #linear regression of subset
    lr <- lm(log(as.storm$X_99133_00003)~log(as.storm$X_00060_00003))
    #calculate slope
    b <- coef(lr)[2] %>%
      as.numeric() %>%
      round(., digits = 2)
    #R2
    r2 <- summary(lr)$r.square %>%
      as.numeric() %>%
      round(., digits = 2)
    #slope significance
    sig <- coefficients(summary(lr))[2,'Pr(>|t|)'] %>%
      as.numeric() 
    #cvq
    cvq <- sd(as.storm$X_00060_00003, na.rm = T)/mean(as.storm$X_00060_00003, na.rm = T)
    #cvc
    cvc <- sd(as.storm$X_99133_00003, na.rm = T)/mean(as.storm$X_99133_00003, na.rm = T)
    #cvc/cvq
    cvc.cvq <- round(cvc/cvq, digits = 2) 
    #construct matrix
    storm.mat <- matrix(c(r2,b,sig,cvc.cvq),ncol = 1)
    rownames(storm.mat) <- c('R2','b','p-val','CVc/CVq')
    colnames(storm.mat) <- 'Stormflow'
    
    storm.mat
  },
  spacing = 'xs',
  rownames = TRUE,
  digits = 2)
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####MAKE A CQ PLOT OF THE BASEFLOW#####
  output$baseflow <- renderPlot({
    validate(
      need(!is.na(events_original$event.df), 'Click the "Scan for Events" button')
    )
    all.data <- site.data$data
    as.storm <- events_original$event.df
    as.storm$jsec <- as.POSIXlt(as.storm$dateTime, format = '%m/%d/%y', tz = 'UTC')
    all.data$jsec <- as.POSIXlt(all.data$dateTime, format = '%m/%d/%y', tz = 'UTC')
    as.base <- all.data[!all.data$jsec %in% as.storm$jsec,]
    #make the plot
    ggplot(as.base, aes(x=log(X_00060_00003), y=log(X_99133_00003))) + 
      geom_point(shape = 1, col = 'black', na.rm = T) +
      geom_smooth(method = 'lm', na.rm = T)+
      labs(title = 'All season baseflow', x = 'log(Q)', y = 'log([NO3])', cex = 1.2) +
      theme_classic()+
      xlim(cq.lims$qlo, cq.lims$qhi)+
      ylim(cq.lims$clo, cq.lims$chi)+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=18))
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####MAKE A TABLE OF BASEFLOW STATISTICS#####
  output$baseflowtable <- renderTable({
    validate(
      need(!is.na(events_original$event.df), '')
    )
    all.data <- site.data$data
    as.storm <- events_original$event.df
    as.storm$jsec <- as.POSIXlt(as.storm$dateTime, format = '%m/%d/%y', tz = 'UTC')
    all.data$jsec <- as.POSIXlt(all.data$dateTime, format = '%m/%d/%y', tz = 'UTC')
    as.base <- all.data[!all.data$jsec %in% as.storm$jsec,]
    
    #linear regression of subset
    lr <- lm(log(as.base$X_99133_00003)~log(as.base$X_00060_00003))
    #calculate slope
    b <- coef(lr)[2] %>%
      as.numeric() %>%
      round(., digits = 2)
    #R2
    r2 <- summary(lr)$r.square %>%
      as.numeric() %>%
      round(., digits = 2)
    #slope significance
    sig <- coefficients(summary(lr))[2,'Pr(>|t|)'] %>%
      as.numeric() 
    #cvq
    cvq <- sd(as.base$X_00060_00003, na.rm = T)/mean(as.base$X_00060_00003, na.rm = T)
    #cvc
    cvc <- sd(as.base$X_99133_00003, na.rm = T)/mean(as.base$X_99133_00003, na.rm = T)
    #cvc/cvq
    cvc.cvq <- round(cvc/cvq, digits = 2) 
    #construct matrix
    base.mat <- matrix(c(r2,b,sig,cvc.cvq),ncol = 1)
    rownames(base.mat) <- c('R2','b','p-val','CVc/CVq')
    colnames(base.mat) <- 'Baseflow'
    
    base.mat
  },
  spacing = 'xs',
  rownames = TRUE,
  digits = 2)
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####EVENT SEASONAL CQ PLOTS######
  
  #OND c-Q plot
  output$plotsfOND <- renderPlot({
    validate(
      need(!is.na(events_original$event.df), '')
    )
    #name the reactive subset
    site.data.month.temp <- events_original$event.df 
    #choose the months of interest
    site.data.ss.mo <- site.data.month.temp[site.data.month.temp$mo == '10' | site.data.month.temp$mo == '11' | site.data.month.temp$mo == '12',]
    
    #make the plot
    ggplot(site.data.ss.mo, aes(x=log(X_00060_00003), y=log(X_99133_00003))) + 
      geom_point(shape = 1, col = '#3B99B1FF', na.rm = T) +
      geom_smooth(method = 'lm', na.rm = T)+
      labs(title = 'Stormflow -- Oct, Nov, Dec', x = 'log(Q)', y = 'log([NO3])', cex = 1.2) +
      theme_classic()+
      xlim(cq.lims$qlo, cq.lims$qhi)+
      ylim(cq.lims$clo, cq.lims$chi)+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=18))
  })
  
  #JFM c-Q plot
  output$plotsfJFM <- renderPlot({
    validate(
      need(!is.na(events_original$event.df), '')
    )
    #name the reactive subset
    site.data.month.temp <- events_original$event.df 
    #choose the months of interest
    site.data.ss.mo <- site.data.month.temp[site.data.month.temp$mo == '01' | site.data.month.temp$mo == '02' | site.data.month.temp$mo == '03',]
    
    #make the plot
    ggplot(site.data.ss.mo, aes(x=log(X_00060_00003), y=log(X_99133_00003))) + 
      geom_point(shape = 1, col = '#A9C392FF', na.rm = T) +
      geom_smooth(method = 'lm', na.rm = T)+
      labs(title = 'Stormflow -- Jan, Feb, Mar', x = 'log(Q)', y = 'log([NO3])', cex = 1.2) +
      theme_classic()+
      xlim(cq.lims$qlo, cq.lims$qhi)+
      ylim(cq.lims$clo, cq.lims$chi)+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=18))
  })
  
  #AMJ c-Q plot
  output$plotsfAMJ <- renderPlot({
    validate(
      need(!is.na(events_original$event.df), '')
    )
    #name the reactive subset
    site.data.month.temp <- events_original$event.df 
    #choose the months of interest
    site.data.ss.mo <- site.data.month.temp[site.data.month.temp$mo == '04' | site.data.month.temp$mo == '05' | site.data.month.temp$mo == '06',]
    
    #make the plot
    ggplot(site.data.ss.mo, aes(x=log(X_00060_00003), y=log(X_99133_00003))) + 
      geom_point(shape = 1, col = '#E9AB1CFF', na.rm = T) +
      geom_smooth(method = 'lm', na.rm = T)+
      labs(title = 'Stormflow -- Apr, May, Jun', x = 'log(Q)', y = 'log([NO3])', cex = 1.2) +
      theme_classic()+
      xlim(cq.lims$qlo, cq.lims$qhi)+
      ylim(cq.lims$clo, cq.lims$chi)+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=18))
  })
  
  #JAS c-Q plot
  output$plotsfJAS<- renderPlot({
    validate(
      need(!is.na(events_original$event.df), '')
    )
    #name the reactive subset
    site.data.month.temp <- events_original$event.df 
    #choose the months of interest
    site.data.ss.mo <- site.data.month.temp[site.data.month.temp$mo == '07' | site.data.month.temp$mo == '08' | site.data.month.temp$mo == '09',]
    
    #make the plot
    ggplot(site.data.ss.mo, aes(x=log(X_00060_00003), y=log(X_99133_00003))) + 
      geom_point(shape = 1, col = '#F5191CFF', na.rm = T) +
      geom_smooth(method = 'lm', na.rm = T)+
      labs(title = 'Stormflow -- Jul, Aug, Sep', x = 'log(Q)', y = 'log([NO3])', cex = 1.2) +
      theme_classic()+
      xlim(cq.lims$qlo, cq.lims$qhi)+
      ylim(cq.lims$clo, cq.lims$chi)+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=18))
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####MAKE AN EVENT SEASONAL CQ TABLE#####
  output$seasonalstormflowtable <- renderTable({
    validate(
      need(!is.na(events_original$event.df), '')
    )
    #name the reactive subset
    site.data.month.temp <- events_original$event.df 
    
    #ond
    site.data.ss.ond <- site.data.month.temp[site.data.month.temp$mo == '10' | site.data.month.temp$mo == '11' | site.data.month.temp$mo == '12',]
    #linear regression of subset
    lr.ond <- lm(log(site.data.ss.ond$X_99133_00003)~log(site.data.ss.ond$X_00060_00003))
    #calculate slope
    b.ond <- coef(lr.ond)[2] %>%
      as.numeric() %>%
      round(., digits = 2)
    #R2
    r2.ond <- summary(lr.ond)$r.square %>%
      as.numeric() %>%
      round(., digits = 2)
    #slope significance
    sig.ond <- coefficients(summary(lr.ond))[2,'Pr(>|t|)'] %>%
      as.numeric() 
    #cvq
    cvq.ond <- sd(site.data.ss.ond$X_00060_00003, na.rm = T)/mean(site.data.ss.ond$X_00060_00003, na.rm = T)
    #cvc
    cvc.ond <- sd(site.data.ss.ond$X_99133_00003, na.rm = T)/mean(site.data.ss.ond$X_99133_00003, na.rm = T)
    #cvc/cvq
    cvc.cvq.ond <- round(cvc.ond/cvq.ond, digits = 2) 
    
    #jfm
    site.data.ss.jfm <- site.data.month.temp[site.data.month.temp$mo == '01' | site.data.month.temp$mo == '02' | site.data.month.temp$mo == '03',]
    #linear regression of subset
    lr.jfm <- lm(log(site.data.ss.jfm$X_99133_00003)~log(site.data.ss.jfm$X_00060_00003))
    #calculate slope
    b.jfm <- coef(lr.jfm)[2] %>%
      as.numeric() %>%
      round(., digits = 2)
    #R2
    r2.jfm <- summary(lr.jfm)$r.square %>%
      as.numeric() %>%
      round(., digits = 2)
    #slope significance
    sig.jfm <- coefficients(summary(lr.jfm))[2,'Pr(>|t|)'] %>%
      as.numeric() 
    #cvq
    cvq.jfm <- sd(site.data.ss.jfm$X_00060_00003, na.rm = T)/mean(site.data.ss.jfm$X_00060_00003, na.rm = T)
    #cvc
    cvc.jfm <- sd(site.data.ss.jfm$X_99133_00003, na.rm = T)/mean(site.data.ss.jfm$X_99133_00003, na.rm = T)
    #cvc/cvq
    cvc.cvq.jfm <- round(cvc.jfm/cvq.jfm, digits = 2) 
    
    #amj
    site.data.ss.amj <- site.data.month.temp[site.data.month.temp$mo == '04' | site.data.month.temp$mo == '05' | site.data.month.temp$mo == '06',]
    #linear regression of subset
    lr.amj <- lm(log(site.data.ss.amj$X_99133_00003)~log(site.data.ss.amj$X_00060_00003))
    #calculate slope
    b.amj <- coef(lr.amj)[2] %>%
      as.numeric() %>%
      round(., digits = 2)
    #R2
    r2.amj <- summary(lr.amj)$r.square %>%
      as.numeric() %>%
      round(., digits = 2)
    #slope significance
    sig.amj <- coefficients(summary(lr.amj))[2,'Pr(>|t|)'] %>%
      as.numeric() 
    #cvq
    cvq.amj <- sd(site.data.ss.amj$X_00060_00003, na.rm = T)/mean(site.data.ss.amj$X_00060_00003, na.rm = T)
    #cvc
    cvc.amj <- sd(site.data.ss.amj$X_99133_00003, na.rm = T)/mean(site.data.ss.amj$X_99133_00003, na.rm = T)
    #cvc/cvq
    cvc.cvq.amj <- round(cvc.amj/cvq.amj, digits = 2) 
    
    #jas
    site.data.ss.jas <- site.data.month.temp[site.data.month.temp$mo == '07' | site.data.month.temp$mo == '08' | site.data.month.temp$mo == '09',]
    #linear regression of subset
    lr.jas <- lm(log(site.data.ss.jas$X_99133_00003)~log(site.data.ss.jas$X_00060_00003))
    #calculate slope
    b.jas <- coef(lr.jas)[2] %>%
      as.numeric() %>%
      round(., digits = 2)
    #R2
    r2.jas <- summary(lr.jas)$r.square %>%
      as.numeric() %>%
      round(., digits = 2)
    #slope significance
    sig.jas <- coefficients(summary(lr.jas))[2,'Pr(>|t|)'] %>%
      as.numeric() 
    #cvq
    cvq.jas <- sd(site.data.ss.jas$X_00060_00003, na.rm = T)/mean(site.data.ss.jas$X_00060_00003, na.rm = T)
    #cvc
    cvc.jas <- sd(site.data.ss.jas$X_99133_00003, na.rm = T)/mean(site.data.ss.jas$X_99133_00003, na.rm = T)
    #cvc/cvq
    cvc.cvq.jas <- round(cvc.jas/cvq.jas, digits = 2) 
    
    #construct matrix
    seasonal.mat <- matrix(c(r2.ond,b.ond,sig.ond,cvc.cvq.ond,
                             r2.jfm,b.jfm,sig.jfm,cvc.cvq.jfm,
                             r2.amj,b.amj,sig.amj,cvc.cvq.amj,
                             r2.jas,b.jas,sig.jas,cvc.cvq.jas),ncol = 4)
    rownames(seasonal.mat) <- c('R2','b','p-val','CVc/CVq')
    colnames(seasonal.mat) <- c('OND','JFM','AMJ','JAS')
    
    seasonal.mat
  },
  spacing = 'xs',
  rownames = TRUE,
  digits = 2)
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####BASEFLOW SEASONAL CQ PLOTS######
  
  #OND c-Q plot
  output$plotbfOND <- renderPlot({
    validate(
      need(!is.na(events_original$event.df), '')
    )
    
    all.data <- site.data$data
    as.storm <- events_original$event.df
    as.storm$jsec <- as.POSIXlt(as.storm$dateTime, format = '%m/%d/%y', tz = 'UTC')
    all.data$jsec <- as.POSIXlt(all.data$dateTime, format = '%m/%d/%y', tz = 'UTC')
    as.base <- all.data[!all.data$jsec %in% as.storm$jsec,]
    
    #choose the months of interest
    site.data.ss.mo <- as.base[as.base$mo == '10' | as.base$mo == '11' | as.base$mo == '12',]
    
    #make the plot
    ggplot(site.data.ss.mo, aes(x=log(X_00060_00003), y=log(X_99133_00003))) + 
      geom_point(shape = 1, col = '#3B99B1FF', na.rm = T) +
      geom_smooth(method = 'lm', na.rm = T)+
      labs(title = 'Baseflow -- Oct, Nov, Dec', x = 'log(Q)', y = 'log([NO3])', cex = 1.2) +
      theme_classic()+
      xlim(cq.lims$qlo, cq.lims$qhi)+
      ylim(cq.lims$clo, cq.lims$chi)+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=18))
  })
  
  #JFM c-Q plot
  output$plotbfJFM <- renderPlot({
    validate(
      need(!is.na(events_original$event.df), '')
    )
    
    all.data <- site.data$data
    as.storm <- events_original$event.df
    as.storm$jsec <- as.POSIXlt(as.storm$dateTime, format = '%m/%d/%y', tz = 'UTC')
    all.data$jsec <- as.POSIXlt(all.data$dateTime, format = '%m/%d/%y', tz = 'UTC')
    as.base <- all.data[!all.data$jsec %in% as.storm$jsec,]
    
    #choose the months of interest
    site.data.ss.mo <- as.base[as.base$mo == '01' | as.base$mo == '02' | as.base$mo == '03',]
    
    #make the plot
    ggplot(site.data.ss.mo, aes(x=log(X_00060_00003), y=log(X_99133_00003))) + 
      geom_point(shape = 1, col = '#A9C392FF', na.rm = T) +
      geom_smooth(method = 'lm', na.rm = T)+
      labs(title = 'Baseflow -- Jan, Feb, Mar', x = 'log(Q)', y = 'log([NO3])', cex = 1.2) +
      theme_classic()+
      xlim(cq.lims$qlo, cq.lims$qhi)+
      ylim(cq.lims$clo, cq.lims$chi)+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=18))
  })
  
  #AMJ c-Q plot
  output$plotbfAMJ <- renderPlot({
    validate(
      need(!is.na(events_original$event.df), '')
    )
    all.data <- site.data$data
    as.storm <- events_original$event.df
    as.storm$jsec <- as.POSIXlt(as.storm$dateTime, format = '%m/%d/%y', tz = 'UTC')
    all.data$jsec <- as.POSIXlt(all.data$dateTime, format = '%m/%d/%y', tz = 'UTC')
    as.base <- all.data[!all.data$jsec %in% as.storm$jsec,]
    
    #choose the months of interest
    site.data.ss.mo <- as.base[as.base$mo == '04' | as.base$mo == '05' | as.base$mo == '06',]
    
    #make the plot
    ggplot(site.data.ss.mo, aes(x=log(X_00060_00003), y=log(X_99133_00003))) + 
      geom_point(shape = 1, col = '#E9AB1CFF', na.rm = T) +
      geom_smooth(method = 'lm', na.rm = T)+
      labs(title = 'Baseflow -- Apr, May, Jun', x = 'log(Q)', y = 'log([NO3])', cex = 1.2) +
      theme_classic()+
      xlim(cq.lims$qlo, cq.lims$qhi)+
      ylim(cq.lims$clo, cq.lims$chi)+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=18))
  })
  
  #JAS c-Q plot
  output$plotbfJAS<- renderPlot({
    validate(
      need(!is.na(events_original$event.df), '')
    )
    all.data <- site.data$data
    as.storm <- events_original$event.df
    as.storm$jsec <- as.POSIXlt(as.storm$dateTime, format = '%m/%d/%y', tz = 'UTC')
    all.data$jsec <- as.POSIXlt(all.data$dateTime, format = '%m/%d/%y', tz = 'UTC')
    as.base <- all.data[!all.data$jsec %in% as.storm$jsec,]
    #choose the months of interest
    site.data.ss.mo <- as.base[as.base$mo == '07' | as.base$mo == '08' | as.base$mo == '09',]
    
    #make the plot
    ggplot(site.data.ss.mo, aes(x=log(X_00060_00003), y=log(X_99133_00003))) + 
      geom_point(shape = 1, col = '#F5191CFF', na.rm = T) +
      geom_smooth(method = 'lm', na.rm = T)+
      labs(title = 'Baseflow -- Jul, Aug, Sep', x = 'log(Q)', y = 'log([NO3])', cex = 1.2) +
      theme_classic()+
      xlim(cq.lims$qlo, cq.lims$qhi)+
      ylim(cq.lims$clo, cq.lims$chi)+
      theme(axis.text=element_text(size=14),
            axis.title=element_text(size=14,face="bold"),
            plot.title = element_text(size=18))
  })
  #####
  #---------------------------------------------------------------#
  
  #---------------------------------------------------------------#
  #####MAKE A BASEFLOW SEASONAL CQ TABLE#####
  output$seasonalbaseflowtable <- renderTable({
    validate(
      need(!is.na(events_original$event.df), '')
    )
    
    all.data <- site.data$data
    as.storm <- events_original$event.df
    as.storm$jsec <- as.POSIXlt(as.storm$dateTime, format = '%m/%d/%y', tz = 'UTC')
    all.data$jsec <- as.POSIXlt(all.data$dateTime, format = '%m/%d/%y', tz = 'UTC')
    site.data.month.temp <- all.data[!all.data$jsec %in% as.storm$jsec,]
    
    #ond
    site.data.ss.ond <- site.data.month.temp[site.data.month.temp$mo == '10' | site.data.month.temp$mo == '11' | site.data.month.temp$mo == '12',]
    #linear regression of subset
    lr.ond <- lm(log(site.data.ss.ond$X_99133_00003)~log(site.data.ss.ond$X_00060_00003))
    #calculate slope
    b.ond <- coef(lr.ond)[2] %>%
      as.numeric() %>%
      round(., digits = 2)
    #R2
    r2.ond <- summary(lr.ond)$r.square %>%
      as.numeric() %>%
      round(., digits = 2)
    #slope significance
    sig.ond <- coefficients(summary(lr.ond))[2,'Pr(>|t|)'] %>%
      as.numeric() 
    #cvq
    cvq.ond <- sd(site.data.ss.ond$X_00060_00003, na.rm = T)/mean(site.data.ss.ond$X_00060_00003, na.rm = T)
    #cvc
    cvc.ond <- sd(site.data.ss.ond$X_99133_00003, na.rm = T)/mean(site.data.ss.ond$X_99133_00003, na.rm = T)
    #cvc/cvq
    cvc.cvq.ond <- round(cvc.ond/cvq.ond, digits = 2) 
    
    #jfm
    site.data.ss.jfm <- site.data.month.temp[site.data.month.temp$mo == '01' | site.data.month.temp$mo == '02' | site.data.month.temp$mo == '03',]
    #linear regression of subset
    lr.jfm <- lm(log(site.data.ss.jfm$X_99133_00003)~log(site.data.ss.jfm$X_00060_00003))
    #calculate slope
    b.jfm <- coef(lr.jfm)[2] %>%
      as.numeric() %>%
      round(., digits = 2)
    #R2
    r2.jfm <- summary(lr.jfm)$r.square %>%
      as.numeric() %>%
      round(., digits = 2)
    #slope significance
    sig.jfm <- coefficients(summary(lr.jfm))[2,'Pr(>|t|)'] %>%
      as.numeric() 
    #cvq
    cvq.jfm <- sd(site.data.ss.jfm$X_00060_00003, na.rm = T)/mean(site.data.ss.jfm$X_00060_00003, na.rm = T)
    #cvc
    cvc.jfm <- sd(site.data.ss.jfm$X_99133_00003, na.rm = T)/mean(site.data.ss.jfm$X_99133_00003, na.rm = T)
    #cvc/cvq
    cvc.cvq.jfm <- round(cvc.jfm/cvq.jfm, digits = 2) 
    
    #amj
    site.data.ss.amj <- site.data.month.temp[site.data.month.temp$mo == '04' | site.data.month.temp$mo == '05' | site.data.month.temp$mo == '06',]
    #linear regression of subset
    lr.amj <- lm(log(site.data.ss.amj$X_99133_00003)~log(site.data.ss.amj$X_00060_00003))
    #calculate slope
    b.amj <- coef(lr.amj)[2] %>%
      as.numeric() %>%
      round(., digits = 2)
    #R2
    r2.amj <- summary(lr.amj)$r.square %>%
      as.numeric() %>%
      round(., digits = 2)
    #slope significance
    sig.amj <- coefficients(summary(lr.amj))[2,'Pr(>|t|)'] %>%
      as.numeric() 
    #cvq
    cvq.amj <- sd(site.data.ss.amj$X_00060_00003, na.rm = T)/mean(site.data.ss.amj$X_00060_00003, na.rm = T)
    #cvc
    cvc.amj <- sd(site.data.ss.amj$X_99133_00003, na.rm = T)/mean(site.data.ss.amj$X_99133_00003, na.rm = T)
    #cvc/cvq
    cvc.cvq.amj <- round(cvc.amj/cvq.amj, digits = 2) 
    
    #jas
    site.data.ss.jas <- site.data.month.temp[site.data.month.temp$mo == '07' | site.data.month.temp$mo == '08' | site.data.month.temp$mo == '09',]
    #linear regression of subset
    lr.jas <- lm(log(site.data.ss.jas$X_99133_00003)~log(site.data.ss.jas$X_00060_00003))
    #calculate slope
    b.jas <- coef(lr.jas)[2] %>%
      as.numeric() %>%
      round(., digits = 2)
    #R2
    r2.jas <- summary(lr.jas)$r.square %>%
      as.numeric() %>%
      round(., digits = 2)
    #slope significance
    sig.jas <- coefficients(summary(lr.jas))[2,'Pr(>|t|)'] %>%
      as.numeric() 
    #cvq
    cvq.jas <- sd(site.data.ss.jas$X_00060_00003, na.rm = T)/mean(site.data.ss.jas$X_00060_00003, na.rm = T)
    #cvc
    cvc.jas <- sd(site.data.ss.jas$X_99133_00003, na.rm = T)/mean(site.data.ss.jas$X_99133_00003, na.rm = T)
    #cvc/cvq
    cvc.cvq.jas <- round(cvc.jas/cvq.jas, digits = 2) 
    
    #construct matrix
    seasonal.mat <- matrix(c(r2.ond,b.ond,sig.ond,cvc.cvq.ond,
                             r2.jfm,b.jfm,sig.jfm,cvc.cvq.jfm,
                             r2.amj,b.amj,sig.amj,cvc.cvq.amj,
                             r2.jas,b.jas,sig.jas,cvc.cvq.jas),ncol = 4)
    rownames(seasonal.mat) <- c('R2','b','p-val','CVc/CVq')
    colnames(seasonal.mat) <- c('OND','JFM','AMJ','JAS')
    
    seasonal.mat
  },
  spacing = 'xs',
  rownames = TRUE,
  digits = 2)
  #####
  #---------------------------------------------------------------#
  
}