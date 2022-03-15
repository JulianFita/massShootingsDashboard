server <- function(input,
                   output,
                   session)
{
  
  df <- reactive({df <- massShootings.order %>%
    filter(year(date) == input$year) %>%
    group_by(state) %>%
    summarise(dead = sum(dead),
              injured = sum(injured),
              total = sum(total),
              description, .groups = 'drop')})
  
# Map Chart  
  output$mapPlot <- renderHighchart({
    
    fn <- "function(){
      console.log(this.name);
      Shiny.onInputChange('mapPlotinput', this.name)
    }"
    
  hcmap(map = 'countries/us/custom/us-all-mainland.js',
        data = df(),
        joinBy = c('name', 'state'),
        value = 'total',
        borderWidth = 0.05,
        nullColor = "#d3d3d3") %>%
    hc_title(text = 'Mass Shooting') %>%
    hc_colorAxis(stops = color_stops(colors = viridisLite::viridis(10,
                                                                   begin = 0.1)),
                 type = "logarithmic") %>%
    hc_tooltip(formatter= JS("function () { return this.point.name.bold() +
                            ' <br />' +
                            ' <br /> <b>Dead:</b> ' + this.point.dead +
                            ' <br /> <b>Injured:</b> ' + this.point.injured ;}")) %>%
    hc_add_theme(hc_my_theme) %>%
    hc_mapNavigation(enabled = TRUE) %>%
    hc_credits(enabled = FALSE) %>%
    hc_exporting(enabled = TRUE) %>%
    hc_plotOptions(series = list(cursor = "pointer",
                                 point = list(events = list(click = JS(fn)))))})
  
  
# Stock chart  
  output$linePlot <- renderHighchart({
    
    nme <- ifelse(is.null(input$mapPlotinput),
                  "United States of America",
                  input$mapPlotinput)
    
    dfClick <- massShootings.order %>%
      filter(state %in% nme) %>%
      filter(year(date) == input$year) %>%
      group_by(date) %>%
      summarise(dead = sum(dead),
                injured = sum(injured),
                total = sum(total),
                .groups = 'drop')
  
    
    highchart(type = "stock") %>%
      hc_chart("line",
               name = "base",
               hcaes(x = date)) %>%
      hc_add_series(dfClick,
                    name = "Total",
                    type = "line",
                    hcaes(
                      x = date,
                      y = total)) %>%
      hc_add_series(dfClick,
                    name = "Dead",
                    type = "line",
                    hcaes(
                      x = date,
                      y = dead)) %>%
      hc_add_series(dfClick,
                    name = "Injured",
                    type = "line",
                    hcaes(
                      x = date,
                      y = injured)) %>%
      hc_add_theme(hc_theme_538()) %>%
      hc_tooltip(
        crosshairs = TRUE,
        shared = TRUE,
        borderWidth = 2,
        table = TRUE)})
  
 
  # valueBox - Total
  output$totals <- renderValueBox({dfTotals <- massShootings.order%>%
                                     filter(year(date) == input$year) %>%
                                     group_by(date) %>%
                                     summarise(total = sum(dead, injured))
  
  valueBox(sum(dfTotals$total), 'Total', icon = icon('calculator') ,color = 'light-blue')})
  
  # valueBox -  Deads
  output$dead <- renderValueBox({dfDeads <- massShootings.order %>%
                                  filter(year(date) == input$year) %>%
                                  group_by(date) %>%
                                  summarise(dead = sum(dead))
  valueBox(sum(dfDeads$dead), 'Deads', icon = icon('skull') ,color = 'red')})
  
  # valueBox - Injureds
  output$injured <- renderValueBox({dfInjureds <- massShootings.order %>%
                                      filter(year(date) == input$year) %>%
                                      group_by(date) %>%
                                      summarise(injured = sum(injured))
  valueBox(sum(dfInjureds$injured), 'Injureds', icon = icon('user-injured') ,color = 'yellow')})
  
  output$descriptionText <- renderUI({HTML(paste('1 in 4 mass shooting victims were children and teens.',
                                               'In the years between 2009 and 2020, the horrific scenes of mass shootings have haunted the nation’s collective conscience.',
                                               'US states with weaker gun laws and higher gun ownership rates have higher rates of mass shootings.',
                                               'Mass shooting is defined as any incident in which four or more people are shot and killed, excluding the shooter.',
                                               'The number of mass shootings that plague this country is far too high, and the counts are just a small fraction of the lives left forever changed after the tragedy of a mass shooting.',
                                               'So here is the data for list of mass shootings in United States from 2018 - 2022.', sep="<br/>"))})
  
  output$summaryText <- renderUI({HTML(paste('With this dashboard I try to show the victims of the mass shootings in the United States 2018~2022.',
                                              'The worst year on record was 2021, leaving 787 dead and 3,305 injured.',
                                               'That are grayed out mean that no workable data has been obtained.', sep="<br/>"))})

 
}

shinyApp(ui, server)
