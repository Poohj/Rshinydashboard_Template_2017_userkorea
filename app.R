install.packages("shiny")
install.packages("shinydashboard")
install.packages("shinyjs")
install.packages("shinythemes")
install.packages("DT")
install.packages("dygraphs")
install.packages("devtools")
install.packages("htmlwidgets")
install.packages("googleVis")
install.packages("flexdashboard")
install.packages("xts")
install.packages("networkD3")
install.packages("TraMineR")
install.packages("sunburstR")
install.packages("pipeR")
install.packages("visNetwork")
install.packages("d3heatmap")
devtools::install_github("timelyportfolio/d3vennR")
install.packages("rpivotTable")

library(shinyjs)
library(shiny)
library(shinydashboard)
library(shinythemes)
library(googleVis)
library(DT)
library(dygraphs)
library(htmlwidgets)
library(flexdashboard)
library(xts)
library(networkD3)
library(TraMineR)
library(sunburstR)
library(pipeR)
library(igraph)
library(visNetwork)
library(d3heatmap)
library(d3vennR)
library(rpivotTable)

##################################################################
###### app.R - UI ################################################
##################################################################

ui <- dashboardPage(
  tags$head(tags$link(rel="shortcut icon", href="pay_favicon.ico")), ## favicon 입력
  header = dashboardHeader(
    title = "Basic_Dashboard"
  ),
  sidebar = dashboardSidebar(
    sidebarMenu(
      menuItem(enc2utf8("상황표"), tabName = "total_dashboard", icon = icon("dashboard"),badgeLabel = "KPI", badgeColor = "red")
      ,
      menuItem(enc2utf8("샘 플"), tabName = "index", icon = icon("dashboard")
               ,startExpanded = FALSE
               ,menuSubItem(enc2utf8("network"), icon = icon("calendar",lib = "font-awesome"), tabName = "index_week")
               ,menuSubItem(enc2utf8("pivot"), icon = icon("calendar",lib = "font-awesome"), tabName = "index_month")
      )
    )
  ),
  body = dashboardBody(
    tabItems(
      tabItem(tabName = "total_dashboard",
              fluidPage(
                box(width = 12,status = "primary", collapsible = TRUE, 
                    title = enc2utf8("KPI"),solidHeader = TRUE,
                    fluidRow(
                      column(3,gaugeOutput("gauge1",width = "100%",height = "150px")),
                      column(3,gaugeOutput("gauge2",width = "100%",height = "150px")),
                      column(3,gaugeOutput("gauge3",width = "100%",height = "150px")),
                      column(3,gaugeOutput("gauge4",width = "100%",height = "150px"))
                    )
                ),
                box(width = 12,status = "primary",collapsible = FALSE, 
                    title = enc2utf8("기본지표"),solidHeader = TRUE,
                    fluidRow(
                      infoBoxOutput("info_1",width = 3),
                      infoBoxOutput("info_2",width = 3),
                      infoBoxOutput("info_3",width = 3),
                      infoBoxOutput("info_4",width = 3)
                    ),
                    fluidRow(
                      shinydashboard::valueBoxOutput("value_1",width = 3),
                      shinydashboard::valueBoxOutput("value_2",width = 3),
                      shinydashboard::valueBoxOutput("value_3",width = 3),
                      shinydashboard::valueBoxOutput("value_4",width = 3)
                    )
                ),
                
                box(width = 12,status = "primary", collapsible = TRUE, collapsed = FALSE,
                    title = enc2utf8("기본통계"),solidHeader = TRUE,
                    fluidPage(
                      column(width = 12,
                             tabBox(width = 12,
                                    side = "left",
                                    selected = enc2utf8("테이블"),                               
                                    tabPanel(title = enc2utf8("차트"), dygraphOutput("Summary_Day_view_9"))
                                    ,tabPanel(title = enc2utf8("테이블"), DT::dataTableOutput('Summary_Day_table_9',width="100%"))
                                    
                             )
                      )
                    )
                )
              )
      ),
      tabItem(tabName = "index_week",
              # Boxes need to be put in a row (or column)
              fluidRow(
                box(width = 6, offset = 0,collapsible = TRUE,status = "primary",
                    title = enc2utf8("network_1"),solidHeader = TRUE,
                    fluidRow(
                      # Dynamic valueBoxes
                      simpleNetworkOutput("network_1")
                      
                    )),
                box(width = 6, offset = 0,collapsible = TRUE,status = "primary",
                    title = enc2utf8("network_2"),solidHeader = TRUE,
                    fluidRow(
                      # Dynamic valueBoxes
                      forceNetworkOutput("network_2")
                      
                    )),
                box(width = 6, offset = 0,collapsible = TRUE,status = "primary",
                    title = enc2utf8("network_3"),solidHeader = TRUE,
                    fluidRow(
                      # Dynamic valueBoxes
                      sankeyNetworkOutput("network_3")
                      
                    )),
                box(width = 6, offset = 0,collapsible = TRUE,status = "primary",
                    title = enc2utf8("network_4"),solidHeader = TRUE,
                    fluidRow(
                      # Dynamic valueBoxes
                      forceNetworkOutput("network_4")
                      
                    )),
                box(width = 12, offset = 0,collapsible = TRUE,status = "primary",
                    title = enc2utf8("network_5"),solidHeader = TRUE,
                    fluidRow(
                      visNetworkOutput("network_5")
                      
                    ))
              )
      ),
      tabItem(tabName = "index_month",
              fluidRow(
                box(width = 12, offset = 0,collapsible = TRUE,status = "primary",
                    title = enc2utf8("pivot"),solidHeader = TRUE,
                    
                    rpivotTableOutput("pivot")
                    
                )
              )
      ),
      tabItem(tabName = "index_etc",
              fluidRow(
                box(width = 12, offset = 0,collapsible = TRUE,status = "primary",
                    title = enc2utf8("heatmap"),solidHeader = TRUE,
                    fluidRow(
                      d3heatmapOutput("heatmap_1")
                    )),
                box(width = 12, offset = 0,collapsible = TRUE,status = "primary",
                    title = enc2utf8("vendiagram"),solidHeader = TRUE,
                    fluidRow(
                      d3vennROutput("vennR_1")
                    ))
              )
      )
    )
  )
)

##################################################################
###### app.R - SERVER ############################################
##################################################################

server <- function(input, output,session) {
  
  source('data.R',encoding = "UTF-8",local = TRUE) ## 데이터 소스가져오기
  
  
  #################################################################################
  #################################################################################
  
  output$info_1 <- renderInfoBox({
    infoBox(
      value = format(1000, nsmall=0, big.mark=","), ## 숫자 콤마표현
      title = "A",
      subtitle = "테스트입니다.",
      icon = icon("user",lib = "font-awesome"),
      color = "aqua",
      fill = TRUE
    )
  })
  
  output$info_2 <- renderInfoBox({
    infoBox(
      value = format(1000, nsmall=0, big.mark=","), ## 숫자 콤마표현
      title = "B",
      subtitle = "테스트입니다.",
      icon = icon("users",lib = "font-awesome"),
      color = "maroon",
      fill = TRUE
    )
  })
  
  
  output$info_3 <- renderInfoBox({
    infoBox(
      value = format(1000, nsmall=0, big.mark=","), ## 숫자 콤마표현
      title = "C",
      subtitle = "테스트입니다.",
      icon = icon("users",lib = "font-awesome"),
      color = "light-blue",
      fill = TRUE
    )
  })
  
  output$info_4 <- renderInfoBox({
    infoBox(
      value = format(1000, nsmall=0, big.mark=","), ## 숫자 콤마표현
      title = "D",
      subtitle = "테스트입니다.",
      icon = icon("user-plus",lib = "font-awesome"),
      color = "olive",
      fill = TRUE
    )
  })
  
  output$value_1 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = format(1000, nsmall=0, big.mark=","), ## 숫자 콤마표현
      subtitle = "E",
      icon = icon("krw",lib = "font-awesome"),
      color = "teal"
    )
  })
  
  output$value_2 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = format(1000, nsmall=0, big.mark=","), ## 숫자 콤마표현
      subtitle = "V",
      icon = icon("krw",lib = "font-awesome"),
      color = "maroon"
    )
  })
  
  output$value_3 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = format(1000, nsmall=0, big.mark=","), ## 숫자 콤마표현
      subtitle = "F",
      icon = icon("krw",lib = "font-awesome"),
      color = "olive"
    )
  })
  
  output$value_4 <- shinydashboard::renderValueBox({
    shinydashboard::valueBox(
      value = paste0(25, "%"),
      subtitle = "D",
      icon = icon("users",lib = "font-awesome"),
      color = "yellow"
    )
  })
  
  ##현황그래프 
  
  output$Summary_Day_view_9 <- renderDygraph({
    
    dygraph(data_time,height = "300px") %>%
      dySeries("y", label = "Male") %>%
      dySeries("z", label = "Female") %>%
      dyAxis("y",label = enc2utf8("cnt")) %>%
      dyOptions(stackedGraph = TRUE,axisLabelWidth = 30) %>%
      dyRangeSelector(height = 20) %>%
      dyHighlight(highlightCircleSize = 3,
                  highlightSeriesBackgroundAlpha = 0.3,
                  hideOnMouseOut = TRUE,
                  highlightSeriesOpts = list(strokeWidth = 3))
  })
  
  ## 테이블 입력
  
  output$Summary_Day_table_9 <-  DT::renderDataTable(
    
    data_time,
    extensions = 'Buttons', 
    options = list(
      dom = 'Bfrtip',
      buttons = list(list(extend='csv',
                          filename=sprintf('data_%s',Sys.Date())),
                     list(extend='excel',
                          filename = sprintf('data_%s',Sys.Date())),
                     list(extend='copy',filename = 'Copy to Clipboard')),
      columnDefs = list(list(className = 'dt-center',width = '500px', targets = "_all")),
      pageLength = 10,
      lengthMenu = c(10, 20, 50, 100),
      autoWidth = TRUE),rownames = TRUE
  )
  
  output$gauge1 <- renderGauge({
    flexdashboard::gauge(8000000, min = 0, max = 10000000, label = paste("A"),abbreviate = TRUE,gaugeSectors(
      success = c(5000000, 10000000), warning = c(2500000,4999000), danger = c(0, 2499000)#, colors = c("#CC6699")
    ))
  })
  
  output$gauge2 <- renderGauge({
    flexdashboard::gauge(40, min = 0, max = 100, label = paste("B"),gaugeSectors(
      success = c(50, 100), warning = c(25,49), danger = c(0, 24) #, colors = c("#CC6699","#25e018")
    ))
  })
  
  output$gauge3 <- renderGauge({
    flexdashboard::gauge(1500, min = 0, max = 10000, label = paste("C"),abbreviate = FALSE,gaugeSectors(
      success = c(5000, 10000), warning = c(2500,4999), danger = c(0, 2499)#, colors = c("#CC6699")
    ))
  })
  
  output$gauge4 <- renderGauge({
    flexdashboard::gauge(9000, min = 0, max = 10000, label = paste("D"),abbreviate = FALSE,gaugeSectors(
      success = c(5000, 10000), warning = c(2500,4999), danger = c(0, 2499)#, colors = c("#CC6699")
    ))
  })
  
  output$gauge4 <- renderGauge({
    flexdashboard::gauge(9000, min = 0, max = 10000, label = paste("D"),abbreviate = FALSE,gaugeSectors(
      success = c(5000, 10000), warning = c(2500,4999), danger = c(0, 2499)#, colors = c("#CC6699")
    ))
  })
  
  output$heatmap_1 <- renderD3heatmap({
    
    d3heatmap(mtcars, scale="column", colors="Blues")
    
  })
  
  
  output$network_1 <- renderSimpleNetwork({
    # Create fake data
    src <- c("A", "A", "A", "A",
             "B", "B", "C", "C", "D")
    target <- c("B", "C", "D", "J",
                "E", "F", "G", "H", "I")
    networkData <- data.frame(src, target)
    
    # Plot
    simpleNetwork(networkData)
  })
  
  output$network_2 <- renderForceNetwork({
    # Load data
    data(MisLinks)
    data(MisNodes)
    
    # Plot
    forceNetwork(Links = MisLinks, Nodes = MisNodes,
                 Source = "source", Target = "target",
                 Value = "value", NodeID = "name",
                 Group = "group", opacity = 0.8)
  })
  
  output$network_3 <- renderSankeyNetwork({
    URL <- paste0(
      "https://cdn.rawgit.com/christophergandrud/networkD3/",
      "master/JSONdata/energy.json")
    Energy <- jsonlite::fromJSON(URL)
    # Plot
    sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
                  Target = "target", Value = "value", NodeID = "name",
                  units = "TWh", fontSize = 12, nodeWidth = 30)
  })
  
  output$network_4 <- renderForceNetwork({
    
    karate <- make_graph("Zachary")
    wc <- cluster_walktrap(karate)
    members <- membership(wc)
    karate_d3 <- igraph_to_networkD3(karate, group = members)
    forceNetwork(Links = karate_d3$links, Nodes = karate_d3$nodes, 
                 Source = 'source', Target = 'target', 
                 NodeID = 'name', Group = 'group')
  })
  
  
  output$network_5 <- renderVisNetwork({
    edges <- data.frame(from = sample(1:10,8), to = sample(1:10, 8),
                        
                        # add labels on edges                  
                        label = paste("Edge", 1:8),
                        
                        # length
                        length = c(100,500),
                        
                        # width
                        width = c(4,1),
                        
                        # arrows
                        arrows = c("to", "from", "middle", "middle;to"),
                        
                        # dashes
                        dashes = c(TRUE, FALSE),
                        
                        # tooltip (html or character)
                        title = paste("Edge", 1:8),
                        
                        # smooth
                        smooth = c(FALSE, TRUE),
                        
                        # shadow
                        shadow = c(FALSE, TRUE, FALSE, TRUE)) 
    
    nodes <- data.frame(id = 1:10, group = c("A", "B"))
    
    visNetwork(nodes, edges)
  })
  
  
  output$pivot <- renderRpivotTable({
    library(rpivotTable)
    library(dplyr)
    iris %>%
      tbl_df() %>%
      filter( Sepal.Width > 3 ) %>%
      rpivotTable()
  })
  
  
  output$vennR_1 <- renderD3vennR({
    
    venn_tooltip <- function( venn ){
      venn$x$tasks[length(venn$x$tasks)+1] <- list(
        htmlwidgets::JS('
                        function(){
                        var div = d3.select(this);
                        
                        // add a tooltip
                        var tooltip = d3.select("body").append("div")
                        .attr("class", "venntooltip")
                        .style("position", "absolute")
                        .style("text-align", "center")
                        .style("width", 128)
                        .style("height", 16)
                        .style("background", "#333")
                        .style("color","#ddd")
                        .style("padding","2px")
                        .style("border","0px")
                        .style("border-radius","8px")
                        .style("opacity",0);
                        
                        div.selectAll("path")
                        .style("stroke-opacity", 0)
                        .style("stroke", "#fff")
                        .style("stroke-width", 0)
                        
                        // add listeners to all the groups to display tooltip on mousover
                        div.selectAll("g")
                        .on("mouseover", function(d, i) {
                        
                        // sort all the areas relative to the current item
                        venn.sortAreas(div, d);
                        
                        // Display a tooltip with the current size
                        tooltip.transition().duration(400).style("opacity", .9);
                        tooltip.text(d.size);
                        
                        // highlight the current path
                        var selection = d3.select(this).transition("tooltip").duration(400);
                        selection.select("path")
                        .style("stroke-width", 3)
                        .style("fill-opacity", d.sets.length == 1 ? .4 : .1)
                        .style("stroke-opacity", 1);
                        })
                        
                        .on("mousemove", function() {
                        tooltip.style("left", (d3.event.pageX) + "px")
                        .style("top", (d3.event.pageY - 28) + "px");
                        })
                        
                        .on("mouseout", function(d, i) {
                        tooltip.transition().duration(400).style("opacity", 0);
                        var selection = d3.select(this).transition("tooltip").duration(400);
                        selection.select("path")
                        .style("stroke-width", 0)
                        .style("fill-opacity", d.sets.length == 1 ? .25 : .0)
                        .style("stroke-opacity", 0);
                        });
                        }
                        ')
        )
      venn
  }
    
    
    venn_tooltip(
      d3vennR(
        data = list(
          list( sets = list("First"), size = 65),
          list( sets = list("Second"), size = 75),
          list( sets = list("Third"), size = 85),
          list( sets = list( "First", "Second"), size = 35),
          list( sets = list( "Second", "Third" ), size = 15),
          list( sets = list( "First", "Third" ), size = 25),
          list( sets = list( "First", "Second", "Third" ), size = 5)
        )
      )
    )
})
  
}

shinyApp(ui, server)
