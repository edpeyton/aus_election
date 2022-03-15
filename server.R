
library(knitr)
library(shiny)
library(eechidna)
library(plotly)
library(networkD3)
library(dplyr)
library(tidyr)
library(leaflet)
library(scales)
library(ggplot2)
library(kableExtra)
library(shinyWidgets)

format_date = function(x) {
  
  x = as.Date(x)
  paste0(format(x, "%B"), " ", scales::label_ordinal()(as.numeric(format(x, "%d"))), ", ", format(x, "%Y"))
  
}

source("sankey.R")

elec_pal2 = function(names) {
  
  names[grepl("^IND \\(", names)] = "IND"
  names = names %>% unique()
  
  cols = c("GRN" = "#008A00",
           "IND" = "#000000",
           "LNP" = "#3632FB",
           "LP" = "#00008B",
           "LNQ" = "#3632FB",
           "NP" = "#006400",
           "NP (WA)" = "#006400",
           "ALP" = "#FF0000",
           "XEN" = "#FF6600",
           "KAP" = "#460000",
           "CLP" = "#FFA500",
           "DLP" = "#000080",
           "REAS" = "#008080",
           "UAPP" = "#FFE800",
           "ON" = "#FFEE33",
           "PUP" = "#8f8f0b",
           "CEC" = "#A9A9A9",
           "RUA" = "#A9A9A9",
           "FUT" = "#A9A9A9",
           "ASP" = "#A9A9A9",
           "FACN" = "#A9A9A9")
  
  cols[match(names, names(cols))]
  
}

election_dates = readRDS("data/election_dates.rds")
polls = readRDS("data/polls.rds")
p1 = readRDS("data/p1.rds") %>% dplyr::filter(!Poll %in% c("YouGov/Galaxy", "Galaxy", "Roy Morgan", "Lonergan", "Nielsen", "AMR"))
p2 = readRDS("data/p2.rds") %>% dplyr::filter(!Poll %in% c("YouGov/Galaxy", "Galaxy", "Roy Morgan", "Lonergan", "Nielsen", "AMR"))
turnout = readRDS("data/turnout.rds")
x = readRDS("data/x.rds")
pop = readRDS("data/pop.rds")
pop2 = readRDS("data/pop2.rds")
demo = readRDS("data/demo.rds")
informal = readRDS("data/informal.rds")
elec_pal = readRDS("data/elec_pal.rds")
maps = readRDS("data/maps_leaflet_reduced.rds")
tab = readRDS("data/tab.rds")
votes = readRDS("data/votes.rds")
voting_dist_data = readRDS("data/voting_dist_data.rds")
voting_dist_data2 = readRDS("data/voting_dist_data2.rds")
elec_table = readRDS("data/elec_table.rds")
issues_df = readRDS("data/issues_df.rds")
medincome = readRDS("data/medincome.rds")
area = readRDS("data/area.rds")
tpp = readRDS("data/tpp.rds")
abs = readRDS("data/abs.rds")


function(input, output, session) {
  
  #session$onSessionEnd(function() {
  #  shiny::stopApp()
  #})
  
  
  
  output$leafletplot = leaflet::renderLeaflet({
    
    shiny::validate(
      shiny::need(input$hor_year, "Waiting for electon year input.")
    )
    
    maps[[paste0(input$hor_year)]] %>%
      leaflet::setView(lng = 133.5, lat = -26.48021, zoom = 4)
    
    
  }) %>% shiny::bindCache(input$hor_year)
  
  
  #output$select_elec = shiny::renderUI({
  #  
  #  shiny::validate(
  #    shiny::need(input$hor_year, "Waiting for electon year input.")
  #  )
  #  
  #  cho = list(
  #    `ACT` = x %>% dplyr::filter(YEAR == input$hor_year, StateAb == "ACT") %>% dplyr::pull(DivisionNm) %>% sort(),
  #    `NSW` = x %>% dplyr::filter(YEAR == input$hor_year, StateAb == "NSW") %>% dplyr::pull(DivisionNm) %>% sort(),
  #    `NT` = x %>% dplyr::filter(YEAR == input$hor_year, StateAb == "NT") %>% dplyr::pull(DivisionNm) %>% sort(),
  #    `QLD` = x %>% dplyr::filter(YEAR == input$hor_year, StateAb == "QLD") %>% dplyr::pull(DivisionNm) %>% sort(),
  #    `SA` = x %>% dplyr::filter(YEAR == input$hor_year, StateAb == "SA") %>% dplyr::pull(DivisionNm) %>% sort(),
  #    `TAS` = x %>% dplyr::filter(YEAR == input$hor_year, StateAb == "TAS") %>% dplyr::pull(DivisionNm) %>% sort(),
  #    `VIC` = x %>% dplyr::filter(YEAR == input$hor_year, StateAb == "VIC") %>% dplyr::pull(DivisionNm) %>% sort(),
  #    `WA` = x %>% dplyr::filter(YEAR == input$hor_year, StateAb == "WA") %>% dplyr::pull(DivisionNm) %>% sort()
  #  )
  #  
  #  
  #  shiny::selectInput("select_elec", label = NULL, choices = cho)
  #  
  #}) %>% shiny::bindCache(input$hor_year)
  
  
  output$example_sankey = networkD3::renderSankeyNetwork({
    
    htmlwidgets::onRender(
      create_sankey(x, 2016, "Gorton", height = 300), 'function(el) {d3.selectAll("rect").attr("stroke-width", 0);
      
      var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i).sort(function(a, b){return a - b});
                            
                            cols_x.forEach((d, i) => {
                              d3.select(el).select("svg")
                                .append("text")
                                .attr("x", d + 27)
                                .attr("y", 10)
                                .attr("font-weight", "bold")
                                .attr("text-align", "left")
                                .text("R" + (i+1))
                            });
      
      }')
    
  })
  
  shiny::observeEvent(input$center_map, {
    
    leaflet::leafletProxy("leafletplot") %>%
      leaflet::setView(lng = 133.5, lat = -26.48021, zoom = 4)
    
  })
  
  output$election_details = shiny::renderUI({
    
    list(
      shiny::h3(shiny::code(paste0(input$hor_year, " Federal Election"))),
      shiny::br(),
      shiny::fluidRow(
        
                      shiny::renderUI({
                        votes = votes %>% 
                          dplyr::filter(YEAR == input$hor_year) %>% 
                          dplyr::pull(count) %>%
                          scales::comma()
                        pop = pop2 %>% dplyr::filter(YEAR == input$hor_year) %>% dplyr::pull(Population)
                        turn = turnout %>% dplyr::filter(YEAR == input$hor_year)
                        list(
                          shiny::HTML(paste0("<strong>Votes: </strong>", votes, "</br><strong>Population: </strong>", scales::comma(pop), "</br><strong>Turnout: </strong>", scales::percent(turn$HOR/100, 0.1), "</br><strong>Date: </strong>", format_date(election_dates[[paste0(input$hor_year)]])))
                        )
                      }),
                      shiny::br(),
                      shiny::renderUI({
                        
                        tab2 = tab %>% 
                          dplyr::filter(YEAR == input$hor_year) %>% 
                          dplyr::select(Role, PartyNm, PartyAb, Seats, SeatsP, Votes, VotesP) %>% 
                          dplyr::arrange(Role, dplyr::desc(Seats), dplyr::desc(Votes)) %>%
                          dplyr::mutate(Seats = scales::comma(Seats, 1))
                        
                        if (input$hor_year==2010) {
                          tab2$Seats[tab2$PartyAb=="ALP"] = paste0(tab2$Seats[tab2$PartyAb=="ALP"], "<sup>2</sup>")
                        }
                        names(tab2) = c("", "Party", " ", "Seats", "  ", "Votes<sup>1</sup>", "   ")
                        
                        cap = paste0("<sup>1</sup> First preference votes.", ifelse(input$hor_year==2010, "<br><sup>2</sup> Greens MP Adam Bandt and independent MPs Andrew Wilkie, Rob Oakeshott and Tony Windsor declared their support for Labor on confidence and supply. This gave the required 76 seats to form a majority government.", ""))
                        
                        tab_out = knitr::kable(tab2, format = "html", escape = FALSE, align = c("l", "l", "c", "r", "r", "r", "r")) %>%
                          kableExtra::column_spec(c(1, 2, 3), bold = TRUE) 
                        
                        for (i in 1:dim(tab2)[1]) {
                          tab_out = tab_out %>% kableExtra::row_spec(i, color = party_cols(tab2$Party[i]))
                        }
                        
                        tab_out = tab_out %>%
                          kableExtra::column_spec(1, color = "black") %>%
                          kableExtra::collapse_rows(columns = 1) %>%
                          kableExtra::kable_styling(bootstrap_options = "striped") %>%
                          kableExtra::footnote(general = cap, escape = FALSE)
                        
                        
                        shiny::HTML(tab_out)     
                      })
      ),
      shiny::br(),
      shiny::hr(),
      shiny::br(),
      shiny::fluidRow(
        shiny::column(width = 4, offset = 0,
                      shiny::h4(shiny::code("Key issues", style = "color: #152238")),
                      shiny::tags$p(shiny::HTML(paste0("The key issues of the ", input$hor_year, " election, according to <a href = 'https://australianelectionstudy.org/citing-aes/' target = '_blank'>Australian Election Study</a>.")), style = "text-align: justify;"),
                      plotly::plotlyOutput("issues")
                      
        ),
        shiny::column(width = 4, offset = 0,
                      shiny::h4(shiny::code("Seats won", style = "color: #152238")),
                      shiny::p("The total seats won in the election by each party, grouped by their role in Parliament."),
                      plotly::renderPlotly({
                        readRDS(paste0("data/seatswon", input$hor_year, ".rds"))
                      })
        ),
        shiny::column(width = 4, offset = 0,
                      shiny::h4(shiny::code("Parliament", style = "color: #152238")),
                      shiny::p("A waffle chart representing the distribution of seats in the House of Representatives."),
                      plotly::plotlyOutput("waffle", width = "375px", height = "375px")
                      
        )
        
        
        
      )
    )
    
  }) %>% shiny::bindCache(input$hor_year)
  
  
  output$waffle = plotly::renderPlotly({
    readRDS(paste0("data/waffle", input$hor_year, ".rds"))
  })
  
  output$polling = shiny::renderUI({
    
    shiny::validate(
      shiny::need(input$polling1, message = FALSE),
      shiny::need(input$hor_year, message = FALSE)
    )
    
    data1 = p1 %>% dplyr::filter(Poll == input$polling1, Date<=election_dates[[paste0(input$hor_year)]], ELEC_YEAR == input$hor_year) %>% dplyr::mutate(Date2 = Date, Vote = Vote/100, Vote2 = Vote)
    data2 = p2 %>% dplyr::filter(Poll == input$polling1, Date<=election_dates[[paste0(input$hor_year)]], ELEC_YEAR == input$hor_year) %>% dplyr::mutate(Date2 = Date, Vote = Vote/100, Vote2 = Vote)
    
    shiny::validate(
      shiny::need(dim(data1)[1], message = FALSE),
      shiny::need(dim(data2)[1], message = FALSE)
    )
    
    list(
      #shiny::fluidRow(
      shiny::column(width = 6, offset = 0,
                    shiny::h4(shiny::code("Two-party-preferred", style = "color: #152238")),
                    plotly::renderPlotly({
                      if (dim(data1)[1]) {
                        plotly::ggplotly(
                          ggplot2::ggplot(data = data1) +
                            ggplot2::geom_point(ggplot2::aes(x = Date, y = Vote, color = Party, group = Poll), size = 1, alpha = 0.5) +
                            ggplot2::geom_smooth(ggplot2::aes(x = Date2, y = Vote2, color = Party), size = 0.5, se = FALSE, method = "loess", formula = y ~ x) +
                            ggplot2::theme(panel.border = ggplot2::element_blank(),
                                           panel.background = ggplot2::element_blank(),
                                           axis.title.x = ggplot2::element_blank(),
                                           legend.position = 'none',
                                           axis.text.x = ggplot2::element_text(angle = 60),
                                           panel.grid = ggplot2::element_line(colour = "#a9a9a933")) +
                            ggplot2::scale_color_manual(values = list(ALP = "#FF0000", `Lib/Nat` = "#00008B") %>% unlist()) +
                            ggplot2::labs(x = NULL,
                                          title = NULL,
                                          subtitle = NULL,
                                          y = NULL) +
                            ggplot2::scale_y_continuous(labels = scales::percent) +
                            ggplot2::scale_x_date(date_breaks = "3 months", labels = function(x) {format(x, "%b %Y")}) +
                            ggplot2::geom_hline(yintercept = 0.5, linetype = "dashed", color = "black", size = 0.3)
                          
                          , tooltip = c("Poll", "Party", "Date", "Vote")) %>% 
                          plotly::config(displayModeBar = FALSE) %>%
                          plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
                        
                      } else {
                        NULL
                      }
                      
                    })
      ),
      shiny::column(width = 6, offset = 0,
                    shiny::h4(shiny::code("First preference", style = "color: #152238")),
                    plotly::renderPlotly({
                      if (dim(data1)[1]) {
                        plotly::ggplotly(
                          ggplot2::ggplot(data = data2) +
                            ggplot2::geom_point(ggplot2::aes(x = Date, y = Vote, color = Party), size = 1, alpha = 0.5) +
                            ggplot2::geom_smooth(ggplot2::aes(x = Date2, y = Vote2, color = Party), size = 0.5, se = FALSE, method = "loess", formula = y ~ x) +
                            ggplot2::theme(panel.border = ggplot2::element_blank(),
                                           panel.background = ggplot2::element_blank(),
                                           axis.title.x = ggplot2::element_blank(),
                                           legend.position = 'none',
                                           axis.text.x = ggplot2::element_text(angle = 60),
                                           panel.grid = ggplot2::element_line(colour = "#a9a9a933")) +
                            ggplot2::scale_color_manual(values = elec_pal(p2$Party) %>% unlist()) +
                            ggplot2::labs(x = NULL,
                                          title = NULL,
                                          subtitle = NULL,
                                          y = NULL) +
                            ggplot2::scale_x_date(date_breaks = "3 months", labels = function(x) {format(x, "%b %Y")}) +
                            ggplot2::scale_y_continuous(limits = c(0, max(data2 %>% dplyr::pull(Vote))), labels = scales::percent)
                          
                          , tooltip = c("Party", "Date", "Vote")) %>% 
                          plotly::config(displayModeBar = FALSE) %>%
                          plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
                        
                      } else {
                        NULL
                      }
                      
                    })
      ))
    
    #)
    
    
  }) #%>% shiny::bindCache(input$hor_year, input$polling1)
  
  
  
  
  
  shiny::observeEvent(input$hor_year, {
    
    shiny::updateSelectInput(inputId = "polling1", label = "Select poll", choices = sort(unique(p1$Poll[p1$ELEC_YEAR==input$hor_year])))
    
  })
  
  
  
  #####################################################
  ################# ELECTORATE VISUALS ################
  #####################################################
  
  output$facts = shiny::renderUI({
    
    list(
      shiny::br(),
      shiny::br(),
      shiny::br(),
      shiny::br(),
      shiny::br(),
      shiny::br(),
      shiny::br(),
      shiny::br(),
      shiny::br(),
      shiny::br(),
      shiny::br(),
      shiny::br(),
      shiny::br(),
      shiny::helpText(shiny::strong("Select an electorate on the map"), align = "center")
    )
    
  })
  
  output$facts = shiny::renderUI({
    
    shiny::validate(
      shiny::need(input$leafletplot_shape_click, message = FALSE)
    )
    
    click = input$leafletplot_shape_click
    
    if (!click$id %in% x$DivisionNm[x$YEAR==input$hor_year]) {
      #click$id = x$DivisionNm[x$YEAR==input$hor_year][1]
      return(NULL)
    }
    
    state = unique(x$StateAb[x$DivisionNm==click$id & x$YEAR==input$hor_year])
    
    list(
      shiny::h3(shiny::strong(click$id)),
      shiny::h4(shiny::code(paste0(input$hor_year, " (", state, ")"), style = "color: #152238")),
      shiny::renderUI({
        informal_vote = paste0(informal %>% dplyr::filter(DivisionNm == click$id) %>% dplyr::pull(paste0("VALUE_", input$hor_year)), "%")
        votes = x %>% 
          dplyr::filter(YEAR == input$hor_year, CalculationType == "Preference Count", CountNumber == 0, DivisionNm == click$id) %>% 
          dplyr::summarise(count = sum(CalculationValue)) %>%
          dplyr::pull(count) %>%
          scales::comma()
        pop = pop %>% dplyr::filter(DivisionNm == toupper(click$id), YEAR == input$hor_year) %>% dplyr::mutate(Population = round(Population)) %>% dplyr::pull(Population)
        
        list(
          shiny::HTML(paste0("<strong>Votes: </strong>", votes, "</br><strong>Informal: </strong>", informal_vote, " (<a href = 'https://www.aec.gov.au/Voting/Informal_Voting/' style='color:blue;' target='_blank'>AEC</a>)</br><strong>Population: </strong>", scales::comma(pop)))
        )
      }),
      shiny::br(),
      shiny::renderTable(colnames = TRUE, striped = TRUE, caption = "<sup>1</sup> First preference votes.</br><sup>2</sup> After allocating preferences.</br><sup>3</sup> Incumbent candidate.", sanitize.text.function = function(x) {x}, {
        
        data = elec_table %>% dplyr::filter(YEAR == input$hor_year, DivisionNm == click$id) %>% dplyr::select(-YEAR, -DivisionNm)
        
        names(data) = c("Candidate", "", "Party", "Votes<sup>1</sup>", "Position<sup>2</sup>")
        
        data
        
      })#,
      #shiny::strong("Comments: "),
      #shiny::p(paste0(stringi::stri_rand_lipsum(1, start_lipsum = TRUE)))
    )
    
    
  }) %>% shiny::bindCache(input$hor_year, input$leafletplot_shape_click$id)
  
  
  
  output$voting_dist = shiny::renderUI({
    
    shiny::validate(
      shiny::need(input$leafletplot_shape_click, message = FALSE)
    )
    
    click = input$leafletplot_shape_click
    
    if (!click$id %in% x$DivisionNm[x$YEAR==input$hor_year]) {
      #click$id = x$DivisionNm[x$YEAR==input$hor_year][1]
      return(NULL)
    }
    
    list(
      shiny::hr(),
      shiny::br(),
      shiny::br(),
      shiny::h3(shiny::code("Voting distribution")),
      shiny::tags$p(shiny::HTML(paste0("This section covers the distribution of voting for <strong>", click$id, "</strong> in <strong>", input$hor_year, "</strong>.")), style = "text-align: justify;"),
      shiny::br(),
      shiny::column(width = 4, offset = 0,
                    shiny::h4(shiny::code("First preference votes", style = "color: #152238")),
                    shiny::span("The first preference distribution shows the proportion of voters' first preferences that went to each party."),
                    plotly::renderPlotly({
                      
                      data = voting_dist_data %>% dplyr::filter(YEAR == input$hor_year, DivisionNm == click$id)
                      
                      if (dim(data %>% dplyr::filter(Party == "IND"))[1] > 1) {
                        data = data %>% dplyr::mutate(Party = ifelse(Party == "IND", paste0(Party, " (", Surname, ")"), Party))
                      }
                      
                      data$Party = factor(data$Party, levels = data$Party[order(data$CalculationValue, decreasing = TRUE)])
                      
                      plotly::ggplotly(
                        suppressWarnings(
                          ggplot2::ggplot(data = data) +
                            ggplot2::geom_bar(ggplot2::aes(x = Party, y = CalculationValue, group = Votes, fill = Party, id = Percentage), stat = "identity", show.legend = FALSE) +
                            ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                           panel.grid.minor = ggplot2::element_blank(),
                                           panel.border = ggplot2::element_blank(),
                                           panel.background = ggplot2::element_blank(),
                                           axis.title.x = ggplot2::element_blank(),
                                           axis.text.y = ggplot2::element_blank(),
                                           axis.ticks.y = ggplot2::element_blank(),
                                           axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 0.5),
                                           legend.position = 'none') +
                            ggplot2::scale_fill_manual(values = elec_pal(data$Party)) +
                            ggplot2::labs(x = NULL,
                                          title = NULL,
                                          subtitle = NULL,
                                          y = NULL) +
                            ggplot2::geom_hline(yintercept = 50, linetype = "dashed", color = "grey", size = 0.5)
                        )
                        
                        
                        , tooltip = c("Votes", "Percentage")) %>% 
                        plotly::config(displayModeBar = FALSE) %>%
                        plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
                      
                      
                    })),
      
      shiny::column(width = 4,
                    offset = 0,
                    shiny::h4(shiny::code("Final preference count", style = "color: #152238")),
                    shiny::span("The final preference count shows the final distribution of votes to the two remaining candidates after distributing all preferences."),
                    plotly::renderPlotly({
                      
                      data = voting_dist_data2 %>% dplyr::filter(YEAR == input$hor_year, DivisionNm == click$id) 
                      
                      if (dim(data %>% dplyr::filter(Party == "IND"))[1] > 1) {
                        data = data %>% dplyr::mutate(Party = ifelse(Party == "IND", paste0(Party, " (", Surname, ")"), Party))
                      }
                      
                      data$Party = factor(data$Party, levels = data$Party[order(data$Votes, decreasing = TRUE)])
                      
                      plotly::ggplotly(
                        suppressWarnings(
                          ggplot2::ggplot(data = data) +
                            ggplot2::geom_bar(ggplot2::aes(x = Party, y = `Preference Percent`, group = Votes, fill = Party, id = Percentage), stat = "identity", show.legend = FALSE) +
                            ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                           panel.grid.minor = ggplot2::element_blank(),
                                           panel.border = ggplot2::element_blank(),
                                           panel.background = ggplot2::element_blank(),
                                           axis.title.x = ggplot2::element_blank(),
                                           axis.text.y = ggplot2::element_blank(),
                                           axis.ticks.y = ggplot2::element_blank(),
                                           axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 0.5),
                                           legend.position = 'none') +
                            ggplot2::scale_fill_manual(values = elec_pal(data$Party)) +
                            ggplot2::labs(x = NULL,
                                          title = NULL,
                                          subtitle = NULL,
                                          y = NULL) +
                            ggplot2::geom_hline(yintercept = 50, linetype = "dashed", color = "grey", size = 0.5)
                        )
                        
                        
                        , tooltip = c("Votes", "Percentage")) %>% 
                        plotly::config(displayModeBar = FALSE) %>%
                        plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
                      
                    })),
      
      shiny::column(width = 4,
                    offset = 0,
                    shiny::h4(shiny::code("Two-party-preferred", style = "color: #152238")),
                    shiny::span("The two-party-preferred distribution shows the relative proportion of votes going to the two major parties."),
                    plotly::renderPlotly({
                      
                      data = tpp %>% dplyr::filter(YEAR == input$hor_year)
                      
                      data = data %>% dplyr::filter(DivisionNm == toupper(click$id)) %>%
                        dplyr::select(LNP_Votes, ALP_Votes, Swing) %>%
                        tidyr::pivot_longer(cols = c("LNP_Votes", "ALP_Votes"), names_to = "Party", values_to = "VALUE") %>%
                        dplyr::mutate(Party = gsub("\\_Votes$", "", Party)) %>%
                        dplyr::mutate(Percentage = scales::percent(VALUE/sum(VALUE), 0.01),
                                      Votes = scales::comma(VALUE, 1),
                                      Swing = ifelse(Party == "ALP", -Swing, Swing),
                                      Swing = ifelse(Swing > 0, paste0("+", Swing, " pp"), paste0(Swing, " pp")))
                      
                      plotly::ggplotly(
                        suppressWarnings(
                          ggplot2::ggplot(data = data) +
                            ggplot2::geom_bar(ggplot2::aes(x = Party, y = VALUE, fill = Party, group = Votes, id = Swing, bla = Percentage), stat = "identity", show.legend = FALSE) +
                            ggplot2::theme(panel.grid.major = ggplot2::element_blank(),
                                           panel.grid.minor = ggplot2::element_blank(),
                                           panel.border = ggplot2::element_blank(),
                                           panel.background = ggplot2::element_blank(),
                                           axis.title.x = ggplot2::element_blank(),
                                           axis.text.y = ggplot2::element_blank(),
                                           axis.ticks.y = ggplot2::element_blank(),
                                           axis.text.x = ggplot2::element_text(angle = 45, hjust = 1, vjust = 0.5),
                                           legend.position = 'none') +
                            ggplot2::scale_fill_manual(values = list(ALP = "#FF0000", LNP = "#00008B")) +
                            ggplot2::labs(x = NULL,
                                          title = NULL,
                                          subtitle = NULL,
                                          y = NULL))
                        
                        , tooltip = c("Votes", "Percentage", "Swing")) %>% 
                        plotly::config(displayModeBar = FALSE) %>%
                        plotly::layout(xaxis = list(fixedrange = TRUE), yaxis = list(fixedrange = TRUE))
                      
                      
                    })
      )
    )
    
    
  }) %>% shiny::bindCache(input$hor_year, input$leafletplot_shape_click$id)
  
  
  output$preferencing = shiny::renderUI({
    
    shiny::validate(
      shiny::need(input$hor_year, message = FALSE),
      shiny::need(input$leafletplot_shape_click, message = FALSE)
    )
    
    click = input$leafletplot_shape_click
    
    if (!click$id %in% x$DivisionNm[x$YEAR==input$hor_year]) {
      #click$id = x$DivisionNm[x$YEAR==input$hor_year][1]
      return(NULL)
    }
    
    shiny::validate(
      shiny::need(!is.null(click$id), message = FALSE)
    )
    
    
    list(
      shiny::hr(),
      shiny::br(),
      shiny::br(),
      shiny::h3(shiny::code("Preferences")),
      shiny::br(),
      
      shiny::fluidRow(
        shiny::column(width = 3, 
                      shiny::br(),
                      shiny::tags$p(shiny::HTML(paste0("This is a <em>Sankey diagram</em>, showing the distribution of preferences for <strong>", click$id, "</strong> in <strong>", input$hor_year, "</strong>.")), style = "text-align: justify;"),
                      shiny::tags$p(shiny::HTML(paste0("The columns represent the various rounds of preferences. The first column is the Primary vote. The size of the coloured nodes are proportional to amount of votes that party holds.")), style = "text-align: justify;"),
                      shiny::tags$p(shiny::HTML(paste0("The flows between nodes show the number of votes being distributed from an eliminated party to the other remaining parties. ")), style = "text-align: justify;"),
                      shiny::tags$p(shiny::HTML(paste0("Once a party is eliminated, it is removed from the flow. If in any count a party has an absolute majority of votes, then that party is the winner.")), style = "text-align: justify;"),
                      shiny::tags$p(shiny::HTML(paste0("It is interesting to observe how <em>voters</em> in each electorate distribute their preferences.")), style = "text-align: justify;"),
                      shiny::br()
        ),
        #shiny::br(),
        #shiny::fluidRow(
        shiny::column(width = 8, offset = 1, 
                      htmlwidgets::onRender(create_sankey(x, input$hor_year, click$id, 
                                                          height = 600 + max(x %>% dplyr::filter(YEAR == input$hor_year, DivisionNm == click$id, CountNumber == 0, CalculationType == "Preference Count") %>% nrow() - 8, 0)*25), 
                                            'function(el) { 
                          
                            var cols_x = this.sankey.nodes().map(d => d.x).filter((v, i, a) => a.indexOf(v) === i).sort(function(a, b){return a - b});
                            
                            cols_x.forEach((d, i) => {
                              d3.select(el).select("svg")
                                .append("text")
                                .attr("x", d + 27)
                                .attr("y", 10)
                                .attr("font-weight", "bold")
                                .attr("text-align", "left")
                                .text("R" + (i + 1))
                            });
                          
                          d3.selectAll("rect")
                            .attr("stroke-width", 0);
                          }')
                      
                      
                      
        ))
    )
    
    
  }) %>% shiny::bindCache(input$hor_year, input$leafletplot_shape_click$id)
  
  
  
  output$demographics = shiny::renderUI({
    
    shiny::validate(
      shiny::need(input$leafletplot_shape_click, message = FALSE)
    )
    
    click = input$leafletplot_shape_click
    
    if (!click$id %in% x$DivisionNm[x$YEAR==input$hor_year]) {
      #click$id = x$DivisionNm[x$YEAR==input$hor_year][1]
      return(NULL)
    }
    
    
    
    list(
      shiny::hr(),
      shiny::br(),
      shiny::br(),
      shiny::h3(shiny::code("Demographics")),
      shiny::tags$p(shiny::HTML(paste0("This section covers some key demographics for <strong>", click$id, "</strong> in <strong>", input$hor_year, "</strong>.")), style = "text-align: justify;"),
      shiny::tags$p(shiny::HTML(paste0("The data used in this section is pulled directly from the <a href = 'https://CRAN.R-project.org/package=eechidna' target = '_blank'>eechidna R package</a>.")), style = "text-align: justify;"),
      shiny::br(),
      shiny::br(),
      shiny::fluidRow(shiny::h4(shiny::code("Occupation", style = "color: #152238")),
                      shiny::column(width = 3,
                                    shiny::tags$p(shiny::HTML(paste0("This boxplot shows the proportion of people working in different professions across all the electorates. Each dark point in the boxplots represent where <strong>", click$id, "</strong> sits relative to the other electorates.")), style = "text-align: justify;"),
                                    shiny::hr(),
                                    shiny::tags$p(shiny::HTML(paste0("<b><em>Distributive</em></b> is defined as wholesale trade, retail trade, transport, post or warehousing related industries.")), style = "text-align: justify; font-size: 8pt;"),
                                    shiny::tags$p(shiny::HTML(paste0("<b><em>Extractive</em></b> is defined as mining, gas, water, agriculture, waste and electricity.")), style = "text-align: justify; font-size: 8pt;"),
                                    shiny::tags$p(shiny::HTML(paste0("<b><em>Transformative</em></b> is defined as construction or manufacturing related industries.")), style = "text-align: justify; font-size: 8pt;"),
                      ),
                      shiny::column(width = 7, offset = 1,
                                    plotly::plotlyOutput("demo_profession", height = "500px")
                      )
      ),
      shiny::hr(),
      shiny::br(),
      shiny::fluidRow(shiny::h4(shiny::code("Income", style = "color: #152238")),
                      shiny::column(width = 3,
                                    shiny::tags$p(shiny::HTML(paste0("This boxplot shows the median weekly income across all the electorates. Each dark point in the boxplots represent where <strong>", click$id, "</strong> sits relative to the other electorates.")), style = "text-align: justify;"),
                                    shiny::tags$p(shiny::HTML(paste0("All dollar amounts are weekly incomes.")), style = "text-align: justify;"),
                      ),
                      shiny::column(width = 7, offset = 1,
                                    plotly::plotlyOutput("demo_income", height = "400px")
                      )
      ),
      shiny::hr(),
      shiny::br(),
      shiny::fluidRow(shiny::h4(shiny::code("Age", style = "color: #152238")),
                      shiny::column(width = 3,
                                    shiny::tags$p(shiny::HTML(paste0("The left boxplot shows the median age across all the electorates. Each dark point in the boxplots represent where <strong>", click$id, "</strong> sits relative to the other electorates.")), style = "text-align: justify;"),
                                    shiny::tags$p(shiny::HTML(paste0("The right boxplot shows the breakdown of the population into discrete age groups and how this compares to other electorates.")), style = "text-align: justify;"),
                      ),
                      shiny::column(width = 3, offset = 1,
                                    plotly::plotlyOutput("demo_age", height = "400px")
                      ),
                      shiny::column(width = 5, offset = 0,
                                    plotly::plotlyOutput("demo_age2", height = "400px")
                      )
      ),
      shiny::hr(),
      shiny::br(),
      shiny::fluidRow(shiny::h4(shiny::code("Education", style = "color: #152238")),
                      shiny::column(width = 3,
                                    shiny::tags$p(shiny::HTML(paste0("This boxplot shows the proportion of people with different education levels across electorates. Each dark point in the boxplots represent where <strong>", click$id, "</strong> sits relative to the other electorates.")), style = "text-align: justify;"),
                                    shiny::hr(),
                                    shiny::tags$p(shiny::HTML(paste0("<b><em>High School</em></b> is defined as the percentage of people who have completed high school.")), style = "text-align: justify; font-size: 8pt;"),
                                    shiny::tags$p(shiny::HTML(paste0("<b><em>Diploma/Certificate</em></b> is defined as the percentage of people who have completed a diploma or certificate.")), style = "text-align: justify; font-size: 8pt;"),
                                    shiny::tags$p(shiny::HTML(paste0("<b><em>Bachelor Degree or Above</em></b> is defined as the percentage of people who have completed a Bachelor degree or above.")), style = "text-align: justify; font-size: 8pt;"),
                      ),
                      shiny::column(width = 7, offset = 1,
                                    plotly::plotlyOutput("demo_edu", height = "400px")
                      )
      ),
      shiny::hr(),
      shiny::br(),
      shiny::fluidRow(shiny::h4(shiny::code("Religion", style = "color: #152238")),
                      shiny::column(width = 3,
                                    shiny::tags$p(shiny::HTML(paste0("This boxplot shows the proportion of people who have different religious beliefs. Each dark point in the boxplots represent where <strong>", click$id, "</strong> sits relative to the other electorates.")), style = "text-align: justify;"),
                      ),
                      shiny::column(width = 7, offset = 1,
                                    plotly::plotlyOutput("demo_religion", height = "400px")
                      )
      ),
      shiny::hr(),
      shiny::br(),
      shiny::fluidRow(shiny::h4(shiny::code("Birth place", style = "color: #152238")),
                      shiny::column(width = 3,
                                    shiny::tags$p(shiny::HTML(paste0("This boxplot shows the proportion of people with different birth places across electorates. Each dark point in the boxplots represent where <strong>", click$id, "</strong> sits relative to the other electorates.")), style = "text-align: justify;"),
                      ),
                      shiny::column(width = 7, offset = 1,
                                    plotly::plotlyOutput("demo_birth", height = "400px")
                      )
      ),
      shiny::hr(),
      shiny::br(),
      shiny::fluidRow(shiny::h4(shiny::code("Housing", style = "color: #152238")),
                      shiny::column(width = 3,
                                    shiny::tags$p(shiny::HTML(paste0("This boxplot shows the proportion of different housing metrics across electorates. Each dark point in the boxplots represent where <strong>", click$id, "</strong> sits relative to the other electorates.")), style = "text-align: justify;"),
                                    shiny::hr(),
                                    shiny::tags$p(shiny::HTML(paste0("<b><em>Mortgage</em></b> is defined as the percentage of people who have a mortgage.")), style = "text-align: justify; font-size: 8pt;"),
                                    shiny::tags$p(shiny::HTML(paste0("<b><em>Owned</em></b> is defined as the percentage of dwellings that are owned outright.")), style = "text-align: justify; font-size: 8pt;"),
                                    shiny::tags$p(shiny::HTML(paste0("<b><em>Public Housing</em></b> is defined as the percentage of dwellings that are owned by government and rented out.")), style = "text-align: justify; font-size: 8pt;"),
                                    shiny::tags$p(shiny::HTML(paste0("<b><em>Renting</em></b> is defined as the percentage of dwellings that are being rented.")), style = "text-align: justify; font-size: 8pt;"),
                                    shiny::tags$p(shiny::HTML(paste0("<b><em>Single person house</em></b> is defined as the percentage of households occupied by a single person.")), style = "text-align: justify; font-size: 8pt;"),
                                    shiny::tags$p(shiny::HTML(paste0("<b><em>One parent house</em></b> is defined as the percentage of households made up on one parent and children.")), style = "text-align: justify; font-size: 8pt;"),
                                    shiny::tags$p(shiny::HTML(paste0("<b><em>Married</em></b> is defined as the percentage of people who are married.")), style = "text-align: justify; font-size: 8pt;"),
                                    shiny::tags$p(shiny::HTML(paste0("<b><em>De Facto</em></b> is defined as the percentage of people who are in a de facto marriage")), style = "text-align: justify; font-size: 8pt;"),
                                    shiny::tags$p(shiny::HTML(paste0("<b><em>Couple with child</em></b> is defined as the percentage of hoouseholds made up of a couple with children.")), style = "text-align: justify; font-size: 8pt;"),
                                    shiny::tags$p(shiny::HTML(paste0("<b><em>Couple with no child</em></b> is defined as the percentage of hoouseholds made up of a couple without children.")), style = "text-align: justify; font-size: 8pt;"),
                                    
                                    
                      ),
                      shiny::column(width = 7, offset = 1,
                                    plotly::plotlyOutput("demo_housing", height = "450px")
                      )
      ),
      shiny::hr(),
      shiny::br(),
      shiny::fluidRow(shiny::h4(shiny::code("Population density", style = "color: #152238")),
                      shiny::column(width = 3,
                                    shiny::tags$p(shiny::HTML(paste0("This boxplot shows the population density across electorates. Each dark point in the boxplots represent where <strong>", click$id, "</strong> sits relative to the other electorates.")), style = "text-align: justify;"),
                                    shiny::hr(),
                                    shiny::tags$p(shiny::HTML(paste0("<b><em>Population density</em></b> is defined as the population of the electorate divided by the area of the electorate (people/km<sup>2</sup>).")), style = "text-align: justify; font-size: 8pt;")
                                    
                      ),
                      shiny::column(width = 7, offset = 1,
                                    plotly::plotlyOutput("demo_pop", height = "450px")
                      )
      ),
      shiny::hr(),
      shiny::br(),
      shiny::fluidRow(shiny::h4(shiny::code("Other", style = "color: #152238")),
                      shiny::column(width = 3,
                                    shiny::tags$p(shiny::HTML(paste0("This boxplot shows some other metrics across all the electorates. Each dark point in the boxplots represent where <strong>", click$id, "</strong> sits relative to the other electorates.")), style = "text-align: justify;"),
                                    shiny::hr(),
                                    shiny::tags$p(shiny::HTML(paste0("<b><em>Internet Access</em></b> is defined as the percentage of people who have access to the internet.")), style = "text-align: justify; font-size: 8pt;"),
                                    shiny::tags$p(shiny::HTML(paste0("<b><em>English Language Only</em></b> is defined as the percentage of people who speak only English at home.")), style = "text-align: justify; font-size: 8pt;"),
                                    shiny::tags$p(shiny::HTML(paste0("<b><em>Other Language at Home</em></b> is defined as the percentage of people who speak a language other than English at home.")), style = "text-align: justify; font-size: 8pt;"),
                                    shiny::tags$p(shiny::HTML(paste0("<b><em>Indigenous</em></b> is defined as the percentage of people who are Indigenous.")), style = "text-align: justify; font-size: 8pt;"),
                      ),
                      shiny::column(width = 7, offset = 1,
                                    plotly::plotlyOutput("demo_other", height = "400px")
                      )
      ),
      shiny::br()
      
      
      
      
    )
    
    
  }) %>% shiny::bindCache(input$hor_year, input$leafletplot_shape_click$id)
  
  
  #})
  
  
  output$demo_age = plotly::renderPlotly({
    
    shiny::validate(
      shiny::need(input$leafletplot_shape_click, message = FALSE)
    )
    
    click = input$leafletplot_shape_click
    
    if (!click$id %in% x$DivisionNm[x$YEAR==input$hor_year]) {
      #click$id = x$DivisionNm[x$YEAR==input$hor_year][1]
      return(NULL)
    }
    
    
    data = abs %>% 
      dplyr::filter(YEAR == input$hor_year) %>%
      dplyr::select(DivisionNm, MedianAge) %>%
      tidyr::pivot_longer(cols = c("MedianAge"), names_to = "TYPE", values_to = "VALUE")
    
    data = data %>% 
      dplyr::group_by(TYPE) %>%
      dplyr::mutate(RANK = paste0(scales::ordinal(rank(-VALUE)), " / ", length(unique(DivisionNm)))) %>%
      dplyr::ungroup()
    
    data$TYPE = gsub("MedianAge", "Median Age", data$TYPE)
    
    data2 = data %>% dplyr::filter(DivisionNm == toupper(click$id))
    data = data %>% dplyr::filter(DivisionNm != toupper(click$id))
    
    options(warn = -1)
    suppressMessages(
      plotly::plot_ly(data, 
                      y = ~VALUE, 
                      color = ~TYPE, 
                      type = "box", 
                      boxpoints = "all", 
                      jitter = 150, 
                      pointpos = 0, 
                      opacity = 0.3, 
                      legendgroup  = ~TYPE,
                      hoverinfo = 'text',
                      hovertext = paste0("<br><b>Electorate: </b>", data$DivisionNm, "</b><br>", "<b>Age: </b>", scales::number(data$VALUE, 0.01), "<br><b>Rank: </b>", data$RANK)) %>%
        plotly::add_markers(data = data2, 
                            x = ~TYPE, 
                            y = ~VALUE, 
                            marker = list(color = "#000000FF", opacity = 1, size = 6), 
                            legendgroup  = ~TYPE,
                            showlegend = F,
                            hoverinfo = 'text',
                            hovertext = paste0("<br><b>Electorate: </b>", data2$DivisionNm, "</b><br>", "<b>Age: </b>", scales::number(data2$VALUE, 0.01), "<br><b>Rank: </b>", data2$RANK, "<br><b>Group: </b>", data2$TYPE)) %>%
        plotly::config(displayModeBar = FALSE) %>% 
        plotly::layout(xaxis = list(fixedrange = TRUE, title = NA), 
                       yaxis = list(fixedrange = TRUE, title = NA))
    )
    
    
  }) %>% shiny::bindCache(input$hor_year, input$leafletplot_shape_click$id)
  
  
  #output$demo_age2 = plotly::renderPlotly({
  #  
  #  shiny::validate(
  #    shiny::need(input$leafletplot_shape_click, message = FALSE)
  #  )
  #  
  #  click = input$leafletplot_shape_click
  #  
  #  if (!click$id %in% x$DivisionNm[x$YEAR==input$hor_year]) {
  #    click$id = x$DivisionNm[x$YEAR==input$hor_year][1]
  #  }
  #  
  #  
  #  data = demo %>% dplyr::filter(DivisionNm == toupper(click$id), STAT == "age", YEAR == input$hor_year) %>% dplyr::rename(Age = TYPE) %>% dplyr::mutate(Age = gsub("-", " - ", Age))
  #  data$Age = factor(data$Age, levels = sort(unique(data$Age)))
  #  blue = RColorBrewer::brewer.pal(9, "Blues")
  #  n = unique(data$Age) %>% length() + 1
  #  colors = c(colorRampPalette(c(blue[2], blue[6]))(floor(n/2)), colorRampPalette(c(blue[6], blue[9]))(ceiling(n/2))[-1])
  #  
  #  plotly::plot_ly(data, 
  #                  labels = ~Age, 
  #                  values = ~VALUE, 
  #                  type = 'pie',
  #                  textposition = 'inside',
  #                  hoverinfo = "text+percent",
  #                  outsidetextfont = list(color = '#000000'),
  #                  text = ~Age,
  #                  marker = list(colors = colors,
  #                                line = list(color = '#FFFFFF', width = 2)
  #                  ),
  #                  showlegend = FALSE,
  #                  sort = FALSE) %>% 
  #    plotly::config(displayModeBar = FALSE) %>%
  #    plotly::layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
  #                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
  #  
  #}) %>% shiny::bindCache(input$hor_year, input$leafletplot_shape_click$id)
  
  
  
  output$demo_age2 = plotly::renderPlotly({
    
    shiny::validate(
      shiny::need(input$leafletplot_shape_click, message = FALSE)
    )
    
    click = input$leafletplot_shape_click
    
    if (!click$id %in% x$DivisionNm[x$YEAR==input$hor_year]) {
      #click$id = x$DivisionNm[x$YEAR==input$hor_year][1]
      return(NULL)
    }
    
    data = abs %>% 
      dplyr::filter(YEAR == input$hor_year) %>%
      dplyr::select(DivisionNm, dplyr::starts_with("Age")) %>%
      tidyr::pivot_longer(cols = names(abs)[grepl("^Age", names(abs))], names_to = "TYPE", values_to = "VALUE")
    
    data = data %>% 
      dplyr::group_by(TYPE) %>%
      dplyr::mutate(RANK = paste0(scales::ordinal(rank(-VALUE)), " / ", length(unique(DivisionNm)))) %>%
      dplyr::ungroup()
    
    data$TYPE = gsub("\\_", "-", data$TYPE)
    data$TYPE = gsub("^Age", "", data$TYPE)
    data$TYPE = gsub("00-04", "0-4", data$TYPE)
    data$TYPE = gsub("05-14", "5-14", data$TYPE)
    data$TYPE = gsub("plus$", "+", data$TYPE)
    
    data$TYPE = factor(data$TYPE, levels = data$TYPE %>% unique())
    data$VALUE = data$VALUE/100
    
    data2 = data %>% dplyr::filter(DivisionNm == toupper(click$id))
    data = data %>% dplyr::filter(DivisionNm != toupper(click$id))
    
    options(warn = -1)
    suppressMessages(
      plotly::plot_ly(data, 
                      y = ~VALUE, 
                      color = ~TYPE, 
                      type = "box", 
                      boxpoints = "all", 
                      jitter = 150, 
                      pointpos = 0, 
                      opacity = 0.3, 
                      legendgroup  = ~TYPE,
                      hoverinfo = 'text',
                      hovertext = paste0("<br><b>Electorate: </b>", data$DivisionNm, "</b><br>", "<b>Proportion: </b>", scales::percent(data$VALUE, 0.01), "<br><b>Rank: </b>", data$RANK, "<br><b>Group: </b>", data$TYPE)) %>%
        plotly::add_markers(data = data2, 
                            x = ~TYPE, 
                            y = ~VALUE, 
                            marker = list(color = "#000000FF", opacity = 1, size = 6), 
                            legendgroup  = ~TYPE,
                            showlegend = F,
                            hoverinfo = 'text',
                            hovertext = paste0("<br><b>Electorate: </b>", data2$DivisionNm, "</b><br>", "<b>Proportion: </b>", scales::percent(data2$VALUE, 0.01), "<br><b>Rank: </b>", data2$RANK, "<br><b>Group: </b>", data2$TYPE)) %>%
        #plotly::config(displayModeBar = FALSE) %>% 
        plotly::layout(xaxis = list(fixedrange = FALSE, title = NA), 
                       yaxis = list(fixedrange = FALSE, title = NA, tickformat = ".0%"))
    )
  
  
  }) %>% shiny::bindCache(input$hor_year, input$leafletplot_shape_click$id)
  
  
  ######################## issues #########################
  
  
  output$issues = plotly::renderPlotly({
    
    shiny::validate(
      shiny::need(input$hor_year, message = FALSE)
    )
    
    data = issues_df %>% dplyr::filter(YEAR == input$hor_year)
    blue = RColorBrewer::brewer.pal(9, "Blues")
    n = unique(data$Issue) %>% length() + 1
    colors = c(colorRampPalette(c(blue[2], blue[6]))(floor(n/2)), colorRampPalette(c(blue[6], blue[9]))(ceiling(n/2))[-1])
    
    plotly::plot_ly(data, 
                    labels = ~Issue, 
                    values = ~VALUE, 
                    type = 'pie',
                    textposition = 'inside',
                    hoverinfo = "text+percent",
                    outsidetextfont = list(color = '#000000'),
                    text = ~Issue,
                    marker = list(colors = colors,
                                  line = list(color = '#FFFFFF', width = 2)
                    ),
                    showlegend = FALSE,
                    sort = FALSE) %>% 
      plotly::config(displayModeBar = FALSE) %>%
      plotly::layout(xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                     yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
    
  }) %>% shiny::bindCache(input$hor_year)
  
  
  output$demo_profession = plotly::renderPlotly({
    
    shiny::validate(
      shiny::need(input$leafletplot_shape_click, message = FALSE)
    )
    
    click = input$leafletplot_shape_click
    
    if (!click$id %in% x$DivisionNm[x$YEAR==input$hor_year]) {
      #click$id = x$DivisionNm[x$YEAR==input$hor_year][1]
      return(NULL)
    }
    
    
    data = abs %>% 
      dplyr::filter(YEAR == input$hor_year) %>%
      dplyr::select(DivisionNm, Professional, Tradesperson, Extractive, CurrentlyStudying, Laborer, Transformative, Finance, Professional, ManagerAdminClericalSales, Distributive, Volunteer, SocialServ, Unemployed) %>%
      tidyr::pivot_longer(cols = c("Professional", "Tradesperson", "Extractive", "CurrentlyStudying", "Laborer", "Transformative", "Finance", "Professional", "ManagerAdminClericalSales", "Distributive", "Volunteer", "SocialServ", "Unemployed"), names_to = "TYPE", values_to = "VALUE")
    
    data = data %>% 
      dplyr::group_by(TYPE) %>%
      dplyr::mutate(RANK = paste0(scales::ordinal(rank(-VALUE)), " / ", length(unique(DivisionNm)))) %>%
      dplyr::ungroup()
    
    data$TYPE = gsub("ManagerAdminClericalSales", "Manager/Admin/Clerical/Sales", data$TYPE)
    data$TYPE = gsub("SocialServ", "Social Services", data$TYPE)
    data$TYPE = gsub("CurrentlyStudying", "Currently Studying", data$TYPE)
    data = data %>% dplyr::mutate(VALUE = VALUE/100)
    
    data2 = data %>% dplyr::filter(DivisionNm == toupper(click$id))
    data = data %>% dplyr::filter(DivisionNm != toupper(click$id))
    
    options(warn = -1)
    suppressMessages(
      plotly::plot_ly(data, 
                      y = ~VALUE, 
                      color = ~TYPE, 
                      type = "box", 
                      boxpoints = "all", 
                      jitter = 150, 
                      pointpos = 0, 
                      opacity = 0.3, 
                      legendgroup  = ~TYPE,
                      hoverinfo = 'text',
                      hovertext = paste0("<br><b>Electorate: </b>", data$DivisionNm, "</b><br>", "<b>Proportion: </b>", scales::percent(data$VALUE, 0.01), "<br><b>Rank: </b>", data$RANK, "<br><b>Group: </b>", data$TYPE)) %>%
        plotly::add_markers(data = data2, 
                            x = ~TYPE, 
                            y = ~VALUE, 
                            marker = list(color = "#000000FF", opacity = 1, size = 6), 
                            legendgroup  = ~TYPE,
                            showlegend = F,
                            hoverinfo = 'text',
                            hovertext = paste0("<br><b>Electorate: </b>", data2$DivisionNm, "</b><br>", "<b>Proportion: </b>", scales::percent(data2$VALUE, 0.01), "<br><b>Rank: </b>", data2$RANK, "<br><b>Group: </b>", data2$TYPE)) %>%
        #plotly::config(displayModeBar = FALSE) %>% 
        plotly::layout(xaxis = list(fixedrange = FALSE, title = NA), 
                       yaxis = list(fixedrange = FALSE, title = NA, tickformat = ".0%"))
    )
    
    
  }) %>% shiny::bindCache(input$hor_year, input$leafletplot_shape_click$id)
  
  
  output$demo_income = plotly::renderPlotly({
    
    shiny::validate(
      shiny::need(input$leafletplot_shape_click, message = FALSE)
    )
    
    click = input$leafletplot_shape_click
    
    if (!click$id %in% x$DivisionNm[x$YEAR==input$hor_year]) {
      #click$id = x$DivisionNm[x$YEAR==input$hor_year][1]
      return(NULL)
    }
    
    
    data = abs %>% 
      dplyr::filter(YEAR == input$hor_year) %>%
      dplyr::select(DivisionNm, MedianFamilyIncome, MedianHouseholdIncome, MedianPersonalIncome) %>%
      tidyr::pivot_longer(cols = c("MedianFamilyIncome", "MedianHouseholdIncome", "MedianPersonalIncome"), names_to = "TYPE", values_to = "VALUE")
    
    data = data %>% 
      dplyr::group_by(TYPE) %>%
      dplyr::mutate(RANK = paste0(scales::ordinal(rank(-VALUE)), " / ", length(unique(DivisionNm)))) %>%
      dplyr::ungroup()
    
    data$TYPE = gsub("MedianFamilyIncome", "Median Family Income", data$TYPE)
    data$TYPE = gsub("MedianHouseholdIncome", "Median Household Income", data$TYPE)
    data$TYPE = gsub("MedianPersonalIncome", "Median Personal Income", data$TYPE)
    
    data2 = data %>% dplyr::filter(DivisionNm == toupper(click$id))
    data = data %>% dplyr::filter(DivisionNm != toupper(click$id))
    
    options(warn = -1)
    suppressMessages(
      plotly::plot_ly(data, 
                      y = ~VALUE, 
                      color = ~TYPE, 
                      type = "box", 
                      boxpoints = "all", 
                      jitter = 150, 
                      pointpos = 0, 
                      opacity = 0.3, 
                      legendgroup  = ~TYPE,
                      hoverinfo = 'text',
                      hovertext = paste0("<br><b>Electorate: </b>", data$DivisionNm, "</b><br>", "<b>Income: </b>", scales::dollar(data$VALUE), "<br><b>Rank: </b>", data$RANK, "<br><b>Group: </b>", data$TYPE)) %>%
        plotly::add_markers(data = data2, 
                            x = ~TYPE, 
                            y = ~VALUE, 
                            marker = list(color = "#000000FF", opacity = 1, size = 6), 
                            legendgroup  = ~TYPE,
                            showlegend = F,
                            hoverinfo = 'text',
                            hovertext = paste0("<br><b>Electorate: </b>", data2$DivisionNm, "</b><br>", "<b>Income: </b>", scales::dollar(data2$VALUE), "<br><b>Rank: </b>", data2$RANK, "<br><b>Group: </b>", data2$TYPE)) %>%
        #plotly::config(displayModeBar = FALSE) %>% 
        plotly::layout(xaxis = list(fixedrange = FALSE, title = NA), 
                       yaxis = list(fixedrange = FALSE, title = NA, tickformat = "$", rangemode = "tozero"))
    )
    
    
  }) %>% shiny::bindCache(input$hor_year, input$leafletplot_shape_click$id)
  
  
  output$demo_edu = plotly::renderPlotly({
    
    shiny::validate(
      shiny::need(input$leafletplot_shape_click, message = FALSE)
    )
    
    click = input$leafletplot_shape_click
    
    if (!click$id %in% x$DivisionNm[x$YEAR==input$hor_year]) {
      #click$id = x$DivisionNm[x$YEAR==input$hor_year][1]
      return(NULL)
    }
    
    
    data = abs %>% 
      dplyr::filter(YEAR == input$hor_year) %>%
      dplyr::select(DivisionNm, HighSchool, DipCert, BachelorAbv) %>%
      tidyr::pivot_longer(cols = c("HighSchool", "DipCert", "BachelorAbv"), names_to = "TYPE", values_to = "VALUE")
    
    data = data %>% 
      dplyr::group_by(TYPE) %>%
      dplyr::mutate(RANK = paste0(scales::ordinal(rank(-VALUE)), " / ", length(unique(DivisionNm)))) %>%
      dplyr::ungroup()
    
    data$TYPE = gsub("HighSchool", "High School", data$TYPE)
    data$TYPE = gsub("BachelorAbv", "Bachelor Degree or Above", data$TYPE)
    data$TYPE = gsub("DipCert", "Diploma/Certificate", data$TYPE)
    data = data %>% dplyr::mutate(VALUE = VALUE/100)
    
    data2 = data %>% dplyr::filter(DivisionNm == toupper(click$id))
    data = data %>% dplyr::filter(DivisionNm != toupper(click$id))
    
    options(warn = -1)
    suppressMessages(
      plotly::plot_ly(data, 
                      y = ~VALUE, 
                      color = ~TYPE, 
                      type = "box", 
                      boxpoints = "all", 
                      jitter = 150, 
                      pointpos = 0, 
                      opacity = 0.3, 
                      legendgroup  = ~TYPE,
                      hoverinfo = 'text',
                      hovertext = paste0("<br><b>Electorate: </b>", data$DivisionNm, "</b><br>", "<b>Proportion: </b>", scales::percent(data$VALUE, 0.01), "<br><b>Rank: </b>", data$RANK, "<br><b>Group: </b>", data$TYPE)) %>%
        plotly::add_markers(data = data2, 
                            x = ~TYPE, 
                            y = ~VALUE, 
                            marker = list(color = "#000000FF", opacity = 1, size = 6), 
                            legendgroup  = ~TYPE,
                            showlegend = F,
                            hoverinfo = 'text',
                            hovertext = paste0("<br><b>Electorate: </b>", data2$DivisionNm, "</b><br>", "<b>Proportion: </b>", scales::percent(data2$VALUE, 0.01), "<br><b>Rank: </b>", data2$RANK, "<br><b>Group: </b>", data2$TYPE)) %>%
        #plotly::config(displayModeBar = FALSE) %>% 
        plotly::layout(xaxis = list(fixedrange = FALSE, title = NA), 
                       yaxis = list(fixedrange = FALSE, title = NA, tickformat = ".0%", rangemode = "tozero"))
    )
    
    
  }) %>% shiny::bindCache(input$hor_year, input$leafletplot_shape_click$id)
  
  
  output$demo_religion = plotly::renderPlotly({
    
    shiny::validate(
      shiny::need(input$leafletplot_shape_click, message = FALSE)
    )
    
    click = input$leafletplot_shape_click
    
    if (!click$id %in% x$DivisionNm[x$YEAR==input$hor_year]) {
      #click$id = x$DivisionNm[x$YEAR==input$hor_year][1]
      return(NULL)
    }
    
    
    data = abs %>% 
      dplyr::filter(YEAR == input$hor_year) %>%
      dplyr::select(DivisionNm, Catholic, Christianity, Islam, Judaism, Anglican, Buddhism, NoReligion) %>%
      tidyr::pivot_longer(cols = c("Catholic", "Christianity", "Islam", "Judaism", "Anglican", "Buddhism", "NoReligion"), names_to = "TYPE", values_to = "VALUE")
    
    data = data %>% 
      dplyr::group_by(TYPE) %>%
      dplyr::mutate(RANK = paste0(scales::ordinal(rank(-VALUE)), " / ", length(unique(DivisionNm)))) %>%
      dplyr::ungroup()
    
    data$TYPE = gsub("NoReligion", "No Religion", data$TYPE)
    data = data %>% dplyr::mutate(VALUE = VALUE/100)
    
    data2 = data %>% dplyr::filter(DivisionNm == toupper(click$id))
    data = data %>% dplyr::filter(DivisionNm != toupper(click$id))
    
    options(warn = -1)
    suppressMessages(
      plotly::plot_ly(data, 
                      y = ~VALUE, 
                      color = ~TYPE, 
                      type = "box", 
                      boxpoints = "all", 
                      jitter = 150, 
                      pointpos = 0, 
                      opacity = 0.3, 
                      legendgroup  = ~TYPE,
                      hoverinfo = 'text',
                      hovertext = paste0("<br><b>Electorate: </b>", data$DivisionNm, "</b><br>", "<b>Proportion: </b>", scales::percent(data$VALUE, 0.01), "<br><b>Rank: </b>", data$RANK, "<br><b>Group: </b>", data$TYPE)) %>%
        plotly::add_markers(data = data2, 
                            x = ~TYPE, 
                            y = ~VALUE, 
                            marker = list(color = "#000000FF", opacity = 1, size = 6), 
                            legendgroup  = ~TYPE,
                            showlegend = F,
                            hoverinfo = 'text',
                            hovertext = paste0("<br><b>Electorate: </b>", data2$DivisionNm, "</b><br>", "<b>Proportion: </b>", scales::percent(data2$VALUE, 0.01), "<br><b>Rank: </b>", data2$RANK, "<br><b>Group: </b>", data2$TYPE)) %>%
        #plotly::config(displayModeBar = FALSE) %>% 
        plotly::layout(xaxis = list(fixedrange = FALSE, title = NA), 
                       yaxis = list(fixedrange = FALSE, title = NA, tickformat = ".0%"))
    )
    
    
  }) %>% shiny::bindCache(input$hor_year, input$leafletplot_shape_click$id)
  
  
  output$demo_birth = plotly::renderPlotly({
    
    shiny::validate(
      shiny::need(input$leafletplot_shape_click, message = FALSE)
    )
    
    click = input$leafletplot_shape_click
    
    if (!click$id %in% x$DivisionNm[x$YEAR==input$hor_year]) {
      #click$id = x$DivisionNm[x$YEAR==input$hor_year][1]
      return(NULL)
    }
    
    
    data = abs %>% 
      dplyr::filter(YEAR == input$hor_year) %>%
      dplyr::select(DivisionNm, dplyr::starts_with("Born")) %>%
      dplyr::mutate(BornAus = 100 - Born_Asia - Born_MidEast - Born_SE_Europe - Born_UK - BornElsewhere) %>%
      tidyr::pivot_longer(cols = c("BornAus", "Born_Asia", "Born_MidEast", "Born_SE_Europe", "Born_UK", "BornElsewhere"), names_to = "TYPE", values_to = "VALUE")
    
    data = data %>% 
      dplyr::group_by(TYPE) %>%
      dplyr::mutate(RANK = paste0(scales::ordinal(rank(-VALUE)), " / ", length(unique(DivisionNm)))) %>%
      dplyr::ungroup()
    
    data$TYPE = gsub("BornAus", "Born in Australia", data$TYPE)
    data$TYPE = gsub("Born_Asia", "Born in Asia", data$TYPE)
    data$TYPE = gsub("Born_MidEast", "Born in Middle East", data$TYPE)
    data$TYPE = gsub("Born_SE_Europe", "Born in SE Europe", data$TYPE)
    data$TYPE = gsub("Born_UK", "Born in UK", data$TYPE)
    data$TYPE = gsub("BornElsewhere", "Born Elsewhere", data$TYPE)
    data = data %>% dplyr::mutate(VALUE = VALUE/100)
    
    data2 = data %>% dplyr::filter(DivisionNm == toupper(click$id))
    data = data %>% dplyr::filter(DivisionNm != toupper(click$id))
    
    options(warn = -1)
    suppressMessages(
      plotly::plot_ly(data, 
                      y = ~VALUE, 
                      color = ~TYPE, 
                      type = "box", 
                      boxpoints = "all", 
                      jitter = 150, 
                      pointpos = 0, 
                      opacity = 0.3, 
                      legendgroup  = ~TYPE,
                      hoverinfo = 'text',
                      hovertext = paste0("<br><b>Electorate: </b>", data$DivisionNm, "</b><br>", "<b>Proportion: </b>", scales::percent(data$VALUE, 0.01), "<br><b>Rank: </b>", data$RANK, "<br><b>Group: </b>", data$TYPE)) %>%
        plotly::add_markers(data = data2, 
                            x = ~TYPE, 
                            y = ~VALUE, 
                            marker = list(color = "#000000FF", opacity = 1, size = 6), 
                            legendgroup  = ~TYPE,
                            showlegend = F,
                            hoverinfo = 'text',
                            hovertext = paste0("<br><b>Electorate: </b>", data2$DivisionNm, "</b><br>", "<b>Proportion: </b>", scales::percent(data2$VALUE, 0.01), "<br><b>Rank: </b>", data2$RANK, "<br><b>Group: </b>", data2$TYPE)) %>%
        #plotly::config(displayModeBar = FALSE) %>% 
        plotly::layout(xaxis = list(fixedrange = FALSE, title = NA), 
                       yaxis = list(fixedrange = FALSE, title = NA, tickformat = ".0%"))
    )
    
    
  }) %>% shiny::bindCache(input$hor_year, input$leafletplot_shape_click$id)
  
  output$demo_housing = plotly::renderPlotly({
    
    shiny::validate(
      shiny::need(input$leafletplot_shape_click, message = FALSE)
    )
    
    click = input$leafletplot_shape_click
    
    if (!click$id %in% x$DivisionNm[x$YEAR==input$hor_year]) {
      #click$id = x$DivisionNm[x$YEAR==input$hor_year][1]
      return(NULL)
    }
    
    
    data = abs %>% 
      dplyr::filter(YEAR == input$hor_year) %>%
      dplyr::select(DivisionNm, OneParent_House, SP_House, Couple_NoChild_House, Couple_WChild_House, Owned, Mortgage, Renting, PublicHousing, DeFacto, Married) %>%
      tidyr::pivot_longer(cols = c("OneParent_House", "SP_House", "Couple_NoChild_House", "Couple_WChild_House", "Owned", "Mortgage", "Renting", "PublicHousing", "DeFacto", "Married"), names_to = "TYPE", values_to = "VALUE")
    
    data = data %>% 
      dplyr::group_by(TYPE) %>%
      dplyr::mutate(RANK = paste0(scales::ordinal(rank(-VALUE)), " / ", length(unique(DivisionNm)))) %>%
      dplyr::ungroup()
    
    data$TYPE = gsub("OneParent_House", "One parent house", data$TYPE)
    data$TYPE = gsub("SP_House", "Single person house", data$TYPE)
    data$TYPE = gsub("Couple_NoChild_House", "Couple with no child", data$TYPE)
    data$TYPE = gsub("Couple_WChild_House", "Couple with child", data$TYPE)
    data$TYPE = gsub("PublicHousing", "Public housing", data$TYPE)
    data$TYPE = gsub("DeFacto", "De Facto", data$TYPE)
    data = data %>% dplyr::mutate(VALUE = VALUE/100)
    
    data2 = data %>% dplyr::filter(DivisionNm == toupper(click$id))
    data = data %>% dplyr::filter(DivisionNm != toupper(click$id))
    
    options(warn = -1)
    suppressMessages(
      plotly::plot_ly(data, 
                      y = ~VALUE, 
                      color = ~TYPE, 
                      type = "box", 
                      boxpoints = "all", 
                      jitter = 150, 
                      pointpos = 0, 
                      opacity = 0.3, 
                      legendgroup  = ~TYPE,
                      hoverinfo = 'text',
                      hovertext = paste0("<br><b>Electorate: </b>", data$DivisionNm, "</b><br>", "<b>Proportion: </b>", scales::percent(data$VALUE, 0.01), "<br><b>Rank: </b>", data$RANK, "<br><b>Group: </b>", data$TYPE)) %>%
        plotly::add_markers(data = data2, 
                            x = ~TYPE, 
                            y = ~VALUE, 
                            marker = list(color = "#000000FF", opacity = 1, size = 6), 
                            legendgroup  = ~TYPE,
                            showlegend = F,
                            hoverinfo = 'text',
                            hovertext = paste0("<br><b>Electorate: </b>", data2$DivisionNm, "</b><br>", "<b>Proportion: </b>", scales::percent(data2$VALUE, 0.01), "<br><b>Rank: </b>", data2$RANK, "<br><b>Group: </b>", data2$TYPE)) %>%
        #plotly::config(displayModeBar = FALSE) %>% 
        plotly::layout(xaxis = list(fixedrange = FALSE, title = NA), 
                       yaxis = list(fixedrange = FALSE, title = NA, tickformat = ".0%"))
    )
    
    
  }) %>% shiny::bindCache(input$hor_year, input$leafletplot_shape_click$id)
  
  
  output$demo_other = plotly::renderPlotly({
    
    shiny::validate(
      shiny::need(input$leafletplot_shape_click, message = FALSE)
    )
    
    click = input$leafletplot_shape_click
    
    if (!click$id %in% x$DivisionNm[x$YEAR==input$hor_year]) {
      #click$id = x$DivisionNm[x$YEAR==input$hor_year][1]
      return(NULL)
    }
    
    
    data = abs %>% 
      dplyr::filter(YEAR == input$hor_year) %>%
      dplyr::select(DivisionNm, InternetAccess, EnglishOnly, OtherLanguageHome, Indigenous) %>%
      tidyr::pivot_longer(cols = c("InternetAccess", "EnglishOnly", "OtherLanguageHome", "Indigenous"), names_to = "TYPE", values_to = "VALUE")
    
    data = data %>% 
      dplyr::group_by(TYPE) %>%
      dplyr::mutate(RANK = paste0(scales::ordinal(rank(-VALUE)), " / ", length(unique(DivisionNm)))) %>%
      dplyr::ungroup()
    
    data$TYPE = gsub("OtherLanguageHome", "Other Language at Home", data$TYPE)
    data$TYPE = gsub("InternetAccess", "Internet Access", data$TYPE)
    data$TYPE = gsub("EnglishOnly", "English Language Only", data$TYPE)
    
    data$TYPE = factor(data$TYPE, levels = c("Internet Access", "English Language Only", "Other Language at Home", "Indigenous"))
    
    data = data %>% dplyr::mutate(VALUE = VALUE/100)
    
    data2 = data %>% dplyr::filter(DivisionNm == toupper(click$id))
    data = data %>% dplyr::filter(DivisionNm != toupper(click$id))
    
    options(warn = -1)
    suppressMessages(
      plotly::plot_ly(data, 
                      x = ~TYPE, 
                      y = ~VALUE, 
                      color = ~TYPE, 
                      type = "box", 
                      boxpoints = "all", 
                      jitter = 150, 
                      pointpos = 0, 
                      opacity = 0.3, 
                      legendgroup  = ~TYPE,
                      hoverinfo = 'text',
                      hovertext = paste0("<br><b>Electorate: </b>", data$DivisionNm, "</b><br>", "<b>Proportion: </b>", scales::percent(data$VALUE, 0.01), "<br><b>Rank: </b>", data$RANK, "<br><b>Group: </b>", data$TYPE)) %>%
        plotly::add_markers(data = data2, 
                            x = ~TYPE, 
                            y = ~VALUE, 
                            marker = list(color = "#000000FF", opacity = 1, size = 6), 
                            legendgroup  = ~TYPE,
                            showlegend = F,
                            hoverinfo = 'text',
                            hovertext = paste0("<br><b>Electorate: </b>", data2$DivisionNm, "</b><br>", "<b>Proportion: </b>", scales::percent(data2$VALUE, 0.01), "<br><b>Rank: </b>", data2$RANK, "<br><b>Group: </b>", data2$TYPE)) %>%
        #plotly::config(displayModeBar = FALSE) %>% 
        plotly::layout(xaxis = list(fixedrange = FALSE, title = NA), 
                       yaxis = list(fixedrange = FALSE, title = NA, tickformat = ".0%"))
    )
    
    
  }) %>% shiny::bindCache(input$hor_year, input$leafletplot_shape_click$id)
  
  
  output$demo_pop = plotly::renderPlotly({
    
    shiny::validate(
      shiny::need(input$leafletplot_shape_click, message = FALSE)
    )
    
    click = input$leafletplot_shape_click
    
    if (!click$id %in% x$DivisionNm[x$YEAR==input$hor_year]) {
      #click$id = x$DivisionNm[x$YEAR==input$hor_year][1]
      return(NULL)
    }
    
    
    data = abs %>% 
      dplyr::filter(YEAR == input$hor_year)  %>%
      dplyr::left_join(area, by = c("DivisionNm", "YEAR")) %>%
      dplyr::mutate(Population = Population/AREA) %>%
      dplyr::select(DivisionNm, Population) %>%
      tidyr::pivot_longer(cols = c("Population"), names_to = "TYPE", values_to = "VALUE")
    
    data = data %>% 
      dplyr::group_by(TYPE) %>%
      dplyr::mutate(RANK = paste0(scales::ordinal(rank(-VALUE)), " / ", length(unique(DivisionNm)))) %>%
      dplyr::ungroup()
    
    data$TYPE = gsub("Population", "Population density", data$TYPE)

    data2 = data %>% dplyr::filter(DivisionNm == toupper(click$id))
    data = data %>% dplyr::filter(DivisionNm != toupper(click$id))
    
    options(warn = -1)
    suppressMessages(
      plotly::plot_ly(data, 
                      x = ~TYPE, 
                      y = ~VALUE, 
                      color = ~TYPE, 
                      type = "box", 
                      boxpoints = "all", 
                      jitter = 100, 
                      pointpos = 0, 
                      opacity = 0.3, 
                      legendgroup  = ~TYPE,
                      hoverinfo = 'text',
                      hovertext = paste0("<br><b>Electorate: </b>", data$DivisionNm, "</b><br>", "<b>Population density: </b>", scales::comma(data$VALUE, 0.01), " people/km<sup>2</sup><br><b>Rank: </b>", data$RANK, "<br><b>Group: </b>", data$TYPE)) %>%
        plotly::add_markers(data = data2, 
                            x = ~TYPE, 
                            y = ~VALUE, 
                            marker = list(color = "#000000FF", opacity = 1, size = 6), 
                            legendgroup  = ~TYPE,
                            showlegend = F,
                            hoverinfo = 'text',
                            hovertext = paste0("<br><b>Electorate: </b>", data2$DivisionNm, "</b><br>", "<b>Population density: </b>", scales::comma(data2$VALUE, 0.01), " people/km<sup>2</sup><br><b>Rank: </b>", data2$RANK, "<br><b>Group: </b>", data2$TYPE)) %>%
        #plotly::config(displayModeBar = FALSE) %>% 
        plotly::layout(xaxis = list(fixedrange = FALSE, title = NA), 
                       yaxis = list(fixedrange = FALSE, title = NA))
    )
    
    
  }) %>% shiny::bindCache(input$hor_year, input$leafletplot_shape_click$id)
  
  #end of server#
  
}
