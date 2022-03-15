
#library(shiny)
#library(eechidna)
#library(plotly)
#library(networkD3)
#library(dplyr)
#library(tidyr)
#library(leaflet)
#library(scales)
#library(ggplot2)


shiny::fluidPage(
  style = "width: 80%", 
  shiny::tags$style(".leaflet-container {background: #F6F6F6;}"),
  shinyWidgets::setBackgroundColor(color = "white"),
  shiny::br(),
  shiny::br(),
  shiny::br(),
  shiny::h1(shiny::code("Australian Federal Elections", style = "color: #152238"), align = "center"),
  shiny::h3(shiny::code("Exploring the data", style = "color: #152238"), align = "center"),
  shiny::br(),
  shiny::br(),
  shiny::br(),
  shiny::fluidRow(
    shiny::column(width = 5, offset = 1, 
                  shiny::tags$p(shiny::HTML(paste0("Australian Federal Elections are triennial 
                                                   events which form the foundation of the democratic society in which we live. Arguably, 
                                                   it is your most important contribution to how the country functions. <strong><em>But how 
                                                   informed are you when it comes to elections?</strong></em>")), style = "text-align: justify;"),
  shiny::p("The motivation for this site was to dig a bit deeper into some of the election data published by the ", shiny::a(href = "https://www.aec.gov.au/Voting/Informal_Voting/", target = "_blank", "AEC"), 
           "and other resources to hopefully provide a bit of context and insight into how recent elections have played out.", style = "text-align: justify;"),
  shiny::p("With the upcoming 2022 Federal Election on the horizon, hopefully the following sections can provide some assistance so that you can be informed and make your vote count.", style = "text-align: justify;"),
    ),
  shiny::column(width = 4, offset = 2,
                shiny::br(),
                shiny::h3(shiny::code("Important links", style = "color: #152238")),
                shiny::tags$ul(
                  shiny::tags$li(shiny::span("Enrol to vote ", shiny::a(href = "https://www.aec.gov.au/enrol/", shiny::strong("here"), target = "_blank"))), 
                  shiny::tags$li(shiny::span("Find your electorate ", shiny::a(href = "https://electorate.aec.gov.au/", shiny::strong("here"), target = "_blank"))), 
                  shiny::tags$li(shiny::span("Get AEC spatial data ", shiny::a(href = "https://www.aec.gov.au/electorates/maps.htm", shiny::strong("here"), target = "_blank"))), 
                  shiny::tags$li(shiny::span("Get AEC election results data ", shiny::a(href = "https://results.aec.gov.au/", shiny::strong("here"), target = "_blank"))),
                  shiny::tags$li(shiny::span("Check out the fantastic ", shiny::code("eechidna", style = "color: #152238"), " R package ", shiny::a(href = "https://CRAN.R-project.org/package=eechidna", shiny::strong("here"), target = "_blank")))
                ))),
  shiny::br(),
  shiny::br(),
  shiny::br(),
  shiny::br(),
  shiny::hr(),
  shiny::br(),
  lapply(1:5, function(i) {shiny::br()}),
  shiny::fluidRow(lapply(1:15, function(i) {shiny::icon("ellipsis-v", align = "center")}), align = "center"),
  lapply(1:5, function(i) {shiny::br()}),
  shiny::h2(shiny::code("House of Representatives", style = "color: #152238"), align = "center"),
  shiny::h4(shiny::code("Lower House", style = "color: #152238"), align = "center"),
  shiny::br(),
  shiny::p("The House of Representatives (HOR) is the lower house of the Australian Parliament. It is currently made up of 151 seats, each representing a distinct electorate. 
           The HOR determines which party forms government and is the main forum for political debate.", style = "text-align: justify;"),
  shiny::p("One of the key characteristics of the HOR is its use of preferential voting during elections, which is different from other voting systems used throughout the world, such as first-past-the-post voting.", style = "text-align: justify;"),
  shiny::br(),
  shiny::br(),
  shiny::h3(shiny::code("Preferential voting explained", style = "color: #152238")),
  shiny::fluidRow(
    shiny::column(width = 6,
        shiny::p("The House of Representatives uses a full-preferential instant-runoff voting system. It's a form
                 of voting that allows each voter to express their full preferences for each one of the candidates on the ballot in their electorate. Voters rank each candidate and the voting 
                 system will (where necessary) take that ordering into account.", style = "text-align: justify;"),
        shiny::br(),
        shiny::div(
          shiny::strong("Advantages"),
          shiny::p("The advantage of this system is it allows the voter to express their opinion of ", shiny::em("all"), " of the candidates. In this sense, it allows a voter to express both approval and 
                   disapproval (relatively).", style = "text-align: justify;"), style = "margin-left: 5%;margin-right: 10%;"),
        shiny::br(),
        shiny::div(
          shiny::strong("Disadvantages"),
          shiny::p("The main disadvantage of this system is its relative complexity compared to simpler voting systems. It can be confusing!", style = "text-align: justify;"),
          style = "margin-left: 5%;margin-right: 5%;"),
        shiny::br(),
        shiny::p("This is why it's important to get your head around how the voting system actually plays out in reality. ", shiny::em("How do those preferences really work?"), style = "text-align: justify;"),
        shiny::br(),
        shiny::h4(shiny::code("How it works", style = "color: #152238")),
        shiny::p(shiny::HTML("Preferential voting involves counting votes in order of their preference. In the first round, the 1<sup>st</sup> preference votes are counted. If no candidate has an absolute majority (50% + 1), 
                             then the candidate with the fewest 1<sup>st</sup> preferences votes is eliminated."), style = "text-align: justify;"),
        shiny::p(shiny::HTML("The eliminated candidate's votes are then distributed to the other remaining candidates. This is done by looking at those ballots and determining who each voter had as their next preference. The votes are then distributed and the process continues."), style = "text-align: justify;"),
        shiny::p("This is repeated until a candidate has a majority of votes and wins the election.", style = "text-align: justify;"),
        shiny::br()),
  shiny::column(width = 5, offset = 1, 
                shiny::h4(shiny::code("Example", style = "color: #152238")),
                shiny::tags$p(shiny::HTML(paste0("Here is a simple example of preferencing shown for <strong>Gorton</strong> in <strong>2016</strong>.")), style = "text-align: justify;"),
                shiny::tags$p(shiny::HTML(paste0("There are 3 candidates. In the first count, the Greens candidate has the fewest (9,690) votes and is eliminated. For each of those ballots that had Greens as first preference, the next preference is then used.")), style = "text-align: justify;"),
                shiny::tags$p(shiny::HTML(paste0("As a result, 7,025 votes transfer to ALP and 2,665 to LP.")), style = "text-align: justify;"),
                shiny::br(),
                networkD3::sankeyNetworkOutput("example_sankey", height = 300),
                shiny::br(),
                shiny::tags$p(shiny::HTML(paste0("Technically, in the example above, ALP has an absolute majority of 1<sup>st</sup> preference votes so there is no need to check for further preferences.")), style = "text-align: justify;"),)),
  shiny::br(),
  shiny::hr(),
  lapply(1:5, function(i) {shiny::br()}),
  shiny::fluidRow(lapply(1:15, function(i) {shiny::icon("ellipsis-v", align = "center")}), align = "center"),
  lapply(1:5, function(i) {shiny::br()}),
  shiny::h1(shiny::code("The data", style = "color: #152238;"), align = "center"),
  shiny::br(),
  shiny::br(),
  shiny::fluidRow(shiny::selectInput("hor_year", choices = seq(2010, 2019, 3), selected = 2019, label = shiny::code("Select election", style = "color: #152238")), align = "center"),
  shiny::br(),
  shiny::fluidRow(shiny::uiOutput("election_details")),
  shiny::br(),
  shiny::br(),
  shiny::br(),
  shiny::br(),
  shiny::fluidRow(shiny::hr(),
                  shiny::br(),
                  shiny::h3(shiny::code("Polling")),
                  shiny::tags$p(shiny::HTML(paste0("The following plots show the pre-election polling results across different opinion polls.")), style = "text-align: justify;"),
                  shiny::br(),
                  shiny::selectInput("polling1", label = NULL, choices = NULL),
                  shiny::uiOutput("polling")),
  shiny::br(),
  shiny::br(),
  shiny::br(),
  shiny::br(),
  shiny::hr(),
  shiny::br(),
  lapply(1:5, function(i) {shiny::br()}),
  shiny::fluidRow(lapply(1:15, function(i) {shiny::icon("ellipsis-v", align = "center")}), align = "center"),
  lapply(1:5, function(i) {shiny::br()}),
  shiny::h1(shiny::code("Electorate data", style = "color: #152238;"), align = "center"),
  shiny::h3(shiny::code("How well do you know your electorate?", style = "color: #152238")),
  shiny::p("First of all, if you don't know which electorate you belong to, check out ", shiny::a(href='https://electorate.aec.gov.au/', "Find my electorate.", target='_blank'), " This will 
           tell you which electorate you belong to for the ", shiny::em("next"), " election, not necessarily which electorate you belonged to in past elections.", style = "text-align: justify;"),
  shiny::p("The electorate you belong to can change over time, even if you haven't changed where you live. Sometimes electorates are added, removed, renamed or sometimes the boundaries of electorates 
           will be shifted to reflect changes in population over time. The redrawing of electorate boundaries is known as ", shiny::a(href='https://www.aec.gov.au/electorates/Redistributions/', "redistribution.", target='_blank'), style = "text-align: justify;"),
  shiny::p("Having some historical context about your electorate can really assist you to make an informed decision on election day. In the sections below you'll find information 
           on election results, voting distributions, and demographics.", style = "text-align: justify;"),
  shiny::br(),
  shiny::hr(),
  shiny::h3(shiny::code("Electorate map", style = "color: #152238;")),
  shiny::fluidRow(shiny::column(width = 8, offset = 0, leaflet::leafletOutput("leafletplot", height = 630)),
                  shiny::column(width = 4, offset = 0, shiny::actionButton("center_map", label = NULL, icon = shiny::icon("refresh"), style = "background-color: transparent; color: #152238; border-color: transparent;"),
                                shiny::uiOutput("facts"))), #shiny::uiOutput("select_elec"),
  shiny::fluidRow(shiny::uiOutput("voting_dist")),
  shiny::fluidRow(shiny::uiOutput("preferencing")),
  shiny::fluidRow(shiny::uiOutput("demographics")),
  shiny::br(),
  shiny::br(),
  shiny::br(),
  shiny::br(),
  shiny::br(),
  shiny::br(),
  shiny::hr(),
  shiny::fluidRow(shiny::p("Created by ", shiny::a(href = "https://github.com/edpeyton/aus_election/", "Ed Peyton", target = "_blank"))),
  shiny::br())


#==================================================================================================================================#

