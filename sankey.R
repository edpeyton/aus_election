

#input = list(hor_year = 2019)
#click = list(id = "Menzies")

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
           "UAPP" = "#FFA500",
           "ON" = "#FFEE33",
           "PUP" = "#FFFF44",
           "CEC" = "#A9A9A9",
           "RUA" = "#A9A9A9",
           "FUT" = "#A9A9A9",
           "ASP" = "#A9A9A9",
           "FACN" = "#A9A9A9")
  
  cols[match(names, names(cols))]
  
}

create_sankey = function(x, year, id, height = 600, ...) {
    
  a0 = x %>% 
    dplyr::filter(YEAR == year, DivisionNm == id, CalculationType == "Preference Count") 

    
  y = x %>% 
    dplyr::filter(YEAR == year, DivisionNm == id, CalculationType %in% c("Preference Count", "Transfer Count")) 
  
  if (dim(y %>% dplyr::filter(PartyAb == "IND", CalculationType == "Preference Count", CountNumber == 0))[1]>1) {
    
    y = y %>% dplyr::mutate(PartyAb = ifelse(PartyAb == "IND", paste0("IND (", Surname, ")"), PartyAb))
    a0 = a0 %>% dplyr::mutate(PartyAb = ifelse(PartyAb == "IND", paste0("IND (", Surname, ")"), PartyAb)) 
  }
  
  a0 = a0 %>% 
    dplyr::select(BallotPosition, Surname, GivenNm, PartyAb, PartyNm, Elected, CalculationValue, CountNumber) %>%
    dplyr::mutate(index = BallotPosition)
  
  ordering = a0 %>% 
    dplyr::filter(CalculationValue == 0) %>% 
    dplyr::group_by(BallotPosition, Surname, GivenNm, PartyAb, PartyNm, Elected, index) %>% 
    dplyr::arrange(CountNumber, .by_group = TRUE) %>% 
    dplyr::summarise(CountNumber = min(CountNumber), .groups = "keep") %>% 
    dplyr::arrange(desc(CountNumber)) %>% 
    dplyr::pull(PartyAb) %>% 
    as.character()
  
  ordering = c(setdiff(a0$PartyAb, ordering), ordering)
  
  y = y %>%
    dplyr::mutate(PartyAb = factor(PartyAb, levels = ordering)) %>%
    dplyr::select(CountNumber, CalculationType, CalculationValue, PartyAb, Surname, GivenNm, PartyNm)

  df = NULL
  for (i in 1:max(y$CountNumber)) {
    
    y2 = y %>% dplyr::filter(CountNumber==i, CalculationType == "Transfer Count") %>% dplyr::filter(CalculationValue < 0)
    y3 = y %>% dplyr::filter(CountNumber==i, CalculationType == "Transfer Count") %>% dplyr::filter(CalculationValue > 0)
  
    df = rbind(df, data.frame(id = i, col = i, source = y2$PartyAb, target = y3$PartyAb, value = y3$CalculationValue))
  
    y4 = y %>% dplyr::filter(CountNumber==i-1, CalculationType == "Preference Count") %>% dplyr::filter(PartyAb != y2$PartyAb & CalculationValue > 0)
    df = rbind(df, data.frame(id = i, col = i, source = y4$PartyAb, target = y4$PartyAb, value = y4$CalculationValue))
  
  }
  
  
  links = df
  
  links = links %>%
    dplyr::mutate(source = paste0(source, '_', col)) %>%
    dplyr::mutate(target = paste0(target, '_', col + 1))
  
  nodes = data.frame(name = unique(c(links$source, links$target)))
  nodes$label0 = sub('_[0-9]*$', '', nodes$name) 
  
  nodes = nodes %>% dplyr::left_join(y %>% dplyr::select(PartyAb, PartyNm) %>% dplyr::distinct(), by = c("label0" = "PartyAb")) 
  nodes$label = paste0(nodes$label0, " (", nodes$PartyNm, ")")
  
  links$source_id = match(links$source, nodes$name) - 1
  links$target_id = match(links$target, nodes$name) - 1
  
  links$linksgroup = sub('_[0-9]*$', '', links$source) 
  
  #links = links %>% dplyr::mutate(header = scales::ordinal(id))
  
  colors = paste0("d3.scaleOrdinal() .range(['", paste0(elec_pal2(nodes$label0 %>% unique()), collapse = "', '"), "'])")

  nodes$label0 = paste0(nodes$label0, " ")
  
  return(networkD3::sankeyNetwork(Links = links, 
                Nodes = nodes, 
                Source = 'source_id',
                Target = 'target_id', 
                Value = 'value', 
                NodeID = 'label0', 
                width = "100%", 
                height = height,
                nodeWidth = 30, 
                fontSize = 10, 
                fontFamily = "Arial", 
                nodePadding = 50, 
                #units = "votes", 
                colourScale = colors, 
                ...))

}




party_cols = function(v) {
  
  col = 
    sapply(v, function(x) {
      if (x %in% c("Liberal")) {
        col = "#00008B"
      } else if (x %in% c("Independent")) {
        col = "#A9A9A9"
      } else if (x %in% c("The Greens")) {
        col = "#008A00"
      } else if (x %in% c("Liberal National Party of Queensland")) {
        col = "#3632FB"
      } else if (x %in% c("National Party", "National Party (WA)", "The Nationals")) {
        col = "#006400"
      } else if (x %in% c("Labor")) {
        col = "#FF0000"
      } else if (x %in% c("Centre Alliance", "Nick Xenophon Team")) {
        col = "#FF6600"
      } else if (x %in% c("Katter's Australian Party")) {
        col = "#460000"
      } else if (x %in% c("Country Liberals (NT)")) {
        col = "#FFA500"
      } else if (x %in% c("United Australia Party", "Palmer United Party")) {
        col = "#FFFF44"
      } else if (x %in% c("Pauline Hanson's One Nation")) {
        col = "#FFEE33"
      }
      return(col)
    })
  
  
  return(col %>% as.vector())
  
}

