scrape_sofifa <- function(page) {
  require(rvest)
  require(dplyr)
  soFifa_base <- paste(page, "&offset=", sep = "")
  
  fifa <- data.frame(Player = character(),
                     Team = character(),
                     Age = integer(),
                     Overall = integer())
  extension <- seq(0,600, by = 60)
  for(i in extension) {
    if(i == 0) {
      url <- page
    } else {
      url <- paste(soFifa_base, i, sep = "")
    }
    
    soFifa <- read_html(url)
    
    tables <- rvest::html_nodes(soFifa, "table")
    links <- rvest::html_nodes(x = tables, css = "td") %>% rvest::html_nodes("a") %>% rvest::html_attr("data-tooltip")
    name_pos_split <-stringr::str_split_fixed(string = links, pattern = "\t", n=2)
    player <- stringi::stri_remove_empty(name_pos_split)
    
    player_team <- soFifa %>%
      rvest::html_nodes("body") %>%
      xml2::xml_find_all("//td[contains(@class, 'col-name')]") %>%
      rvest::html_text()
    
    dirty_team <- player_team[c(FALSE, TRUE)]
    team <- substring(dirty_team, 2, nchar(dirty_team)-14)
    
    age <- soFifa %>%
      rvest::html_nodes("body") %>%
      xml2::xml_find_all("//td[contains(@class, 'col col-ae')]") %>%
      rvest::html_text()
    
    overall <- soFifa %>%
      rvest::html_nodes("body") %>%
      xml2::xml_find_all("//td[contains(@class, 'col col-oa')]") %>%
      rvest::html_text()
    
    temp <- data.frame(Player = player,
                       Team = team,
                       Age = as.integer(age),
                       Overall = as.integer(overall))
    fifa <- rbind(fifa, temp)
  }
  
  return(fifa)
}
