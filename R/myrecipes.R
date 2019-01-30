library(rvest)
library(tidyverse)

url <- "https://www.myrecipes.com"

categories <- read_html(paste0(url, "/course")) %>%
  xml_nodes(css = ".pane-content .node-topic-page-child h2 > a") %>%
  map_df(~data_frame(Category = xml_text(.), link = xml_attr(., "href")))

page_links <- read_html(paste0(url, categories$link)[1]) %>%
  xml_nodes(css = ".well a") %>%
  map_df(~data_frame(Category = xml_text(.), link = xml_attr(., "href")))
