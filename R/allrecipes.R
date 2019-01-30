library(rvest)
library(tidyverse)

get_category_urls <- function(node) {
  # node here is a section with h3 and ul elements
  tibble(heading_name = xml_child(node, "h3") %>% xml_text(),
         values = xml_child(node, "ul") %>% xml_children(),
         category = xml_text(values),
         url = xml_child(values) %>% xml_attr("href")) %>%
    select(-values)
}

get_recipe_links <- function(url) {
  # https://www.allrecipes.com/recipes/78/breakfast-and-brunch/?page=2
  cards <- read_html(url) %>%
    xml_nodes(".fixed-recipe-card")

  card_data <- map(cards, xml_child) %>%
    map(xml_attrs)

  ads <- map_int(card_data, length) == 1

  cards <- cards[!ads]
  card_data <- card_data[!ads]

  recipe_names <- map_chr(card_data, "data-name") %>%
    str_remove_all("[[:punct:]]")

  card_data %>%
    set_names(recipe_names) %>%
    map_dfr(~as_data_frame(t(.))) %>%
    set_names(str_remove(names(.), "data-")) %>%
    mutate(recipe_link = map_chr(cards, ~xml_child(., search = 2) %>% xml_child() %>% xml_attr("href"))) %>%
    select(-segmentpageproperties) %>%
    mutate(type = str_remove_all(type, "\\W"),
           name = str_remove_all(name, "\\W"))
}

get_recipe_pages <- function(url, pages = 10) {
  purrr::map_df(paste(url, "?page=", 1:pages), get_recipe_links)
}


url <- "https://www.allrecipes.com/recipes/"

tmp <- read_html(url) %>%
  xml_nodes(css = ".all-categories-col > section") %>%
  purrr::map_df(., get_category_urls) %>%
  magrittr::extract(1:3) %>%
  mutate(recipes = purrr::map(url, get_recipe_links))


