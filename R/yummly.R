library(rvest)
library(jsonlite)
library(tidyverse)

base_url <- "http://api.yummly.com/v1/api/"
api_type <- "recipes"
qstring <- "?%s&maxResult=500&q=%s"
api_key <- readLines("extra/api_key")

tmp <- read_html(paste0(base_url, api_type, sprintf(qstring, api_key, "bread")))

tmp2 <- tmp %>%
  xml_child() %>%
  xml_child() %>%
  xml_text() %>%
  fromJSON(simplifyDataFrame = T)

bread <-
  tibble(source = tmp2$matches$sourceDisplayName,
             id = tmp2$matches$id,
             recipe_name = tmp2$matches$recipeName,
             total_time = tmp2$matches$totalTimeInSeconds,
             rating = tmp2$matches$rating,
             course = purrr::map_chr(tmp2$matches$attributes$course, function(x) ifelse(is.null(x), NA, paste(x, collapse = ", "))),
             cuisine = purrr::map_chr(tmp2$matches$attributes$cuisine, function(x) ifelse(is.null(x), NA, paste(x, collapse = ", "))),
             holiday = purrr::map_chr(tmp2$matches$attributes$holiday, function(x) ifelse(is.null(x), NA, paste(x, collapse = ", "))),
             imageurl = tmp2$matches$imageUrlsBySize$`90`,
             small_imageurl = purrr::map_chr(tmp2$matches$smallImageUrls, function(x) ifelse(is.null(x), NA, x))
             ) %>%
  bind_cols(tmp2$matches$flavors) %>%
  nest(sweet:bitter, .key = 'flavors') %>%
  bind_cols(tibble(ingredients = tmp2$matches$ingredients)) %>%
  unnest(ingredients) %>%
  mutate(basic_ingredients = str_replace_all(ingredients, c(".*water" = "water",
                                                            "(?:\\d\\% )?(?:fat free )?milk" = "milk",
                                                            "unbleached " = "",
                                                            "(?:large )?(?:medium )?(?:cage free )?egg(?:s)?(?: wash)?" = "eggs",
                                                            ".*yeast" = "yeast",
                                                            "(pure|crumbled|dark|whipping|firmly|packed|unsweetened|nonstick|organic|old-fashioned|frozen|chopped|softened|grated|ground|fresh|cooked|dried|coarse|strong|shredded|smoked|crushed|soft|fine) " = ""
                                                            )))


stbread$image90 <- as.vector(tmp2$matches$imageUrlsBySize$`90`, "character")


%>%
  as_tibble()
  mutate(imageUrl90 = )
