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
                                                            "(?:\\d\\% )?(?:fat free )?(?:full-fat )?(?:[Ww]hole )?(skim )?(?:low fat )?[Mm]ilk" = "milk",
                                                            "unbleached " = "",
                                                            "(?:large )?(?:medium )?(?:small )?(?:cage free )?egg(?:s)?(?: wash)?" = "egg",
                                                            ".*[Yy]east" = "yeast",
                                                            "bicarbonate of soda" = "baking soda",
                                                            ".*brown sugar" = "brown sugar",
                                                            "nonfat dry milk powder" = "powdered milk",
                                                            "granulated sugar" = "sugar",
                                                            "wholewheat" = "whole wheat",
                                                            "(greek )?yog[h]?urt" = "yogurt",
                                                            "Guinness Beer" = "guinness",
                                                            "Cream of Tartar" = "cream of tartar",
                                                            "corn ?meal" = "corn meal",
                                                            "(full|low) fat " = "",
                                                            "granulated " = "",
                                                            " with juice" = "",
                                                            "ginger powder" = "ginger",
                                                            "The Cheese Guy Asiago" = "asiago",
                                                            "gouda cheese" = "gouda",
                                                            "[Oo]ranges?" = "orange",
                                                            "Equal|(liquid stevia)" = "artificial sweetener",
                                                            "(?:mashed )?bananas?" = "banana",
                                                            "all[- ]purpose " = "",
                                                            "Butter" = "butter",
                                                            "(.*)[Ss]alt" = "salt",
                                                            " purée" = "",
                                                            "(extra|sharp) " = "",
                                                            "(black |instant )?coffee( granules)?( powder)?" = "coffee",
                                                            "powdered " = "convectioners ",
                                                            "caraway.*" = "caraway seed",
                                                            "garlic powder|cloves" = "garlic",
                                                            "blanched " = "",
                                                            "basil.*" = "basil",
                                                            "seeds" = "seed",
                                                            "(white|yolk)s" = "\\1",
                                                            "Dutch process " = "",
                                                            "jalapeno chiles" = "jalapenos",
                                                            "instant.*pudding" = "instant pudding",
                                                            "(mashed )?potato.*" = "potato",
                                                            " ?cheese" = "",
                                                            "russet " = "",
                                                            "Buttermilk" = "buttermilk",
                                                            "^red " = "", "^raw " = "",
                                                            "semi[- ]sweet chocolate (chips|morsels)" = "chocolate chips",
                                                            " ?(flakes|runny|McCormick|natural|iodized|finely|melted|minced) ?" = "",
                                                            "(porridge|old[- ]fashioned|quick-cooking|extra[- ]|virgin|white|yellow) " = "",
                                                            "(diced|plain|[Ss]weet.[Cc]ream|salted|Lucerne|[Uu]nsalted|pure|crumbled|dark) " = "",
                                                            "(whipping|firmly|packed|unsweetened|nonstick|organic|frozen|chopped) " = "",
                                                            "(softened|grated|ground|fresh|cooked|sun|dried|coarse|strong) " = "",
                                                            "(kalamata|mini|distilled|toasted|Borden|shredded|smoked|crushed|soft|fine) " = ""
                                                            )) %>% str_to_lower())

unique(bread$basic_ingredients) %>% sort()
