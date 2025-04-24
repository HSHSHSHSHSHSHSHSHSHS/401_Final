# Pre ---------------------------------------------------------------------

rm(list = ls())

library(tidyverse)
#Lab 3
library(stargazer)
library(rms)
library(klaR)
library(MASS)
#Lab 4
library(tree)
library(rpart)
library(rpart.plot)	
library(randomForest)
library(gbm)
library(ggfortify)
#Xtra
library(GGally)
library(cluster)

#API lol
library(httr2)
library(jsonlite)

##Hehe
library(ggcorrplot)
library(fastDummies)
library(colorspace)

#user <- read_table('userpw.txt')

MU_genre_list_small <- c(
  "Shoujo Ai",
  "Shounen Ai",
  "Yaoi",
  "Yuri"
)

MU_genre_list <- c(
  "Action",
  "Adult",
  "Adventure",
  "Comedy",
  "Doujinshi",
  "Drama",
  "Ecchi",
  "Fantasy",
  "Gender Bender",
  "Harem",
  "Hentai",
  "Historical",
  "Horror",
  "Josei",
  "Lolicon",
  "Martial Arts",
  "Mature",
  "Mecha",
  "Mystery",
  "Psychological",
  "Romance",
  "School Life",
  "Sci-fi",
  "Seinen",
  "Shotacon",
  "Shoujo",
  "Shoujo Ai",
  "Shounen",
  "Shounen Ai",
  "Slice of Life",
  "Smut",
  "Sports",
  "Supernatural",
  "Tragedy",
  "Yaoi",
  "Yuri"
)

MU_relation_list <- c(
  "Adapted From",
  "Side Story",
  "Prequel",
  "Sequel",
  "Alternate Version",
  "Spin-Off",
  "Main Story",
  "Full Anthology",
  "Part of Anthology",
  "Other"
)

# Setup -------------------------------------------------------------------

# 
# ## MU user credentials
# 
# mu_user = user[[1]][[1]]
# mu_pw = user[[1]][[2]]
# 
# mu_auth_url = "https://api.mangaupdates.com/v1/account/login"
# mu_auth_j = list(
#     username=mu_user,
#     password=mu_pw
# )
# 
# mu_auth_headers = c(
#     "Content-Type"="application/json"
# )
# 
# mu_auth_response = PUT(mu_auth_url,
#                        body = toJSON(mu_auth_j, auto_unbox = TRUE),
#                        add_headers(mu_auth_headers))
# 
# mu_auth_data <- fromJSON(content(mu_auth_response, "text"))
# 
# mu_session_token = mu_auth_data$context$session_token
# print(mu_auth_data$context$session_token)
# 
# ## MU sidetrack
# 
# #pt 1
# mu_list_url = paste0("https://api.mangaupdates.com/v1/lists/", 101)
# 
# mu_list_headers = c(
#   "Content-Type"="application/json"
# )
# 
# mu_list_response = GET(mu_list_url,
#                        add_headers(Authorization = paste("Bearer", mu_session_token)))
# 
# mu_list_data <- fromJSON(content(mu_list_response, "text"))
# print(mu_list_data)
# 
# #pt2
# 
# series_title_list = non$title
# 
# for (i in 1:(length(series_title_list)/100)){
#   print(i)
#   
#   mu_add_list_send_data <- list()
#   
#   for (title in series_title_list[((i-1) * 100):(i* 100 - 1)]) {
#     mu_add_list_send_data <- append(mu_add_list_send_data, list(list(priority = "Low", series_title = title)))
#   }
#   
#   print(length(mu_add_list_send_data))
#   
#   mu_add_list_url = "https://api.mangaupdates.com/v1/lists/101/series/bulk"
#   
#   mu_add_list_response = POST(mu_add_list_url,
#                               body = toJSON(mu_add_list_send_data, auto_unbox = TRUE),
#                               add_headers(Authorization = paste("Bearer", mu_session_token),
#                                           "Content-Type" = "application/json"))
#   
#   mu_add_list_data <- fromJSON(content(mu_add_list_response, "text"))
#   print(mu_add_list_data)
#   
#   Sys.sleep(5.5)
# }
# 
# 
# 
# #check
# 
# 
# ## MU gimme the data
# 
# ##
# 
# for (my_select_genre in "ok_now_none_MORE"){#MU_genre_list_small) {
# 
#   print(paste("Now on genre", my_select_genre))
#   ## MU manga search
# 
#   MU_manga_search <- function(genre_list){
# 
#     BIG_MU_holder <- tibble(
#       series_id = as.numeric(NA),
#       title = as.character(NA),
#       url = as.character(NA),
#       description = as.character(NA),
#       type = as.character(NA),
#       year = as.character(NA),
#       bayesian_rating = as.numeric(NA),
#       votes = as.numeric(NA),
#       genres = as.list(NA),
#       rank_this_year = as.numeric(NA),
#       lists_reading = as.numeric(NA),
#       lists_wish = as.numeric(NA),
#       lists_complete = as.numeric(NA),
#       lists_unfinished = as.numeric(NA),
#       lists_custom = as.numeric(NA),
#       last_updated = as.numeric(NA)
#     )%>%
#       slice(0)
# 
#     for (i in genre_list){
# 
#       mu_search_url = "https://api.mangaupdates.com/v1/series/search"
#       mu_search_headers = c(
#         "Content-Type"= "application/json",
#         Authorization = paste("Bearer", mu_session_token)
#       )
#       mu_search_j = list(
#         session_token= mu_session_token,
#         #genre = list(i),
#         exclude_genre = MU_genre_list_small,
#         include_rank_metadata = TRUE,
#         list = "none",
#         page= 1,
#         perpage= 1
#       )
# 
#       mu_search_response = POST(mu_search_url,
#                                 body = toJSON(mu_search_j, auto_unbox = TRUE),
#                                 add_headers(mu_search_headers))
# 
#       mu_search_data <- fromJSON(content(mu_search_response, "text"))
#       total_hits <- mu_search_data$total_hits
#       print(paste("There are", total_hits, "hits for the genre", i))
# 
#       for (n in 1:((total_hits/100) + 1)){ #edit as needed
# 
#         mu_search_j = list(
#           session_token= mu_session_token,
#           #genre = list(i),
#           exclude_genre = MU_genre_list_small,
#           include_rank_metadata = TRUE,
#           list = "none",
#           page= n,
#           perpage= 100
#         )
# 
#         mu_search_response = POST(mu_search_url,
#                                   body = toJSON(mu_search_j, auto_unbox = TRUE),
#                                   add_headers(mu_search_headers))
# 
#         mu_search_data <- fromJSON(content(mu_search_response, "text"))
# 
#         print(mu_search_data$results[1:5,])
# 
#         new_mu_series <- tibble(
#           series_id = mu_search_data$results$record$series_id,
#           title = mu_search_data$results$record$title,
#           url = mu_search_data$results$record$url,
#           description = mu_search_data$results$record$description,
#           type = mu_search_data$results$record$type,
#           year = mu_search_data$results$record$year,
#           bayesian_rating = mu_search_data$results$record$bayesian_rating,
#           votes = mu_search_data$results$record$rating_votes,
#           genres = mu_search_data$results$record$genres,
#           rank_this_year = mu_search_data$results$record$rank$position$year,
#           lists_reading = mu_search_data$results$record$rank$lists$reading,
#           lists_wish = mu_search_data$results$record$rank$lists$wish,
#           lists_complete = mu_search_data$results$record$rank$lists$complete,
#           lists_unfinished = mu_search_data$results$record$rank$lists$unfinished,
#           lists_custom = mu_search_data$results$record$rank$lists$custom,
#           last_updated = mu_search_data$results$record$last_updated$timestamp
#         )
# 
#         BIG_MU_holder <- BIG_MU_holder %>%
#           add_row(new_mu_series)%>%
#           print(n = 5)
#       }
#     }
#     return(BIG_MU_holder)
#   }
# 
#   genre_list <- MU_manga_search(my_select_genre)
# 
#   t <- genre_list %>%
#     mutate(across( #Reading list
#       starts_with("lists_"),
#       ~ replace_na(.x, 0)
#     ),
#     description = replace_na(description, ""), #Description
#     total_lists = rowSums(across(starts_with("lists_"))))
# 
# 
#   BIG_save_name = paste0(my_select_genre,"all_IDS.rds")
# 
#   saveRDS(t, BIG_save_name)
#   print(paste("Backup saved for", my_select_genre, "ids"))
# 
# 
#   ############# Get extra stuff from specific series id later
#   # Categories
#   # Status
#   # Licensed
#   # Completed
#   # related_series
#   # publishers
# 
#   # column_names <- c("series_id",
#   #                   "categories",
#   #                   "latest_ch",
#   #                   "status",
#   #                   "licensed",
#   #                   "scanlated",
#   #                   "relation_n",
#   #                   "relation_type",
#   #                   "pub_name",
#   #                   "pub_id",
#   #                   "pub_type")
# 
#   ids_list <- t$series_id
#   ids_list
# 
#   MU_specific_search <- function(ids_list){
# 
#     compiled_mu_series_specific_data <- tibble(
#       series_id = as.numeric(NA),
#       categories = as.list(NA),
#       latest_ch = as.numeric(NA),
#       status = as.character(NA),
#       licensed = as.logical(NA),
#       fully_scanlated = as.logical(NA),
#       relation_n = as.numeric(NA),
#       relation_type = as.list(NA),
#       pub_name_og = as.character(NA),
#       pub_id_og = as.numeric(NA),
#       pub_name_en = as.character(NA),
#       pub_id_en = as.numeric(NA)
#     )%>%
#       slice(0)
# 
#     for (id in ids_list) {
# 
#       # if(which(ids_list == id) <= 7500){
#       #   print('Already got this id')
#       #   next
#       # }
# 
#       mu_series_specific_url = paste0("https://api.mangaupdates.com/v1/series/",id)
#       print(mu_series_specific_url)
# 
#       mu_series_specific_response = GET(mu_series_specific_url,
#                                         add_headers(Authorization = paste("Bearer", mu_session_token)))
# 
#       ## IF 503 error
#       if (status_code(mu_series_specific_response) == 503) {
#         print("Error 503!!!!!!!!!!")
#         Sys.sleep(600)
# 
#         mu_series_specific_response = GET(mu_series_specific_url,
#                                           add_headers(Authorization = paste("Bearer", mu_session_token)))
#         mu_series_specific_data <- fromJSON(content(mu_series_specific_response, "text"))
# 
#       } else {
#         mu_series_specific_data <- fromJSON(content(mu_series_specific_response, "text"))
#         print("Got the data")
#       }
# 
# 
#       print(mu_series_specific_data$publishers)
# 
#       do_pubs_work = FALSE
# 
#       if(length(mu_series_specific_data$publishers) > 0){
# 
#         english_pub <- mu_series_specific_data$publishers[mu_series_specific_data$publishers$type == "English",]
#         original_pub <- mu_series_specific_data$publishers[mu_series_specific_data$publishers$type == "Original",]
#         do_pubs_work = TRUE
# 
#         print(nrow(english_pub))
#       }
# 
#       print(do_pubs_work)
# 
#       print(unique(mu_series_specific_data$related_series$relation_type))
# 
#       formatted_mu_series_specific_data <- tibble(
#         series_id = mu_series_specific_data$series_id,
#         # All categories can only appear once per series
#         categories = ifelse(!(is.null(mu_series_specific_data$categories$category)),
#                         list(mu_series_specific_data$categories$category), list("")),
#         latest_ch = mu_series_specific_data$latest_chapter,
#         status = mu_series_specific_data$status,
#         licensed = mu_series_specific_data$licensed,
#         fully_scanlated = mu_series_specific_data$completed,
#         relation_n = ifelse(length(mu_series_specific_data$related_series) > 0,
#                               length(mu_series_specific_data$related_series$relation_id), 0),
#         # -> prevents stuff like 5x Side Story
#         relation_type = ifelse(length(mu_series_specific_data$related_series) > 0,
#                                 list(unique(mu_series_specific_data$related_series$relation_type)), list("")),
#         pub_name_en = ifelse(do_pubs_work && nrow(english_pub) > 0, #haha lazy eval
#                                 english_pub$publisher_name[[1]], ""),
#         pub_name_og = ifelse(do_pubs_work && nrow(original_pub) > 0,
#                               original_pub$publisher_name[[1]], ""),
#         pub_id_en = ifelse(do_pubs_work && nrow(english_pub) > 0,
#                             english_pub$publisher_id[[1]], 0),
#         pub_id_og = ifelse(do_pubs_work && nrow(original_pub) > 0,
#                             original_pub$publisher_id[[1]], 0),
#       )%>%
#         distinct()
#       print("Formatted the data")
# 
#       print(formatted_mu_series_specific_data)
# 
#       compiled_mu_series_specific_data <- compiled_mu_series_specific_data %>%
#         add_row(formatted_mu_series_specific_data)%>%
#         print(n = 10)
#       print("Appended the data")
# 
#       #Save temp
# 
#       index = which(ids_list == id)
# 
#       if (index %% 500 == 0){
#         save_name = paste0(my_select_genre,index,".rds")
# 
#         saveRDS(compiled_mu_series_specific_data, save_name)
#         print(paste("Backup saved for", my_select_genre, "specifics on the", index, "id"))
#       }
# 
#       Sys.sleep(0.8)
# 
#     }
# 
#     return(compiled_mu_series_specific_data)
#   }
# 
#   a2 <- MU_specific_search(ids_list)
# 
# 
#   #sapply(a2, function(x) sum(is.na(x)))
#   a2 <- a2%>%
#     mutate(status = replace_na(status, ""),
#            #MU lets you name a publisher without creating a page, so no id for below lol
#            pub_id_og = replace_na(pub_id_og, 9999999999999),
#            pub_id_en = replace_na(pub_id_en, 9999999999999),)
# 
#   #### Join em up
# 
#   b <- full_join(t,
#                  a2,
#                  by = "series_id"
#         )%>%distinct()
# 
#   ############# Notes
#   # Do i deal with doujin......
#   # Same w oneshots?
#   # <- for now, no
# 
#   ############# Backup
#   to_export_b <- b #%>%
#     # dplyr::select(-c(categories_backup, relation_type_backup, genre_backup))
# 
#   save_name <- paste0(my_select_genre, ".rds")
# 
#   saveRDS(b, save_name)
# 
#   print("Hey we just saved something")
# 
#   # write.csv(b%>%dplyr::select(-c(categories_backup, relation_type_backup, genres_backup)),
#   #           yaoi_500.csv")
# 
# }

# Data re-read and fix ----------------------------------------------------

shoujoai <- readRDS('/RawData/Shoujo Ai.rds')
shoujoai
glimpse(shoujoai)
sapply(shoujoai, function(x) sum(is.na(x)))

yaoi <- readRDS('/RawData/Yaoi.rds')
yaoi
glimpse(yaoi)
sapply(yaoi, function(x) sum(is.na(x)))

yuri <- readRDS('/RawData/Yuri.rds')
yuri
glimpse(yuri)
sapply(yuri, function(x) sum(is.na(x)))

shounenai <- readRDS('/RawData/Shounen Ai.rds')
shounenai
glimpse(shounenai)
sapply(shounenai, function(x) sum(is.na(x)))

non <- readRDS('/RawData/ok_now_none.rds')
non
glimpse(non)
sapply(non, function(x) sum(is.na(x)))

non2 <-readRDS('/RawData/ok_now_none_MORE.rds')
non2
glimpse(non2)
sapply(non2, function(x) sum(is.na(x)))

####

MU_df_raw <- bind_rows(list
                  (shoujoai,
                    yaoi,
                    yuri,
                    shounenai,
                    non,
                    non2))

############# Data cleaning notes:

sapply(MU_df_raw, function(x) sum(is.na(x)))

## NA values

####### EARLIER
#Any of the lists if NA should be 0 <- caught earlier
#Description should be "" <- caught earlier

####### FILL NA
#pub ids -- not entered
#status -- unknown i guess

####### FILTER OUT
#Rating ? Maybe just exclude them <- actually there r already some ratings of 0 for no votes. bruh.
#Rank this year? Set to high number or exclude yeah
#Year -- must be numeric :(

MU_df <- MU_df_raw%>%
  mutate(pub_id_og = replace_na(pub_id_og, 0))%>%
  mutate(pub_id_en = replace_na(pub_id_en, 0))%>%
  mutate(status = replace_na(status, "Unknown"))%>%
  mutate(last_updated = as_datetime((last_updated)))%>%
  mutate(bayesian_rating = ifelse(votes == 0, 0, bayesian_rating))%>%
  mutate(year = ifelse(year == "2004/12/14", "2004", year),
         year = ifelse(year == "2016-12-23", "2016", year),
         year = ifelse(year == "2011-2012", "2011", year),
         year = ifelse(year == "2014-2016", "2014", year),
         year = ifelse(year == "2018-2019", "2018", year),
         year = ifelse(year == "2019-2020", "2019", year),
         year = ifelse(year == "2020-2021", "2020", year),
         year = ifelse(year == "2018; 2020", "2018", year),
         year = ifelse(year == "", NA, year),
         year = as.numeric(year))%>%
  mutate(latest_ch = ifelse(latest_ch == 122640, 8, latest_ch),
         latest_ch = ifelse(latest_ch == 2019, 72, latest_ch),
         latest_ch = ifelse(latest_ch == 2017, 6, latest_ch))%>%
  #filter(bayesian_rating > 0)%>%
  drop_na()%>%
  distinct()%>%
  group_by(series_id)%>%
  slice(1)

#how r there dupes bruh
#ITS BECAUSE LATER DATA CHANGED LIST NUMBERS LMAO
# duped_id <- MU_df[duplicated(MU_df$series_id),]
# a <- MU_df[MU_df$series_id == 323597984, ]
# b <- a %>%
#   summarise(across(everything(), ~ n_distinct(.)))


## More info

MU_df <- MU_df %>%
  mutate(desc_length = nchar(description))%>%
  relocate(desc_length, .after = description)%>%
  mutate(cat_n = ifelse(categories == "", 0, length(categories[[1]])))%>%
  mutate(genre_n = ifelse(genres == "", 0, length(unlist(genres))))


MU_df <- MU_df%>%
  mutate(small_status = case_when(
    grepl("Complete", status) ~ "Complete",
    grepl("Ongoing", status) ~ "Ongoing",
    grepl("Axed", status) ~ "Cancelled",
    grepl("Cancelled", status) ~ "Cancelled",
    grepl("Discontinued", status) ~ "Cancelled",
    grepl("Hiatus", status) ~ "Hiatus",
    TRUE ~ "Unknown"
  ))
#Hiatus, Complete, Ongoing, Axed, Cancelled, Discontinued, Unknown, blank


# Genre break up
MU_df$genre_backup <- MU_df$genres
MU_df$genres <- lapply(MU_df$genres, function(x) paste(unlist(x), collapse = ", "))
## <- this deletes everything with no genre.
## <- However, they aren't going to be picked up by the api grab initially anyways.
## <- Also, if unlabelled can't tell what their genre is for grouping so OK to cut.
MU_df_biggened <- MU_df %>%
  separate_rows(genres, sep = ",") |>
  filter(genres != "") |>
  mutate(genres = trimws(genres),
         value = 1) |>
  pivot_wider(
    names_from =  genres,
    values_from = value,
    values_fill = 0
  )


MU_df_biggened$queer <- ifelse(
  rowSums(MU_df_biggened[, MU_genre_list_small]) > 0, 
  1, 
  0
)
MU_df_biggened$girlslove <- ifelse(
  rowSums(MU_df_biggened[, MU_genre_list_small[c(1,4)]]) > 0, 
  1, 
  0
)
MU_df_biggened$boyslove <- ifelse(
  rowSums(MU_df_biggened[, MU_genre_list_small[c(2,3)]]) > 0, 
  1, 
  0
)


## Break out relation type and categories

unique(unlist(MU_df_biggened$categories))
unique(unlist(MU_df_biggened$relation_type))

MU_df_biggened$categories_backup = MU_df_biggened$categories
MU_df_biggened$categories <- lapply(MU_df_biggened$categories, function(x) paste(unlist(x), collapse = ", "))
MU_df_biggened$relation_type_backup = MU_df_biggened$relation_type
MU_df_biggened$relation_type <- lapply(MU_df_biggened$relation_type, function(x) paste(unlist(x), collapse = ", "))

MU_df_biggened_2 <- MU_df_biggened %>%
  separate_rows(relation_type, sep = ",") |>
  mutate(relation_type = ifelse(relation_type == "", "Nope", relation_type))%>%
  mutate(relation_type = trimws(relation_type),
         value = 1) |>
  distinct()%>%
  pivot_wider(
    names_from =  relation_type,
    values_from = value,
    values_fill = 0
  )%>%
  dplyr::select(-Nope)%>%
  separate_rows(categories, sep = ",") |>
  mutate(categories = ifelse(categories == "", "None", categories))%>%
  mutate(categories = trimws(categories),
         value = 1) |>
  mutate(categories = ifelse(categories == "Psychological", "Psychological_CAT", categories),
         categories = ifelse(categories == "Romance", "Romance_CAT", categories),
         categories = ifelse(categories == "Lolicon", "Lolicon_CAT", categories),
         categories = ifelse(categories == "Shotacon", "Shotacon_CAT", categories),
         categories = ifelse(categories == "Gender Bender", "Gender Bender_CAT", categories),
         categories = ifelse(categories == "Martial Arts", "Martial Arts_CAT", categories)
                             )%>%
  pivot_wider(
    names_from =  categories,
    values_from = value,
    values_fill = 0
  )%>%
  dplyr::select(-None)

##getting rid of small cats

MU_df_biggened_3 <- MU_df_biggened_2 %>%
  dplyr::select(1:81, names(MU_df_biggened_2)[which(colSums(MU_df_biggened_2[, 81:ncol(MU_df_biggened_2)]) >= 250)])

og_pub <- sort(table(MU_df_biggened_3$pub_name_og), decreasing = TRUE)[1:20]
common_og_pub <- names(og_pub)
en_pub <- sort(table(MU_df_biggened_3$pub_name_en), decreasing = TRUE)[1:20]
common_en_pub <- names(en_pub)

MU_df_biggened_3 <- MU_df_biggened_3%>%
  dplyr::mutate(pub_name_en = as.character(pub_name_en),
                pub_name_og = as.character(pub_name_og),
                pub_en_lump = ifelse(pub_name_en %in% common_en_pub, pub_name_en, "Other"),
                pub_og_lump = ifelse(pub_name_og %in% common_og_pub, pub_name_og, "Other"),
                pub_en_lump = fct(pub_en_lump),
                pub_og_lump = fct(pub_og_lump))


MU_df_biggened_3$type = fct_lump_n(MU_df_biggened_3$type, 5)
levels(MU_df_biggened_3$type)

MU_df_biggened_3 <- MU_df_biggened_3%>%
  mutate(last_updated_year = year(last_updated))

MU_df_biggened_3$small_status = fct_relevel(MU_df_biggened_3$small_status,
                                    "Unknown",
                                    "Ongoing",
                                    "Complete",
                                    "Cancelled",
                                    "Hiatus")
levels(MU_df_biggened_3$small_status)

MU_df_biggened_3 <- MU_df_biggened_3 %>%
  mutate(mutual_exclu_queer = case_when(
    boyslove == 0 & girlslove == 0 ~ "Not Queer",
    boyslove == 1 & girlslove == 0 ~ "Boys Love",
    boyslove == 0 & girlslove == 1 ~ "Girls Love",
    boyslove == 1 & girlslove == 1 ~ "Both"
  ))

MU_df_biggened_3 <- MU_df_biggened_3 %>%
  mutate(mutual_exclu_gl= case_when(
    `Shoujo Ai` == 0 & Yuri == 0 ~ "Not GL",
    `Shoujo Ai` == 1 & Yuri == 0 ~ "Shoujo Ai",
    `Shoujo Ai` == 0 & Yuri == 1 ~ "Yuri",
    `Shoujo Ai` == 1 & Yuri == 1 ~ "Both"
  ))

MU_df_biggened_3 <- MU_df_biggened_3 %>%
  mutate(mutual_exclu_bl = case_when(
    `Shounen Ai` == 0 & Yaoi == 0 ~ "Not BL",
    `Shounen Ai` == 1 & Yaoi == 0 ~ "Shounen Ai",
    `Shounen Ai` == 0 & Yaoi == 1 ~ "Yaoi",
    `Shounen Ai` == 1 & Yaoi == 1 ~ "Both"
  ))

MU_df_biggened_4 <- MU_df_biggened_3%>%
  filter(bayesian_rating > 0)

# Settings ----------------------------------------------------------------

MU_colors <- c(
  "Pink" =  "#D4666F",
  "Orange" = "#ff931c",
  "Yellow" = "#fecf69",
  "Green" =  "#74D3AE",
  "Blue" =  "#A6BCDD",
  "Purple" =  "#9C6B9B",
  "Light Grey" = "#eff1f5",
  "Medium Grey" = "#d1d9e3",
  "Dark Grey" = "#51637a"
)

genre_colors <- c(
  "Shoujo Ai"= "#ff9b55",
  "Yuri" = "#d462a6",
  "Shounen Ai"= "#99e8c2",
  "Yaoi" = "#7bade3",
  "Girls Love" = "#d62900",
  "Boys Love" ="#5049cb",
  "Queer" = "#ffee00",
  "Not Queer" = "#d1d9e3"
)

# Data explora ------------------------------------------------------------

##### Genre

round(sapply(MU_df_biggened_4[,MU_genre_list], table)/length(MU_df_biggened_4$queer)*100, 1)
table(MU_df_biggened_4$Yaoi)/length(MU_df_biggened_4$queer)
table(MU_df_biggened_4$Yuri)/length(MU_df_biggened_4$queer)

length(MU_df_biggened_4$`Shoujo Ai`[MU_df_biggened_4$`Shoujo Ai` == 1 & MU_df_biggened_4$Yuri == 1])/sum(MU_df_biggened_4$girlslove)
length(MU_df_biggened_4$`Shounen Ai`[MU_df_biggened_4$`Shounen Ai` == 1 & MU_df_biggened_4$Yaoi == 1])/sum(MU_df_biggened_4$boyslove)
length(MU_df_biggened_4$`Shounen Ai`[MU_df_biggened_4$girlslove == 1 & MU_df_biggened_4$boyslove == 1])/sum(MU_df_biggened_4$queer)

summary(MU_df_biggened_4$genre_n)
MU_df_biggened_4%>%
  #dplyr::group_by(queer)%>%
  # dplyr::group_by(girlslove)%>%
  # dplyr::group_by(boyslove)%>%
  dplyr::summarize(mean_n = mean(genre_n))


  MU_contingent = MU_df_biggened_4

  MU_contingent$Yaoi <- ifelse(MU_contingent$Yaoi == 1, "Yaoi", "non-Yaoi")
  MU_contingent$`Shounen Ai` <- ifelse(MU_contingent$`Shounen Ai` == 1, "Shounen Ai", "non-Shounen Ai")
  MU_contingent$Yuri <- ifelse(MU_contingent$Yuri == 1, "Yuri", "non-Yuri")
  MU_contingent$`Shoujo Ai` <- ifelse(MU_contingent$`Shoujo Ai` == 1, "Shoujo Ai", "non-Shoujo Ai")
  MU_contingent$boyslove <- ifelse(MU_contingent$boyslove == 1, "BL", "non-BL")
  MU_contingent$girlslove <- ifelse(MU_contingent$girlslove == 1, "GL", "non-GL")

  contingency_table <- table(MU_contingent$boyslove, MU_contingent$girlslove)
  contingency_table2 <- table(MU_contingent$Yaoi, MU_contingent$`Shounen Ai`)
  contingency_table3 <- table(MU_contingent$Yuri, MU_contingent$`Shoujo Ai`)
  
  round(contingency_table / length(MU_contingent$queer) * 100, 2)
  round(contingency_table2 / length(MU_contingent$queer) * 100, 2)
  round(contingency_table3 / length(MU_contingent$queer) * 100, 2)
  
corr_matrix = round(cor(MU_df_biggened_4[, MU_genre_list]), 2)
ggcorrplot(corr_matrix)
corr_matrix

corr_matrix_masked <- ifelse((corr_matrix > 0.15 | corr_matrix < -0.15), corr_matrix, NA)
print(corr_matrix_masked)

###### Description

summary(MU_df_biggened_4$desc_length[MU_df_biggened_4$girlslove==1])

MU_df_biggened_4%>%
  # dplyr::group_by(queer)%>%
  # dplyr::group_by(girlslove)%>%
  # dplyr::group_by(boyslove)%>%
  dplyr::summarize(mean_n = mean(desc_length))

MU_df_biggened_4%>%
  # dplyr::group_by(queer)%>%
  # dplyr::group_by(girlslove)%>%
  # dplyr::group_by(boyslove)%>%
  dplyr::summarize(yes_desc = length(desc_length[desc_length > 0])/
                                       length(desc_length))

length(MU_df_biggened_4$desc_length[MU_df_biggened_4$desc_length > 0])/
  length(MU_df_biggened_4$desc_length)


length_hist <- MU_df_biggened_4 %>%
  filter(desc_length < 5000) %>%
  ggplot(aes(x = desc_length,
             fill = mutual_exclu_queer,
             color = mutual_exclu_queer)) +
  geom_density(alpha = 0.5, adjust = 1.2, position = "identity") +
  scale_fill_manual(values = c("Girls Love" = genre_colors[['Girls Love']],
                               "Boys Love" = genre_colors[['Boys Love']],
                               "Both" = genre_colors[['Queer']],
                               "Not Queer" = genre_colors[['Not Queer']]))+
  scale_color_manual(values = c("Girls Love" = genre_colors[['Girls Love']],
                               "Boys Love" = genre_colors[['Boys Love']],
                               "Both" = genre_colors[['Queer']],
                               "Not Queer" = genre_colors[['Not Queer']]))+
  theme_minimal()+
  xlab("Description Length")+
  theme(legend.title=element_blank()) +
  facet_grid(~mutual_exclu_queer)
length_hist

length_hist_2 <- MU_df_biggened_4 %>%
  filter(desc_length < 5000) %>%
  ggplot(aes(x = desc_length)) +
  geom_histogram(bins = 50,
                 fill = MU_colors[['Medium Grey']],
                 color = MU_colors[['Medium Grey']]) +
  xlab("Description Length") +
  theme_minimal()+
  theme(legend.position = "none") 
length_hist_2


##Type
MU_df_biggened_4%>%
  # dplyr::group_by(queer)%>%
  # dplyr::group_by(girlslove)%>%
  # dplyr::group_by(boyslove)%>%
  count(type)%>%
  mutate(type_prop = round(100 * n/sum(n),2))%>%
  print(n = Inf)

MU_df_biggened_4%>%
  dplyr::group_by(type)%>%
  dplyr::count(type)%>%
  ungroup()%>%
  mutate(prop = round(100 * n/sum(n),2))%>%
  arrange(desc(n))

MU_df_biggened_4 %>%
  group_by(mutual_exclu_queer, type) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count) * 100) %>%
  ggplot(aes(x = type, 
             color = type, 
             fill = type, 
             y = proportion)) +
  geom_bar(stat = "identity") +
  xlab("Type") +
  ylab("Proportion (%)") +
  theme_minimal() +
  facet_grid(~mutual_exclu_queer) +
  scale_fill_manual(values = unname(MU_colors)[1:6]) +
  scale_color_manual(values = unname(MU_colors)[1:6]) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

MU_df_biggened_4 %>%
  group_by(type) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count) * 100) %>%
  ggplot(aes(x = type, 
             color = type, 
             fill = type, 
             y = proportion)) +
  geom_bar(stat = "identity") +
  xlab("Type") +
  ylab("Proportion (%)") +
  theme_minimal() +
  scale_fill_manual(values = unname(MU_colors)[1:6]) +
  scale_color_manual(values = unname(MU_colors)[1:6]) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))


##Year
summary(MU_df_biggened_4$year[MU_df_biggened_4$queer==0])
summary(MU_df_biggened_4$year[MU_df_biggened_4$queer==0])

MU_df_biggened_4%>%
  dplyr::group_by(year)%>%
  dplyr::count(year)%>%
  ungroup()%>%
  mutate(prop = round(100 * n/sum(n),2))%>%
  arrange(desc(n))%>%
  print(n = Inf)

summary(MU_df_biggened_4$year)


MU_df_biggened_4%>%
  # dplyr::group_by(queer)%>%
  # dplyr::group_by(girlslove)%>%
  # dplyr::group_by(boyslove)%>%
  count(year)%>%
  mutate(prop = round(100 * n/sum(n),2))%>%
  arrange(desc(n))%>%
  print(n = Inf)

MU_df_biggened_4 %>%
  ggplot(aes(x = year,
             fill = mutual_exclu_queer,
             color = mutual_exclu_queer)) +
  geom_density(alpha = 0.5, adjust = 1.2, position = "identity") +
  scale_fill_manual(values = c("Girls Love" = genre_colors[['Girls Love']],
                               "Boys Love" = genre_colors[['Boys Love']],
                               "Both" = genre_colors[['Queer']],
                               "Not Queer" = genre_colors[['Not Queer']]))+
  scale_color_manual(values = c("Girls Love" = genre_colors[['Girls Love']],
                                "Boys Love" = genre_colors[['Boys Love']],
                                "Both" = genre_colors[['Queer']],
                                "Not Queer" = genre_colors[['Not Queer']]))+
  theme_minimal()+
  xlab("Year")+
  theme(legend.title=element_blank())

MU_df_biggened_4 %>%
  ggplot(aes(x = year)) +
  geom_histogram(bins = 50,
                 fill = MU_colors[['Medium Grey']],
                 color = MU_colors[['Medium Grey']]) +
  xlab("Year") +
  theme_minimal()+
  theme(legend.position = "none") 


## Categories
MU_df_biggened_4%>%
  dplyr::group_by(cat_n)%>%
  dplyr::count(cat_n)%>%
  ungroup()%>%
  mutate(prop = round(100 * n/sum(n),2))%>%
  arrange(desc(n))%>%
  print(n = Inf)

MU_df_biggened_4%>%
  # dplyr::group_by(queer)%>%
  # dplyr::group_by(girlslove)%>%
  # dplyr::group_by(boyslove)%>%
  dplyr::summarize(mean = mean(cat_n))
  dplyr::count(cat_n)%>%
  ungroup()%>%
  mutate(prop = round(100 * n/sum(n),2))%>%
  arrange(desc(n))%>%
  print(n = Inf)

summary(MU_df_biggened_4$cat_n[MU_df_biggened_4])

round(
  sort(colSums(MU_df_biggened_4[MU_df_biggened_4$boyslove == 1,
                                    82:(ncol(MU_df_biggened_4)-7)]),
           decreasing = TRUE)[1:20]/nrow(
             MU_df_biggened_4[MU_df_biggened_4$boyslove == 1,])*100, 2)


## Relations
summary(MU_df_biggened_4$relation_n)

MU_df_biggened_4%>%
  #filter(boyslove == 1)%>%
  dplyr::group_by(relation_n)%>%
  dplyr::count(relation_n)%>%
  ungroup()%>%
  mutate(prop = round(100 * n/sum(n),2))%>%
  arrange(desc(n))%>%
  print(n = Inf)

MU_df_biggened_4%>%
  # dplyr::group_by(queer)%>%
  # dplyr::group_by(girlslove)%>%
  # dplyr::group_by(boyslove)%>%
  dplyr::summarize(mean = mean(relation_n))
  dplyr::count(relation_n)%>%
  ungroup()%>%
  mutate(prop = round(100 * n/sum(n),2))%>%
  arrange(desc(n))%>%
  print(n = Inf)

round(
  sort(colSums(MU_df_biggened_4[MU_df_biggened_4$boyslove == 1,
                                72:82]),
       decreasing = TRUE)[1:10]/nrow(
         MU_df_biggened_4[MU_df_biggened_4$boyslove == 1,])*100, 2)

round(
  sort(colSums(MU_df_biggened_4[,
                                72:82]),
       decreasing = TRUE)[1:10]/nrow(
         MU_df_biggened_4) *100, 2)

## Rating

summary(MU_df_biggened_4$bayesian_rating)
# length(MU_df_biggened_4$bayesian_rating[MU_df_biggened_4$bayesian_rating == 0])/(
#   length(MU_df_biggened_4$bayesian_rating)
# )

MU_df_biggened_4%>%
  group_by(mutual_exclu_queer)%>%
  dplyr::summarise(n = n(),
                   rating = sum(bayesian_rating > 0))%>%
  mutate(pct = rating/n)


MU_df_biggened_4 %>%
  ggplot(aes(x = bayesian_rating,
             fill = mutual_exclu_queer,
             color = mutual_exclu_queer)) +
  geom_density(alpha = 0.3, adjust = 1.2, position = "identity") +
  scale_fill_manual(values = c("Girls Love" = genre_colors[['Girls Love']],
                               "Boys Love" = genre_colors[['Boys Love']],
                               "Both" = genre_colors[['Queer']],
                               "Not Queer" = genre_colors[['Not Queer']]))+
  scale_color_manual(values = c("Girls Love" = genre_colors[['Girls Love']],
                                "Boys Love" = genre_colors[['Boys Love']],
                                "Both" = genre_colors[['Queer']],
                                "Not Queer" = genre_colors[['Not Queer']]))+
  theme_minimal()+
  xlab("Rating")+
  theme(legend.title=element_blank())

# #Votes
summary(MU_df_biggened_4$votes)
# length(MU_df_biggened_4$votes[MU_df_biggened_4$votes == 0])/(
#   length(MU_df_biggened_4$votes)
# )

MU_df_biggened_4%>%
  group_by(mutual_exclu_queer)%>%
  dplyr::summarise(n = n(),
                   n_votes = sum(votes > 500))%>%
  mutate(pct = n_votes/n)


MU_df_biggened_4 %>%
  filter(votes < 500)%>%
  ggplot(aes(x = votes,
             fill = mutual_exclu_queer,
             color = mutual_exclu_queer)) +
  geom_density(alpha = 0.3, adjust = 1.2, position = "identity") +
  scale_fill_manual(values = c("Girls Love" = genre_colors[['Girls Love']],
                               "Boys Love" = genre_colors[['Boys Love']],
                               "Both" = genre_colors[['Queer']],
                               "Not Queer" = genre_colors[['Not Queer']]))+
  scale_color_manual(values = c("Girls Love" = genre_colors[['Girls Love']],
                                "Boys Love" = genre_colors[['Boys Love']],
                                "Both" = genre_colors[['Queer']],
                                "Not Queer" = genre_colors[['Not Queer']]))+
  theme_minimal()+
  xlab("Number of votes")+
  theme(legend.title=element_blank())

## Rank this year

summary(MU_df_biggened_4$rank_this_year)

MU_df_biggened_4%>%
  group_by(mutual_exclu_queer)%>%
  dplyr::summarise(n = n(),
                   n_top = sum(rank_this_year < 500))%>%
  mutate(pct = n_top/n)


MU_df_biggened_4 %>%
  ggplot(aes(x = rank_this_year,
             fill = mutual_exclu_queer,
             color = mutual_exclu_queer)) +
  geom_density(alpha = 0.3, adjust = 1.2, position = "identity") +
  scale_fill_manual(values = c("Girls Love" = genre_colors[['Girls Love']],
                               "Boys Love" = genre_colors[['Boys Love']],
                               "Both" = genre_colors[['Queer']],
                               "Not Queer" = genre_colors[['Not Queer']]))+
  scale_color_manual(values = c("Girls Love" = genre_colors[['Girls Love']],
                                "Boys Love" = genre_colors[['Boys Love']],
                                "Both" = genre_colors[['Queer']],
                                "Not Queer" = genre_colors[['Not Queer']]))+
  theme_minimal()+
  xlab("Rank this year")+
  theme(legend.title=element_blank())

## Lists

summary(MU_df_biggened_4$total_lists)

MU_df_biggened_4%>%
  group_by(mutual_exclu_queer)%>%
  dplyr::summarise(n = n(),
                   n_top = sum(total_lists > 5000))%>%
  mutate(pct = n_top/n)

MU_df_biggened_4 %>%
  filter(total_lists <5000)%>%
  ggplot(aes(x = total_lists,
             fill = mutual_exclu_queer,
             color = mutual_exclu_queer)) +
  geom_density(alpha = 0.3, adjust = 1.2, position = "identity") +
  scale_fill_manual(values = c("Girls Love" = genre_colors[['Girls Love']],
                               "Boys Love" = genre_colors[['Boys Love']],
                               "Both" = genre_colors[['Queer']],
                               "Not Queer" = genre_colors[['Not Queer']]))+
  scale_color_manual(values = c("Girls Love" = genre_colors[['Girls Love']],
                                "Boys Love" = genre_colors[['Boys Love']],
                                "Both" = genre_colors[['Queer']],
                                "Not Queer" = genre_colors[['Not Queer']]))+
  theme_minimal()+
  xlab("Total number of lists")+
  theme(legend.title=element_blank())

## Last Updated
summary(MU_df_biggened_4$last_updated[MU_df_biggened_4$last_updated == ymd_hms("1970-01-01 00:00:00.0000")])

summary(MU_df_biggened_4$last_updated[MU_df_biggened_4$last_updated > ymd_hms("1970-01-01 00:00:00.0000") &
                                        MU_df_biggened_4$queer == 0])

MU_df_biggened_4%>%
  group_by(mutual_exclu_queer)%>%
  dplyr::summarise(n = n(),
                   n_top = sum(last_updated > ymd_hms("1970-01-01 00:00:00.0000")))%>%
  mutate(pct = n_top/n)


MU_df_biggened_4 %>%
  ggplot(aes(x = last_updated,
             fill = mutual_exclu_queer,
             color = mutual_exclu_queer)) +
  geom_density(alpha = 0.3, adjust = 1.2, position = "identity") +
  scale_fill_manual(values = c("Girls Love" = genre_colors[['Girls Love']],
                               "Boys Love" = genre_colors[['Boys Love']],
                               "Both" = genre_colors[['Queer']],
                               "Not Queer" = genre_colors[['Not Queer']]))+
  scale_color_manual(values = c("Girls Love" = genre_colors[['Girls Love']],
                                "Boys Love" = genre_colors[['Boys Love']],
                                "Both" = genre_colors[['Queer']],
                                "Not Queer" = genre_colors[['Not Queer']]))+
  theme_minimal()+
  xlab("Last Updated")+
  theme(legend.title=element_blank())


MU_df_biggened_4%>%
  filter(queer == 0)%>%
  mutate(year = year(last_updated))%>%
  group_by(year)%>%
  dplyr::summarize(n = n())%>%
  arrange(n)

## Latest ch

summary(MU_df_biggened_4$latest_ch[MU_df_biggened_4$latest_ch > 0])
table(MU_df_biggened_4$latest_ch)

MU_df_biggened_4%>%
  group_by(mutual_exclu_queer)%>%
  dplyr::summarise(n = n(),
                   n_top = sum(latest_ch > 100))%>%
  mutate(pct = n_top/n)

MU_df_biggened_4 %>%
  filter(latest_ch>0 & latest_ch < 100)%>%
  ggplot(aes(x = latest_ch,
             fill = mutual_exclu_queer,
             color = mutual_exclu_queer)) +
  geom_density(alpha = 0.3, adjust = 1.2, position = "identity") +
  scale_fill_manual(values = c("Girls Love" = genre_colors[['Girls Love']],
                               "Boys Love" = genre_colors[['Boys Love']],
                               "Both" = genre_colors[['Queer']],
                               "Not Queer" = genre_colors[['Not Queer']]))+
  scale_color_manual(values = c("Girls Love" = genre_colors[['Girls Love']],
                                "Boys Love" = genre_colors[['Boys Love']],
                                "Both" = genre_colors[['Queer']],
                                "Not Queer" = genre_colors[['Not Queer']]))+
  theme_minimal()+
  xlab("Latest chapter")+
  theme(legend.title=element_blank())

## Status
round(table(MU_df_biggened_4$small_status)/length(MU_df_biggened_4$small_status), 2)

MU_df_biggened_4%>%
  group_by(mutual_exclu_queer, small_status)%>%
  dplyr::summarize(n = n())%>%
  mutate(prop = round(n/sum(n), 2))%>%
  pivot_wider(id_cols = mutual_exclu_queer,
              names_from = small_status,
              values_from = prop)

MU_df_biggened_4 %>%
  group_by(mutual_exclu_queer, small_status) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count) * 100) %>%
  ggplot(aes(x = small_status, 
             color = small_status, 
             fill = small_status, 
             y = proportion)) +
  geom_bar(stat = "identity") +
  xlab("Status") +
  ylab("Proportion (%)") +
  theme_minimal() +
  theme(legend.title = element_blank())+
  facet_grid(~mutual_exclu_queer) +
  scale_fill_manual(values = unname(MU_colors)[1:5]) +
  scale_color_manual(values = unname(MU_colors)[1:5]) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))


MU_df_biggened_4 %>%
  group_by(small_status) %>%
  summarise(count = n()) %>%
  mutate(proportion = count / sum(count) * 100) %>%
  ggplot(aes(x = small_status, 
             color = small_status, 
             fill = small_status, 
             y = proportion)) +
  geom_bar(stat = "identity") +
  xlab("Status") +
  ylab("Proportion (%)") +
  theme_minimal() +
  theme(legend.title = element_blank())+
  scale_fill_manual(values = unname(MU_colors)[1:5]) +
  scale_color_manual(values = unname(MU_colors)[1:5]) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))

## Licensed
round(table(MU_df_biggened_4$licensed)/length(MU_df_biggened_4$licensed), 2)

MU_df_biggened_4%>%
  group_by(mutual_exclu_queer, licensed)%>%
  dplyr::summarize(n = n())%>%
  mutate(prop = round(n/sum(n), 2))%>%
  pivot_wider(id_cols = mutual_exclu_queer,
              names_from = licensed,
              values_from = prop)

MU_df_biggened_4 %>%
  mutate(licensed = ifelse(licensed, "Licensed", "Not licensed")) %>%
  group_by(mutual_exclu_queer, licensed) %>%
  dplyr::summarize(count = n()) %>%
  group_by(mutual_exclu_queer) %>%
  mutate(proportion = count / sum(count)) %>%
  ggplot(aes(x = mutual_exclu_queer,
             y = proportion,
             color = licensed,
             fill = licensed)) +
  geom_bar(stat = "identity",
           position = "fill") +
  xlab("License status") +
  ylab("Proportion (%)") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = unname(MU_colors)[1:2]) +
  scale_color_manual(values = unname(MU_colors)[1:2]) +
  scale_y_continuous(labels = scales::percent_format(scale = 100), 
                     breaks = scales::pretty_breaks(n = 5))

## Fully_scanlated
round(table(MU_df_biggened_4$fully_scanlated)/length(MU_df_biggened_4$fully_scanlated), 2)

MU_df_biggened_4%>%
  group_by(mutual_exclu_queer, fully_scanlated)%>%
  dplyr::summarize(n = n())%>%
  mutate(prop = round(n/sum(n), 2))%>%
  pivot_wider(id_cols = mutual_exclu_queer,
              names_from = fully_scanlated,
              values_from = prop)

MU_df_biggened_4 %>%
  mutate(fully_scanlated = ifelse(fully_scanlated, "Full fan translation", "Incomplete fan translation")) %>%
  group_by(mutual_exclu_queer, fully_scanlated) %>%
  dplyr::summarize(count = n()) %>%
  ggplot(aes(x = mutual_exclu_queer,
             y = count,
             color = fully_scanlated,
             fill = fully_scanlated)) +
  geom_bar(stat = "identity",
           position = "fill") +
  xlab("Fan translation status") +
  ylab("Proportion (%)") +
  theme_minimal() +
  theme(legend.title = element_blank()) +
  scale_fill_manual(values = unname(MU_colors)[1:2]) +
  scale_color_manual(values = unname(MU_colors)[1:2]) +
  scale_y_continuous(labels = scales::percent_format(scale = 100), 
                     breaks = scales::pretty_breaks(n = 5))


## Pub names
sort(table(MU_df_biggened_4$pub_og_lump), decreasing = TRUE)[1:10]/length(MU_df_biggened_4$pub_en_lump)

MU_df_biggened_4%>%
  filter(pub_og_lump != "")%>%
  group_by(mutual_exclu_queer, pub_og_lump)%>%
  dplyr::summarize(n = n())%>%
  mutate(prop = round(n/sum(n), 2)) %>%
  filter(prop > 0.01)%>%
  arrange(mutual_exclu_queer, desc(prop))%>%
  print(n = Inf)

sort(table(MU_df_biggened_4$pub_en_lump), decreasing = TRUE)[1:10]/length(MU_df_biggened_4$pub_en_lump)

MU_df_biggened_4%>%
  filter(pub_en_lump != "")%>%
  group_by(mutual_exclu_queer, pub_en_lump)%>%
  dplyr::summarize(n = n())%>%
  mutate(prop = round(n/sum(n), 2)) %>%
  filter(prop > 0.01)%>%
  arrange(mutual_exclu_queer, desc(prop))%>%
  print(n = Inf)


###############################
## OTHER STUFF

## Correlations

MU_df_corr <- MU_df_biggened_4

MU_df_corr$type <- as.numeric(MU_df_corr$type)
MU_df_corr$small_status <- as.numeric(MU_df_corr$small_status)
MU_df_corr$licensed <- as.numeric(MU_df_corr$licensed)
MU_df_corr$fully_scanlated <- as.numeric(MU_df_corr$fully_scanlated)
MU_df_corr$last_updated_year <- as.numeric(format(MU_df_corr$last_updated, "%Y"))


corr_matrix2 = round(cor(MU_df_corr[, c("desc_length",
                                        #"type",
                                        "year",
                                        "bayesian_rating",
                                        "votes",
                                        "rank_this_year",
                                        "lists_reading",
                                        "lists_wish",
                                        "lists_complete",
                                        "lists_unfinished",
                                        "lists_custom",
                                        "total_lists",
                                        "last_updated_year",
                                        "latest_ch",
                                        "small_status",
                                        "licensed",
                                        "fully_scanlated",
                                        "relation_n",
                                        "cat_n",
                                        "genre_n"
                                        #MU_relation_list,
                                        #MU_genre_list
                                            )]), 2)
ggcorrplot(corr_matrix2)
corr_matrix2

corr_matrix_masked_2 <- ifelse((corr_matrix2 > 0.3 | corr_matrix2 < -0.3), corr_matrix2, NA)
print(corr_matrix_masked_2)

'
Licensed, scanlated <- prop
boxplots <- rating, votes, rank, lists (all), relation_n

'








# Principal Component  ----------------------------------------------------------

MU_PCA <- MU_df_biggened_4%>%
  dplyr::select(
         desc_length,
         #type,
         year,
         bayesian_rating,
         votes,
         rank_this_year,
         lists_reading,
         lists_wish,
         lists_complete,
         lists_unfinished,
         lists_custom,
         #total_lists,
         #small_status,
         last_updated_year,
         latest_ch,
         #licensed,
         #fully_scanlated,
         relation_n,
         cat_n,
         genre_n)
MU_PCA$series_id <- NULL

MU_PCA_labels <- MU_df_biggened_4%>%
  dplyr::select(`Shoujo Ai`,
         Yuri,
         girlslove,
         `Shounen Ai`,
         Yaoi,
         boyslove,
         queer,
         mutual_exclu_queer,
         mutual_exclu_gl,
         mutual_exclu_bl)

MU_PCA_labels$mutual_exclu_queer <- factor(MU_PCA_labels$mutual_exclu_queer, 
                                     levels = c("Both", "Boys Love", "Girls Love", "Not Queer"))
MU_PCA_labels$mutual_exclu_gl <- factor(MU_PCA_labels$mutual_exclu_gl, 
                                     levels = c("Both", "Shoujo Ai", "Yuri", "Not GL"))
MU_PCA_labels$mutual_exclu_bl <- factor(MU_PCA_labels$mutual_exclu_bl, 
                                     levels = c("Both", "Shounen Ai", "Yaoi", "Not BL"))

# pair <- ggpairs(MU_PCA)

pca <- prcomp(MU_PCA,
              scale = TRUE)
pca

autoplot(pca,
         data = MU_PCA %>%
           #mutate(mutual_exclu_gl= MU_PCA_labels$mutual_exclu_gl),
           mutate(mutual_exclu_bl= MU_PCA_labels$mutual_exclu_bl),
           #mutate(mutual_exclu_queer = MU_PCA_labels$mutual_exclu_queer),
         loadings = TRUE,
         loadings.color = "Black",
         loadings.label = TRUE,
         loadings.label.size = 3,
         loadings.label.color = "Black",
         loadings.label.hjust= 1,
         loadings.label.vjust= -1,
         alpha = 0.5,
         #color = "mutual_exclu_queer") +
         color = "mutual_exclu_bl") +
         #color = "mutual_exclu_gl") +
    scale_color_manual(values = c(
      # "Both" = genre_colors[["Girls Love"]],
      # "Shoujo Ai" = genre_colors[["Shoujo Ai"]],
      # "Yuri" = genre_colors[["Yuri"]],
      # "Not GL" = genre_colors[["Not Queer"]]
      "Both" = genre_colors[["Boys Love"]],
      "Shounen Ai" = genre_colors[["Shounen Ai"]],
      "Yaoi" = genre_colors[["Yaoi"]],
      "Not BL" = genre_colors[["Not Queer"]]
      # "Both" = genre_colors[["Queer"]],
      # "Boys Love" = genre_colors[["Boys Love"]],
      # "Girls Love" = genre_colors[["Girls Love"]],
      # "Not Queer" = genre_colors[["Not Queer"]]
    )) + 
    theme_minimal() + 
    guides(color = guide_legend(title = "Status"))+
  facet_wrap(~mutual_exclu_bl)


pve <- (pca$sdev^2)/sum(pca$sdev^2)
par(mfrow=c(1,2))
plot(pve, ylim=c(0,1))
plot(cumsum(pve), ylim=c(0,1))

# LDA ---------------------------------------------------------------------

myqda = qda(mutual_exclu_queer ~
              votes +
              total_lists +
              cat_n +
              desc_length +
              year +
              licensed+
              fully_scanlated+
              #small_status, #both is too small for both status and type at same time. 
              type,
            data = MU_df_biggened_4)
myqda

# Plot
# partimat(as.factor(binary_queer) ~
#            votes +
#            total_lists +
#            cat_n +
#            desc_length +
#            year,
#          data = MU_df_biggened_4,
#          method = "qda",
#          image.colors = c("white",
#                           "grey",
#                           "blue",
#                           "red"))

##Cross validation !!



# Forest ------------------------------------------------------------------

MU_df_biggened_5 <- MU_df_biggened_4%>%
  filter(mutual_exclu_queer != "Both") %>%
  mutate(mutual_exclu_queer = fct(mutual_exclu_queer))%>%
  bind_cols(
    dummy_cols(.$pub_en_lump),
    dummy_cols(.$pub_og_lump),
    dummy_cols(.$small_status),
    dummy_cols(.$type)
  )%>%
  rename_with(~ str_replace_all(., "[[:space:]-/!]", "_"))%>%
  rename(Twentyfirst_Century = `21st_Century`)%>%
  rename(Twentyth_Century = `20th_Century`)

genre_formula <- paste(
  sapply(setdiff(MU_genre_list, MU_genre_list_small), function(x) gsub(" |-|/", "_", x)),
  collapse = " + "
)

relation_formula <- paste(
  sapply(MU_relation_list, function(x) gsub(" |-|/", "_", x)),
  collapse = " + "
)

normals_formula <- "desc_length +
                          year +
                          bayesian_rating +
                          votes +
                          rank_this_year +
                          lists_reading +
                          lists_wish +
                          lists_complete +
                          lists_unfinished +
                          lists_custom +
                          total_lists +
                          last_updated_year +
                          latest_ch + 
                          .data_Unknown+
                          .data_Ongoing+
                          .data_Complete+
                          .data_Cancelled+
                          .data_Hiatus +
                          .data_Renta_+
                          .data_Harlequin_K.K.+
                          .data_Coolmic+
                          .data_BiliBili_Comics+
                          .data_Seven_Seas+
                          .data_Lezhin_US+
                          .data_Other...230+
                          .data_Webnovel+
                          .data_INKR_Comics+
                          .data_Futekiya+
                          .data_Webcomics+
                          .data_Manta+
                          .data_Tapas+
                          .data_Yen_Press+
                          .data_LINE_Webtoon+
                          .data_Viz+
                          .data_Renta+
                          .data_Tappytoon+
                          .data_Pocket_Comics+
                          .data_June+
                          .data_Harlequin+
                          .data_Kodansha+
                          .data_Other...248+
                          .data_Kadokawa_Shoten+
                          .data_Shueisha+
                          .data_Shogakukan+
                          .data_Gentosha+
                          .data_Futabasha+
                          .data_Ohzora_Shuppan+
                          .data_Hakusensha+
                          .data_Houbunsha+
                          .data_Bomtoon+
                          .data_Ichijinsha+
                          .data_Akita_Shoten+
                          .data_Kadokawa+
                          .data_Enterbrain+
                          .data_Shinshokan+
                          .data_Naver+
                          .data_Doujinshi+
                          .data_Manga+
                          .data_Manhua+
                          .data_Manhwa+
                          .data_Novel+
                          .data_Other...278+
                          fully_scanlated +
                          licensed+
                          relation_n +
                          cat_n +
                          genre_n"

FULL_tree_formula <- as.formula(paste0("mutual_exclu_queer ~",
                                      genre_formula, "+",
                                      relation_formula, "+",
                                      normals_formula))
  
# myforest2 = randomForest(FULL_tree_formula,
#                         ntree = 500,
#                         data = MU_df_biggened_5,
#                         importance = TRUE,
#                         na.action = na.omit,
#                         do.trace = TRUE)
# 
# myforest2
# importance(myforest2)
# 
# '
# lists_custom
# Josei
# genre_n
# year
# Hentai
# Shoujo
# desc_length
# Shounen
# Seinen
# total_lists
# lists_complete
# votes
# lists_wish
# lists_reading
# rank_this_year
# bayesian_rating
# lists_unfinished
# Shotacon
# latest_ch
# last_updated_year
# 
# .data_Doujinshi
# 
# cat_n
# .data_Ichijinsha
# .data_Complete
# .data_Manga
# fully_scanlated
# 
# '
# 
# varImpPlot(myforest2)
# 
# #saveRDS(myforest2, "normalsforest3.rds")
# myforest2 <- readRDS("normalsforest2.rds")


###

#82-207
#208-333
#334-459
#460-575


cats_formula <- paste(
  sapply(colnames(MU_df_biggened_5)[82:215], function(x) gsub(" |-|/", "_", x)),
  collapse = " + "
)

secondtree_formula1 <- as.formula(paste0("mutual_exclu_queer ~",
                                  cats_formula))

secondmyforest1 = randomForest(secondtree_formula1,
                         ntree = 500,
                         data = MU_df_biggened_5,
                         importance = TRUE,
                         na.action = na.omit,
                         do.trace = TRUE)

importance(secondmyforest1)
varImpPlot(secondmyforest1)

'
Harlequin
Older_Female_Younger_Male
All_Girls_School
Full_Color
Twentyfirst_Century
Web_Comic
LGBT_Scenes
Female_Protagonist
Male_Lead_Falls_in_Love_First
Borderline_H

Young__Male_Lead
'

#saveRDS(secondmyforest1, "catsforest.rds")
secondmyforest1 <- readRDS("catsforest.rds")

######## 
#4th lol

fourthtree_formula <- as.formula(paste0("mutual_exclu_queer ~",
                                         genre_formula, "+",
                                         relation_formula, "+",
                                         normals_formula, "+",
                                         cats_formula))

fourthforest = randomForest(fourthtree_formula,
                         ntree = 500,
                         data = MU_df_biggened_5,
                         importance = TRUE,
                         na.action = na.omit,
                         do.trace = TRUE)

#saveRDS(fourthforest, "fourthforest.rds")
#fourthforest <- readRDS("fourthforest.rds")

fourthforest
varImpPlot(fourthforest)
importance(fourthforest)

imp <- as.data.frame(importance(fourthforest))
imp$Label <- rownames(imp)
top50 <- imp[order(-imp$MeanDecreaseAccuracy), ][1:50, ]
top50$Label
top50_2 <- imp[order(-imp$MeanDecreaseGini), ][1:50, ]
top50_2$Label

predictors <- unique(c(top50$Label,
                       top50_2$Label))
predictors
test2_formula <- reformulate(termlabels = predictors,
                             response = "mutual_exclu_queer")



######
mythird_forest <- randomForest(mutual_exclu_queer ~
                                 lists_custom + 
                                 Josei + 
                                 genre_n + 
                                 year + 
                                 Hentai + 
                                 Shoujo + 
                                 desc_length + 
                                 Shounen + 
                                 Seinen + 
                                 total_lists + 
                                 lists_complete + 
                                 votes + 
                                 lists_wish + 
                                 lists_reading + 
                                 rank_this_year + 
                                 bayesian_rating + 
                                 lists_unfinished + 
                                 Shotacon + 
                                 latest_ch + 
                                 last_updated_year + 
                                 .data_Doujinshi + 
                                 cat_n + 
                                 .data_Ichijinsha + 
                                 Romance+
                                 Ecchi+
                                 Gender_Bender+
                                 Adult+
                                 School_Life+
                                 Action+
                                 .data_Complete + 
                                 .data_Manga + 
                                 .data_Other...248+
                                 Adapted_From+
                                 relation_n+
                                 fully_scanlated+
                                 Harlequin +
                                 Older_Female_Younger_Male +
                                 All_Girls_School +
                                 Full_Color +
                                 Twentyfirst_Century +
                                 Web_Comic +
                                 LGBT_Scenes +
                                 Female_Protagonist +
                                 Male_Lead_Falls_in_Love_First +
                                 Borderline_H +
                                 Young_Male_Lead,
                               ntree = 500,
                               data = MU_df_biggened_5,
                               importance = TRUE,
                               na.action = na.omit,
                               do.trace = TRUE)
varImpPlot(mythird_forest)

#saveRDS(mythird_forest, "mythird_forest.rds")
#mythird_forest <- readRDS("mythird_forest.rds")



########
'
Harlequin
Twentyfirst_Century
Borderline_H
Cheating_Infidelity
LGBT_Scenes
Maid_s
Young_Male_Lead
Award_Winning_Work
Full_Color
Perverted_Female_Lead
Sister_s
Manly_Gay_Couple
Female_Protagonist
Magic
Male_Lead_Falls_in_Love_First
Short_Chapter_s
Tomboy_s
All_Girls_School
Web_Comic
Twentyth_Century
Young_Female_Lead
Older_Female_Younger_Male
Isekai
Episodic

Harem_Seeking_Male_Lead
Indecisive_Protagonist
Love_Hotel
Polyamory
Sexual_Tension
Coach_es
School_Nurse
Tennis
Drunken_Confession
Playboy_Male_Lead
Serious_Female_Lead
Brown_Haired_Male_Lead
Debt_Motivated_Protagonist
Ex_Delinquent_s
Author_Transmigrated_to_Own_Creation
Couple_Growth
Dragon_s
Special_Blood
Wrestling
Searching_for_Someone
Separation_s
Cooking
Food_Gourmet
Mixed_Blood
Onmyouji
Childhood_Friends_Become_Lovers
Death
Video_Game_s
Drug_s
Fight_s
Ninja_s
Bully_ies
Plain_Looking_Protagonist
Team
Bar_s
Abusive_Lover
Aggressive_Lover
Pansexual
Boss_es
Sexual_Violence
Ancient_Civilization
Nonhuman_Protagonist
Middle_Eastern
Headless
Important_Secondary_Character_s
Human_Becomes_Nonhuman
Bandit_s
Interracial_Couple_s
Mysophobia_Germaphobe
'
########
relevant_categories <- c('Harlequin', 'Twentyfirst_Century', 'Borderline_H', 'Cheating_Infidelity', 'LGBT_Scenes',
             'Maid_s', 'Young_Male_Lead', 'Award_Winning_Work', 'Full_Color', 'Perverted_Female_Lead',
             'Sister_s', 'Manly_Gay_Couple', 'Female_Protagonist', 'Magic', 
             'Male_Lead_Falls_in_Love_First', 'Short_Chapter_s', 'Tomboy_s', 'All_Girls_School', 
             'Web_Comic', 'Twentyth_Century', 'Young_Female_Lead', 'Older_Female_Younger_Male', 'Isekai', 
             'Episodic', 'Harem_Seeking_Male_Lead', 'Indecisive_Protagonist',
             'Love_Hotel', 'Polyamory', 'Sexual_Tension', 'Coach_es', 'School_Nurse', 'Tennis',
             'Drunken_Confession', 'Playboy_Male_Lead', 'Serious_Female_Lead', 'Brown_Haired_Male_Lead',
             'Debt_Motivated_Protagonist', 'Ex_Delinquent_s', 
             'Author_Transmigrated_to_Own_Creation', 'Couple_Growth', 'Dragon_s', 'Special_Blood',
             'Wrestling', 'Searching_for_Someone', 'Separation_s', 'Cooking', 'Food_Gourmet', 
             'Mixed_Blood', 'Onmyouji', 'Childhood_Friends_Become_Lovers', 'Death', 'Video_Game_s', 
             'Drug_s', 'Fight_s', 'Ninja_s', 'Bully_ies', 'Plain_Looking_Protagonist',
             'Team', 'Bar_s', 'Abusive_Lover', 'Aggressive_Lover', 'Pansexual', 'Boss_es', 
             'Sexual_Violence', 'Ancient_Civilization', 'Nonhuman_Protagonist', 'Middle_Eastern', 
             'Headless', 'Important_Secondary_Character_s', 'Human_Becomes_Nonhuman', 'Bandit_s', 
             'Interracial_Couple_s', 'Mysophobia_Germaphobe')

relevant_categories_formula <- paste(relevant_categories,
                                     collapse = " + ")

FULL_tree_formula_3 <- as.formula(paste0("mutual_exclu_queer ~",
                                       genre_formula, "+",
                                       relation_formula, "+",
                                       relevant_categories_formula, "+",
                                       normals_formula))


#####
lighter_genre_colors <- lighten(genre_colors, amount = 0.5)

# testtree3 <- rpart(fourthtree_formula,
#                   data = MU_df_biggened_5,
#                   cp = 0.0001,
#                   na.action = na.omit)
# 
# plotcp(testtree3)
# optimal_cp3 <- testtree3$cptable[which.min(testtree3$cptable[,"xerror"]),"CP"]
# optimal_cp3
# 
# pruned_tree3 <- prune(testtree3,
#                      cp = optimal_cp3)
# 
# summary(pruned_tree3)
# 
# predictions3 <- predict(pruned_tree3,
#                        MU_df_biggened_5,
#                        type = "class")
# 
# confusion_matrix3 <- table(MU_df_biggened_5$mutual_exclu_queer, predictions3)
# error_rate3 <- 1 - diag(confusion_matrix3) / rowSums(confusion_matrix3)
# 
# confusion_matrix_with_error3 <- cbind(confusion_matrix3,
#                                       ErrorRate = round(error_rate3, 4))
# confusion_matrix_with_error3
# 
# rpart.plot(pruned_tree3,
#            box.palette = list(
#              lighter_genre_colors[["Not Queer"]],
#              lighter_genre_colors[["Boys Love"]],
#              lighter_genre_colors[["Girls Love"]]))

###
Error_nq_2 <- rep(NA, 20)
Error_gl_2 <- rep(NA, 20)
Error_bl_2 <- rep(NA, 20)

for (i in 1:20){
  testtree2 <- rpart(test2_formula,
                     data = MU_df_biggened_5,
                     cp = 0.0001,
                     na.action = na.omit)
  
  #plotcp(testtree2)
  optimal_cp2 <- testtree2$cptable[which.min(testtree2$cptable[,"xerror"]),"CP"]
  #print(optimal_cp2)
  
  pruned_tree2 <- prune(testtree2,
                        cp = optimal_cp2)
  #summary(pruned_tree2)
  
  predictions2 <- predict(pruned_tree2,
                          MU_df_biggened_5,
                          type = "class")
  
  confusion_matrix2 <- table(MU_df_biggened_5$mutual_exclu_queer, predictions2)
  error_rate2 <- 1 - diag(confusion_matrix2) / rowSums(confusion_matrix2)
  
  confusion_matrix_with_error2 <- cbind(confusion_matrix2,
                                        ErrorRate = round(error_rate2, 4))
  
  nqmean_error <- confusion_matrix_with_error2[1,4]
  blmean_error <- confusion_matrix_with_error2[2,4]
  glmean_error <- confusion_matrix_with_error2[3,4]
  
  print(paste("On loop", i))
  print(nqmean_error)
  print(blmean_error)
  print(glmean_error)
  
  Error_nq_2[i] <- nqmean_error
  Error_gl_2[i] <- blmean_error
  Error_bl_2[i] <- glmean_error

}

# png("tree_plot_wider.png", width = 8000,
#               height = 4828,
#               res = 600)

rpart.plot(pruned_tree2,
           box.palette = list(
             lighter_genre_colors[["Not Queer"]],
             lighter_genre_colors[["Boys Love"]],
             lighter_genre_colors[["Girls Love"]]))
# dev.off()


###
testtesttest_formula <- reformulate(termlabels = predictors,
                             response = "queer")
testtesttest <- rpart(testtesttest_formula,
                   data = MU_df_biggened_5,
                   cp = 0.0001,
                   na.action = na.omit)

optimal_cptesttesttest <- testtesttest$cptable[which.min(testtesttest$cptable[,"xerror"]),"CP"]

pruned_treetesttesttest <- prune(testtesttest,
                      cp = optimal_cptesttesttest)

summary(pruned_treetesttesttest)


# png("tree_plot_ww.png", width = 8000,
#               height = 4828,
#               res = 600)

rpart.plot(pruned_treetesttesttest,
           box.palette = list(
             lighter_genre_colors[["Not Queer"]],
             lighter_genre_colors[["Boys Love"]],
             lighter_genre_colors[["Girls Love"]]))
# dev.off()


# Logistic ----------------------------------------------------------------
# 
# MU_df_biggened_5$queer <- as.factor(MU_df_biggened_5$queer)
# 
# logit_formula <- reformulate(termlabels = setdiff(predictors, c("lists_complete", "lists_reading", "lists_unfinished", "lists_wish", "lists_custom")),
#                              response = "queer")
# 
# logit = glm(logit_formula,
#             data = MU_df_biggened_5,
#             family = "binomial")
# summary(logit)
# 
# mlogit2 =lrm(logit_formula,
#              data = MU_df_biggened_5)
# mlogit2















