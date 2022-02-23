# data
data_dir <- "~/Documents/Data/DER_output/"

base_data_df <-
  readRDS(file = paste0(data_dir, "food_feed_pathways_proj.rds"))

world_data_df <- base_data_df %>%
  group_by(diet_plan, scenario, food_group, year, target_year) %>%
  summarise(kt_yr_fit = sum(kt_yr_fit, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(iso_alpha3 = "WORLD")

data_df <- base_data_df %>%
  dplyr::bind_rows(world_data_df) %>%
  dplyr::mutate(diet_plan = if_else(
    diet_plan == "ELA-PHD",
    "EAT-Lancet",
    if_else(diet_plan == "FDA-SDP",
            "USDA",
            diet_plan)
  ))

country_codes <-
  data_df %>% distinct(iso_alpha3) %>% pull(iso_alpha3)