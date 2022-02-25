# local_dir <- "~/Documents/R_Projects/der-model-app/"

# load libraries ----
# source(paste0(local_dir,"R/libraries.R"))
library(shiny)
library(shinytest)
library(rsconnect)
library(tidyverse)
library(cowplot)
library(scales)
library(readr)
library(rdrop2)
library(DT)

# data ----
# source(paste0(local_dir,"R/app-data.R"))
# data_dir <- "~/Documents/R_Projects/der-model-app/data/"
base_data_df <-
  readRDS(file = "data/food_feed_pathways_proj.rds")

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

# plot graphics elements ----
# source(paste0(local_dir,"R/plot-graphics-elements.R"))
colorz <-
  c(
    "animal products" = "#F17EB8",
    "fish & seafood" = "#489ED3",
    "plant products" = "#EEAF35"
  )
xlabz <- c("CNS-CFP" = "CFP",
           "ELA-PHD" = "ELA",
           "FDA-SDP" = "USDA")
labz <-
  c("Animal Products", "Fish and Seafood", "Plant Products")

mitigate_arrow <-
  ggplot(data = data.frame(x = c(0, 1), y = c(0, 1)),
         aes(x = x, y = y)) +
  annotate(
    "segment",
    x = 0,
    xend = 0,
    y = 0,
    yend = 1,
    arrow = arrow()
  ) +
  annotate(
    "text",
    x = -0.02,
    y = 0.5,
    label = "Socio-economic challenges for mitigation",
    size = 4,
    angle = 90
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_cartesian(xlim = c(-0.04, 0.04), y = c(0, 1))

adapt_arrow <- ggplot(data = data.frame(x = c(0, 1), y = c(0, 1)),
                      aes(x = x, y = y)) +
  annotate(
    "segment",
    x = 0,
    xend = 1,
    y = 0,
    yend = 0,
    arrow = arrow()
  ) +
  annotate(
    "text",
    x = 0.5,
    y = 0.025,
    label = "Socio-economic challenges for adaptation",
    size = 4
  ) +
  theme_minimal() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    panel.grid = element_blank()
  ) +
  coord_cartesian(xlim = c(0, 1), y = c(-0.04, 0.04))

# UI ----
not_sel <- "Not Selected"
about_page <- tabPanel(title = "About",
                       titlePanel("About"),
                       "Created by Marcus Thomson, NCEAS",
                       br(),
                       "February 2022")

## UI main page elements ----
main_page <- tabPanel(title = "Plots",
                      titlePanel("Plots"),
                      sidebarLayout(
                        sidebarPanel(
                          width = 3,
                          title = "Inputs",
                          ## country id input ----
                          selectInput(
                            inputId = "country_id",
                            label = "Select a country (or WORLD)",
                            choices = country_codes,
                            selected = NULL,
                            multiple = FALSE,
                            selectize = TRUE,
                            width = NULL
                          ),
                          ## diet id input ----
                          selectInput(
                            inputId = "diet_type",
                            label = "Dietary guideline",
                            choices = c("EAT-Lancet", "CNS-CFP", "USDA"),
                            selected = NULL,
                            multiple = FALSE,
                            selectize = TRUE,
                            width = NULL
                          ),
                          ## target year input ----
                          selectInput(
                            inputId = "targ_year",
                            label = "Target year",
                            choices = seq(2045, 2065, by = 5),
                            selected = 2050,
                            multiple = FALSE,
                            selectize = TRUE,
                            width = NULL
                          )
                        ),
                        mainPanel(width = 9,
                                  tabsetPanel(
                                    tabPanel(title = "Plot",
                                             ### pathways plot output ----
                                             shiny::plotOutput(outputId = "derPathways_areaPlot")),
                                    tabPanel(
                                      title = "Data",
                                      ### pathways table output ----
                                      DT::dataTableOutput(outputId = "derPathways_dataTable")
                                    )
                                  ))
                      ))

## UI page ----
ui <- navbarPage(title = "DER Transformation Pathways App",
                 main_page,
                 about_page)

# server ----
server <- function(input, output) {
  ## render area plot ----
  output$derPathways_areaPlot <- renderPlot({
    ### data frame filtered by SSP ----
    data_ssp1_df <- reactive({
      data_df %>%
        filter(iso_alpha3 %in% input$country_id) %>%
        filter(diet_plan == input$diet_type) %>%
        filter(target_year == input$targ_year) %>%
        filter(scenario == "SSP1") %>%
        group_by(year, food_group) %>%
        summarize(kt_yr = sum(kt_yr_fit, na.rm = TRUE),
                  .groups = "drop")
    })
    data_ssp2_df <- reactive({
      data_df %>%
        filter(iso_alpha3 %in% input$country_id) %>%
        filter(diet_plan == input$diet_type) %>%
        filter(target_year == input$targ_year) %>%
        filter(scenario == "SSP2") %>%
        group_by(year, food_group) %>%
        summarize(kt_yr = sum(kt_yr_fit, na.rm = TRUE),
                  .groups = "drop")
    })
    data_ssp3_df <- reactive({
      data_df %>%
        filter(iso_alpha3 %in% input$country_id) %>%
        filter(diet_plan == input$diet_type) %>%
        filter(target_year == input$targ_year) %>%
        filter(scenario == "SSP3") %>%
        group_by(year, food_group) %>%
        summarize(kt_yr = sum(kt_yr_fit, na.rm = TRUE),
                  .groups = "drop")
    })
    data_ssp4_df <- reactive({
      data_df %>%
        filter(iso_alpha3 %in% input$country_id) %>%
        filter(diet_plan == input$diet_type) %>%
        filter(target_year == input$targ_year) %>%
        filter(scenario == "SSP4") %>%
        group_by(year, food_group) %>%
        summarize(kt_yr = sum(kt_yr_fit, na.rm = TRUE),
                  .groups = "drop")
    })
    data_ssp5_df <- reactive({
      data_df %>%
        filter(iso_alpha3 %in% input$country_id) %>%
        filter(diet_plan == input$diet_type) %>%
        filter(target_year == input$targ_year) %>%
        filter(scenario == "SSP5") %>%
        group_by(year, food_group) %>%
        summarize(kt_yr = sum(kt_yr_fit, na.rm = TRUE),
                  .groups = "drop")
    })
    
    ### plot generation ----
    p_ssp1 <- ggplot(na.omit(data_ssp1_df()),
                     aes(
                       x = year,
                       y = kt_yr * 1e-6,
                       fill = food_group
                     )) +
      geom_area(position = "stack") +
      geom_hline(yintercept = 0, alpha = 0.8) +
      geom_vline(
        xintercept = as.numeric(input$targ_year),
        alpha = 0.5,
        linetype = 2,
        colour = "darkblue"
      ) +
      scale_fill_manual(values = colorz,
                        labels = labz) +
      theme_minimal() +
      theme(
        legend.position = "right",
        legend.title = element_blank(),
        axis.text.x = element_text(
          size = 7,
          angle = 90,
          vjust = 0.55,
          hjust = 0.9
        ),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        strip.text = element_text(size = 8)
      ) +
      labs(title = "SSP1",
           x = "year",
           y = "billion tonnes")
    
    p_legend <- cowplot::get_legend(p_ssp1)
    
    p_ssp1 <- p_ssp1 +
      theme(legend.position = "none")
    
    p_ssp2 <- ggplot(na.omit(data_ssp2_df()),
                     aes(
                       x = year,
                       y = kt_yr * 1e-6,
                       fill = food_group
                     )) +
      geom_area(position = "stack") +
      geom_hline(yintercept = 0, alpha = 0.8) +
      geom_vline(
        xintercept = as.numeric(input$targ_year),
        alpha = 0.5,
        linetype = 2,
        colour = "darkblue"
      ) +
      scale_fill_manual(values = colorz,
                        labels = labz) +
      theme_minimal() +
      theme(
        legend.position = "none",
        legend.title = element_blank(),
        axis.text.x = element_text(
          size = 7,
          angle = 90,
          vjust = 0.55,
          hjust = 0.9
        ),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        strip.text = element_text(size = 8)
      ) +
      labs(title = "SSP2",
           x = "year",
           y = "billion tonnes")
    
    p_ssp3 <- ggplot(na.omit(data_ssp3_df()),
                     aes(
                       x = year,
                       y = kt_yr * 1e-6,
                       fill = food_group
                     )) +
      geom_area(position = "stack") +
      geom_hline(yintercept = 0, alpha = 0.8) +
      geom_vline(
        xintercept = as.numeric(input$targ_year),
        alpha = 0.5,
        linetype = 2,
        colour = "darkblue"
      ) +
      scale_fill_manual(values = colorz,
                        labels = labz) +
      theme_minimal() +
      theme(
        legend.position = "none",
        legend.title = element_blank(),
        axis.text.x = element_text(
          size = 7,
          angle = 90,
          vjust = 0.55,
          hjust = 0.9
        ),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        strip.text = element_text(size = 8)
      ) +
      labs(title = "SSP3",
           x = "year",
           y = "billion tonnes")
    
    p_ssp4 <- ggplot(na.omit(data_ssp2_df()),
                     aes(
                       x = year,
                       y = kt_yr * 1e-6,
                       fill = food_group
                     )) +
      geom_area(position = "stack") +
      geom_hline(yintercept = 0, alpha = 0.8) +
      geom_vline(
        xintercept = as.numeric(input$targ_year),
        alpha = 0.5,
        linetype = 2,
        colour = "darkblue"
      ) +
      scale_fill_manual(values = colorz,
                        labels = labz) +
      theme_minimal() +
      theme(
        legend.position = "none",
        legend.title = element_blank(),
        axis.text.x = element_text(
          size = 7,
          angle = 90,
          vjust = 0.55,
          hjust = 0.9
        ),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        strip.text = element_text(size = 8)
      ) +
      labs(title = "SSP4",
           x = "year",
           y = "billion tonnes")
    
    p_ssp5 <- ggplot(na.omit(data_ssp2_df()),
                     aes(
                       x = year,
                       y = kt_yr * 1e-6,
                       fill = food_group
                     )) +
      geom_area(position = "stack") +
      geom_hline(yintercept = 0, alpha = 0.8) +
      geom_vline(
        xintercept = as.numeric(input$targ_year),
        alpha = 0.5,
        linetype = 2,
        colour = "darkblue"
      ) +
      scale_fill_manual(values = colorz,
                        labels = labz) +
      theme_minimal() +
      theme(
        legend.position = "none",
        legend.title = element_blank(),
        axis.text.x = element_text(
          size = 7,
          angle = 90,
          vjust = 0.55,
          hjust = 0.9
        ),
        axis.text.y = element_text(size = 7),
        axis.title.x = element_text(size = 8),
        axis.title.y = element_text(size = 8),
        strip.text = element_text(size = 8)
      ) +
      labs(title = "SSP5",
           x = "year",
           y = "billion tonnes")
    
    pa <- plot_grid(p_ssp5, p_ssp1, ncol = 1)
    pb <-
      plot_grid(p_legend,
                p_ssp2,
                NA,
                ncol = 1,
                rel_heights = c(0.5, 1, 0.5))
    pc <- plot_grid(p_ssp3, p_ssp4, ncol = 1)
    
    p2 <- cowplot::plot_grid(pa, pb, pc, ncol = 3)
    
    cowplot::plot_grid(
      mitigate_arrow,
      p2,
      NA,
      adapt_arrow,
      ncol = 2,
      rel_widths = c(0.1, 0.9),
      rel_heights = c(0.9, 0.1)
    )
    
  })
  
  ### data table ----
  output$derPathways_dataTable <- DT::renderDataTable(
    DT::datatable(
      data_df %>%
        dplyr::select(
          iso_alpha3,
          year,
          target_year,
          diet_plan,
          scenario,
          food_group,
          kt_yr_fit,
          kt_yr_se
        ) %>%
        dplyr::rename(
          country = iso_alpha3,
          kt_est = kt_yr_fit,
          kt_err = kt_yr_se
        ) %>%
        dplyr::mutate(
          kt_est = round(kt_est, digits = 1),
          kt_err = round(kt_err, digits = 1)
        ),
      caption = "Annual crop and livestock product pathway values (kt_est) with standard error (kt_err), in thousands of tonnes. ",
      options = list(pageLength = 10)
    )
  )
  
}

# combine UI & server into an app ----
shinyApp(ui = ui, server = server)