#### R SHINY; AGOL & GAD #######################################################
### v1.0 20250522

#### LIBRARIES #################################################################
# remotes::install_github("R-ArcGIS/r-bridge")
library(shiny)
library(httr)
library(jsonlite)
library(purrr)
library(tidyverse)
library(lubridate)
library(knitr)
library(kableExtra)
library(arcpullr)
library(scales)

#### SHINY ACCOUNT AUTHENTICATION ##############################################
rsconnect::setAccountInfo(name='giraffeconservation',
                          token='xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx',
                          secret='xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx')


#### IMPORT MASTER DATAFRAME FROM ESRI AGOL ####################################
df = arcpullr::get_table_layer("https://services1.arcgis.com/xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
                                token="xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx") %>%
  as.data.frame()

  
#### SETUP SHINY USER INTERFACE ################################################
ui = fluidPage(
  tags$div(tags$img(src="GCF_logo.png", height="100px", align="right"),         # GCF logo
           titlePanel("Giraffe Africa Database (GAD v1.0)")),                   # main title
  sidebarLayout(sidebarPanel(width=2,                                           # selection sidebar
                             selectInput("year", "Year", choices=sort(unique(df$Year), decreasing=TRUE), selected=max(df$Year)),
                             selectInput("species", "Species", choices=unique(df$Species), multiple=TRUE),
                             selectInput("subspecies", "Subspecies", choices=unique(df$Subspecies), multiple=TRUE),
                             selectInput("country", "Country", choices=unique(df$Country), multiple=TRUE),
                             selectInput("admin0", "Region0", choices=unique(df$Region0), multiple=TRUE),
                             selectInput("admin1", "Region1", choices=unique(df$Region1), multiple=TRUE),
                             selectInput("site", "Site", choices=unique(df$Site), multiple=TRUE) 
  ),
  mainPanel(width = 10,
            uiOutput("summaryTable") 
  )
  )
)


#### SHINY SERVER FUNCTION #####################################################
server = function(input, output, session) {
  ## filters based on user selections
  observe({
    species_selected = input$species
    subspecies_choices = df %>%
      filter(Species %in% species_selected) %>%
      pull(Subspecies) %>%
      unique()
    updateSelectInput(session, "subspecies", choices=subspecies_choices)
  })
  
  observe({
    subspecies_selected = input$subspecies
    country_choices = df %>%
      filter(Subspecies %in% subspecies_selected) %>%
      pull(Country) %>%
      unique()
    updateSelectInput(session, "country", choices=country_choices)
  })
  
  observe({
    country_selected = input$country
    region0_choices = df %>%
      filter(Country %in% country_selected) %>%
      pull(Region0) %>%
      unique()
    updateSelectInput(session, "admin0", choices=region0_choices)
  })
  
  observe({
    region0_selected = input$admin0
    region1_choices = df %>%
      filter(Region0 %in% region0_selected) %>%
      pull(Region1) %>%
      unique()
    updateSelectInput(session, "admin1", choices=region1_choices)
  })
  
  observe({
    region1_selected = input$admin1
    site_choices = df %>%
      filter(Region1 %in% region1_selected) %>%
      pull(Site) %>%
      unique()
    updateSelectInput(session, "site", choices=site_choices)
  })
  
  filtered_data = reactive({
    data = df
    if (!is.null(input$year)) {
      data = data %>% filter(Year <= input$year)
    }
    if (!is.null(input$species) && length(input$species) > 0) {
      data = data %>% filter(Species %in% input$species)
    }
    if (!is.null(input$subspecies) && length(input$subspecies) > 0) {
      data = data %>% filter(Subspecies %in% input$subspecies)
    }
    if (!is.null(input$country) && length(input$country) > 0) {
      data = data %>% filter(Country %in% input$country)
    }
    if (!is.null(input$admin0) && length(input$admin0) > 0) {
      data = data %>% filter(Region0 %in% input$admin0)
    }
    if (!is.null(input$admin1) && length(input$admin1) > 0) {
      data = data %>% filter(Region1 %in% input$admin1)
    }
    if (!is.null(input$site) && length(input$site) > 0) {
      data = data %>% filter(Site %in% input$site)
    }
    data
  })
  
  
  ###### CODE FOR SUMMARISING DATA ###############################################   
  summary_data = reactive({
    latest_data = filtered_data() %>%
      mutate(
        # set spatial scale rank
        SCALE = case_when(SCALE == "ISO" ~ 1,                                   # nationwide survey
                          SCALE == "REGION" ~ 2,                                # regional survey 
                          SCALE == "SUBREGION" ~ 3,                             # subregional survey
                          SCALE == "SITE" ~ 4,                                  # site specific survey
                          TRUE ~ 5),                                            # other/unknown
        # set data quality rank
        IQI = case_when(Methods__field == "Observation" ~ 1,                    # total population known/monitored
                        Methods__field == "Ground sample" & (!is.na(Std_Err) | !is.na(CI_upper)) ~ 1,  # ground sample with precision
                        Methods__field == "Ground sample" & (is.na(Std_Err) & is.na(CI_upper)) ~ 2,  # ground sample, no precision
                        Methods__field == "Aerial sample" & (!is.na(Std_Err) | !is.na(CI_upper)) ~ 2,  # aerial sample with precision
                        Methods__field == "Aerial sample" & (is.na(Std_Err) & is.na(CI_upper)) ~ 3,  # aerial sample, no precision
                        Methods__field == "Ground total" ~ 3,                   # ground total count
                        Methods__field == "Aerial total" ~ 3,                   # aerial total count
                        Methods__field == "Guesstimate" ~ 4,                    # guesstimate
                        TRUE ~ 5),                                              # degraded data
        Estimate = as.numeric(Estimate),                                        # ensure estimate is numeric
        Std_Err = as.numeric(Std_Err),                                          # ensure std err is numeric
        # create lower estimate
        Lower = case_when(!is.na(CI_lower) ~ CI_lower,                          # use lower CI if available
                          !is.na(Std_Err) ~ Estimate-Std_Err,                   # use estimate-se if SE available
                          Methods__field == "Aerial total" ~ Estimate,          # use estimate if aerial total (ie no lower)
                          Methods__field == "Ground total" ~ Estimate,          # use estimate if ground total (ie no lower)
                          Methods__field == "Guesstimate" ~ Estimate-(Estimate*0.5), # use -estimate*0.5 if guesstimate
                          Methods__field == "Ground sample"&is.na(CI_lower)&is.na(Std_Err) ~ Estimate-(Estimate*0.2),  # use -estimate*0.2 if ground sample used but no CI/SE
                          TRUE ~ Estimate),                                     # orelse just use estimate (ie no lower)
        # create upper estimate
        Upper = case_when(!is.na(CI_upper) ~ CI_upper,                          # use upper CI if available
                          !is.na(Std_Err) ~ Estimate+Std_Err,                   # use estimate+se if SE available
                          Methods__field == "Aerial total" ~ Estimate*1.6,      # use estimate*1.6 if aerial total
                          Methods__field == "Ground total" ~ Estimate*1.2,      # use estimate*1.2 if ground total
                          Methods__field == "Guesstimate" ~ Estimate+(Estimate*0.5), # use +estimate*0.5 if guesstimate
                          Methods__field == "Ground sample"&is.na(CI_upper)&is.na(Std_Err) ~ Estimate+(Estimate*0.2),  # use +estimate-0.2 if ground sample used but no CI/SE
                          TRUE ~ Estimate),                                     # orelse just use estimate (ie no upper)
        # data recency rank
        TIME = year(Sys.Date())-Year,                                           # calculate years since the survey
        # data final rank
        RANK = (IQI*3) + (SCALE*2) + (TIME*1)) %>%                              # lower = better
      arrange(SCALE, TIME) %>%                                                  # arrange data by SCALE and TIME in ascending order
      group_by(Species, Subspecies, Country, Region0, Region1, Site) %>%        # group by species, subspecies, country, region, subregion, site
      slice(1) %>%                                                              # select row with the lowest total RANK within each group
      ungroup() %>%
      select(Country, Region0, Region1, Site, Species, Subspecies, Year, Estimate, Std_Err, IQI, SCALE, TIME, Reference, ref_url, Lower, Upper)  # tidy up what is retained
    
    # Summarize data at the site level
    site_data = latest_data %>%
      filter(!is.na(Site)) %>%                                                  # retain only site level records
      group_by(Country, Species, Subspecies, Region0, Region1, Site) %>%
      summarize(Estimate = sum(Estimate, na.rm=TRUE),                           # sum the total estimates
                Lower = sum(Lower, na.rm=TRUE),                                 # sum the total lower estimates
                Upper = sum(Upper, na.rm=TRUE),                                 # sum the total upper estimates
                Year = max(Year),                                               # calculate max year of collection
                IQI = mean(IQI),                                                # calculate mean IQI of collection
                Reference = first(Reference),                                   # show the reference for the collection
                ref_url = first(ref_url),                                       # include Zotero URL for easy access
                .groups = 'drop')
    
    # Summarize data at the region1 level
    region1_data = latest_data %>%
      filter(is.na(Site)) %>%                                                   # retain subregion level records
      group_by(Country, Species, Subspecies, Region0, Region1) %>%
      summarize(Estimate = sum(Estimate, na.rm=TRUE),                           # sum the total estimates
                Lower = sum(Lower, na.rm=TRUE),                                 # sum the total lower estimates
                Upper = sum(Upper, na.rm=TRUE),                                 # sum the total upper estimates
                Year = max(Year),                                               # calculate max year of collection
                IQI = mean(IQI),                                                # calculate mean IQI of collection
                Reference = first(Reference),                                   # show the reference for the collection
                ref_url = first(ref_url),                                       # include Zotero URL for easy access
                .groups = 'drop')
    
    # Summarize data at the region0 level
    region0_data = latest_data %>%
      filter(is.na(Site) & is.na(Region1)) %>%                                  # retain subregion level records
      group_by(Country, Species, Subspecies, Region0) %>%
      summarize(Estimate = sum(Estimate, na.rm=TRUE),                           # sum the total estimates
                Lower = sum(Lower, na.rm=TRUE),                                 # sum the total lower estimates
                Upper = sum(Upper, na.rm=TRUE),                                 # sum the total upper estimates
                Year = max(Year),                                               # calculate max year of collection
                IQI = mean(IQI),                                                # calculate mean IQI of collection
                Reference = first(Reference),                                   # show the reference for the collection
                ref_url = first(ref_url),                                       # include Zotero URL for easy access
                .groups = 'drop')
    
    # Summarize data at the country (ISO) level
    iso_data = latest_data %>%
      filter(is.na(Site) & is.na(Region1) & is.na(Region0)) %>%  # Only ISO-level records
      group_by(Country, Species, Subspecies) %>%
      summarize(
        Estimate = sum(Estimate, na.rm=TRUE),
        Lower = sum(Lower, na.rm=TRUE),
        Upper = sum(Upper, na.rm=TRUE),
        Year = max(Year),
        IQI = mean(IQI),
        Reference = first(Reference),
        ref_url = first(ref_url),
        .groups = 'drop'
      )
    
    
    ###### EXCLUDE SPATIAL DUPLICATION #############################################
    # there are no country-level records (ISO)
    # if subregion has an estimate, remove site level
    site_data = site_data %>%
      anti_join(region1_data, by = c("Country", "Species", "Subspecies", "Region0", "Region1"))
    
    # if region has an estimate, remove subregion level
    region1_data = region1_data %>%
      anti_join(region0_data, by = c("Country", "Species", "Subspecies", "Region0"))

    # combine the remaining site, subregion, and region level data
    combined_data = bind_rows(site_data, region1_data, region0_data)
    
    
    ###### TOTAL SUMS ##############################################################  
    # calculate total population estimate
    total_population = sum(combined_data$Estimate, na.rm=TRUE)
    total_se = sqrt(sum((combined_data$Std_Err)^2, na.rm=TRUE))
    total_lower = sum(combined_data$Lower, na.rm=TRUE)
    total_upper = sum(combined_data$Upper, na.rm=TRUE)
    total_iqi = ceiling(mean(combined_data$IQI, na.rm=TRUE))                    # Calculate mean IQI for total row
    
    # add a total row to the data frame
    total_row = data.frame(Country = "Total",
                           Region0 = "Total",
                           Region1 = "Total",
                           Site = "",
                           Species = "",
                           Subspecies = "",
                           Year = as.numeric(input$year), 
                           Estimate = total_population,
                           Std_Err = total_se,
                           Lower = total_lower,
                           Upper = total_upper,
                           IQI = total_iqi,
                           SCALE = NA,
                           Reference = "",
                           stringsAsFactors=FALSE)
    

    latest_data = bind_rows(total_row, combined_data)                           # bind the total row and the combined data
    latest_data = latest_data %>% select(-SCALE)                                # Exclude SCALE rank from the final output
    print(paste("Summary data rows:", nrow(latest_data)))                       # for debugging: print number of rows in summary data
    print(latest_data)                                                          # for debugging: print first few rows of summary data
    latest_data
  })
  
  ###### FINAL SUMMARISED DATA TABLE #############################################
  output$summaryTable = renderUI({
    summary_data() %>%
      mutate(Region0 = ifelse(Region0 == "Total", paste0("**", Region0, "**"), Region0),
             Estimate = scales::comma(round(as.numeric(Estimate))),             # ensure estimate is numeric, rounded, with comma
             Lower = scales::comma(round(as.numeric(Lower))),                   # ensure lower is numeric, rounded, with comma
             Upper = scales::comma(round(as.numeric(Upper))),                   # ensure upper is numeric, rounded, with comma
             Reference = if_else(!is.na(ref_url) & ref_url!="", 
                                 paste0('<a href="', ref_url, '" target="_blank">', Reference, '</a>'), Reference) ) %>% # make reference a hyperlink if URL exists
      select(-Std_Err, -ref_url) %>%                                            # exclude Std_Err, ci_lower, and ci_upper from the final display
      kable("html", escape=FALSE) %>%                                           # format display
      kable_styling("striped", full_width=F) %>%                                # add row striping for easier reading
      row_spec(1, bold=TRUE) %>%                                                # make the first row (total) bold
      row_spec(1, extra_css="height: 100px;") %>%                               # add extra space after total
      scroll_box(height="500px") %>%                                            # allow scrolling for more data
      as.character() %>%
      HTML()
  })
}






#### RUN SHINY APP #############################################################
shinyApp(ui=ui, server=server)


