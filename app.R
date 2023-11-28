#profvis::profvis({
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


#### Set up (run once) #### 
library(shiny); library(shinyWidgets); library(shinyBS)
library(ggplot2);library(dplyr);library(tidyr);library(stringr);library(readr)
library(scales); library(ggrepel); library(viridis)
theme_set(theme_bw())

unisex_names <- read_csv("data/unisex_names.csv", show_col_types = FALSE)


#### Define UI for application #### 
ui <- fluidPage(
  #shinythemes::themeSelector(),
  ##### Application title ####
  titlePanel(windowTitle = "Name Explorer", 
             h1("Name Explorer (v0.2)")),
  
  tabsetPanel(
    type = "tabs",
    ##### SPLASH PAGE ####
    tabPanel("About",
             
             #fluidRow(#column("R1, C1", width=11),
                      #column("R1, C2", width=1)),
             fluidRow(
             #  column(10),
             #  column(2,
             #         img(src="https://www.staff.ncl.ac.uk/linglab/files/2023/11/newcastle-university-logo-small.png",
             #             #width="100%", 
             #             height=100, align="right")),
               mainPanel(width = 12,
                         h3("Purpose"),
                         p("This website comes from a project which aims to provide a dataset and tools for building informed linguistics example sentences and stimuli."),
                         p("For example, the Name Explorer can help to:"),
                         tags$ul(
                           tags$li("select gender-fair names, informed by participant age range"),
                           tags$li("find and select racially/ethnically/culturally representative names"),
                           tags$li("generate age-appropriate lists of gender-balanced (unisex) names"),
                           tags$li("identify names perceived to be (more) 'nonbinary' or 'binary'"),
                           tags$li("identify names perceived to be (more) 'young' or 'old' (relative to participants' age)"),
                           tags$li("identify names marked for other stereotypes (e.g. race, country of origin, socio-economic class)"),
                           tags$li("gather this information by region and by age, as these stereotypes can vary ",
                                   "internationally and generationally")
                           ),
                         p("Development of Name Explorer is ongoing, and (with luck) many more features and resources should be rolled out in the coming months and years."),
                         h3("Instructions"),
                         p("The \"Trends in Gender over Time\" tab contains a tool for exploring and navigating the data compiled from a large ",
                           "number of sources. Instructions for navigating this tool are provided within the tool itself."),
                         p("The \"References\" tab contains information on where the data for this tool was retrieved and what the database contains, ",
                           "as well as information about similar resources."),
                         h3("How to cite"),
                         p("If you use this tool for anything else, such as creative works, naming characters, naming babies, renaming yourself, ",
                           "or anything else, please drop me a line to let me know at ", 
                           tags$a(href="mailto:lauren.ackerman@newcastle.ac.uk?subject=Name Explorer App",
                                  "lauren.ackerman@newcastle.ac.uk")
                           ),
                         p("If you use this tool in your research, please cite it as:"),
                         p("APA: ",
                           tags$pre("Ackerman, Lauren. (2023). Name Explorer App (v0.2) [Shiny Web App].\nAvailable: https://lmackerman.shinyapps.io/Names_Explorer_App/"
                                    )),
                         p("BibTeX: ",
                           pre("@misc{ackerman_name_2023,\ntitle = {{Name Explorer App} (v0.2)},\nurl = {https://lmackerman.shinyapps.io/Names_Explorer_App/},\nauthor = {Ackerman, Lauren},\nyear = {2023},\n}")
                           ),
                         h4("Support"),
                         p("This project is based at Newcastle University and the Newcastle University Experimental Linguistics Lab ",
                           tags$a(href="https://www.staff.ncl.ac.uk/linglab/","(LingLab)"),
                           "."))#,
                             #mainPanel(width = 12,
                             #          img(src="https://www.staff.ncl.ac.uk/linglab/files/2023/11/%E2%80%8Ebanner-bg.png", 
                             #              width="100%", align="center")
                             #          )
               )
             ),
    #### end of splash page ####
    ##### GENDER OVER TIME ####
    tabPanel("Trends in Gender over Time",
    ###### Sidebar #####
      sidebarLayout(
        sidebarPanel(h3("Name picker"),
                     p("Please select which name to view. You can narrow your choices by changing the parameters below."),
                     pickerInput(inputId = 'name_picker',
                                 label = 'Which name?',
                                 choices = unisex_names |> pull(name) |> unique() |> sort(),
                                 options = list(`style` = "btn-info")),
                     hr(),
                     p("Since there are so many names, we can narrow down the options based on how many babies were given the name and/or how many years the name is attested."),
                     sliderInput(inputId = 'minimum_years',
                                 label = 'Minimum number of years where the name is attested',
                                 value = 50,
                                 min = 2, max = max(unisex_names$year_count)),
                     sliderInput(inputId = 'minimum_observations',
                                 label = 'Minumum number of annual name assignments',
                                 value = 400,
                                 #step = 10,
                                 min = 0, max = 500),
                     checkboxGroupInput(inputId = "which_regions",
                                        label = "Select names from only these regions",
                                        choices = c("Australia","Canada","England and Wales","Ireland",
                                                    "Northern Ireland","New Zealand","Scotland","USA"),
                                        selected = c("Australia","Canada","England and Wales","Ireland",
                                                     "Northern Ireland","New Zealand","Scotland","USA"),
                                        inline = TRUE),
                     checkboxGroupInput(inputId = "exclude_names",
                                        label = "Exclude \"default\" baby names?",
                                        choices = c("Baby","Infant","Unnamed","Unknown"),
                                        selected = c("Baby","Infant","Unnamed","Unknown"),
                                        inline = TRUE)
        ), # sidebar panel
        
        ###### main panel ######
        mainPanel(
          h4("Learn about how names are used across time and space in predominantly English-speaking countries"),
          
          div(class = "pull-right",
            dropdown( # https://dreamrs.github.io/shinyWidgets/reference/actionBttn.html
            tags$h3("Visual settings"),
            sliderInput(inputId = "user_defined_plot_width",
                        label = "Size of the plot in your browser window",
                        value = .85,
                        min = 0, max = 1, #step = .01, 
                        animate = FALSE),
            
            pickerInput(inputId = 'line_color_palette',
                        label = 'Color palette',
                        choices = c("viridis","magma","plasma","inferno","cividis","mako","rocket","turbo"),
                        selected = "turbo",
                        options = list(`style` = "btn-default")),
            
            sliderInput(inputId = 'year_overlaps',
                        label = 'Number of permitted overlapping labels',
                        value = 10,
                        min = 0, max = 30),
            
            style = "bordered", # simple, bordered, minimal, stretch, jelly, gradient, fill, material-circle, material-flat, pill, float, unite
            icon = icon("gear"),
            size = "xs", # xs,sm, md, lg
            status = "primary", # default, primary, warning, danger, success, royal
            width = "350px",
            right = TRUE,
            tooltip = tooltipOptions(placement = "bottom", title = "Adjust visual properties of the graph", html = FALSE),
            checkboxInput(inputId = "xy_line", label = "Show the `x=y` line", value = TRUE),
            checkboxInput(inputId = "axis_scaling", label = "Scale axes logarithmically", value = TRUE),
            checkboxInput(inputId = "mirror_aspect_ratio", label = "Equi-balanced view", value = TRUE)
            )
           ),
          
          plotOutput("names_plot", height = "auto", width = "auto"),
          hr(),
          h4("Options for modifying the content of the graph"),
          checkboxInput(inputId = "exclude_zeros",
                        label = "Exclude years where either AMAB and AFAB counts are zero?",
                        value = TRUE, width = "75%"),
          checkboxGroupInput(inputId = "switch_regions",
                             label = "Show or hide data from these regions",
                             choices = c("Australia","Canada","England and Wales","Ireland",
                                         "Northern Ireland","New Zealand","Scotland","USA"),
                             selected = c("Australia","Canada","England and Wales","Ireland",
                                          "Northern Ireland","New Zealand","Scotland","USA"),
                             inline = TRUE, width = "75%"),
          sliderInput(inputId = "which_years_range",
                      label = "Which years to plot",
                      min = min(unisex_names$year),
                      max = max(unisex_names$year),
                      value = c(min(unisex_names$year),max(unisex_names$year)),
                      width = "75%",
                      sep = "", 
                      animate = animationOptions(interval = 500, 
                                                 loop = FALSE, 
                                                 playButton = NULL,
                                                 pauseButton = NULL)),
          bsPopover(id = "which_years_range", 
                    title = "Animate your selection", 
                    content = "By selecting a shorter range and clicking the \"play\" button on the right of the scale on the right, you can animate the plot to show year on year changes.", 
                    placement = "top", 
                    trigger = "hover",
                    options = NULL)
        ) # main panel
      ) # sidebar layout
    ), # tab panel: trends in gender
    #### end of trends in gender code ####
    ##### REFERENCES ####
    tabPanel("References", 
             h3("Similar resources"),
             p("While there are many projects around collating and exploring baby name data, ",
               "there does not seem to be another one that looks at frequency (popularity) over time and ",
               "country for all the countries I'm interested in."),
             tags$ul(
               tags$li(tags$a(href="https://opendata.stackexchange.com/questions/46/multinational-list-of-popular-first-names-and-surnames","Multinational (huge, but no time dimension)")),
               tags$li(tags$a(href="https://cran.r-project.org/web/packages/babynames/index.html","CRAN `babynames` package (only USA until 2017)")),
               tags$li(tags$a(href="https://github.com/robjhyndman/ozbabynames","Australian data `ozbabynames` package (last updated 5+ years ago)"),
                       tags$ul(tags$li(tags$a(href="https://www.monicaalexander.com/posts/2019-20-01-babynames/","Another attempt to analyze this dataset")))),
               tags$li(tags$a(href="https://damegender.davidam.com/","DAMEgender gender estimator (no years)")),#"([suspicious views on gender](https://github.com/davidam/damegender/blob/dev/faq.org))"
               tags$li(tags$a(href="https://github.com/philipperemy/name-dataset","First and Last Names Database (no years or counts, just rank)")),
               tags$li(tags$a(href="https://github.com/sigpwned/popular-names-by-country-dataset","Popular Names by Country Dataset (excellent but fewer than 3k forenames)"))
             ),
             p("There are some good and fairly comprehensive ones out there, but many of them don't document their gathering and wrangling processes or are designed for different purposes."),
             h3("Data sources"),
             h4("Australia"),
             p("Due to the historical status of the states and territories of Australia as independent colonies, each records and collates its data separately. However, there is not a clear ",
               "pattern for how patterns of indigenous languages or immigration routes distinctly influence name trends in each subregion. Additionally, the relatively small populations lend ",
               "themselves to analysis in aggregate. Therefore, this database combines the data from these states and territories into one national database."),
             tags$ul(tags$li("Australian Capital Territory: (does not report sufficient name data for analysis)"),
                     tags$li(tags$a(href="https://data.nsw.gov.au/data/dataset/popular-baby-names-from-1952","New South Wales")),
                     tags$li(tags$a(href="https://nt.gov.au/law/bdm/popular-baby-names","Northern Territory")),
                     tags$li(tags$a(href="https://www.data.qld.gov.au/dataset/top-100-baby-names","Queensland")),
                     tags$li(tags$a(href="https://data.sa.gov.au/data/dataset/popular-baby-names","South Australia")),
                     tags$li(tags$a(href="https://data.gov.au/data/organization/department-of-justice-tasmania?q=names&sort=score+desc%2C+metadata_modified+desc","Tasmania")),
                     tags$li(tags$a(href="https://discover.data.vic.gov.au/dataset/popular-baby-names-victoria-api","Victoria")),
                     tags$li(tags$a(href="https://bdm.justice.wa.gov.au/_apps/BabyNames/Default.aspx","Western Australia")),
                     tags$li("Birth data for proportions: "),
                     tags$ul(tags$li(tags$a(href="https://explore.data.abs.gov.au/vis?tm=births&pg=0&df[ds]=ABS_ABS_TOPICS&df[id]=BIRTHS_SUMMARY&df[ag]=ABS&df[vs]=1.0.0&hc[Measure]=Births&pd=1975%2C&dq=5%2B4%2B1..A&ly[cl]=TIME_PERIOD",
                                            "National birth data from 1975 to 2022, with assigned sex at birth")),
                             tags$li(tags$a(href="https://www.abs.gov.au/statistics/people/population/births-australia/latest-release",
                                            "National birth data since 1934 (but not by ASAB)"))
                             )),
             h4("Canada"),
             tags$ul(tags$li(tags$a(href="https://www150.statcan.gc.ca/n1/pub/71-607-x/71-607-x2023021-eng.htm","Baby Names Observatory; StatCan data")),
                     tags$li("Downloaded from:", tags$a(href="https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710014701",
                                                        "https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710014701)"))),
             h4("Ireland"),
             tags$ul(tags$li("Downloaded from :", tags$a(href="https://data.cso.ie/","https://data.cso.ie/"),
                             tags$ol(tags$li("search \"name\" >"),
                                     tags$li("select \"Girls/Boys Names in Ireland with 3 or More Occurrences\""),
                                     tags$li("select \"Statistic (all)\""),
                                     tags$li("select \"Year (all)\" and download file")))),
             h4("New Zealand"),
             tags$ul(tags$li("Top 100 names since 1954: ",tags$a(href="https://smartstart.services.govt.nz/news/baby-names","https://smartstart.services.govt.nz/news/baby-names"))),
             h4("United Kingdom"),
             p("Scotland and Northern Ireland governments are partially devolved, and as such, data for these three regions is collated separately. ",
               "The choice to keep these regions separate in this database was made to respect the linguistic differences and historical trends in names in these regions. ",
               "Specifically, Scotland has a high prevalence of etymologically Scots Gaelic names, Northern Ireland has a high prevalence of etymologically Irish names, ",
               "and England and Wales have a high prevalence of etymologically Welsh (and of course, English) names."),
             h5("England and Wales"),
             tags$ul(tags$li(tags$a(href="https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/livebirths/datasets/babynamesinenglandandwalesfrom1996","Baby names for England and Wales; Office of National Statistics data"))),
             h5("Northern Ireland"),
             tags$ul(tags$li(tags$a(href="https://www.nisra.gov.uk/publications/data-baby-names-dashboard","Baby Name Dashboard; NISRA data"))),
             h5("Scotland"),
             tags$ul(tags$li(tags$a(href="https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/vital-events/names/babies-first-names/babies-first-names-2022","National Records of Scotland: Full list 1974-2022"))),
             h4("USA"),
             tags$ul(tags$li(tags$a(href="https://www.ssa.gov/OACT/babynames/limits.html","Beyond the Top 1000 Names; Social Security Administration data")),
                     tags$li("Rank info compiled from: ",tags$a(href="https://www.ssa.gov/OACT/babynames/index.html","Popular Baby Names (by Birth Year; Number)")),
                     tags$ul(tags$li("Ranks are eventually recalculated using the same method as used by the SSA (`ties.method = \"first\"`).")),
                     tags$li("Proportion info compiled from: ",tags$a(href="https://www.ssa.gov/OACT/babynames/numberUSbirths.html","SSN Holders"),
                             tags$ul(tags$li("Proportions are estimates based on SSN registrations in a given year and number of babies registered under a given name."))))
             ), # tab panel: references
    #### end of references text ####
  )
)

#### Define server logic #### 
server <- function(input, output, session) {
  observe(updatePickerInput(session, 
                            inputId = "name_picker", 
                            choices = unisex_names |>
                              filter(number_AFAB >= input$minimum_observations,
                                     number_AMAB >= input$minimum_observations,
                                     region %in% input$which_regions,
                                     !name %in% input$exclude_names,
                                     year_count >= input$minimum_years) |> 
                              pull(name) |> unique() |> sort()
                            )
          )
  
  output$names_plot <- renderPlot({
    line_color_palette <- viridis(n = 8, #unisex_names |> pull(region) |> unique() |> length(), 
                                  alpha = 1, begin = .95, end = .1, direction = 1, 
                                  option = input$line_color_palette)
    xy_line_color <- ifelse(input$xy_line, "black", NA)
    
    # names plot
    unisex_names |> 
      filter(case_when(input$exclude_zeros ~ (number_AMAB & number_AFAB) > 0,
                       TRUE ~ (number_AMAB & number_AFAB) >= 0),
             region %in% input$switch_regions,
             name == input$name_picker,
             year >= input$which_years_range[1] & year <= input$which_years_range[2]) |> 
      ggplot(aes(x = proportion_AMAB,
                 y = proportion_AFAB)) +
      geom_abline(slope = 1, intercept = 0, color = xy_line_color) +
      {if (input$mirror_aspect_ratio) geom_point(aes(x = proportion_AFAB, y = proportion_AMAB), color = NA)} +
      geom_path(aes(colour = region,
                    group = region),
                na.rm = TRUE) +
      geom_text_repel(aes(label = year), 
                      verbose = FALSE,
                      #size = 3, 
                      color = "black", 
                      alpha = .5,
                      max.overlaps = input$year_overlaps) +
      {if (input$axis_scaling) scale_y_log10(labels = scales::percent_format())} + 
      {if (input$axis_scaling) scale_x_log10(labels = scales::percent_format())} +
      {if (!input$axis_scaling) scale_y_continuous(labels = scales::percent_format())} + 
      {if (!input$axis_scaling) scale_x_continuous(labels = scales::percent_format())} +
      scale_color_manual(values = c("Australia" = line_color_palette[1],
                                    "Canada" = line_color_palette[2],
                                    "England and Wales" = line_color_palette[3],
                                    "Ireland" = line_color_palette[4],
                                    "Northern Ireland" = line_color_palette[5],
                                    "New Zealand" = line_color_palette[6],
                                    "Scotland" = line_color_palette[7],
                                    "USA" = line_color_palette[8])) +
      ggtitle(paste0("Evolution of \"",as.character(input$name_picker),"\" over time"),
              #subtitle = paste(session$clientData$output_names_plot_width,",",
              #                 session$clientData$output_names_plot_height," : ",
              #                 input$user_defined_plot_width)
              ) +
      xlab("proportion of AMAB babies assigned given name each year") +
      ylab("proportion of AFAB babies assigned given name each year") +
      NULL
    }, 
    height = function() {session$clientData$output_names_plot_width * (input$user_defined_plot_width * 0.6 + 0.4) * 0.8},
    width  = function() {session$clientData$output_names_plot_width * (input$user_defined_plot_width * 0.6 + 0.4)}
  )
  
  
}

#### Run the application #### 
shinyApp(ui = ui, server = server)
#})