library(shiny)
library(DT)
library(readxl)
library(MORE)
library(visNetwork)
library(clusterProfiler)
source("helpers.R")

options(shiny.maxRequestSize = 300*1024^2)

read_file <- function(file_path) {
  ext <- tools::file_ext(file_path)
  if (ext == "csv") {
    read.csv(file_path, row.names = 1)
  } else if (ext == "txt") {
    read.table(file_path, header = TRUE, sep = "\t", row.names = 1)
  } else if (ext == "xlsx") {
    read_excel(file_path)
  } else {
    NULL
  }
}

ui <- fluidPage(
  
  
  tags$head(tags$style(HTML("
      .header-home-image {
        display: flex;
        justify-content: center;
        align-items: flex-start;
        position: relative;
        height: 50px;
        padding-top: 0px;
      }
        
      .header-home-image img.more1 {
        width: 35%;
        max-width: 300px;
        top: 10px;
        margin-top:50px;
      }
        
      .header-home-image img.biostatomics1 {
        position: absolute;
        top: 10px;
        right: 10px;
        width: 150px;
        height: auto;
      }
      
      .header-home-image img.logoUPV1 {
        position: absolute;
        top: 10px;
        left: 10px;
        width: 200px;
        height: auto;
      }
      
      .header-Download-Example-Dataset-image {
        display: flex;
        justify-content: space-between;
        align-items: center;
        position: relative;
        height: 50px;
        padding-top: 80px;
      }
      
      .title{
        flex-grow: 1;
        text-align: center;
      }
      
      .images-right{
      display: flex;
      }
      
      .header-Download-Example-Dataset-image img.more2 {
        width: 180px;
        height: auto;
      }
      
      .header-Download-Example-Dataset-image img.biostatomics2 {
        width: 150px;
        height: auto;
      }
      
      .header-Download-Example-Dataset-image .title {
        margin: 0 auto;
        font-size: 24px;
        font-weight: bold;
        text-align: center;
        color: #003366;
        font-family: 'Arial', sans-serif;
      }
      
      .button-container {
            display: flex;
            justify-content: space-around;
            margin-top: 12px;
            
      }
      .button-container button {
        width: 200px;
        height: 50px;
        font-size: 16px;
        background-color: #007bff;
        color: white;
        border: none;
        border-radius: 5px;
      }
      .button-container button:hover {
        background-color: #0056b3;
      }
      .text-section {
            text-align: justify;
            font-size: 18px;
            margin-top: 200px;
            padding: 10px;
            color: #333;
      }
      
      .text-section-2 {
            text-align: justify;
            font-size: 18px;
            margin-top: 50px;
            padding: 10px;
            color: #333;
      }
      
      .text-section-3 {
            text-align: justify;
            font-size: 18px;
            margin-top: 5px;
            padding: 10px;
            color: #333;
      }
      
      .download-example-dataset-section {
        display: flex;
        justify-content: space-around;
        align-items: center;
        margin-top: 40px;
      }
      
      .download-item {
        text-align: center;
        width: 200px;
      }
      
      .download-item img {
        width: 100px;
        height: 100px;
        margin-bottom: 10px;
      }
      
      .download-item .shiny-download-link {
        display: block;
        width: 100%;
        padding: 10px;
        background-color: #007bff;
        color: white;
        text-align: center;
        border-radius: 5px;
        text-decoration: none;
      }
      
      .download-item .shiny-download-link:hover {
        background-color: #0056b3;
      }
      
      .text-section-download {
            text-align: justify;
            font-size: 18px;
            margin-top: 100px;
            padding: 10px;
            color: #333;
      }
      
      .Target-Data-Section {
            text-align: justify;
            font-size: 18px;
            margin-top: 100px;
            padding: 10px;
            color: #333;
      }
      
      .title-2-box {
        border: 1px solid #ccc;
        padding: 10px;
        background-color: #98A6D4;
        margin-top: 10px;
        margin-bottom: 20px;
        text-align: center;
      }
    
      .title-2 {
        font-size: 20px;
        font-weight: bold;
        margin-top: 10px;
        margin-bottom: 0px;
        color: #003366;
      }
      
      .description-box {
        border: 1px solid #ccc;
        padding: 5px;
        background-color: #CCCCCC;
        margin-top: 0px;
        margin-bottom: 20px;
        text-align: justify;
      }
      .text-section-example {
        text-align: justify;
        font-size: 20; /* Tamaño de fuente */
        color: #564D80; /* Color del texto */
      }
      .next-button-container {
      display: flex;
      justify-content: flex-end; 
      margin-top: 20px;
      }
      .next-button {
        width: 300px;
        height: 50px;
        font-size: 16px;
        background-color: #007bff;
        color: white;
        border: none;
        border-radius: 5px;
      }
      .next-button:hover {
        background-color: #0056b3;
      }
      .regulatory-table {
            width: 100%;
            border-collapse: collapse;
          }
          .regulatory-table th, .regulatory-table td {
            padding: 10px;
            border: 1px solid #ddd;
            text-align: left;
          }
          .regulatory-table th {
            background-color: #f2f2f2;
          }
          .regulatory-table td a {
            color: blue;
            text-decoration: underline;
            cursor: pointer;
          }
          .regulatory-table td {
            padding-left: 50px;
          }
          .regulator-name-box {
            font-size: 1.2em; /* Tamaño de fuente más grande */
            font-weight: bold; /* Negrita */
            color: #003366; /* Color del texto */
            margin-top: 10px; /* Espacio superior */
            margin-bottom: 10px; /* Espacio inferior */
            background-color: #f0f0f0; /* Color de fondo */
            padding: 10px; /* Espaciado interno */
            border-radius: 5px; /* Bordes redondeados */
            text-align: left; /* Alinear el texto a la izquierda */
            width: fit-content; /* Ajustar el ancho al contenido */
          }
        .regulator-name-box {
    font-size: 1.2em; /* Tamaño de fuente más grande */
    font-weight: bold; /* Negrita */
    color: #003366; /* Color del texto */
    margin-top: 10px; /* Espacio superior */
    margin-bottom: 10px; /* Espacio inferior */
    background-color: #f0f0f0; /* Color de fondo */
    padding: 10px; /* Espaciado interno */
    border-radius: 5px; /* Bordes redondeados */
    text-align: left; /* Alinear el texto a la izquierda */
    width: fit-content; /* Ajustar el ancho al contenido */
}

.title-box {
    font-size: 1em; /* Tamaño de fuente más pequeño */
    font-weight: bold; /* Negrita */
    color: #003366; /* Color del texto */
    margin-top: 10px; /* Espacio superior */
    margin-bottom: 10px; /* Espacio inferior */
    background-color: #ccffcc; /* Color de fondo verde clarito */
    padding: 10px; /* Espaciado interno */
    border-radius: 5px; /* Bordes redondeados */
    text-align: left; /* Alinear el texto a la izquierda */
    width: fit-content; /* Ajustar el ancho al contenido */
}
    .filter-section {
        background-color: #f9f9f9;
        border: 1px solid #ddd;
        padding: 20px;
        border-radius: 10px;
        margin-bottom: 20px;
      }
      .filter-title {
        font-family: 'Arial', sans-serif;
        font-size: 20px;
        color: #333;
        font-weight: bold;
        margin-bottom: 10px;
      }
      .filter-input {
        margin-bottom: 15px;
      }
      
      .start-button {
        background-color: #FFA500 !important; /* Color naranja */
        color: white !important;
        border: none;
        border-radius: 5px;
        width: 200px;
        height: 50px;
        font-size: 16px;
      }
      .start-button:hover {
        background-color: #CC8400 !important; /* Color naranja más oscuro */
      }
      
    "))),
  
  tabsetPanel(id = "tabset",
              tabPanel("Home", div(class = "header-home-image",
                                   img(src = "MORE.png", alt = "More Image", class = "more1"),
                                   img(src = "Biostatomics.jpg", alt = "Biostatomics Image", class = "biostatomics1"),
                                   img(src = "logoUPV.png", alt = "Logo UPV", class = "logoUPV1")),
                       div(class = "text-section",
                           p("MORE (Multi-Omics REgulation) is a tool to generate multi-omic regulatory networks for different 
                           phenotypes or experimental conditions from a dataset containing at least two omic modalities (target
                           omic and regulatory omic). MORE will estimate the networks from the data and help you to compare 
                           networks between phenotypes and interpret the regulatory mechanisms described in the networks.")),
                       
                       div(class = "button-container",
                           actionButton("download_data", "Download Example Data"),
                           actionButton("example_data", "Example Data"),
                           actionButton("start", "Start Using MORE", class = "start-button"),
                           actionButton("tutorial", "Tutorial")
                       ),
                       div(class = "text-section-2",
                           p("Remember! This Shiny app provides a simplified version of MORE methodology. 
                             For additional functionalities, customized options, and full analytical capabilities, 
                             check out the MORE R package on GitHub.")
                       ),
                       
                       tags$script(HTML("function goToPage() {
                                        window.location.href = https://github.com/BiostatOmics/MORE/}")),
                       div(class = "button-container",
                           
                           tags$a(href = "https://github.com/BiostatOmics/MORE/", 
                                  HTML('<img src="github.jpg" alt="Github Icon" style="width:150px;height:150px;">')))
                       
              ),
              
              tabPanel("Download Example Dataset", value= "Download Example Dataset", div(class = "header-Download-Example-Dataset-image",
                                                       img(src = "MORE.png", alt = "More Image", class = "more2"),
                                                       span(class = "title", "Download Example Dataset"),
                                                       img(src = "Biostatomics.jpg", alt = "Biostatomics Image", class = "biostatomics2")),
                       div(class= "text-section-download", p("Select the format to download the example data:")),
                       div(class = "download-example-dataset-section",
                           div(class = "download-item",
                               img(src = "excel.png"),
                               downloadButton("download_excel", "Download Excel")),
                           
                           div(class = "download-item", 
                               img(src = "csv.png"),
                               downloadButton("download_csv", "Download CSV")),
                           
                           div(class = "download-item",
                               img(src = "txt.png"),
                               downloadButton("download_txt", "Download TXT"))
                       )),
              
              tabPanel("Run MORE with Example Dataset", value = "Run MORE with Example Dataset", div(class = "header-Download-Example-Dataset-image",
                                                            img(src = "MORE.png", alt = "More Image", class = "more2"),
                                                            span(class = "title", "Run MORE with Example Dataset"),
                                                            img(src = "Biostatomics.jpg", alt = "Biostatomics Image", class = "biostatomics2")),
                       div(class = "Target-Data-Section",
                           div(class = "title-2-box",
                               span(class = "title-2", "Target Data")),
                           div(span(class = "text-section-example", 
                                    div(class = "description-box", 
                                        span(class ="text-section-example", "Target Data are the expression values 
                                        for features of a target omic (e.g., genes of gene expression data). 
                                        The features of the target omic (e.g., genes) should be in rows, and the condition which
                                        they have been measured (e.g., patients) should be in columns.")),
                                    DTOutput("table_target_data"))),
                           div(class = "regulatory-data-section", 
                               div(class = "title-2-box",
                                   span(class = "title-2", "Regulatory Data & Associations")),
                               div(span(class = "text-section-example", 
                                        div(class = "description-box", 
                                            span(class = "description-box",
                                                 "The object regulatory contains the data for each “regulatory” omic with a 
                                                   structure similar to target data: regulators in rows and conditions in columns. \n
                                                   In this example we have 6 different regulatory omics.")),
                                        div(span(class = "text-section-example", 
                                                 div(class = "description-box", 
                                                     span(class = "description-box",
                                                          p("The association object contains the data for the potential interactions
                                                            between target omic features and regulators for that omic. 
                                                            The data frame should include 2 columns (optionally 3 columns)."),
                                                          p("The first column must contain the regulators, the second the target features IDs, and an additional column 
                                                            can be added to describe the type of interaction (for example, in methylation data,
                                                            if a CpG site is located in the promoter region of the gene, in the first exon, etc.)."),
                                                          p("Optionally, the user can not use the associations data frame of an omic if they want to consider all the 
                                                            regulators of that omic as potential regulators for all the target features. They can even choose to not use any associations
                                                            if they want to consider all regulators of all omics in regulatoryData as potential regulators to 
                                                            all target features. In this example, only 3 of the regulatory omics have associations.")))),
                                            
                                            tableOutput("regulatory_associations_table")))),
                               uiOutput("regulatory_tables")),
                           
                           div(class = "Condition-Section",
                               div(class = "title-2-box",
                                   span(class = "title-2", "Condition")),
                               div(span(class = "text-section-example", 
                                        div(class = "description-box", 
                                            span(class ="text-section-example", 
                                            "The object Condition represent the conditions of the samples, such as treatments, 
                                                                  diseases, strains, dose of a drug, etc. 
                                                                  
                                              The rows of the object must be labeled exactly as in Target Data and they should be in the
                                              same order as well.")),
                                        DTOutput("table_condition"))))
                       ),
                       tabsetPanel(id = "sub_tabs", tabPanel(value = "data_format", "Check data format", 
                                            div(),
                                            div(class = "text-section-3",
                                                p("On this page, you can verify that the uploaded data has the correct format.
                                                    A message will inform you whether the data format is valid or not. Once 
                                                    the data is confirmed to be correct, you can proceed to specify whether your omics dataset 
                                                    contains binary or numerical values.
                                                    In addition to defining the data type, you can also set a threshold to filter out 
                                                  low-variation regulators. For numerical regulators, this is the minimum change in standard 
                                                  deviation required across conditions. For binary regulators, it's the minimum change in 
                                                  proportion across conditions. Regulators that do not meet these thresholds will be excluded 
                                                  from the regression models.")),
                                            div(),
                                            div(class = "title-2-box", span(class = "title-2", "Step 1: Verify the data format.")),
                                            div(),
                                            div(class = "text-section-3", 
                                                p("Click the button to check whether your data has the correct format.")),
                                            div(class = "button-container", actionButton("validate_data_example", "Check data format")),
                                            div(),
                                            div(class = "text-section-3", p("If the data is incorrect, a message will appear here explaining the issue.
                                                                              If the data is correct, a temporary confirmation message will appear 
                                                                            in the bottom-right corner of the screen.")),
                                            textOutput("error_message"),
                                            div(),
                                            div(class = "title-2-box", span(class = "title-2", "Step 2: Select the data configuration.")),
                                            div(class = "text-section-3",
                                            p("First, specify whether each regulatory omic contains numeric or binary data. 
                                            You can do this using the selection menu located under each omic.
                                            Next, set the minimum change in standard deviation for each omic. This value determines 
                                            the threshold for filtering low-variation regulators.
                                            If you choose not to set a value, a default of 0 will be applied.
                                            Remember to click the Save button after selecting all the values.")),
                                            div(class = "button-container", actionButton("select_config_example", "Select Configuration")),
                                            div(),
                                            div(class = "title-2-box", span(class = "title-2", "Step 3: Proceed to model preparation.")),
                                            div(class = "text-section-3", p("Click the button to go to the model configuration page.")),
                                            div(),
                                            div(class = "button-container", actionButton("prepare_model_example", "Prepare Your Model"))),
                                   tabPanel(value = "prepare_model","Prepare your model",
                                            div(class = "text-section-3", 
                                                p("On this page, you will select the model you want to use along with its fitting parameters.
                                                  Once the configuration is complete, you can proceed to run MORE. 
                                                  The model will then execute and generate the results.")),
                                            div(class = "title-2-box", span(class = "title-2", "Step 1: Choose the method.")),
                                            div(),
                                            div(class = "text-section-3", 
                                                p("Select one of the available predictive modeling options: MLR 
                                                  (Multiple Linear Regression) or PLS (Partial Least Squares).")),
                                            div(class = "text-section-3", 
                                                p("MLR is a simple and traditional approach that tries to find a straight-line
                                                  relationship between a group of input variables and the outcome you're trying 
                                                  to predict. It works best when your input data is not too complex and the 
                                                  variables are not too closely related to each other.")),
                                            div(class = "text-section-3", 
                                                p("In contrast, PLS is a more advanced method designed for situations with
                                                  many input variables, especially when they are correlated or when there 
                                                  are more variables than samples. PLS reduces data complexity by creating 
                                                  new, combined variables that capture the most important information for prediction. 
                                                  One of the key advantages of PLS is that it can handle missing values, making 
                                                  it particularly useful for real-world biological or multi-omics datasets where 
                                                  complete data is often not available")),
                                            selectInput("method_choice", "Method:", choices = c("PLS1", "MLR"), selected = "PLS1"),
                                            conditionalPanel( # Inputs condicionales solo si se elige PLS1
                                              condition = "input.method_choice == 'PLS1'",
                                              div(class = "title-2-box", span(class = "title-2", "Step 1.1: Choose the parameters.")),
                                              div(),
                                              div(class = "text-section-3", 
                                                  p("If you selected the PLS model, you will now need to choose two parameters.")),
                                              div(class = "text-section-3",
                                                  p("The first is alfa, which is the significance level used to decide whether a 
                                                    regulator is considered important in the PLS model. By default, this value is 
                                                    set to 0.05.")),
                                              numericInput("alfa", "Alpha value:", value = 0.05, min = 0, max = 1, step = 0.01),
                                              div(class = "text-section-3", 
                                                  p("The second is VIP (Variable Importance in Projection), which is 
                                                    a score that reflects how much each variable contributes to the prediction. 
                                                    A variable is considered significant only if it meets both the alfa and VIP 
                                                    thresholds. By default, the VIP threshold is 0.8.")),
                                              numericInput("vip", "VIP value:", value = 0.8, min = 0, step = 0.1)),
                                            div(),
                                            div(class = "title-2-box", span(class = "title-2", "Step 2: Run MORE.")),
                                            div(class = "text-section-3",
                                                p("Click the button to run the model.")),
                                            div(class = "button-container", actionButton("runMore", "RUN MORE"))),
                                   tabPanel("Regulation Per Condition",
                                            div(class = "text-section-3",
                                                p("On this page, a function called RegulationPerCondition will be applied. It 
                                                  returns a summary table containing all the relevant or significant regulations, 
                                                  that is, all target feature–regulator pairs identified as important by the 
                                                  MORE models (depending on whether an MLR or a PLS model was applied). Additionally, 
                                                  it provides the regression coefficient that links each target feature with its 
                                                  corresponding regulator, evaluated separately for each experimental condition. This 
                                                  coefficient is tested to determine whether it is statistically relevant or 
                                                  significant.")),
                                            div(),
                                            div(class = "title-2-box", span(class = "title-2", "Step 1: Run Regulation Per Condition.")),
                                            div(),
                                            div(class = "text-section-3",
                                                p("Click the button to run Regulation Per Condition.")),
                                            div(class = "button-container", actionButton("runRegulation", "Regulation Per Condition")),
                                            div(class = "text-section-3",
                                                p("You can use the following table to search specific features")),
                                            DTOutput("table_example"),
                                            plotOutput("summaryPlot_example"),
                                            div(),
                                            div(class = "title-2-box", span(class = "title-2", "Step 2: Explore feature-regulator relationships.")),
                                            div(class = "text-section-3", 
                                                p("MORE also provides functions for visualization to explore gene-regulator relationships for the 
                                                pairs identified as significant by MORE.")),
                                            div(class = "text-section-3",
                                                p("When no specific regulator is provided, a plot is generated for each feature, displaying all 
                                                  its significant regulators for the feature, together with the feautre’s expression pattern.")),
                                            div(class = "text-section-3",
                                                p("If a regulator is specified but no feature is provided, MORE will generate plots for all feature–regulator 
                                                  pairs in which that regulator was identified as significant.")),
                                            div(class = "text-section-3",
                                                p("Use the following selectors to choose the feature and regulator. If you do not want to provide a specific feature
                                                  or regulator, please select NULL.")),
                                            div(style = "display: flex; justify-content: center; align-items: center; gap: 120px;",
                                                fluidRow(column(6, selectInput("selected_target", "Select Target Feature:", choices = NULL)),
                                                        column(6, selectInput("selected_regulator", "Select Regulator:", choices = NULL)))),
                                            br(),
                                            plotOutput("plot_more_output_example"),
                                            div(class = "title-2-box", span(class = "title-2", "Step 3: Filter the better features modeled by MORE.")),
                                            div(),
                                            div(class = "text-section-3",
                                                p("After completing this initial analysis, if you want to focus on the regulatory networks of a 
                                                  smaller group of features that were better modeled by MORE, you can use the following filter. 
                                                  This helps by filtering out target features whose model performance, 
                                                  measured by R² (a value that indicates how well the model fits the data), is below a
                                                  threshold you specify. In other words, it keeps only those genes for which the MORE 
                                                  model explains a sufficient amount of variation, allowing you to concentrate on the most 
                                                  reliable results.")),
                                            fluidRow(
                                              column(width = 12,
                                                     div(style = "display: flex; justify-content: center; align-items: center; gap: 120px;",
                                                         numericInput("R2", "Select R2", value = 0.5, min = 0, max = 1, step = 0.01),
                                                         div(class = "button-container", actionButton("updatePlot_example", "Update Plot with R2")),
                                                         div(class = "button-container", actionButton("runRegulationWithR2_example", "Filter the model"))))),
                                            div(),
                                            div(class = "text-section-3",
                                                p("You can view the updated results in the table and plots located just above."))
                                            
                                                            ),
                                   tabPanel("MORENetwork",
                                            div(class = "text-section-3", 
                                                p("On this page you can visualize the network generated. To effectively configure the network 
                                                visualization, you will need to specify certain parameters within the filters. ")),
                                            div(class = "text-section-3", 
                                                p("First, you can define a reference group 
                                                  using the Group 1 filter. Providing a group name here establishes a baseline 
                                                  for creating differential networks. If this field is left empty, the system will, 
                                                  by default, plot networks for all available groups. However, if a group is 
                                                  specified for Group 1 and Group 2 remains blank, only the network specific to 
                                                  the chosen Group 1 will be generated.")),
                                            div(class = "text-section-3", 
                                                p("Secondly, you have the option to specify a comparison group via the Group 2
                                                  filter. This group will then be compared against the reference group designated in Group 1
                                                  for the creation of differential networks. Similar to Group 1, if Group 2 is not provided 
                                                  (and Group 1 is also left blank), the networks for all groups will be plotted. 
                                                  Both Group 1 and Group 2 are set to null by default, meaning no specific groups are pre-selected.")),
                                            div(class = "text-section-3", 
                                                p("Finally, you must set the percentile. This crucial filter allows the user to display only 
                                                  the most significant regulatory connections. By entering a value for percentile only those regulations 
                                                  that fall within the specified top percentage, indicating their strongest influence on regulators, 
                                                  will be included in the plot.")),
                                            div(class = "text-section-3", 
                                                p("Optionally, you can provide an annotation matrix in order to make use the pathway parameter.
                                                  In that parameter you must write the term of the biologic pathway you are interested to plot.")),
                                            div(),
                                            fluidRow(
                                              column(width = 12,
                                                     div(style = "display: flex; justify-content: center; align-items: center; gap: 120px;",
                                                         selectInput("Group1", "Select Group 1", choices = NULL),
                                                         selectInput("Group2", "Select Group 2", choices = NULL),
                                                         numericInput("pc", "Percentile", value = 0.9, min = 0, max = 1, step = 0.01),
                                                         fileInput("annot_data_example", "Annotation", accept = c(".csv", ".txt", ".xlsx")),
                                                         selectInput("pathway_example", "Select the pathway term", choices = NULL)))),
                                            div(class = "button-container", actionButton("runNetwork_example", "Ejecutar networkMORE")),
                                            div(),
                                            visNetworkOutput("networkPlot_example", height = "500px")),
                                   tabPanel("Over Representation Analysis", 
                                            div(class = "text-section-3", 
                                                p("On this page, you can carry out the Over Representation Analysis. Our implementation offers flexibility 
                                                  in how you define your target gene set.")),
                                            div(class = "text-section-3", 
                                                p("You can either use genes identified as hub target 
                                                  features, which represent highly connected nodes in your network, or opt for target features 
                                                  that are regulated by what we define as global regulators under specific conditions. 
                                                  This dual approach ensures you can tailor the ORA to best fit your specific 
                                                  research questions")),
                                            div(class = "text-section-3", 
                                                p("You can also select for which group do you want to carry out the ORA. And if you choose a  
                                                  regulatory omic it performs the ORA to the regulators of the specified omic ")),
                                            div(class ="text-section-3",
                                                p("Users have the flexibility to select their desired alpha level cutoff. This value determines the threshold 
                                                  for statistical significance, allowing you to control which results are considered meaningful.")),
                                            div(class = "text-section-3", p("In this analysis the annotation matrix is compulsory. Providing an accurate and comprehensive annotation is essential for 
                                                drawing meaningful biological conclusions from your analysis.")),
                                            fluidRow(
                                              column(width = 12,
                                                     div(style = "display: flex; justify-content: center; align-items: center; gap: 120px;",
                                                          fileInput("annot_data_example", "Annotation", accept = c(".csv", ".txt", ".xlsx")),
                                                          checkboxInput("ora_byHubs", "Group by Hubs (byHubs)", value = TRUE),
                                                          selectInput("ora_group", "Select Group", choices = NULL),
                                                          selectInput("ora_byOmic", "Select Omic Type", choices = NULL),
                                                          numericInput("ora_alpha", "Alpha level", value = 0.05, min = 0, max = 1, step = 0.01)))),
                                            div(class = "button-container", actionButton("runORA", "Run ORA")),
                                            DTOutput("ora_table")),
                                   tabPanel("Gene Set Enrichment Analysis",
                                            div(class = "text-section-3", 
                                                p("On this page you can carry out a Gene Set Enrichment Analysis. We center the
                                                  analysis of the differences in the number of regulators of the genes in the experimental
                                                  conditions.")),
                                            div(class = "text-section-3", 
                                                p("You can also select for which group do you want to carry out the ORA. Group 1 is compulsory,
                                                  Group 2, is optional. Choose NULL for Group 2 if you want focus solely on your primary 
                                                  study group. In this scenario, the results will describe the enrichment within that 
                                                  single group but will not highlight statistical differences between groups")),
                                            div(class ="text-section-3",
                                                p("Users have the flexibility to select their desired alpha level cutoff. This value determines the threshold 
                                                  for statistical significance, allowing you to control which results are considered meaningful.")),
                                            div(class = "text-section-3", 
                                                p("In this analysis the annotation matrix is compulsory. Providing an accurate and comprehensive annotation is essential for 
                                                  drawing meaningful biological conclusions from your analysis.")),
                                            fluidRow(
                                              column(width = 12,
                                                     div(style = "display: flex; justify-content: center; align-items: center; gap: 120px;",
                                                         fileInput("annot_data_example", "Annotation", accept = c(".csv", ".txt", ".xlsx")),
                                                         selectInput("gsea_group1", "Select Group", choices = NULL),
                                                         selectInput("gsea_group2", "Select Group", choices = NULL),
                                                         numericInput("gsea_alpha", "Alpha level", value = 0.05, min = 0, max = 1, step = 0.01)))),
                                            div(class = "button-container", actionButton("runGSEA", "Run GSEA")),
                                            plotOutput("gsea_plot", height = "700px")))),
              
              
              # RUN MORE WITH OWN DATASET
              tabPanel("Run MORE with Own Dataset", div(class = "header-Download-Example-Dataset-image",
                                                        img(src = "MORE.png", alt = "More Image", class = "more2"),
                                                        span(class = "title", "Run MORE with Own Dataset"),
                                                        img(src = "Biostatomics.jpg", alt = "Biostatomics Image", class = "biostatomics2")),
                       div(class = "Target-Data-Section",
                           div(class = "title-2-box",
                               span(class = "title-2", "Target Data")),
                           div(span(class = "text-section-example", 
                                    div(class = "description-box", 
                                        span(class ="text-section-example", "Target Data are the expression values 
                                        for features of a target omic (e.g., genes of gene expression data). 
                                        The features of the target omic (e.g., genes) should be in rows, and the condition which
                                        they have been measured (e.g., patients) should be in columns.")),
                                    fileInput("target_data", "Target Data", accept = c(".csv", ".txt", ".xlsx")))),
                           div(class = "regulatory-data-section", 
                               div(class = "title-2-box",
                                   span(class = "title-2", "Regulatory Data & Associations")),
                               div(span(class = "text-section-example", 
                                        div(class = "description-box", 
                                            span(class = "description-box",
                                                 "The object regulatory contains the data for each “regulatory” omic with a 
                                                   structure similar to target data: regulators in rows and conditions in columns. \n
                                                   In this example we have 6 different regulatory omics.")),
                                        div(span(class = "text-section-example", 
                                                 div(class = "description-box", 
                                                     span(class = "description-box",
                                                          p("The association object contains the data for the potential interactions
                                                            between target omic features and regulators for that omic. 
                                                            The data frame should include 2 columns (optionally 3 columns)."),
                                                          p("The first column must contain the regulators, the second the target features IDs, and an additional column 
                                                            can be added to describe the type of interaction (for example, in methylation data,
                                                            if a CpG site is located in the promoter region of the gene, in the first exon, etc.)."),
                                                          p("Optionally, the user can not use the associations data frame of an omic if they want to consider all the 
                                                            regulators of that omic as potential regulators for all the target features. They can even choose to not use any associations
                                                            if they want to consider all regulators of all omics in regulatoryData as potential regulators to 
                                                            all target features. In this example, only 3 of the regulatory omics have associations."))))),
                                        
                                        numericInput("num_omics", "Number of regulatory omics", value = 1, min = 1),
                                        uiOutput("omics_inputs")))),
                           
                           div(class = "Condition-Section",
                               div(class = "title-2-box",
                                   span(class = "title-2", "Condition")),
                               div(span(class = "text-section-example", 
                                        div(class = "description-box", 
                                            span(class ="text-section-example", 
                                                 "The object Condition represent the conditions of the samples, such as treatments, 
                                                                  diseases, strains, dose of a drug, etc. 
                                                                  
                                              The rows of the object must be labeled exactly as in Target Data and they should be in the
                                              same order as well.")),
                                        fileInput("condition_data", "Condition Data", accept = c(".csv", ".txt", ".xlsx"))))),
                           div(class = "Clinic-Section",
                               div(class = "title-2-box",
                                   span(class = "title-2", "Clinic")),
                               div(span(class = "text-section-example", 
                                        div(class = "description-box", 
                                            span(class ="text-section-example", 
                                                 "The object clinic represents the clinical variables values where rows represent samples
                                                 and columns the variables.The rows of the object must be labeled exactly as in Target 
                                                 Data and they should be in the
                                                same order as well. This dataset is completly optional. If no clinical data is needed just 
                                                 leave the input in blank.")),
                                        fileInput("clinic_data", "Clinic Data", accept = c(".csv", ".txt", ".xlsx")))))
                       ),
                       tabsetPanel(id = "sub_tabs_2", tabPanel(value = "data_format", "Check data format", 
                                                             div(),
                                                             div(class = "text-section-3",
                                                                 p("On this page, you can verify that the uploaded data has the correct format.
                                                    A message will inform you whether the data format is valid or not. Once 
                                                    the data is confirmed to be correct, you can proceed to specify whether your omics dataset 
                                                    contains binary or numerical values.
                                                    In addition to defining the data type, you can also set a threshold to filter out 
                                                  low-variation regulators. For numerical regulators, this is the minimum change in standard 
                                                  deviation required across conditions. For binary regulators, it's the minimum change in 
                                                  proportion across conditions. Regulators that do not meet these thresholds will be excluded 
                                                  from the regression models.")),
                                                             div(),
                                                             div(class = "title-2-box", span(class = "title-2", "Step 1: Verify the data format.")),
                                                             div(),
                                                             div(class = "text-section-3", 
                                                                 p("Click the button to check whether your data has the correct format.")),
                                                             div(class = "button-container", actionButton("validate_data_own", "Check data format")),
                                                             div(),
                                                             div(class = "text-section-3", p("If the data is incorrect, a message will appear here explaining the issue.
                                                                              If the data is correct, a temporary confirmation message will appear 
                                                                            in the bottom-right corner of the screen.")),
                                                             textOutput("error_message"),
                                                             div(),
                                                             div(class = "title-2-box", span(class = "title-2", "Step 2: Select the data configuration.")),
                                                             div(class = "text-section-3",
                                                                 p("First, specify whether each regulatory omic contains numeric or binary data. 
                                            You can do this using the selection menu located under each omic.
                                            Next, set the minimum change in standard deviation for each omic. This value determines 
                                            the threshold for filtering low-variation regulators.
                                            If you choose not to set a value, a default of 0 will be applied.
                                            Remember to click the Save button after selecting all the values.")),
                                                             div(class = "button-container", actionButton("select_config_own", "Select Configuration")),
                                                             div(),
                                                             div(class = "title-2-box", span(class = "title-2", "Step 3: Proceed to model preparation.")),
                                                             div(class = "text-section-3", p("Click the button to go to the model configuration page.")),
                                                             div(),
                                                             div(class = "button-container", actionButton("prepare_model_own", "Prepare Your Model"))),
                                   tabPanel(value = "prepare_model_own","Prepare your model",
                                            div(class = "text-section-3", 
                                                p("On this page, you will select the model you want to use along with its fitting parameters.
                                                  Once the configuration is complete, you can proceed to run MORE. 
                                                  The model will then execute and generate the results.")),
                                            div(class = "title-2-box", span(class = "title-2", "Step 1: Choose the method.")),
                                            div(),
                                            div(class = "text-section-3", 
                                                p("Select one of the available predictive modeling options: MLR 
                                                  (Multiple Linear Regression) or PLS (Partial Least Squares).")),
                                            div(class = "text-section-3", 
                                                p("MLR is a simple and traditional approach that tries to find a straight-line
                                                  relationship between a group of input variables and the outcome you're trying 
                                                  to predict. It works best when your input data is not too complex and the 
                                                  variables are not too closely related to each other.")),
                                            div(class = "text-section-3", 
                                                p("In contrast, PLS is a more advanced method designed for situations with
                                                  many input variables, especially when they are correlated or when there 
                                                  are more variables than samples. PLS reduces data complexity by creating 
                                                  new, combined variables that capture the most important information for prediction. 
                                                  One of the key advantages of PLS is that it can handle missing values, making 
                                                  it particularly useful for real-world biological or multi-omics datasets where 
                                                  complete data is often not available")),
                                            selectInput("method_choice", "Method:", choices = c("PLS1", "MLR"), selected = "PLS1"),
                                            conditionalPanel( # Inputs condicionales solo si se elige PLS1
                                              condition = "input.method_choice == 'PLS1'",
                                              div(class = "title-2-box", span(class = "title-2", "Step 1.1: Choose the parameters.")),
                                              div(),
                                              div(class = "text-section-3", 
                                                  p("If you selected the PLS model, you will now need to choose two parameters.")),
                                              div(class = "text-section-3",
                                                  p("The first is alfa, which is the significance level used to decide whether a 
                                                    regulator is considered important in the PLS model. By default, this value is 
                                                    set to 0.05.")),
                                              numericInput("alfa", "Alpha value:", value = 0.05, min = 0, max = 1, step = 0.01),
                                              div(class = "text-section-3", 
                                                  p("The second is VIP (Variable Importance in Projection), which is 
                                                    a score that reflects how much each variable contributes to the prediction. 
                                                    A variable is considered significant only if it meets both the alfa and VIP 
                                                    thresholds. By default, the VIP threshold is 0.8.")),
                                              numericInput("vip", "VIP value:", value = 0.8, min = 0, step = 0.1)),
                                            div(),
                                            div(class = "title-2-box", span(class = "title-2", "Step 2: Run MORE.")),
                                            div(class = "text-section-3",
                                                p("Click the button to run the model.")),
                                            div(class = "button-container", actionButton("runMore_own", "RUN MORE"))),
                                   tabPanel("Regulation Per Condition",
                                            div(class = "text-section-3",
                                                p("On this page, a function called RegulationPerCondition will be applied. It 
                                                  returns a summary table containing all the relevant or significant regulations, 
                                                  that is, all target feature–regulator pairs identified as important by the 
                                                  MORE models (depending on whether an MLR or a PLS model was applied). Additionally, 
                                                  it provides the regression coefficient that links each target feature with its 
                                                  corresponding regulator, evaluated separately for each experimental condition. This 
                                                  coefficient is tested to determine whether it is statistically relevant or 
                                                  significant.")),
                                            div(),
                                            div(class = "title-2-box", span(class = "title-2", "Step 1: Run Regulation Per Condition.")),
                                            div(),
                                            div(class = "text-section-3",
                                                p("Click the button to run Regulation Per Condition.")),
                                            div(class = "button-container", actionButton("runRegulation_own", "Regulation Per Condition")),
                                            div(class = "text-section-3",
                                                p("You can use the following table to search specific features")),
                                            DTOutput("table_own"),
                                            plotOutput("summaryPlot_own"),
                                            div(),
                                            div(class = "title-2-box", span(class = "title-2", "Step 2: Explore feature-regulator relationships.")),
                                            div(class = "text-section-3", 
                                                p("MORE also provides functions for visualization to explore gene-regulator relationships for the 
                                                pairs identified as significant by MORE.")),
                                            div(class = "text-section-3",
                                                p("When no specific regulator is provided, a plot is generated for each feature, displaying all 
                                                  its significant regulators for the feature, together with the feautre’s expression pattern.")),
                                            div(class = "text-section-3",
                                                p("If a regulator is specified but no feature is provided, MORE will generate plots for all feature–regulator 
                                                  pairs in which that regulator was identified as significant.")),
                                            div(class = "text-section-3",
                                                p("Use the following selectors to choose the feature and regulator. If you do not want to provide a specific feature
                                                  or regulator, please select NULL.")),
                                            div(style = "display: flex; justify-content: center; align-items: center; gap: 120px;",
                                                fluidRow(column(6, selectInput("selected_target_own", "Select Target Feature:", choices = NULL)),
                                                         column(6, selectInput("selected_regulator_own", "Select Regulator:", choices = NULL)))),
                                            br(),
                                            plotOutput("plot_more_output_own"),
                                            div(class = "title-2-box", span(class = "title-2", "Step 3: Filter the better features modeled by MORE.")),
                                            div(),
                                            div(class = "text-section-3",
                                                p("After completing this initial analysis, if you want to focus on the regulatory networks of a 
                                                  smaller group of features that were better modeled by MORE, you can use the following filter. 
                                                  This helps by filtering out target features whose model performance, 
                                                  measured by R² (a value that indicates how well the model fits the data), is below a
                                                  threshold you specify. In other words, it keeps only those genes for which the MORE 
                                                  model explains a sufficient amount of variation, allowing you to concentrate on the most 
                                                  reliable results.")),
                                            fluidRow(
                                              column(width = 12,
                                                     div(style = "display: flex; justify-content: center; align-items: center; gap: 120px;",
                                                         numericInput("R2_own", "Select R2", value = 0.5, min = 0, max = 1, step = 0.01),
                                                         div(class = "button-container", actionButton("updatePlot_own", "Update Plot with R2")),
                                                         div(class = "button-container", actionButton("runRegulationWithR2_own", "Filter the model"))))),
                                            div(),
                                            div(class = "text-section-3",
                                                p("You can view the updated results in the table and plots located just above."))
                                            
                                   ),
                                   tabPanel("MORENetwork",
                                            div(class = "text-section-3", 
                                                p("On this page you can visualize the network generated. To effectively configure the network 
                                                visualization, you will need to specify certain parameters within the filters. ")),
                                            div(class = "text-section-3", 
                                                p("First, you can define a reference group 
                                                  using the Group 1 filter. Providing a group name here establishes a baseline 
                                                  for creating differential networks. If this field is left empty, the system will, 
                                                  by default, plot networks for all available groups. However, if a group is 
                                                  specified for Group 1 and Group 2 remains blank, only the network specific to 
                                                  the chosen Group 1 will be generated.")),
                                            div(class = "text-section-3", 
                                                p("Secondly, you have the option to specify a comparison group via the Group 2
                                                  filter. This group will then be compared against the reference group designated in Group 1
                                                  for the creation of differential networks. Similar to Group 1, if Group 2 is not provided 
                                                  (and Group 1 is also left blank), the networks for all groups will be plotted. 
                                                  Both Group 1 and Group 2 are set to null by default, meaning no specific groups are pre-selected.")),
                                            div(class = "text-section-3", 
                                                p("Finally, you must set the percentile. This crucial filter allows the user to display only 
                                                  the most significant regulatory connections. By entering a value for percentile only those regulations 
                                                  that fall within the specified top percentage, indicating their strongest influence on regulators, 
                                                  will be included in the plot.")),
                                            div(),
                                            fluidRow(
                                              column(width = 12,
                                                     div(style = "display: flex; justify-content: center; align-items: center; gap: 120px;",
                                                         selectInput("Group1_own", "Select Group 1", choices = NULL),
                                                         selectInput("Group2_own", "Select Group 2", choices = NULL),
                                                         numericInput("pc_own", "Percentile", value = 0.9, min = 0, max = 1, step = 0.01),
                                                         fileInput("annot_data_example", "Annotation", accept = c(".csv", ".txt", ".xlsx")),
                                                         selectInput("pathway_example", "Select the pathway term", choices = NULL)))),
                                            div(class = "button-container", actionButton("runNetwork", "Ejecutar networkMORE")),
                                            div(),
                                            visNetworkOutput("networkPlot_own", height = "500px")),
                                   tabPanel("Over Representation Analysis", 
                                            div(class = "text-section-3", 
                                                p("On this page, you can carry out the Over Representation Analysis. Our implementation offers flexibility 
                                                  in how you define your target gene set.")),
                                            div(class = "text-section-3", 
                                                p("You can either use genes identified as hub target 
                                                  features, which represent highly connected nodes in your network, or opt for target features 
                                                  that are regulated by what we define as global regulators under specific conditions. 
                                                  This dual approach ensures you can tailor the ORA to best fit your specific 
                                                  research questions")),
                                            div(class = "text-section-3", 
                                                p("You can also select for which group do you want to carry out the ORA. And if you choose a  
                                                  regulatory omic it performs the ORA to the regulators of the specified omic ")),
                                            div(class ="text-section-3",
                                                p("Users have the flexibility to select their desired alpha level cutoff. This value determines the threshold 
                                                  for statistical significance, allowing you to control which results are considered meaningful.")),
                                            fluidRow(
                                              column(width = 12,
                                                     div(style = "display: flex; justify-content: center; align-items: center; gap: 120px;",
                                                         fileInput("annot_data_example", "Annotation", accept = c(".csv", ".txt", ".xlsx")),
                                                         checkboxInput("ora_byHubs_own", "Group by Hubs (byHubs)", value = TRUE),
                                                         selectInput("ora_group_own", "Select Group", choices = NULL),
                                                         selectInput("ora_byOmic_own", "Select Omic Type", choices = NULL),
                                                         numericInput("ora_alpha_own", "Alpha level", value = 0.05, min = 0, max = 1, step = 0.01)))),
                                            div(class = "button-container", actionButton("runORA_own", "Run ORA")),
                                            DTOutput("ora_table_own")),
                                   tabPanel("Gene Set Enrichment Analysis",
                                            div(class = "text-section-3", 
                                                p("On this page you can carry out a Gene Set Enrichment Analysis. We center the
                                                  analysis of the differences in the number of regulators of the genes in the experimental
                                                  conditions.")),
                                            div(class = "text-section-3", 
                                                p("You can also select for which group do you want to carry out the ORA. Group 1 is compulsory,
                                                  Group 2, is optional. Choose NULL for Group 2 if you want focus solely on your primary 
                                                  study group. In this scenario, the results will describe the enrichment within that 
                                                  single group but will not highlight statistical differences between groups")),
                                            div(class ="text-section-3",
                                                p("Users have the flexibility to select their desired alpha level cutoff. This value determines the threshold 
                                                  for statistical significance, allowing you to control which results are considered meaningful.")),
                                            fluidRow(
                                              column(width = 12,
                                                     div(style = "display: flex; justify-content: center; align-items: center; gap: 120px;",
                                                         fileInput("annot_data_example", "Annotation", accept = c(".csv", ".txt", ".xlsx")),
                                                         selectInput("gsea_group1_own", "Select Group", choices = NULL),
                                                         selectInput("gsea_group2_own", "Select Group", choices = NULL),
                                                         numericInput("gsea_alpha_own", "Alpha level", value = 0.05, min = 0, max = 1, step = 0.01)))),
                                            div(class = "button-container", actionButton("runGSEA_own", "Run GSEA")),
                                            plotOutput("gsea_plot_own", height = "700px")))
                       ),
              
              # PESTAÑA MORE VIDEOTUTORIAL
              tabPanel("MORE Videotutorial", div(class = "header-Download-Example-Dataset-image",
                                                 img(src = "MORE.png", alt = "More Image", class = "more2"),
                                                 span(class = "title", "MORE Videotutorial"),
                                                 img(src = "Biostatomics.jpg", alt = "Biostatomics Image", class = "biostatomics2")))))
              
              



server <- function(input, output, session) {
  
  data_storage_example <- reactiveValues(
    target = NULL,
    condition = NULL,
    regulatory = list(),
    associations = list()
  )
  
  omicType <- reactiveVal(NULL)
  minVariation <- reactiveVal(NULL)
  
  valid_data <- reactiveVal(FALSE)
  
  error_msg <- reactiveVal(NULL)
  filtered_data_list <- reactiveVal(NULL)
  filtered_data <- reactiveVal(FALSE)
  
  result_more <- reactiveVal(NULL)
  result_rpc <- reactiveVal(NULL)
  network_data <- reactiveVal(NULL)
  
  # CUANDO SE PULSA EL BOTÓN, CAMBIA DE PESTAÑA
  observeEvent(input$download_data, {
    updateTabsetPanel(session, "tabset", selected = "Download Example Dataset")
  })
  
  observeEvent(input$example_data, {
    updateTabsetPanel(session, "tabset", selected = "Run MORE with Example Dataset")
  })
  
  observeEvent(input$start, {
    updateTabsetPanel(session, "tabset", selected = "Run MORE with Own Dataset")
  })
  
  observeEvent(input$tutorial, {
    updateTabsetPanel(session, "tabset", selected = "MORE Videotutorial")
  })
  
  # CUANDO SE PULSE EL BOTÓN IR A GITHUB
  observeEvent(input$github_link, {
    session$sendCustomMessage(type = 'goToPage', message = NULL)
  })
  
  # DESCARGAR FICHERO EXCEL
  output$download_excel = downloadHandler(
    filename = function() {"excel.zip"},
    content = function(file){
      file.copy("www/ficheros/excel.zip", file)
    })
  
  # DESCARGAR FICHERO CSV
  output$download_csv = downloadHandler(
    filename = function() {"csv.zip"},
    content = function(file){
      file.copy("www/ficheros/csv.zip", file)
    })
  
  # DESCARGAR FICHERO TXT
  output$download_txt = downloadHandler(
    filename = function() {"txt.zip"},
    content = function(file){
      file.copy("www/ficheros/txt.zip", file)
    })
  
  # MOSTRAR TABLA TARGET DATA
  output$table_target_data = renderDT({
    file_path = "www/TARGET DATA/MatTargetOmic_50.csv"
    
    if (file.exists(file_path)){
      df = read.csv(file_path, row.names = 1)
      df = head(df, 5)
      
      is_num <- sapply(df, is.numeric)
      df[is_num] <- lapply(df[is_num], function(x) round(x, 3))
      
      data_storage_example$target <- data.frame(df)
      
      datatable(df, options = list(pageLength = 10, scrollX = TRUE))
    } else{
      datatable(data.frame(Message = "File not found"))
    }
  })
  
  # MOSTRAR TABLA RESUMEN REGULADORES Y ASOCIACIONES
  
  output$regulatory_associations_table <- renderTable({
    regulatory_associations_data <- data.frame(
      Regulator = c("lncRNA", "RBPprot", "RBPrna", "TFphos", "TFprot", "TFrna"),
      Associations = c("No", "No", "No", "Yes", "Yes", "Yes"),
      stringsAsFactors = FALSE
    )
    regulatory_associations_data
  })
  
  # TABLAS DE REGULADORES Y ASOCIACIONES
  
  ## Se organiza la información de los datasets de cada ómica
  regulator_info <- list(
    lncRNA = list(
      regulator_data = "www/REGULATORY DATA/lncRNA.csv",
      association_data = "There is no association list for this regulator"
    ),
    RBPprot = list(
      regulator_data = "www/REGULATORY DATA/RBPprot.csv",
      association_data = "There is no association list for this regulator"
    ),
    RBPrna = list(
      regulator_data = "www/REGULATORY DATA/RBPrna.csv",
      association_data = "There is no association list for this regulator"
    ),
    TFphos = list(
      regulator_data = "www/REGULATORY DATA/TFphos.csv",
      association_data = "www/ASSOCIATIONS/TFphos.csv"
    ),
    TFprot = list(
      regulator_data = "www/REGULATORY DATA/TFprot.csv",
      association_data = "www/ASSOCIATIONS/TFprot.csv"
    ),
    TFrna = list(
      regulator_data = "www/REGULATORY DATA/TFrna.csv",
      association_data = "www/ASSOCIATIONS/TFrna.csv"
    ))
  
  
  # REGULATORY
  regulatory <- list(
    lncRNA = if (file.exists("www/REGULATORY DATA/lncRNA.csv")) {
      read.csv("www/REGULATORY DATA/lncRNA.csv", row.names = 1)
    } else { NULL },
    
    RBPprot = if (file.exists("www/REGULATORY DATA/RBPprot.csv")) {
      read.csv("www/REGULATORY DATA/RBPprot.csv", row.names = 1)
    } else { NULL },
    
    RBPrna = if (file.exists("www/REGULATORY DATA/RBPrna.csv")) {
      read.csv("www/REGULATORY DATA/RBPrna.csv", row.names = 1)
    } else { NULL },
    
    TFphos = if (file.exists("www/REGULATORY DATA/TFphos.csv")) {
      read.csv("www/REGULATORY DATA/TFphos.csv", row.names = 1)
    } else { NULL },
    
    TFprot = if (file.exists("www/REGULATORY DATA/TFprot.csv")) {
      read.csv("www/REGULATORY DATA/TFprot.csv", row.names = 1)
    } else { NULL },
    
    TFrna = if (file.exists("www/REGULATORY DATA/TFrna.csv")) {
      read.csv("www/REGULATORY DATA/TFrna.csv", row.names = 1)
    } else { NULL }
  )
  
  data_storage_example$regulatory <- regulatory
  
  # ASSOCIATIONS
  associations <- list(
    lncRNA = NULL,  # No tiene archivo
    
    RBPprot = NULL,  # No tiene archivo
    
    RBPrna = NULL,  # No tiene archivo
    
    TFphos = if (file.exists("www/ASSOCIATIONS/TFphos.csv")) {
      read.csv("www/ASSOCIATIONS/TFphos.csv", row.names = 1)
    } else { NULL },
    
    TFprot = if (file.exists("www/ASSOCIATIONS/TFprot.csv")) {
      read.csv("www/ASSOCIATIONS/TFprot.csv", row.names = 1)
    } else { NULL },
    
    TFrna = if (file.exists("www/ASSOCIATIONS/TFrna.csv")) {
      read.csv("www/ASSOCIATIONS/TFrna.csv", row.names = 1)
    } else { NULL }
  )
  
  data_storage_example$associations <- associations
  
  
  # TABLA RESUMEN REGULATIONS AND CONDITIONS
  output$regulatory_tables <- renderUI({
    lapply(names(regulator_info), function(reg_name) {
      info <- regulator_info[[reg_name]]
      
      # Leer datos del regulador
      df_regulator <- read.csv(info$regulator_data, row.names = 1)
      
      # Crear elementos base
      elements <- list(
        div(class = "regulator-name-box", paste("Regulator Name:", reg_name)),
        div(class = "title-box", "Regulatory Data"),
        DT::renderDataTable({
          datatable(head(df_regulator, 5), options = list(pageLength = 10, scrollX = TRUE))
        })
      )
      
      # Añadir asociaciones solo si existen
      if (!grepl("There is no", info$association_data)) {
        df_association <- read.csv(info$association_data, row.names = 1)
        elements <- c(elements, list(
          div(class = "title-box", "Associations"),
          DT::renderDataTable({
            datatable(head(df_association, 5), options = list(pageLength = 10, scrollX = TRUE))
          })
        ))
      }
      
      # Devolver el div general
      do.call(div, elements)
      
    })
  })
  
  # TABLA DE CONDITIONS
  output$table_condition <- renderDT({
    condition_data <- read.csv("www/CONDITION/condition.csv", row.names = 1)
    
    data_storage_example$condition <- as.data.frame(condition_data)
    
    datatable(condition_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # VALIDAR LOS DATOS DE EJEMPLO
  
  observeEvent(input$validate_data_example, {
    req(data_storage_example$target)
    error_msg(NULL)
    
    target_ncol <- ncol(data_storage_example$target)
    cond_nrow <- if (!is.null(data_storage_example$condition)) nrow(data_storage_example$condition) else NA
    
    # 1. Validar número de muestras
    for (omic_name in names(data_storage_example$regulatory)) {
      reg_matrix <- data_storage_example$regulatory[[omic_name]]
      if (ncol(reg_matrix) != target_ncol) {
        error_msg(paste0("ERROR: The number of samples in 'regulatoryData' (omic '", omic_name, "') must match 'targetData'."))
        valid_data(FALSE)
        return(invisible())
      }
      
      if (!is.null(data_storage_example$condition) && ncol(reg_matrix) != cond_nrow) {
        error_msg(paste0("ERROR: The number of samples in 'regulatoryData' (omic '", omic_name, "') must match 'conditionData'."))
        valid_data(FALSE)
        return(invisible())
      }
    }
    
    # 2. Validar coincidencia de nombres de muestras
    if (is.null(data_storage_example$condition)) {
      name_problem <- !all(sapply(data_storage_example$regulatory, function(x)
        length(intersect(colnames(x), colnames(data_storage_example$target))) == ncol(data_storage_example$target)))
      
      if (name_problem) {
        error_msg("ERROR: targetData and regulatoryData samples have not the same names.")
        valid_data(FALSE)
        return(invisible())
      }
    } else {
      name_problem <- !all(c(
        sapply(data_storage_example$regulatory, function(x)
          length(intersect(colnames(x), colnames(data_storage_example$target))) == ncol(data_storage_example$target)),
        length(intersect(rownames(data_storage_example$condition), colnames(data_storage_example$target))) == ncol(data_storage_example$target)
      ))
      
      if (name_problem) {
        error_msg("ERROR: targetData, condition and regulatoryData samples have not the same names. We assume that they are ordered. 
                  If they are not ordered please upload again the file with samples with the correct names.")
        valid_data(FALSE)
        return(invisible())
      }
    }
    
    # 3. Control de valores NA
    percNA <- 0  # Buscamos cualquier NA
    
    # Funciones auxiliares
    missing_rows <- function(regOmic, percNA) {
      highNA <- apply(regOmic, 1, function(x) sum(is.na(x)) / length(x)) > percNA
      rownames(regOmic)[highNA]
    }
    
    missing_cols <- function(regOmic, percNA) {
      highNA <- apply(regOmic, 2, function(x) sum(is.na(x)) / length(x)) > percNA
      colnames(regOmic)[highNA]
    }
    
    # Detectar reguladores con NA
    myregNA <- lapply(data_storage_example$regulatory, function(x) missing_rows(x, percNA))
    num_regNA <- sapply(myregNA, length)
    
    # Detectar observaciones (columnas) con NA en alguna ómica
    myobsNA <- lapply(data_storage_example$regulatory, function(x) missing_cols(x, percNA))
    myobsNA <- unique(unlist(myobsNA))
    
    # Mostrar advertencias si hay NAs
    if (sum(num_regNA) > 0) {
      warning_msg <- paste0("Warning !!! Some regulators have missing values.")
      for (omic in names(num_regNA)) {
        warning_msg <- paste0(warning_msg, "- ", omic, ": ", num_regNA[omic], " regulators\n")
      }
      showNotification(warning_msg, type = "warning", duration = NULL)
    }
    
    if (length(myobsNA) > 0) {
      warning_msg <- paste0("Warning!!! There are", length(myobsNA),
                            " samples with missing values.")
      showNotification(warning_msg, type = "warning", duration = NULL)
    }
    
    # Si pasa todas las validaciones
    valid_data(TRUE)
    showNotification("All data validated successfully. You can continue to run MORE." , type = "message")
  })
  
  # SELECCIONAR CONFIGURACIÓN: TIPO DE ÓMICAS Y FILTRO VARIABILIDAD
  
  observeEvent(input$select_config_example, {
    omic_names <- names(data_storage_example$regulatory)
    n <- length(omic_names)
    
    # Crear un selectInput por ómica
    select_inputs <- lapply(seq_len(n), function(i) {
      selectInput(
        inputId = paste0("omic_type_", i),
        label = paste("Type of omic:", omic_names[i]),
        choices = c("Numeric" = 0, "Binary" = 1),
        selected = 0
      )
    })
    
    select_cv <- lapply(seq_len(n), function(i) {
      numericInput(
        inputId = paste0("min_variation_", i),
        label = paste("Minimum change in standard deviation for omic", omic_names[i]),
        value = 0,      
        min = 0,        
        max = Inf,        
        step = 0.1        
      )
    })
    
    # Mostrar el modal con todos los selectInputs
    showModal(modalDialog(
      tagList(select_inputs, select_cv),
      footer = tagList(
        actionButton("save_config", "Save"),
        modalButton("Cancel")
      ),
      size = "l"
    ))
  })
  
  # GUARDAR CONFIGURACIÓN DE OMIC TYPE Y LOW VARIATION FILTER
  observeEvent(input$save_config, {
    
    removeModal()
    
    omic_names <- names(data_storage_example$regulatory)
    n <- length(omic_names)
    
    omic_t = list()
    variations = list()
    
    # Recoger los valores seleccionados
    omic_t <- c(omic_t, (sapply(seq_len(n), function(i) {
      as.numeric(input[[paste0("omic_type_", i)]])
    })))
    
    #names(omic_t) = names(data_storage_example$regulatory)
    
    
    variations <- c(variations, sapply(seq_len(n), function(i) {
      as.numeric(input[[paste0("min_variation_", i)]])
    }))
    
    #names(variations) = names(data_storage_example$regulatory)
    
    # Guardar en el reactivo
    omicType(omic_t)
    minVariation(variations)
    
    
    showNotification("Configuration saved.", type = "message")
  })
  
  # CAMBIO DE PESTAÑA
  
  observeEvent(input$prepare_model_example, {
    updateTabsetPanel(session, inputId = "sub_tabs", selected = "prepare_model")
  })
  
  # RUN MORE
  observeEvent(input$runMore, {
    
    req(omicType(), minVariation(), data_storage_example$condition)
    
    var = minVariation()
    config_omic_type = omicType()
    names(var) = names(data_storage_example$regulatory)
    
    method_selected <- input$method_choice
    
    if (method_selected == "MLR") {
    varSel_val <- "EN"
    alfa_val <- NULL
    vip_val <- NULL
  } else if (method_selected == "PLS1") {
    grupos <- table(data_storage_example$condition)
    if (any(grupos > 20)) {
      varSel_val <- "Perm"
    } else {
      varSel_val <- "Jack"
    }
    alfa_val <- input$alfa
    vip_val <- input$vip
  }
    
    print("Ejecutando botón RUN MORE")
    
    result <- tryCatch({
      more(
        targetData = data_storage_example$target,
        regulatoryData = data_storage_example$regulatory,
        associations = data_storage_example$associations,
        condition = data_storage_example$condition,
        omicType = config_omic_type,
        scaleType = 'auto',
        interactions = TRUE,
        minVariation = var,
        varSel = varSel_val,
        alfa = alfa_val,
        vip = vip_val,
        method = method_selected,
        parallel = FALSE
      )
    }, error = function(e) {
      paste("There was an error while executing MORE:", e$message)
    })
    
    if (inherits(result, "try-error")) {
      error_msg(result)
    } else {
      result_more(result)
    }
  })
  
  # MENSAJE SI HAY UN ERROR EN RUN MORE
  output$error_message <- renderText({
    error_msg()
  })
  
  # REGULATION PER CONDITION
  
  observeEvent(input$runRegulation, {
    req(result_more())  # Aquí se requiere que esté disponible
    
    showNotification("Ejecutando regulation per condition...", type = "message")
    
    updated_result <- RegulationPerCondition(result_more())
    result_rpc(updated_result)
    
    output$summaryPlot_example <- renderPlot({
      summaryPlot(result_more(), result_rpc(), NULL)
    })

    output$table_example <- renderDT({
        datatable(data.frame(result_rpc()), options = list(pageLength = 10, searchHighlight = TRUE))
      })
  
    
    showNotification("Regulation per condition completado.", type = "message")
  })
  
  # ACTUALIZAR PLOT
  
  observeEvent(input$updatePlot_example, {
    req(result_more())
    
    # Mostrar notificación de actualización del plot
    showNotification("Actualizando plot con R2...", type = "message")
    
    # Ejecutar el método summaryPlot() y mostrar el resultado con R2
    output$summaryPlot_example <- renderPlot({
      summaryPlot(result_more(), result_rpc(), input$R2)
    })
    
    output$table_example <- renderDT({
      datatable(data.frame(result_rpc()), options = list(pageLength = 10, searchHighlight = TRUE))
    })
    
    # Mostrar notificación de finalización
    showNotification("Plot actualizado con R2.", type = "message")
  })
  
  # FILTRAR EL MODELO
  
  observeEvent(input$runRegulationWithR2_example, {
    req(result_more())
    
    # Mostrar notificación de ejecución
    showNotification("Ejecutando regulation per condition con R2...", type = "message")
    
    # Ejecutar regulation per condition con R2
    result_rpc(RegulationPerCondition(result_more(), filterR2 = input$R2))
    
    # Mostrar notificación de finalización
    showNotification("Regulation per condition con R2 completado.", type = "message")
    
    # Ejecutar el método summaryPlot() y mostrar el resultado con R2
    output$summaryPlot_example <- renderPlot({
      summaryPlot(result_more(), result_rpc(), input$R2)
    })
    
    output$table_example <- renderDT({
      datatable(data.frame(result_rpc()), options = list(pageLength = 10, searchHighlight = TRUE))
    })
  })
  
  # EXTRAER LOS VALORES ÚNICOS PARA LOS SELECTORES
  observeEvent(result_rpc(), {
    req(result_rpc())
    
    # Extraer los valores únicos para los selectores
    rpc_df <- as.data.frame(result_rpc())
    updateSelectInput(session, "selected_target",
                      choices = unique(rpc_df$targetF))
    updateSelectInput(session, "selected_regulator",
                      choices = unique(rpc_df$regulator))
  })
  
  # PLOT MORE
  output$plot_more_output_example <- renderPlot({
    req(input$selected_target, input$selected_regulator, result_more())
    
    plotMORE(
      result_more(),
      targetF = input$selected_target,
      regulator = input$selected_regulator,
      order = TRUE
    )
  })
  
  # SELECCIONAR GRUPOS DE CONDITION
  observe({
    req(data_storage_example$condition)
    condition = data_storage_example$condition
    groups <- unique(as.character(condition$Group))
    updateSelectInput(session, "Group1", choices = groups)
    updateSelectInput(session, "Group2", choices = c("NULL", groups))
  })
  
  # LEER ANNOTATION SI HUBIESE
  
  read_annotation_file <- function(file_input) {
    if (is.null(file_input)) return(NULL)
    
    ext <- tools::file_ext(file_input$name)
    
    switch(tolower(ext),
           "csv"  = read.csv(file_input$datapath),
           "txt"  = read.delim(file_input$datapath),
           "tsv"  = read_tsv(file_input$datapath),
           "xlsx" = read_excel(file_input$datapath),
           {
             showNotification("Formato de archivo no compatible. Use CSV, TXT, TSV o XLSX.", type = "error")
             return(NULL)
           }
    )
  }
  
  # GENERAR RED
  
  observeEvent(input$runNetwork_example, {
    req(result_rpc(), input$Group1)
    
    group2_val <- if (input$Group2 == "NULL") NULL else input$Group2
    group1_val <- if (input$Group1 == "NULL") NULL else input$Group1
    
    
    net <- networkMORE_cyj(result_rpc(), group1 = group1_val, group2 = group2_val, pc = input$pc)
    
    network_data(net)
    showNotification("Red generada correctamente.", type = "message")
  })
  
  # ENSEÑAR LA RED
  
  output$networkPlot_example <- renderVisNetwork({
    
    net <- network_data()
    edges <- net$edges %>% rename(from = source, to = target)
    edges <- style_edges_by_line_type(edges)
    
    legend_items <- generate_legend(net$nodes)
    
    
    nodes <- net$nodes %>%
      mutate(
        label = id,
        font.align = "top",
        font.color = "black",
        font.size = 14
      )
    
    legend_items <- generate_legend(nodes)
    
    visNetwork(nodes, edges) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLegend(addNodes = legend_items, useGroups = FALSE, position = "right") %>%
      visLayout(randomSeed = 42) %>% 
      
      visPhysics(
        enabled = TRUE, 
        solver = "repulsion", 
        repulsion = list(
          nodeDistance = 300,
          springLength = 200 
        ),
        
        stabilization = list(
          iterations = 2500 
        )
      )
  })
  
  annot = reactiveVal(NULL)
  
  # ENSEÑAR ANNOT MATRIX
  output$table_annot = renderDT({
    file_path = "www/ANNOT/GO_annot_human.xlsx"
    
    if (file.exists(file_path)){
      df = read_excel(file_path)
      
      df <- na.omit(df)
      annot(data.frame(df))
      
      datatable(df, options = list(pageLength = 10, scrollX = TRUE))
    } else{
      datatable(data.frame(Message = "File not found"))
    }
  })
  
  # FILTRO DE GRUPOS
  
  observe({
    # Llenar dinámicamente grupos desde condition si ya existe
    if (!is.null(data_storage_example$condition)) {
      groups <- unique(data_storage_example$condition[, 1])
      updateSelectInput(session, "ora_group", choices = groups)
    }
    
    # Llenar dinámicamente omicTypes desde nombres de regulatory
    if (!is.null(data_storage_example$regulatory)) {
      updateSelectInput(session, "ora_byOmic", choices = c("NULL", names(data_storage_example$regulatory)))
    }
  })
  
  # ORA MORE
  
  observeEvent(input$runORA, {
    req(input$ora_group, input$ora_byOmic)
    
    # Obtener subconjunto por condición
    reg_in_cond <- RegulationInCondition(result_rpc(), cond = input$ora_group)
    
    # Filtrar modelo por omic
    model <- result_more()[[input$ora_byOmic]]
    
    # Ejecutar oraMORE
    ora_result <- tryCatch({
      oraMORE(model, reg_in_cond,
              byHubs = input$ora_byHubs,
              annotation = annot_sin_na,
              alpha = input$ora_alpha)
    }, error = function(e) {
      showNotification(paste("Error in ORA:", e$message), type = "error")
      return(NULL)
    })
    
    output$ora_table <- renderDT({
      req(ora_result)
      datatable(ora_result, options = list(pageLength = 10, scrollX = TRUE))
    })
  })
  
  # SELECCIONAR GRUPOS GSEA MORE
  
  observe({
    if (!is.null(data_storage_example$condition)) {
      groups <- unique(data_storage_example$condition[, 1])
      updateSelectInput(session, "gsea_group1", choices = groups, selected = groups[1])
      updateSelectInput(session, "gsea_group2", choices = c("NULL", groups), selected = "NULL")
    }
  })
  
  result_gsea <- reactiveVal(NULL)
  
  # RUN GSEA MORE
  
  observeEvent(input$runGSEA, {
    
    annotat = annot()
    
    rpc = result_rpc()
    reg1 <- RegulationInCondition(rpc, cond = input$gsea_group1)
    reg2 <- if (input$gsea_group2 != "NULL") RegulationInCondition(rpc, cond = input$gsea_group2) else NULL
    
    print(str(reg1))
    print(str(reg2))
   
    gsea_out <- tryCatch({
       gseaMORE(reg1, reg2,  annotation = annotat, p.adjust.method = 'none', alpha = 1)
    }, error = function(e) {
      showNotification(paste("Error in GSEA:", e$message), type = "error")
      return(NULL)
    })
    
    if (!is.null(gsea_out)) {
      result_gsea(gsea_out)
    }
    
  })
  
  # GSEA PLOT
  
  output$gsea_plot <- renderPlot({
    req(result_gsea())  # Aquí va con paréntesis
    
    gsea_out <- result_gsea()
    
    clusterProfiler::dotplot(gsea_out, split = ".sign") +
      facet_grid(. ~ .sign, labeller = as_labeller(c(
        activated = input$gsea_group2,
        suppressed = input$gsea_group1
      ))) +
      theme(
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 4),
        strip.text = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9)
      ) +
      scale_y_discrete(labels = gsea_out@result$ID)
  })
  
  # VARIABLES PARA OWN DATA
  data_storage <- reactiveValues(
    target = NULL,
    condition = NULL,
    regulatory = list(),
    associations = list(),
    clinic = NULL
  )
  
  omicType_own <- reactiveVal(NULL)
  minVariation_own <- reactiveVal(NULL)
  
  # VALIDAR OWN DATA
  
  observeEvent(input$validate_data_own, {
    req(data_storage$target)
    error_msg(NULL)
    
    target_ncol <- ncol(data_storage$target)
    cond_nrow <- if (!is.null(data_storage$condition)) nrow(data_storage$condition) else NA
    
    # 1. Validar número de muestras
    for (omic_name in names(data_storage$regulatory)) {
      reg_matrix <- data_storage$regulatory[[omic_name]]
      if (ncol(reg_matrix) != target_ncol) {
        error_msg(paste0("ERROR: The number of samples in 'regulatoryData' (omic '", omic_name, "') must match 'targetData'."))
        valid_data(FALSE)
        return(invisible())
      }
      
      if (!is.null(data_storage$condition) && ncol(reg_matrix) != cond_nrow) {
        error_msg(paste0("ERROR: The number of samples in 'regulatoryData' (omic '", omic_name, "') must match 'conditionData'."))
        valid_data(FALSE)
        return(invisible())
      }
    }
    
    # 2. Validar coincidencia de nombres de muestras
    if (is.null(data_storage$condition)) {
      name_problem <- !all(sapply(data_storage$regulatory, function(x)
        length(intersect(colnames(x), colnames(data_storage$target))) == ncol(data_storage$target)))
      
      if (name_problem) {
        error_msg("ERROR: targetData and regulatoryData samples have not the same names.")
        valid_data(FALSE)
        return(invisible())
      }
    } else {
      name_problem <- !all(c(
        sapply(data_storage$regulatory, function(x)
          length(intersect(colnames(x), colnames(data_storage$target))) == ncol(data_storage$target)),
        length(intersect(rownames(data_storage$condition), colnames(data_storage$target))) == ncol(data_storage$target)
      ))
      
      if (name_problem) {
        error_msg("ERROR: targetData, condition and regulatoryData samples have not the same names. We assume that they are ordered. 
                  If they are not ordered please upload again the file with samples with the correct names.")
        valid_data(FALSE)
        return(invisible())
      }
    }
    
    # 3. Control de valores NA
    percNA <- 0  # Buscamos cualquier NA
    
    # Funciones auxiliares
    missing_rows <- function(regOmic, percNA) {
      highNA <- apply(regOmic, 1, function(x) sum(is.na(x)) / length(x)) > percNA
      rownames(regOmic)[highNA]
    }
    
    missing_cols <- function(regOmic, percNA) {
      highNA <- apply(regOmic, 2, function(x) sum(is.na(x)) / length(x)) > percNA
      colnames(regOmic)[highNA]
    }
    
    # Detectar reguladores con NA
    myregNA <- lapply(data_storage$regulatory, function(x) missing_rows(x, percNA))
    num_regNA <- sapply(myregNA, length)
    
    # Detectar observaciones (columnas) con NA en alguna ómica
    myobsNA <- lapply(data_storage$regulatory, function(x) missing_cols(x, percNA))
    myobsNA <- unique(unlist(myobsNA))
    
    # Mostrar advertencias si hay NAs
    if (sum(num_regNA) > 0) {
      warning_msg <- paste0("Warning !!! Some regulators have missing values.")
      for (omic in names(num_regNA)) {
        warning_msg <- paste0(warning_msg, "- ", omic, ": ", num_regNA[omic], " regulators\n")
      }
      showNotification(warning_msg, type = "warning", duration = NULL)
    }
    
    if (length(myobsNA) > 0) {
      warning_msg <- paste0("Warning!!! There are", length(myobsNA),
                            " samples with missing values.")
      showNotification(warning_msg, type = "warning", duration = NULL)
    }
    
    # Si pasa todas las validaciones
    valid_data(TRUE)
    showNotification("All data validated successfully. You can continue to run MORE." , type = "message")
  })
  
  # SELECT CONFIG EXAMPLE
  
  observeEvent(input$select_config_own, {
    omic_names <- names(data_storage$regulatory)
    n <- length(omic_names)
    
    # Crear un selectInput por ómica
    select_inputs <- lapply(seq_len(n), function(i) {
      selectInput(
        inputId = paste0("omic_type_", i),
        label = paste("Type of omic:", omic_names[i]),
        choices = c("Numeric" = 0, "Binary" = 1),
        selected = 0
      )
    })
    
    select_cv <- lapply(seq_len(n), function(i) {
      numericInput(
        inputId = paste0("min_variation_", i),
        label = paste("Minimum change in standard deviation for omic", omic_names[i]),
        value = 0,      
        min = 0,        
        max = Inf,        
        step = 0.1        
      )
    })
    
    # Mostrar el modal con todos los selectInputs
    showModal(modalDialog(
      tagList(select_inputs, select_cv),
      footer = tagList(
        actionButton("save_config_own", "Save"),
        modalButton("Cancel")
      ),
      size = "l"
    ))
  })
  
  # GUARDAR CONFIG OMIC
  
  observeEvent(input$save_config_own, {
    
    removeModal()
    
    omic_names <- names(data_storage$regulatory)
    n <- length(omic_names)
    
    omic_t = list()
    variations = list()
    
    # Recoger los valores seleccionados
    omic_t <- c(omic_t, (sapply(seq_len(n), function(i) {
      as.numeric(input[[paste0("omic_type_", i)]])
    })))
    
    #names(omic_t) = names(data_storage_example$regulatory)
    
    
    variations <- c(variations, sapply(seq_len(n), function(i) {
      as.numeric(input[[paste0("min_variation_", i)]])
    }))
    
    #names(variations) = names(data_storage_example$regulatory)
    
    # Guardar en el reactivo
    omicType_own(omic_t)
    minVariation_own(variations)
    
    
    showNotification("Configuration saved.", type = "message")
  })
  
  # CAMBIO DE PESTAÑA
  
  observeEvent(input$prepare_model_own, {
    updateTabsetPanel(session, inputId = "sub_tabs_2", selected = "prepare_model_own")
  })
  
  # RUN MORE OWN
  observeEvent(input$runMore_own, {
    
    req(omicType_own(), minVariation_own(), data_storage$condition)
    
    var = minVariation_own()
    config_omic_type = omicType_own()
    names(var) = names(data_storage$regulatory)
    
    method_selected <- input$method_choice
    
    if (method_selected == "MLR") {
      varSel_val <- "EN"
      alfa_val <- NULL
      vip_val <- NULL
    } else if (method_selected == "PLS1") {
      grupos <- table(data_storage$condition)
      if (any(grupos > 20)) {
        varSel_val <- "Perm"
      } else {
        varSel_val <- "Jack"
      }
      alfa_val <- input$alfa
      vip_val <- input$vip
    }
    
    print("Ejecutando botón RUN MORE")
    
    result <- tryCatch({
      more(
        targetData = data_storage$target,
        regulatoryData = data_storage$regulatory,
        associations = data_storage$associations,
        condition = data_storage$condition,
        omicType = config_omic_type,
        scaleType = 'auto',
        interactions = TRUE,
        minVariation = var,
        varSel = varSel_val,
        alfa = alfa_val,
        vip = vip_val,
        method = method_selected,
        parallel = FALSE
      )
    }, error = function(e) {
      paste("There was an error while executing MORE:", e$message)
    })
    
    if (inherits(result, "try-error")) {
      error_msg(result)
    } else {
      result_more(result)
    }
  })
  
  # MENSAJE SI HAY UN ERROR EN RUN MORE OWN 
  output$error_message <- renderText({
    error_msg()
  })
  
  # REGULATION PER CONDITION OWN
  
  observeEvent(input$runRegulation_own, {
    req(result_more())  # Aquí se requiere que esté disponible
    
    showNotification("Ejecutando regulation per condition...", type = "message")
    
    updated_result <- RegulationPerCondition(result_more())
    result_rpc(updated_result)
    
    output$summaryPlot_own <- renderPlot({
      summaryPlot(result_more(), result_rpc(), NULL)
    })
    
    output$table_own <- renderDT({
      datatable(data.frame(result_rpc()), options = list(pageLength = 10, searchHighlight = TRUE))
    })
    
    
    showNotification("Regulation per condition completado.", type = "message")
  })
  
  # ACTUALIZAR PLOT OWN
  
  observeEvent(input$updatePlot_own, {
    req(result_more())
    
    # Mostrar notificación de actualización del plot
    showNotification("Actualizando plot con R2...", type = "message")
    
    # Ejecutar el método summaryPlot() y mostrar el resultado con R2
    output$summaryPlot_own <- renderPlot({
      summaryPlot(result_more(), result_rpc(), input$R2_own)
    })
    
    output$table_example <- renderDT({
      datatable(data.frame(result_rpc()), options = list(pageLength = 10, searchHighlight = TRUE))
    })
    
    # Mostrar notificación de finalización
    showNotification("Plot actualizado con R2.", type = "message")
  })
  
  # FILTRAR EL MODELO OWN
  
  observeEvent(input$runRegulationWithR2_own, {
    req(result_more())
    
    # Mostrar notificación de ejecución
    showNotification("Ejecutando regulation per condition con R2...", type = "message")
    
    # Ejecutar regulation per condition con R2
    result_rpc(RegulationPerCondition(result_more(), filterR2 = input$R2_own))
    
    # Mostrar notificación de finalización
    showNotification("Regulation per condition con R2 completado.", type = "message")
    
    # Ejecutar el método summaryPlot() y mostrar el resultado con R2
    output$summaryPlot_own <- renderPlot({
      summaryPlot(result_more(), result_rpc(), input$R2_own)
    })
    
    output$table_own <- renderDT({
      datatable(data.frame(result_rpc()), options = list(pageLength = 10, searchHighlight = TRUE))
    })
  })
  
  # EXTRAER LOS VALORES ÚNICOS PARA LOS SELECTORES OWN
  observeEvent(result_rpc(), {
    req(result_rpc())
    
    # Extraer los valores únicos para los selectores
    rpc_df <- as.data.frame(result_rpc())
    updateSelectInput(session, "selected_target_own",
                      choices = c(unique(rpc_df$targetF), NULL))
    updateSelectInput(session, "selected_regulator_own",
                      choices = c(unique(rpc_df$regulator), NULL))
  })
  
  # PLOT MORE OWN
  output$plot_more_output_own <- renderPlot({
    req(input$selected_target_own, input$selected_regulator_own, result_more())
    
    plotMORE(
      result_more(),
      targetF = input$selected_target_own,
      regulator = input$selected_regulator_own,
      order = TRUE
    )
  })
  
  observe({
    req(data_storage_example$condition)
    condition = data_storage_example$condition
    groups <- unique(as.character(condition$Group))
    updateSelectInput(session, "Group1", choices = groups)
    updateSelectInput(session, "Group2", choices = c("NULL", groups))
  })
  
  # SELECCIONAR GRUPOS DE CONDITION
  observe({
    req(data_storage$condition)
    condition = data_storage_example$condition
    groups <- unique(as.character(condition$Group))
    updateSelectInput(session, "Group1_own", choices = c("NULL", groups))
    updateSelectInput(session, "Group2_own", choices = c("NULL", groups))
  })
  
  # LEER ANNOTATION SI HUBIESE
  
  read_annotation_file <- function(file_input) {
    if (is.null(file_input)) return(NULL)
    
    ext <- tools::file_ext(file_input$name)
    
    switch(tolower(ext),
           "csv"  = read.csv(file_input$datapath),
           "txt"  = read.delim(file_input$datapath),
           "tsv"  = read_tsv(file_input$datapath),
           "xlsx" = read_excel(file_input$datapath),
           {
             showNotification("Formato de archivo no compatible. Use CSV, TXT, TSV o XLSX.", type = "error")
             return(NULL)
           }
    )
  }
  
  # GENERAR RED
  
  observeEvent(input$runNetwork, {
    req(result_rpc(), input$Group1_own)
    
    group2_val <- if (input$Group2_own == "NULL") NULL else input$Group2
    group1_val <- if (input$Group1_own == "NULL") NULL else input$Group1
    
    
    net <- networkMORE_cyj(result_rpc(), group1 = group1_val, group2 = group2_val, pc_own = input$pc)
    
    network_data(net)
    showNotification("Red generada correctamente.", type = "message")
  })
  
  # ENSEÑAR LA RED
  
  output$networkPlot_own <- renderVisNetwork({
    
    net <- network_data()
    edges <- net$edges %>% rename(from = source, to = target)
    edges <- style_edges_by_line_type(edges)
    
    legend_items <- generate_legend(net$nodes)
    
    
    nodes <- net$nodes %>%
      mutate(
        label = id,
        font.align = "top",
        font.color = "black",
        font.size = 14
      )
    
    legend_items <- generate_legend(nodes)
    
    visNetwork(nodes, edges) %>%
      visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visLegend(addNodes = legend_items, useGroups = FALSE, position = "right") %>%
      visLayout(randomSeed = 42) %>% 
      
      visPhysics(
        enabled = TRUE, 
        solver = "repulsion", 
        repulsion = list(
          nodeDistance = 300,
          springLength = 200 
        ),
        
        stabilization = list(
          iterations = 2500 
        )
      )
  })
  
  # FILTRO DE GRUPOS
  
  observe({
    # Llenar dinámicamente grupos desde condition si ya existe
    if (!is.null(data_storage$condition)) {
      groups <- unique(data_storage$condition[, 1])
      updateSelectInput(session, "ora_group_own", choices = groups)
    }
    
    # Llenar dinámicamente omicTypes desde nombres de regulatory
    if (!is.null(data_storage$regulatory)) {
      updateSelectInput(session, "ora_byOmic_own", choices = c("NULL", names(data_storage$regulatory)))
    }
  })
  
  # ORA MORE
  
  observeEvent(input$runORA_own, {
    req(input$ora_group_own, input$ora_byOmic_own)
    
    # Obtener subconjunto por condición
    reg_in_cond <- RegulationInCondition(result_rpc(), cond = input$ora_group_own)
    
    # Filtrar modelo por omic
    model <- result_more()[[input$ora_byOmic_own]]
    
    # Ejecutar oraMORE
    ora_result_own <- tryCatch({
      oraMORE(model, reg_in_cond,
              byHubs = input$ora_byHubs_own,
              annotation = annot_sin_na,
              alpha = input$ora_alpha_own)
    }, error = function(e) {
      showNotification(paste("Error in ORA:", e$message), type = "error")
      return(NULL)
    })
    
    output$ora_table_own <- renderDT({
      req(ora_result_own)
      datatable(ora_result_own, options = list(pageLength = 10, scrollX = TRUE))
    })
  })
  
  # SELECCIONAR GRUPOS GSEA MORE
  
  observe({
    if (!is.null(data_storage$condition)) {
      groups <- unique(data_storage$condition[, 1])
      updateSelectInput(session, "gsea_group1_own", choices = groups, selected = groups[1])
      updateSelectInput(session, "gsea_group2_own", choices = c("NULL", groups), selected = "NULL")
    }
  })
  
  result_gsea <- reactiveVal(NULL)
  
  # RUN GSEA MORE
  
  observeEvent(input$runGSEA_own, {
    
    annotat = annot()
    
    rpc = result_rpc()
    reg1 <- RegulationInCondition(rpc, cond = input$gsea_group1_own)
    reg2 <- if (input$gsea_group2_own != "NULL") RegulationInCondition(rpc, cond = input$gsea_group2_own) else NULL
    
    print(str(reg1))
    print(str(reg2))
    
    gsea_out <- tryCatch({
      gseaMORE(reg1, reg2,  annotation = annotat, p.adjust.method = 'none', alpha = 1)
    }, error = function(e) {
      showNotification(paste("Error in GSEA:", e$message), type = "error")
      return(NULL)
    })
    
    if (!is.null(gsea_out)) {
      result_gsea(gsea_out)
    }
    
  })
  
  # GSEA PLOT
  
  output$gsea_plot_own <- renderPlot({
    req(result_gsea())  # Aquí va con paréntesis
    
    gsea_out <- result_gsea()
    
    clusterProfiler::dotplot(gsea_out, split = ".sign") +
      facet_grid(. ~ .sign, labeller = as_labeller(c(
        activated = input$gsea_group2_own,
        suppressed = input$gsea_group1_own
      ))) +
      theme(
        axis.text.x = element_text(size = 8),
        axis.text.y = element_text(size = 4),
        strip.text = element_text(size = 10),
        legend.text = element_text(size = 8),
        legend.title = element_text(size = 9)
      ) +
      scale_y_discrete(labels = gsea_out@result$ID)
  })
  
  
  
  
  
  
 

}

shinyApp(ui, server)