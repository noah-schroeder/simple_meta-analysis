
library(shiny)
library(shinydashboard)

# Define UI ---- 
ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      tags$img(src = "bird.png", style = "height:3em; vertical-align:middle; padding-right:1px;"),
      "Simple Meta-Analysis"
    ),
    titleWidth = "300px"  # Adjust the width as needed
  ),
  dashboardSidebar(width = 300,
                   sidebarMenu(
                     menuItem("Welcome!", tabName = "tab1", icon = icon("dashboard"),
                              menuSubItem("Welcome!", tabName = "subtab10"),
                              menuSubItem("Need Help?", tabName = "subtab11")
                     ),
                     menuItem("Effect Size Calculator", tabName = "tab2", icon = icon("chart-line"),
                              menuSubItem("Standardized Mean Difference", tabName = "subtab31")
                              ),
                     menuItem("Conventional Meta-Analysis", tabName = "tab3", icon = icon("table"),
                              menuSubItem("Start Here: Data Formatting", tabName = "subtab30"),
                              menuSubItem("Step 1: Run the Analysis", tabName = "subtab32"),
                              menuSubItem("Step 2: Check for Outliers and Influence", tabName = "subtab33"),
                              menuSubItem("Step 3a: Categorical Moderator Analysis", tabName = "subtab34"),
                              menuSubItem("Step 3b: Continuous Moderator Analysis", tabName = "subtab341"),
                              menuSubItem("Option: Multiple Meta-Regression", tabName = "subtab342"),
                              menuSubItem("Step 4: Publication Bias", tabName = "subtab35")
                     ),
                     menuItem("Three-Level Meta-Analysis", tabName = "tab4", icon = icon("table"),
                              menuSubItem("Start Here: Data Formatting", tabName = "subtab401"),
                              menuSubItem("Step 1: Run the Analysis", tabName = "subtab42"),
                              menuSubItem("Step 2: Explain the Variance", tabName = "subtab43"),
                              menuSubItem("Step 3: Check for Outliers and Influence", tabName = "subtab44"),
                              menuSubItem("Step 4a: Categorical Moderator Analysis", tabName = "subtab45"),
                              menuSubItem("Step 4b: Continuous Moderator Analysis", tabName = "subtab451"),
                              menuSubItem("Step 5: Publication Bias and Plots", tabName = "subtab46")
                     ),
                     menuItem("Three-Level Meta-Analysis with CHE RVE", tabName = "tab5", icon = icon("table"),
                              menuSubItem("Start Here: Data Formatting", tabName = "subtab51"),
                              menuSubItem("Step 1: Run the Analysis", tabName = "subtab52"),
                              menuSubItem("Step 2: Explain the Variance", tabName = "subtab53"),
                              menuSubItem("Step 3: Check for Outliers and Influence", tabName = "subtab54"),
                              menuSubItem("Step 4a: Categorical Moderator Analysis", tabName = "subtab55"),
                              menuSubItem("Step 4b: Continuous Moderator Analysis", tabName = "subtab56"),
                              menuSubItem("Step 5: Publication Bias and Plots", tabName = "subtab57")
                     ),
                     menuItem("Cite This Software", tabName = "tab6", icon = icon("table"),
                              menuSubItem("Cite This Software", tabName = "subtab61")
                     ),
                     menuItem("References", tabName = "tab7", icon = icon("table"),
                              menuSubItem("Reference List", tabName = "subtab71"),
                              menuSubItem("Underlying R Code", tabName = "subtab12")
                     ),
                     menuItem("Validation Evidence", tabName = "tab8", icon = icon("table"),
                              menuSubItem("Conventional Meta-Analysis", tabName = "subtab81"),
                              menuSubItem("Three-Level Meta-Analysis", tabName = "subtab82"),
                              menuSubItem("Three-Level Meta-Analysis with CHE RVE", tabName = "subtab83")
                     ),
                     menuItem("Acknowledgements", tabName = "tab9", icon = icon("table"),
                              menuSubItem("Acknowledgements", tabName = "subtab91")
                     ),
                     menuItem("Change Log", tabName = "tab10", icon = icon("table"),
                              menuSubItem("Change Log", tabName = "subtab101")
                     )
                   )
  ),
  dashboardBody(
    tags$head(
      tags$style(HTML(".scrollable {overflow-x: auto; }")),
      tags$style(HTML("
      .btn-primary {
        color: #fff;
        background-color: #007BFF;
        border-color: #007BFF;
        padding: 10px 20px;
        text-align: center;
        text-decoration: none;
        display: inline-block;
        font-size: 16px;
        margin: 4px 2px;
        cursor: pointer;
        border-radius: 8px;
      }
      .btn-primary:hover {
        background-color: #0056b3;
        border-color: #0056b3;
      }
    ")),
      ),
    # Main content area
    tabItems(
  ################# Define content for Welcome sub-tabs
      tabItem(tabName = "subtab10",
              h2("Welcome!"),
              p("Welcome to Simple Meta-Analysis, a software program designed to enable the non-R-coding meta-analyst to use meta-analytical techniques that do not (at the time of writing) exist in any GUI-based software. This software can help you calculate standardized mean difference effect sizes, run random effects conventional meta-analysis, random effects three-level meta-analysis, and random effects three-level meta-analysis with correlated and hierarchical robust variance estimation."),
              p("This app is primarily built upon the metafor package for R, as well as the clubSandwich package. The goal was to help make these packages accessible to the non-coding meta-analyst. Please note that the packages are capable of much more than only the functionality built into this app, however I have tried to focus the app on the techniques that most meta-analysts I know are using (or should be using)."),
              box(title = "Please Note", width = 12, status = "primary",
                  p("This app was built to use standardized mean difference effect sizes, as they are quite common in many fields. Other effect sizes could work (other than the effect size calculator) because the metafor code for rma functions does not specify the type of effect size. However, only SMD has been validated in the app.")),
              h2("Please Consider Supporting This Software"),
              p("If you found this software helpful, please consider donating to keep the software going."),
              tags$a(href = "https://www.paypal.com/donate/?hosted_button_id=WPLT2F5FLZ6B6", target = "_blank", class = "btn-primary", "Donate and Support This Software"),
              p("If you are running the software locally instead of using the web version, the link to donate is: https://www.paypal.com/donate/?hosted_button_id=WPLT2F5FLZ6B6"),
              h2("Please Cite This Software"),
              p("If you use this software, please be sure to cite the software:"),
              box(title = "Citation", width = 12, status = "primary",
                  p("Schroeder, N. L. (2024). Simple meta-analysis. Available from https://github.com/noah-schroeder/simple_meta-analysis/")),
      ), 
      tabItem(tabName = "subtab11",
              h2("Need Help Learning Meta-Analysis?"),
              p("If you want help learning meta-analysis or interpreting the results presented by this app, please see ", HTML("<a href='https://noah-schroeder.github.io/reviewbook/'>my open book</a>"), "."),
              ),
      tabItem(tabName = "subtab12",
              h2("Underlying R Code"),
              p("The app code repo is located at", HTML("<a href='https://github.com/noah-schroeder/simple_meta-analysis'>github</a>"),"."),
      ),
      tabItem(tabName = "subtab61",
              h2("Please Cite This Software"),
              p("If you use this software, please be sure to cite the software:"),
              box(title = "Citation", width = 12, status = "primary",
                  p("Schroeder, N. L. (2024). Simple meta-analysis. Available from https://github.com/noah-schroeder/simple_meta-analysis/")),
      ),

  ### Conventional MA sub-tabs----
      tabItem(tabName = "subtab30",
              h2("Data Formatting"),
              h4("General Considerations"),
              p(" In conventional meta-analysis, a very important limitation is known as the principle of statistical independence, meaning that each participant can only be counted once. What does this mean for you as a researcher?"),
              p("Let’s look at an example: Say you are comparing the impact of a computer on learners’ reading proficiency compared to other media. You found a study that meets your inclusion criteria but it has three groups: a computer group, a tablet group, and a paper book group. You can see you have two possible comparisons here: computer vs tablet, and computer vs paper book. You may think that you can include both of these comparisons in your conventional meta-analysis. However, this is incorrect. Doing this would count the computer group twice, therefore violating the principle of statistical independence. As such, you must make a decision: which comparison do you want to include? Alternatively, you could (but I wouldn’t) take the weighted mean and pooled standard deviation of the two non-computer groups to create a comparison that does not duplicate the computer group’s scores. I say I would not do this latter approach because it will add conflating factors into your analysis. Remember - a meta-analysis is only as useful as the types of data that went into it!"),
              box(title = "Practical Requirements and Recommendations", width = 12, status = "primary", 
              p("1. Each row should be an independent comparison."),
              p("2. No participant should be counted twice."),
              p("3. In the data file, your effect size column must be labeled yi and the variance must be labeled vi. Without these labels, the software will not work. If you used the effect size software in this app and downloaded the output file, your data file has these columns.
                ")),
              h2("Sample Data Table"),
              p("Note that the ES_number column is not necessary for conventional meta-analysis. However, you should have a Study column with Author names, a yi column (effect size), and a vi column (effect size variance)."),
              div(class = "scrollable", tableOutput("sampledata1")),
  ),    
       tabItem(tabName = "subtab31",
              h2("Calculating effect sizes"), p("In order for this app to calcuate your standardized mean difference effect size (Hedge's g) for each comparison, there are two requirements."),
              h3("Data File Requirements"),
              p("1. Your file must be in .csv format."),
              p("2. Your data must be organized in a specific way. You must have the treatment mean, treatment standard deviation, treatment sample size, control mean, control standard deviation, and control sample size each in their own columns."),
              box(title = "The respective columns must be labeled as follows:", width = 12, status = "primary", 
              p("Treatment mean must be labeled", strong("Exp_mean")),
              p("Treatment standard deviation must be labeled", strong("Exp_sd")),
              p("Treatment sample size must be labeled", strong("Exp_n")),
              p("Control mean must be labeled", strong("Ctrl_mean")),
              p("Control standard deviation must be labeled", strong("Ctrl_sd")),
              p("Control sample size must be labeled", strong("Ctrl_n"))),
              h4("Sample Data File"),
              div(class = "scrollable", tableOutput("sampledatatable")),
              p("A sample data file is available at:", HTML("<a href='https://github.com/noah-schroeder/reviewbook/blob/main/360%20sample%20data.csv'>Sample Data File.</a>"), "Be sure to scroll to the right in that file to see the relevant columns."),
              p("When your file is properly formatted, you can upload your file and proceed."),
              fileInput("cesfile", "Data", accept = ".csv"),
              actionButton("calc_es_c", "Calculate Effect Sizes and Variances"),
              uiOutput("dynamicResultses")
              ),
      tabItem(tabName = "subtab32",
              h2("Run the Meta-Analysis"),
              p("Now you are ready to run your conventional meta-analysis. This app will help you run a random effects meta-analysis."),
              h3("Upload Your Dataset"),
              box(title = "Data File Requirements", width = 12, status = "primary",            
                  p("If you used the effect size calculator in this app, simply upload the file the calculator generated for you."),
                  p("If you did not use this app to calculate your effect sizes, ensure your effect sizes are in a column labeled yi and the effect size variances are in a column labeled vi. The file must be a .csv.")),
              
              fileInput("cmafilec", "Data", accept = ".csv"),
              
              h3("Run the Meta-Analysis"),
              p("As long as your data uploaded, press the Run Meta-Analysis button to run a random effects meta-analysis."),
              actionButton("run_cmac", "Run Meta-Analysis"),
              uiOutput("dynamicResultscma")
      ),
      tabItem(tabName = "subtab33",
              h2("Check for Outliers and Influence"),
              p("Now you that you ran your meta-analysis, we need to make sure there isn't undue influence or outliers in the data set. We will do that using the influence function in metafor. You should upload the same dataset file as you used to run the meta-analysis."),
              h3("Upload Your Dataset"),
              p("Please use the same file you used to run your meta-analysis."),
              fileInput("inffilec", "Data", accept = ".csv"),
              actionButton("run_infc", "Run Outlier and Influence Analysis"),
              uiOutput("dynamicResultscmainf")
      ),
  tabItem(tabName = "subtab34",
          h2("Categorical Moderating Variables"),
          p("This tool will help you check for", strong("categorical"), "moderating variables. Do not use this tool for continuous variables! The first step is to upload your data. Please use the same file you used to run your meta-analysis."),
          fileInput("modfilec", "Upload Moderator Analysis Data", accept = ".csv"),
          p("Once your file is uploaded, you can choose which column in your spreadsheet you want to examine as a moderator variable. Again,", strong("this is for categorical moderators only."), "After you choose your variable from the dropdown menu, click run and your results will be shown."),
          selectInput("dropdownc", "Choose Column for Moderator Analysis", choices = NULL),
          actionButton("run_analysisCcat", "Run Moderator Analysis"),
          uiOutput("dynamicResultsCcat"),
  ),
  tabItem(tabName = "subtab341",
          h2("Continuous Moderator Analysis"),
          p("Continuous Moderator Analysis (single variable meta-regression) can be used to examine the impact of potentially moderating", strong("continuous"), "variables. Do not use this tool for categorical variables you want examined as categorical! The first step is to upload your data. Please use the same file you used to run your meta-analysis."),
          fileInput("modfilecc", "Upload Meta-Regression Analysis Data", accept = ".csv"),
          p("Once your file is uploaded, you can choose which column in your spreadsheet you want to examine as a moderator variable. Again,", strong("this is for continuous moderators only."), "After you choose your variable from the dropdown menu, click run and your results will be shown."),
          selectInput("dropdowncc", "Choose Column for Moderator Analysis", choices = NULL),
          actionButton("run_analysiscc", "Run Moderator Analysis"),
          uiOutput("dynamicResultsCc")
  ),
  ######Meta-regression----
  tabItem(tabName = "subtab342",
          h2("Multiple Meta-Regression"),
          p("This analysis will help you run a random effects multiple meta-regression using any combination of continuous and categorical variables. The first step is to upload your data. Please use the same file you used to run your meta-analysis."),
          # File upload
          fileInput("mregc", "Upload CSV File", accept = ".csv"),
          # Continuous variables selection
          p("Next, specify how many continuous and categorical variables you would like to include in your model. You can select 0-20 of each type of variable if your dataset supports it."),
          numericInput("num_continuous", "How many continuous variables?",
                       min = 0, max = 20, value = 0),
          # Categorical variables selection
          numericInput("num_categorical", "How many categorical variables?",
                       min = 0, max = 20, value = 0),
          # Render variables button
          actionButton("render_variables", "Select My Variables"),
          uiOutput("dynamicResultsCmregVar"),
          uiOutput("dynamicResultsCmregRes"),
          
  ),
   tabItem(tabName = "subtab35",
              h2("Publication Bias"),
              p("There are a variety of ways to evaluate publication bias. This tool provides a number of computational and graphic options."),
              h3("Upload Your Data"),
              p("First you need to upload your data. Please use the same file you used to run your meta-analysis."),
              fileInput("pubbiasfilec", "Upload Data", accept = ".csv"),
              actionButton("run_pubc", "Run Publication Bias Analyses"),
              uiOutput("dynamicResultsCpub"),
      ),
  
  ### 3LMA MA sub-tabs ----
  tabItem(tabName = "subtab401",
          h2("General Considerations"),
          p("In conventional meta-analysis each participant can only be counted once. That means we exclude A LOT of data when we use conventional meta-analysis in many education fields. We can use three-level meta-analysis to get around that because it allows us to use dependent data in our analysis. Let’s look at an example: Say you are comparing the impact of learning from a virtual character to a game on learning outcomes. The study you’re coding has two groups, a virtual character group and a game group. It has an immediate learning test, a one week delayed learning test, and a month delayed learning test. Which test do you code? In a three-level meta-analysis, you can code all three! This calculator will help you run a random effects three-level meta-analysis."),
          box(title = "Data Formatting Requirements", width = 12, status = "primary", 
          p("To run a three-level meta-analysis with this app, you need to have your data file organized in a certain fashion. Specifically, you need to have the following columns:"),
          p("Each row should be a comparison."),
          p("Comparisons do not necessarily need to be independent (if they they are not independent, three-level meta-analysis with CHE and RVE may be more appropriate)."),
          p("You should have a column labeled", strong("ES_number"), "which sequentially numbers every row."),
          p("You should have a column labeled", strong("Study"), "which is the name of each study."),
          p("The effect size column must be labeled", strong("yi")),
          p("The effect size variance column must be labeled", strong("vi")),
          p("If you used the effect size calculator in this app, then yi and vi columns should exist in the data file it generated for you.")),
          h2("What if I already calculated effect sizes?"),
          p("If you already calculated your effect sizes outside of this app, you will also need the variance. The columns will need to be labeled", strong("yi"), "and",strong("vi"), "respectively. I find it easier to code the descriptive statistics (mean, standard deviation, sample size) for each group and then use the effect size calculator to calculate my effect sizes and variances. So in the example below, you will see only the descriptive statistics in the table."),
          h4("Sample Data File"),
          p("In the table below you will see the columns as indicated above. The additional columns are moderator variables. The order of your columns does not matter, only the titles of the columns listed above. Your moderators can have any name, but I recommend having no spaces in the name."),
          div(class = "scrollable", tableOutput("sampledatatable3lma")),
  ),    
  tabItem(tabName = "subtab41",
              h1("Calculating effect sizes"), p("In order for this app to calcuate your standardized mean difference effect size (Hedge's g) for each comparison, there are two requirements."),
              h2("Data File Requirements"),
              p("1. Your file must be in .csv format."),
              p("2. Your data must be organized in a specific way. "),
              h3("Your data file must contain columns labeled as follows:"),
              p("You should have a column labeled", strong("ES_number"), "which sequentially numbers every row."),
              p("You should have a column labeled", strong("Study"), "which is the name of each study."),
              p("Treatment mean must be labeled", strong("intmean")),
              p("Treatment standard deviation must be labeled", strong("intsd")),
              p("Treatment sample size must be labeled", strong("intn")),
              p("Control mean must be labeled", strong("cmean")),
              p("Control standard deviation must be labeled", strong("csd")),
              p("Control sample size must be labeled", strong("cn")),
              h3("Sample Data File"),
              p("A sample data file is available at:", HTML("<a href='https://github.com/noah-schroeder/reviewbook/blob/abfdb439ef81267b388ef75067a03262e1e59020/360%20sample%20data.csv'>Sample Data File</a>")),
              p("When your file is properly formatted, you can upload your file and proceed."),
              fileInput("file", "Data", accept = ".csv"),
              actionButton("calc_es", "Calculate Effect Sizes and Variances"),
              conditionalPanel(
                condition = "input.calc_es > 0",
                h2("Understand the results"),
                p("Your results appear below. You will see your entire data file, but at the end are appended columns yi and vi, which are your effect size (Hedge's g) and variance, respectively. You are now ready to move forward to data analysis."), strong("You must download the result of this analysis to use for the rest of your analysis. Simply click 'download data' and save the file, then proceed to the next step."),
                downloadButton("download_button", label = "Download Data"),
                verbatimTextOutput("esresult_output")
              ),
      ),
      tabItem(tabName = "subtab42",
              h2("Run the Meta-Analysis"),
              p("Now you are ready to run your three-level meta-analysis. This app will help you run a random effects meta-analysis."),
              h3("Upload Your Dataset"),
              p("If you used this app to calculate your effect sizes and variance, simply upload the file the app generated for you, assuming you have all the required Study and ES_number columns (see the Start Here: Data Formatting tab for this analysis). If you did not use this app's effect size calculator, ensure your effect sizes (Hedges' g) are in a column labeled yi and the effect size variances are in a column labeled vi. The file must be a .csv."),
              fileInput("cmafile", "Data", accept = ".csv"),
              
              h3("Run the Meta-Analysis"),
              p("As long as your data uploaded, press the Run Meta-Analysis button to run a random effects meta-analysis."),
              actionButton("run_cma", "Run Meta-Analysis"),
              uiOutput("dynamicResults3lmaoverall"),
      ),
      tabItem(tabName = "subtab43",
              h2("Explaining the Variance"),
              p("Before moving forward, it is important to understand the variance within your three-level meta-analysis. Let's explore that by calculating I", HTML("<sup>2</sup>.")),
              h3("Upload Your Dataset"),
              p("You should use the same data file you used to run the meta-analysis. The file must be a .csv."),
              fileInput("i2file", "Data", accept = ".csv"),
              
              h3("Calculate I2"),
              p("As long as your data uploaded, press the Calculate I2", "button to examine where the variance in your model can be attributed to."),
              actionButton("run_i2", "Calculate I2"),
              uiOutput("dynamicResults3lmai2"),
      ),
  tabItem(tabName = "subtab44",
          h2("Check for Outliers and Influence"),
          p("Now you that you ran your meta-analysis, we need to make sure there isn't undue influence or outliers in the data set. We will do that using the van Lissa's (n.d.) method of checking for outliers, and examining the Cook's Distance, DFBETAs, and hat values for influence."),
          h3("Upload Your Dataset"),
          p("You should use the same data file you used to run the meta-analysis. The file must be a .csv."),
          fileInput("inffile", "Data", accept = ".csv"),
          actionButton("run_inf", "Run Outlier and Influence Analysis"),
          conditionalPanel(
            condition = "input.run_inf > 0",
            box( title = "Important Note", width = 12, status = "primary",
                 p("The results may take a minute (literally a minute) to load. Please be patient. If you're seeing this message, the app is processing the analyses. There is a progress bar in the bottom right corner of the screen. The DFBETAS analysis typically takes the longest to run."))),
          uiOutput("dynamicResults3lmaout"),
  ),
      tabItem(tabName = "subtab45",
              h2("Categorical Moderating Variables"),
              p("This tool will help you check for", strong("categorical"), "moderating variables. Do not use this tool for continuous variables! The first step is to upload your data. This should be the same data you used to run the meta-analysis."),
              fileInput("modfile", "Upload Moderator Analysis Data", accept = ".csv"),
              p("Once your file is uploaded, you can choose which column in your spreadsheet you want to examine as a moderator variable. Again,", strong("this is for categorical moderators only."), "After you choose your variable from the dropdown menu, click run and your results will be shown."),
              selectInput("dropdown", "Choose Column for Moderator Analysis", choices = NULL),
              actionButton("run_analysis", "Run Moderator Analysis"),
              uiOutput("dynamicResults3lCcat")
      ),
  tabItem(tabName = "subtab451",
          h2("Continuous Moderator Analysis"),
          p("This tool will help you check for", strong("continuous"), "moderating variables (single variable meta-regression). Do not use this tool for categorical variables you wish to examine by category! The first step is to upload your data. This should be the same data you used to run the meta-analysis."),
          fileInput("modfilea", "Upload Moderator Analysis Data", accept = ".csv"),
          p("Once your file is uploaded, you can choose which column in your spreadsheet you want to examine as a moderator variable. Again,", strong("this is for continuous moderators only."), "After you choose your variable from the dropdown menu, click run and your results will be shown."),
          selectInput("dropdowna", "Choose Column for Moderator Analysis", choices = NULL),
          actionButton("run_analysisa", "Run Moderator Analysis"),
          uiOutput("dynamicResults3lCcont")
  ),
      tabItem(tabName = "subtab46",
              h2("Publication Bias"),
              p("There are a variety of ways to evaluate publication bias. This tool provides a number of computational and graphic options."),
              h3("Upload Your Data"),
              p("First you need to upload your data. This is the same data file you used for the overall meta-analysis and moderator analysis. Only .csv files are accepted."),
              fileInput("pubbiasfile", "Upload Data", accept = ".csv"),
              actionButton("run_pub", "Run Publication Bias Analyses"),
              uiOutput("dynamicResults3lpub"),
      ),
### CHERVE 3LMA Subtabs ---- 
      tabItem(tabName = "subtab51",
              h2("General Considerations"),
              p("In conventional meta-analysis each participant can only be counted once. That means we exclude A LOT of data when we use conventional meta-analysis in many education fields. We can use three-level meta-analysis with correlated and hierarchical effects and robust variance estimation to get around that because it allows us to use dependent data in our analysis. Let’s look at an example: Say you are comparing the impact of learning from a virtual character to a game on learning outcomes. The study you’re coding has two groups, a virtual character group and a game group. It has an immediate learning test, a one week delayed learning test, and a month delayed learning test. Which test do you code? In a three-level meta-analysis, you can code all three! With CHE RVE, we can also make assumptions about the correlation between these tests, leading to a more precise estimate. This calculator will help you run a random effects three-level meta-analysis with CHE RVE."),
              box(title = "Data Formatting Requirements", width = 12, status = "primary", 
                  p("To run a three-level meta-analysis with CHE and RVE with this app, you need to have your data file organized in a certain fashion. Specifically, you need to have the following columns:"),
                  p("Each row should be a comparison."),
                  p("Comparisons do not necessarily need to be independent."),
                  p("You should have a column labeled", strong("ES_number"), "which sequentially numbers every row."),
                  p("You should have a column labeled", strong("Study"), "which is the name of each study."),
                  p("The effect size column must be labeled", strong("yi")),
                  p("The effect size variance column must be labeled", strong("vi")),
                  p("If you used the effect size calculator in this app, then yi and vi columns should exist in the data file it generated for you.")),
              h2("What if I already calculated effect sizes?"),
              p("If you already calculated your effect sizes outside of this app, you will also need the variance. The columns will need to be labeled", strong("yi"), "and",strong("vi"), "respectively. I find it easier to code the descriptive statistics (mean, standard deviation, sample size) for each group and then use the effect size calculator to calculate my effect sizes and variances. So in the example below, you will see only the descriptive statistics in the table."),
              h4("Sample Data File"),
              p("In the table below you will see the columns as indicated above. The additional columns are moderator variables. The order of your columns does not matter, only the titles of the columns listed above. Your moderators can have any name, but I recommend having no spaces in the name."),
              div(class = "scrollable", tableOutput("sampledatatable")),
      ),
  tabItem(tabName = "subtab52",
          # Header and paragraph for data upload
          h2("Upload Your Data"),
          p("First you need to upload your data. This is the same data file you used for the overall meta-analysis and moderator analysis. Only .csv files are accepted."),
          fileInput("chefile", "Upload Data", accept = ".csv"),
          
          
          # Header and paragraph for correlation setting
          h2("Set The Correlation"),
          p("You must select the correlation between effects to be assumed in the analysis. If you do not know the actual correlation, you may find it necessary to make an assumption about the correlation between effects. For example, one may assume rho = .60 if they have reason to believe there is a large, positive correlation between effects. In these cases when rho is not exactly known, sensitivity analyses are recommended. The software will automatically run sensitivity analyses with rho vaules +.20 and -.20 higher than you select."),
          
          # Dropdown for selecting the correlation
          h4("Correlation to be assumed"),
          selectInput("correlation", label = NULL, choices = seq(-1, 1, by = 0.01), selected = 0.60),
          actionButton("run_che", "Run Three-level CHE RVE Meta-Analysis"),
          uiOutput("dynamicResultsche"),
     ),
  tabItem(tabName = "subtab53",
          h2("Explaining the Variance"),
          p("Before moving forward, it is important to understand the variance within your three-level CHE RVE meta-analysis. Let's explore that by calculating I", HTML("<sup>2</sup>.")),
          h3("Upload Your Dataset"),
          p("You should use the same data file you used to run the meta-analysis. The file must be a .csv."),
          fileInput("i2fileRVE", "Data", accept = ".csv"),
          h3("Set The Correlation"),
          p("You should assume the same correlation that you set for your overall meta-analysis."),
          selectInput("correlationi2", "Set rho value:", choices = seq(-1, 1, by = 0.01), selected = 0.60),
          h3("Calculate I2"),
          p("As long as your data uploaded, press the Calculate I2", "button to examine where the variance in your model can be attributed to."),
          actionButton("run_i2RVE", "Calculate I2"),
          uiOutput("dynamicResultschevar")
  ),
  tabItem(tabName = "subtab54",
          h2("Check for Outliers and Influence"),
          p("Now you that you ran your meta-analysis, we need to make sure there isn't undue influence or outliers in the data set. We will do that using the van Lissa's (n.d.) method of checking for outliers, and examining the Cook's Distance, DFBETAs, and hat values for influence."),
          h3("Upload Your Dataset"),
          p("You should use the same data file you used to run the meta-analysis. The file must be a .csv."),
          fileInput("inffilerve", "Data", accept = ".csv"),
          
          # Header and paragraph for correlation setting
          h3("Correlation to be assumed"),
          p("You should assume the same correlation that you set for your overall meta-analysis."),
          selectInput("correlationrve", "Set rho value:", choices = seq(-1, 1, by = 0.01), selected = 0.60),
          # Button to run analysis
          actionButton("run_infrve", "Run Outlier and Influence Analysis"),
          conditionalPanel(
            condition = "input.run_infrve > 0",
            box(title = "Important Note", width = 12, status = "primary", 
                p("The results may take a minute (literally a minute) to load. Please be patient. If you're seeing this message, the app is processing the analyses. There is a progress bar in the bottom right corner of the screen. The DFBETAS analysis typically takes the longest to run."))),
          conditionalPanel(
            condition = "output.progressActiverve",
            uiOutput("progressrve"),
          ),
          uiOutput("dynamicResultsinf"),  
  ),
  tabItem(tabName = "subtab55",
          # Header and paragraph for data upload
          h2(" Categorical Moderating Variables"),
          p("This tool will help you check for", strong("categorical"), "moderating variables. Do not use this tool for continuous variables! The first step is to upload your data. This should be the same data you used to run the meta-analysis."),
          h3("Upload Your Data"),
          p("First you need to upload your data. This is the same data file you used for the overall meta-analysis and moderator analysis. Only .csv files are accepted."),
          fileInput("chefileCat", "Upload Data", accept = ".csv"),
          
          
          # Header and paragraph for correlation setting
          h3("Correlation to be assumed"),
          p("You should assume the same correlation that you set for your overall meta-analysis."),
          selectInput("correlationCat", "Set rho value:", choices = seq(-1, 1, by = 0.01), selected = 0.60),
          
          #choose Moderator
          h3("Choose the Categorical Moderator"),
          selectInput("mod_RVECat", "Select Moderator Variable:", choices = NULL),
          # Button to run analysis
          actionButton("run_cheCat", "Run Analysis", icon = icon("play")),
          uiOutput("dynamicResults"),
  ),
tabItem(tabName = "subtab56",
        # Header and paragraph for data upload
        h2("Continuous Moderator Analysis"),
        p("This tool will help you check for", strong("continuous"), "moderating variables (single variable meta-regression). Do not use this tool for categorical variables you wish to examine by category! The first step is to upload your data. This should be the same data you used to run the meta-analysis."),
        h3("Upload Your Data"),
        p("First you need to upload your data. This is the same data file you used for the overall meta-analysis and moderator analysis. Only .csv files are accepted."),
        fileInput("chefileCont", "Upload Data", accept = ".csv"),
        
        
        # Header and paragraph for correlation setting
        h3("Correlation to be assumed"),
        p("You should assume the same correlation that you set for your overall meta-analysis."),
        selectInput("correlationCont", "Set rho value:", choices = seq(-1, 1, by = 0.01), selected = 0.60),
        
        #choose Moderator
        h3("Choose the Continuous Moderator"),
        selectInput("mod_RVECont", "Select Moderator Variable:", choices = NULL),
        # Button to run analysis
        actionButton("run_cheCont", "Run Analysis", icon = icon("play")),
        uiOutput("dynamicResultsCont"),
),
  tabItem(tabName = "subtab57",
          h3("Upload Your Data"),
          p("First you need to upload your data. This is the same data file you used for the overall meta-analysis and moderator analysis. Only .csv files are accepted."),
          fileInput("chefileplot", "Upload Data", accept = ".csv"),
          
          
          # Header and paragraph for correlation setting
          h3("Correlation to be assumed"),
          p("You should assume the same correlation that you set for your overall meta-analysis."),
          selectInput("correlationplot", "Set rho value:", choices = seq(-1, 1, by = 0.01), selected = 0.60),
          
          # Button to run analysis
          actionButton("run_cheplot", "Create Plots", icon = icon("play")),
          uiOutput("dynamicResultcheplot"),
  ),

### References----
      tabItem(tabName = "subtab71",
              h2("References"),
              p("Chang, W, Cheng, J, Allaire, J, Sievert, C, Schloerke, B, Xie, Y, Allen, J, McPherson, J, Dipert, A, Borges, B. (2023). _shiny: Web Application Framework for R_. R package version 1.8.0, <https://CRAN.R-project.org/package=shiny>."),
              p("Cheung, M. W. L. (2015). Meta-analysis: A structural equation modeling approach. Wiley Interdisciplinary Reviews: Computational Statistics, 7(3), 149-161. doi:10.1002/wics.1340"),
              p("Dowle, M., Srinivasan, A., Gorecki, J., & Chirico, M. (2021). data.table: Extension of 'data.frame'. R package version 1.14.2. https://CRAN.R-project.org/package=data.table"),
              p("Fernández-Castilla, B., Declercq, L., Jamshidi, L., Beretvas, S. N., Onghena, P., & Van Den Noortgate, W. (2020). Visual representations of meta-analyses of multiple outcomes: Extensions to forest plots, funnel plots, and caterpillar plots. Methodology, 16(4), 299–315. https://doi.org/10.5964/meth.4013"),
              p("R Core Team (2021). R: A language and environment for statistical computing. R Foundation for Statistical Computing, Vienna, Austria. URL https://www.R-project.org/"),
              p("Schroeder, N. L. (2024). A beginner’s guide to systematic review and meta-analysis. Available at https://noah-schroeder.github.io/reviewbook/"),
              p("Slowikowski, K. (2020). ggrepel: Automatically position non-overlapping text labels with 'ggplot2'. R package version 0.9.1. https://CRAN.R-project.org/package=ggrepel"),
              p("Viechtbauer, W. (2010). Conducting meta-analyses in R with the metafor package. Journal of Statistical Software, 36(3), 1-48. https://doi.org/10.18637/jss.v036.i03"),
              p("Wickham, H. (2011). The split-apply-combine strategy for data analysis. Journal of Statistical Software, 40(1), 1-29. Retrieved from https://www.jstatsoft.org/v40/i01/"),
              p("Wickham, H., Averick, M., Bryan, J., Chang, W., McGowan, L. D. A., François, R., ... & Yutani, H. (2019). Welcome to the Tidyverse. Journal of open source software, 4(43), 1686."),
              p("Wickham, H., François, R., Henry, L., & Müller, K. (2021). dplyr: A grammar of data manipulation. R package version 1.0.8. https://CRAN.R-project.org/package=dplyr")
              
      ),
##Validation ----
tabItem(tabName = "subtab81",
        h2("Conventional Meta-Analysis"),
        h3("R Code"),
        p("Below is R code that will provide the same results as the analyses on the respective pages for conventional meta-analysis models."),
        htmlOutput("ccode"),
        h3("Video Walk-Through"),
        p("You can find a video walkthrough at this link:", a("Video Walk-Through", href = "https://www.youtube.com/watch?v=w2xFzp21nks", target = "_blank")),
        p("The R code file to repeat the analyses in the video is located", a("at github", href = "https://github.com/noah-schroeder/simple_meta-analysis", target = "_blank")),
        
),
tabItem(tabName = "subtab82",
        h2("Three-level Meta-Analysis"),
        h3("R Code"),
        p("Below is R code that will provide the same results as the analyses on the respective pages for conventional meta-analysis models."),
        htmlOutput("threecode"),
        h3("Video Walk-Through"),
        p("You can find a video walkthrough at this link:", a("Video Walk-Through", href = "https://youtu.be/LQVLtBIHut0", target = "_blank")),
        p("The R code to repeat the analyses in the video is located", a("at github", href = "https://github.com/noah-schroeder/simple_meta-analysis", target = "_blank")),
        
),
tabItem(tabName = "subtab83",
        h2("Three-level Meta-Analysis with CHE and RVE"),
        h3("R Code"),
        p("Below is R code that will provide the same results as the analyses on the respective pages for conventional meta-analysis models."),
        htmlOutput("chervecode"),
        h3("Video Walk-Through"),
        p("You can find a video walkthrough at this link:", a("Video Walk-Through", href = "https://youtu.be/8XN2_-E4hpg", target = "_blank")),
        p("The R code to repeat the analyses in the video is located", a("at github", href = "https://github.com/noah-schroeder/simple_meta-analysis", target = "_blank")),
        
),

  ################# Define content for Acknowledgments sub-tabs
   tabItem(tabName = "subtab91",
          h2("Acknowledgements"),
          p("I was not famililar with Shiny when I began building this app, so a lot of the code was created with the assistance of ChatGPT 3.5 and Claude."),
      ),
################# Define content for Change log 
##Change Log---- 
tabItem(tabName = "subtab101",
        h2("Change Log"),
        p("7.14.24 - Added residual heterogeneity to conventional meta-analysis for categorical and continuous moderator analyses. Updated those tables for consistent headings with other analyses. Updated p value display for those analyses so that instead of p = .001, it will display p < .001 if its less."),
)
    )
  )
)
  
  
  
  



















  

# Main Panel

# Functions ----

#' Calculate I-squared values and variance distribution for multilevel meta-analysis models
#' adapted from Harrer et al. to show 'within cluster' and 'between cluster' rather than level 2 and level 3, respectively.
#'
#' This function calculates values of \eqn{I^2} and the variance distribution for multilevel meta-analysis
#' models fitted with \code{\link[metafor]{rma.mv}}.
#'
#'
#' @usage mlm.variance.distribution(x)
#'
#' @param x An object of class \code{rma.mv}. Must be a multilevel model with two random effects (three-level meta-analysis model).
#'
#' @details This function estimates the distribution of variance in a three-level meta-analysis
#' model (fitted with the \code{\link[metafor]{rma.mv}} function). The share of variance attributable to
#' sampling error, within and between-cluster heterogeneity is calculated,
#' and an estimate of \eqn{I^2} (total and for Level 2 and Level 3) is provided. The function uses the formula by
#' Cheung (2014) to estimate the variance proportions attributable to each model component and to derive the \eqn{I^2} estimates.
#'
#'
#' @references
#'
#' Harrer, M., Cuijpers, P., Furukawa, T.A, & Ebert, D. D. (2019).
#' \emph{Doing Meta-Analysis in R: A Hands-on Guide}. DOI: 10.5281/zenodo.2551803. \href{https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/mlma.html}{Chapter 12}.
#'
#'Cheung, M. W. L. (2014). Modeling dependent effect sizes with three-level meta-analyses: a structural equation modeling approach. \emph{Psychological Methods, 19}(2), 211.
#'
#' @author Mathias Harrer & David Daniel Ebert
#'
#' @aliases var.comp
#'
#' @import ggplot2
#' @importFrom stats model.matrix
#'
#' @return Returns a data frame containing the results. A plot summarizing the variance distribution and \eqn{I^2} values can be generated using \code{plot}.
#'
#' @export mlm.variance.distribution
#' @export var.comp
#'
#' @examples
#' # Use dat.konstantopoulos2011 from the "metafor" package
#' library(metafor)
#'
#' # Build Multilevel Model (Three Levels)
#' m = rma.mv(yi, vi, random = ~ 1 | district/school, data=dat.konstantopoulos2011)
#'
#' # Calculate Variance Distribution
#' mlm.variance.distribution(m)
#'
#' # Use alias 'var.comp' and 'Chernobyl' data set
#' data("Chernobyl")
#' m2 = rma.mv(yi = z, V = var.z, data = Chernobyl, random = ~ 1 | author/es.id)
#' res = var.comp(m2)
#'
#' # Print results
#' res
#'
#' # Generate plot
#' plot(res)



mlm.variance.distribution = var.comp = function(x){
  
  m = x
  
  # Check class
  if (!(class(m)[1] %in% c("rma.mv", "rma"))){
    stop("x must be of class 'rma.mv'.")
  }
  
  # Check for three level model
  if (m$sigma2s != 2){
    stop("The model you provided does not seem to be a three-level model. This function can only be used for three-level models.")
  }
  
  # Check for right specification (nested model)
  if (sum(grepl("/", as.character(m$random[[1]]))) < 1){
    stop("Model must contain nested random effects. Did you use the '~ 1 | cluster/effect-within-cluster' notation in 'random'? See ?metafor::rma.mv for more details.")
  }
  
  # Get variance diagonal and calculate total variance
  n = m$k.eff
  vector.inv.var = 1/(diag(m$V))
  sum.inv.var = sum(vector.inv.var)
  sum.sq.inv.var = (sum.inv.var)^2
  vector.inv.var.sq = 1/(diag(m$V)^2)
  sum.inv.var.sq = sum(vector.inv.var.sq)
  num = (n-1)*sum.inv.var
  den = sum.sq.inv.var - sum.inv.var.sq
  est.samp.var = num/den
  
  # Calculate variance proportions
  level1=((est.samp.var)/(m$sigma2[1]+m$sigma2[2]+est.samp.var)*100)
  level2=((m$sigma2[2])/(m$sigma2[1]+m$sigma2[2]+est.samp.var)*100)
  level3=((m$sigma2[1])/(m$sigma2[1]+m$sigma2[2]+est.samp.var)*100)
  
  # Prepare df for return
  Level=c("Sampling Error", "Within clusters (level 2)", "Between clusters (level 3)")
  Variance=c(level1, level2, level3)
  df.res=data.frame(Variance)
  colnames(df.res) = c("% of total variance")
  rownames(df.res) = Level
  I2 = c("---", round(Variance[2:3], 2))
  df.res = as.data.frame(cbind(df.res, I2))
  
  totalI2 = Variance[2] + Variance[3]
  
  
  # Generate plot
  df1 = data.frame("Level" = c("Sampling Error", "Total Heterogeneity"),
                   "Variance" = c(df.res[1,1], df.res[2,1]+df.res[3,1]),
                   "Type" = rep(1,2))
  
  df2 = data.frame("Level" = rownames(df.res),
                   "Variance" = df.res[,1],
                   "Type" = rep(2,3))
  
  df = as.data.frame(rbind(df1, df2))
  
  
  g = ggplot(df, aes(fill=Level, y=Variance, x=as.factor(Type))) +
    coord_cartesian(ylim = c(0,1), clip = "off") +
    geom_bar(stat="identity", position="fill", width = 1, color="black") +
    scale_y_continuous(labels = scales::percent)+
    theme(axis.title.x=element_blank(),
          axis.text.y = element_text(color="black"),
          axis.line.y = element_blank(),
          axis.title.y=element_blank(),
          axis.line.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          axis.ticks.y = element_line(lineend = "round"),
          legend.position = "none",
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.background = element_blank(),
          legend.background = element_rect(linetype="solid",
                                           colour ="black"),
          legend.title = element_blank(),
          legend.key.size = unit(0.75,"cm"),
          axis.ticks.length=unit(.25, "cm"),
          plot.margin = unit(c(1,3,1,1), "lines")) +
    scale_fill_manual(values = c("darkseagreen3", "deepskyblue3", "darkseagreen2",
                                 "deepskyblue1", "deepskyblue2")) +
    
    # Add Annotation
    
    # Total Variance
    annotate("text", x = 1.5, y = 1.05,
             label = paste("Total Variance:",
                           round(m$sigma2[1]+m$sigma2[2]+est.samp.var, 3))) +
    
    # Sampling Error
    annotate("text", x = 1, y = (df[1,2]/2+df[2,2])/100,
             label = paste("Sampling Error Variance: \n", round(est.samp.var, 3)), size = 3) +
    
    # Total I2
    annotate("text", x = 1, y = ((df[2,2])/100)/2-0.02,
             label = bquote("Total"~italic(I)^2*":"~.(round(df[2,2],2))*"%"), size = 3) +
    annotate("text", x = 1, y = ((df[2,2])/100)/2+0.05,
             label = paste("Variance not attributable \n to sampling error: \n", round(m$sigma2[1]+m$sigma2[2],3)), size = 3) +
    
    # Within clusters (level 2)
    annotate("text", x = 2, y = (df[1,2]/2+df[2,2])/100, label = paste("Within clusters (level 2): \n",
                                                                       round(df$Variance[2],2), "%", sep=""), size = 3) +
    
    # Between clusters (level 3)
    annotate("text", x = 2, y = (df[5,2]+(df[4,2]/2))/100,
             label = bquote(Between~clusters~(level~3)^2*":"~.(round(df[4,2],2))*"%"), size = 3)
  
  returnlist = list(results = df.res,
                    totalI2 = totalI2,
                    plot = g)
  class(returnlist) = c("mlm.variance.distribution", "list")
  
  invisible(returnlist)
  
  returnlist
  
}

library(plyr)
library(grid)
library(clubSandwich)
library(metaSEM)
library(ggrepel)
forest_plot_3 <- function(author, study, ES, out, var, se, size_lines){
                            size_lines=size_lines
                            dataset<-data.frame(study, author, ES, out, var, se)
                            row = 1
                            nrow=max(dataset$study)
                            studyn=max(dataset$study)
                            studyinfo = data.frame(Study = numeric(nrow),
                                                   author = numeric(nrow),
                                                   id = numeric(nrow),
                                                   ES= numeric(nrow),
                                                   SE= numeric(nrow),
                                                   Var=numeric(nrow),
                                                   cilb= numeric(nrow),
                                                   ciub= numeric(nrow),
                                                   k= numeric(nrow),
                                                   out=numeric(nrow),
                                                   median_Var=numeric(nrow),
                                                   S_cilb=numeric(nrow),
                                                   S_ciub=numeric(nrow),
                                                   Weight=numeric(nrow))
                            Study1 =c()
                            Study2 =c()
                            dataset$author<-as.character(dataset$author)
                            meta_abu <- summary(meta3(y=ES, v=var, cluster=study, data=dataset))
                            estimate<-round(meta_abu$coefficients$Estimate[1], digits=2)
                            tau<-meta_abu$coefficients$Estimate[3]
                            out<-meta_abu$coefficients$Estimate[2]
                            
                            
                            
                            for (i in 1:max(dataset$study)){
                              data<-subset(dataset, study==i)
                              uni=nrow(data)
                              
                              if (uni==1) {
                                studyinfo$ES[row]<-data$ES
                                studyinfo$SE[row]<-data$se
                                studyinfo$cilb[row]<-(data$ES-(data$se*1.96))
                                studyinfo$ciub[row]<-(data$ES+(data$se*1.96))
                                studyinfo$S_cilb[row]<-(data$ES-(data$se*1.96))
                                studyinfo$S_ciub[row]<-(data$ES+(data$se*1.96))
                                studyinfo$Weight[row]<-1/ (data$se^2)
                              }
                              else {
                                a<-rma(y=data$ES, vi=data$var, data=data, method="REML")
                                
                                diagonal<-1/(data$var+out)
                                D<-diag(diagonal)
                                obs<-nrow(data)
                                I<-matrix(c(rep(1,(obs^2))),nrow=obs)
                                M<-D%*%I%*%D
                                inv_sumVar<-sum(1/(data$var+out))
                                O<-1/((1/tau)+inv_sumVar)
                                V<-D-(O*M)
                                T<-as.matrix(data$ES)
                                X<-matrix(c(rep(1,obs)), ncol=1)
                                var_effect<-solve(t(X)%*%V%*%X)
                                
                                studyinfo$ES[row]<-a$b
                                studyinfo$SE[row]<-a$se
                                studyinfo$cilb[row]<-a$ci.lb
                                studyinfo$ciub[row]<-a$ci.ub
                                studyinfo$S_cilb[row]<-a$b - 1.96*median(data$se)
                                studyinfo$S_ciub[row]<-a$b + 1.96*median(data$se)
                                studyinfo$Weight[row]<-1/ var_effect
                              }
                              
                              studyinfo$Study[row]<-c(Study1,paste("Study",i))
                              studyinfo$id[row]<-i
                              studyinfo$k[row]<-nrow(data)
                              studyinfo$author[row]<-data$author[1]
                              studyinfo$out[row] <- c(Study2, paste("J =",studyinfo$k[i]))
                              studyinfo$median_Var[row]<-median(data$var)
                              studyinfo$Var<-(studyinfo$SE)^2
                              row = row + 1      
                            }
                            
                            
                            minimum<-min(studyinfo$S_cilb)
                            maximum<-max(studyinfo$S_ciub)
                            lim_minimum<-minimum-0.10
                            lim_maximum<-maximum+0.25
                            r_lim_minimum<-round(lim_minimum, digits=0)
                            r_lim_maximum<-round(lim_maximum, digits=0)
                            abs_r_lim_minimum<-abs(r_lim_minimum)
                            abs_r_lim_maximum<-abs(r_lim_maximum)
                            dec_min<-round(abs((lim_minimum-r_lim_minimum)*100), digits=0)
                            dec_max<-round(abs((lim_maximum-r_lim_maximum)*100), digits=0)
                            
                            if (dec_min < 25) {
                              c=25/100
                            } else if (dec_min>25 & dec_min<50) {
                              c=50/100
                            } else if (dec_min>50 & dec_min<75) {
                              c=75/100
                            } else {
                              c=abs_r_lim_minimum+1
                            }
                            
                            if (dec_max < 25) {
                              d=25/100
                            } else if (dec_max>25 & dec_max<50) {
                              d=50/100
                            } else if (dec_max>50 & dec_max<75) {
                              d=75/100
                            } else {
                              d=abs_r_lim_maximum+1
                            }
                            
                            lim_minimum<-r_lim_minimum-c
                            lim_maximum<-r_lim_maximum+d
                            
                            Axis_ES <- seq(lim_minimum, lim_maximum, by=0.50)
                            Axis_ES<-Axis_ES[order(Axis_ES)]
                            empty <- data.frame(id=c(NA,NA), ES=c(NA, NA), cilb=c(NA, NA),ciub=c(NA,NA),
                                                k=c(NA,NA), Study=c(NA,NA), SE=c(NA, NA), 
                                                out=c(NA,NA),median_Var=c(NA,NA), S_cilb=c(NA,NA), S_ciub=c(NA,NA),
                                                Var=c(NA, NA), Weight=c(NA,NA), author=c("","Summary"))
                            
                            studyinfo <- rbind(studyinfo, empty)
                            studyinfo$Study=factor(studyinfo$Study ,levels=unique(studyinfo$Study))
                            studyinfo$author=factor(studyinfo$author ,levels=unique(studyinfo$author))
                            r_diam<-studyn-2
                            sum.y <- c(1, 0.7, 1, 1.3, rep(NA,r_diam )) 
                            sum.x <- c(meta_abu$coefficients$lbound[1], meta_abu$coefficients$Estimate[1], meta_abu$coefficients$ubound[1], meta_abu$coefficients$Estimate[1], rep(NA, r_diam))
                            studyinfo<-data.frame(studyinfo, sum.x, sum.y )
                            studyinfo<-studyinfo[, c(15,16,3,4,5,6,7,8,9,10,11,12,13,14,1,2)]
                            
                            forest<-ggplot()+ geom_point(data=studyinfo, aes(y=factor(author), x = ES, xmin =cilb, xmax = ciub, size=Weight), shape=15) +
                              #scale_size_area()+
                              geom_errorbarh(data=studyinfo, aes(y=factor(author), x = ES, xmin =cilb, xmax = ciub), size=1, height=.2)+
                              scale_x_continuous(limits=c(lim_minimum,lim_maximum),breaks=Axis_ES)+ 
                              scale_y_discrete(limits=rev(levels(studyinfo$author)))+
                              geom_vline(xintercept=0)+
                              theme(panel.grid.major = element_blank(),
                                    panel.grid.minor = element_blank(),
                                    legend.position="none",
                                    panel.background = element_blank(),
                                    axis.line.x = element_line(colour = "black"),
                                    axis.ticks.y =element_blank(),
                                    axis.title.x=element_text(size=10, color ="black",family="sans"),
                                    axis.title.y=element_blank(),
                                    axis.text.y = element_text(family="sans",size=10, color = "black",hjust=0, angle=0),
                                    axis.text.x = element_text(size=10, color="black",family="sans"), 
                                    axis.line.y =element_blank())+
                              labs(x = paste("Pooled Effect Size", estimate), hjust=-2)+
                              geom_polygon(aes(x=sum.x, y=sum.y))+
                              geom_vline(xintercept=estimate, colour="black",linetype=4)+
                              geom_text(aes(x=lim_maximum, y=factor(studyinfo$author),label = studyinfo$out), size=3)
                            
                            if (size_lines==1){
                              
                              forest<-forest+geom_point(data=studyinfo, aes(y=factor(author), x=ES, xmin = S_cilb, xmax =  S_ciub), shape=15)+
                                geom_errorbarh(data=studyinfo, aes(y=factor(author), x=ES, xmin = S_cilb, xmax =  S_ciub, size=k), width=.8,  height=.4, alpha=.2) #Cambiar .3 por .8
                            } else{
                              
                              forest<-forest+geom_point(data=studyinfo, aes(y=factor(author), x=ES, xmin = S_cilb, xmax =  S_ciub), shape=15)+
                                geom_errorbarh(data=studyinfo, aes(y=factor(author), x=ES, xmin = S_cilb, xmax =  S_ciub), width=.8,  height=.4, alpha=.5) #Cambiar .3 por .8
                              
                            }
                            print(forest)
                            
                          }


three_funnel<-function(study, ES, out, var, se){
  
  dataset<-data.frame(study, ES, out, var, se)
  contour.points=200
  meta_abu <- summary(meta3(y=ES, v=var, cluster=study, data=dataset))
  estimate<-meta_abu$coefficients$Estimate[1]
  tau<-meta_abu$coefficients$Estimate[3]
  out<-meta_abu$coefficients$Estimate[2]
  
  maxse<-max(dataset$se)
  ylim<-c(0, maxse)
  csize <- seq(ylim[1], ylim[2], length.out = contour.points)
  csize[csize <= 0] <- 1e-07 * min(dataset$se)
  csize
  
  CI_Lim<-matrix(0, nrow=length(csize), ncol=2)
  colnames(CI_Lim)<-c("lb_total", "ub_total")
  
  for (i in 1:length(csize)){
    CI_Lim[i,1]<-estimate-1.96*sqrt((csize[i]^2)+tau+out) #add 1.96*
    CI_Lim[i,2]<-estimate+1.96*sqrt((csize[i]^2)+tau+out)
  }
  CI_Lim<-as.data.frame(CI_Lim)
  
  dataset$study<-as.character(dataset$study)
  dataset$study <- factor(dataset$study)
  geom.text.size = 3
  max_SE<-max(dataset$se)
  le<-length(CI_Lim[,1])
  
  if ((CI_Lim[le,1])< 0) {
    minimum=min(CI_Lim[,1])
  } else {
    minimum=max(CI_Lim[,1])
  } 
  
  if ((CI_Lim[le,2]) > 0) {
    maximum=max(CI_Lim[,2])
  } else {
    maximum=min(CI_Lim[,2])
  } 
  
  
  lim_minimum<-floor(minimum-0.10)
  lim_maximum<-ceiling(maximum+0.10)
  Axis_ES <- seq(lim_minimum, lim_maximum, by=1)
  
  d <- ggplot(data=dataset, aes(x = se, y = ES, ylim(0,max_SE)))+
    geom_point()+
    xlab('Standard Error')+ 
    ylab('Effect size: g')+
    geom_hline(yintercept= estimate)+
    geom_hline(yintercept= 0, color='grey')+
    scale_x_reverse()+
    scale_y_continuous(breaks=Axis_ES, limits =c(lim_minimum,lim_maximum))+
    coord_flip()+
    theme(panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          panel.background = element_blank(),
          axis.line=element_line(),
          axis.title = element_text(size=14),
          axis.text = element_text(size=12, color="black"),
          text=element_text())
  
  d <- d + geom_line(data=CI_Lim, aes(y=lb_total, x=csize), colour="black")+
    geom_line(data=CI_Lim, aes(y=ub_total, x=csize), colour="black")
  print(d)
}

                          

three_funnel_study<-function(study, ES, out, var, se, size_dots, numbers){
  numbers=numbers
  size_dots=size_dots
  dataset<-data.frame(study, ES, out, var, se)
  contour.points=200
  
  meta_abu <- summary(meta3(y=ES, v=var, cluster=study, data=dataset))
  estimate<-meta_abu$coefficients$Estimate[1]
  tau<-meta_abu$coefficients$Estimate[3]
  out<-meta_abu$coefficients$Estimate[2]
  
  row = 1
  nrow=max(dataset$study)
  studyinfo = data.frame(Study = numeric(nrow),
                         id = numeric(nrow),
                         ES= numeric(nrow),
                         SE= numeric(nrow),
                         k= numeric(nrow),
                         median_SE=numeric(nrow))
  Study1 =c()
  geom.text.size = 3
  
  for (i in 1:max(dataset$study)){
    data<-subset(dataset, study==i)
    uni=nrow(data)
    
    if (uni==1) {
      studyinfo$ES[row]<-data$ES
      studyinfo$SE[row]<-data$se
      studyinfo$median_SE[row]<-data$se
    }
    
    else {
      
      a<-rma(y=data$ES, vi=data$var, data=data, method="REML")
      studyinfo$ES[row]<-a$b
      studyinfo$SE[row]<-a$se
      studyinfo$median_SE[row]<-median(data$se)
    }
    
    studyinfo$id[row]<-i
    studyinfo$k[row]<-nrow(data)
    studyinfo$Study[row]<-c(Study1,paste("Study",i))
    row = row + 1      
  }
  
  median_k<- median(studyinfo$k)
  maxse<-max(studyinfo$SE)
  ylim<-c(0, maxse)
  csize <- seq(ylim[1], ylim[2], length.out = contour.points)
  csize[csize <= 0] <- 1e-07 * min(studyinfo$SE)
  CI_Lim<-matrix(0, nrow=length(csize), ncol=2)
  colnames(CI_Lim)<-c("lb_total", "ub_total")
  
  for (i in 1:length(csize)){
    CI_Lim[i,1]<-estimate-1.96*sqrt((((csize[i]^2)+out)/median_k)+tau)#add 1.96*
    CI_Lim[i,2]<-estimate+1.96*sqrt((((csize[i]^2)+out)/median_k)+tau)
  }
  CI_Lim<-as.data.frame(CI_Lim)
  
  le<-length(CI_Lim[,1])
  
  
  
  if ((CI_Lim[le,1])< 0) {
    minimum=min(CI_Lim[,1])
  } else {
    minimum=max(CI_Lim[,1])
  } 
  
  if ((CI_Lim[le,2]) > 0) {
    maximum=max(CI_Lim[,2])
  } else {
    maximum=min(CI_Lim[,2])
  } 
  
  
  lim_minimum<-floor(minimum-0.10)
  lim_maximum<-ceiling(maximum+0.10)
  Axis_ES <- seq(lim_minimum, lim_maximum, by=1)
  
  if (size_dots==1){
    if(numbers==1){
      e <- ggplot(data=studyinfo, aes(x = SE, y = ES, ylim(0,maxse))) +
        geom_point(data=studyinfo, aes(size=k)) +
        geom_text_repel(aes(label=factor(studyinfo$k)), hjust=0, vjust=-0.40, size=geom.text.size, direction="x", segment.size  = 0.2, segment.color = "grey50")+
        xlab('Meta-analytic standard error') + ylab('Study mean effect')+
        geom_hline(yintercept= estimate)+
        geom_hline(yintercept= 0, color='grey')+
        scale_x_reverse()+
        scale_y_continuous(breaks=Axis_ES , limits =c(lim_minimum,lim_maximum))+
        coord_flip()+
        theme_bw()+
        theme(panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              panel.border=element_blank(),
              panel.background = element_blank(),
              axis.line=element_line(),
              axis.title = element_text(size=14),
              axis.text = element_text(size=12, colour = "black"),
              text=element_text(),
              legend.position="none")
    } else {
      e <- ggplot(data=studyinfo, aes(x = SE, y = ES, ylim(0,maxse))) +
        geom_point(data=studyinfo, aes(size=k)) +
        xlab('Meta-analytic standard error') + ylab('Study mean effect')+
        geom_hline(yintercept= estimate)+
        geom_hline(yintercept= 0, color='grey')+
        scale_x_reverse()+
        scale_y_continuous(breaks=Axis_ES , limits =c(lim_minimum,lim_maximum))+
        coord_flip()+
        theme_bw()+
        theme(panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              panel.border=element_blank(),
              panel.background = element_blank(),
              axis.line=element_line(),
              axis.title = element_text(size=14),
              axis.text = element_text(size=12, colour = "black"),
              text=element_text(),
              legend.position="none")
    }
    
  } else {
    
    if (numbers==1){
      e <- ggplot(data=studyinfo, aes(x = SE, y = ES, ylim(0,maxse))) +
        geom_point() +
        geom_text_repel(aes(label=factor(studyinfo$k)), hjust=0, vjust=-0.40, size=geom.text.size, direction="x", segment.size  = 0.2, segment.color = "grey50")+
        xlab('Meta-analytic standard error') + ylab('Study mean effect')+
        geom_hline(yintercept= estimate)+
        geom_hline(yintercept= 0, color='grey')+
        scale_x_reverse()+
        scale_y_continuous(breaks=Axis_ES , limits =c(lim_minimum,lim_maximum))+
        coord_flip()+
        theme_bw()+
        theme(panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              panel.border=element_blank(),
              panel.background = element_blank(),
              axis.line=element_line(),
              axis.title = element_text(size=14),
              axis.text = element_text(size=12, colour = "black"),
              text=element_text(),
              legend.position="none")
    }else{
      e <- ggplot(data=studyinfo, aes(x = SE, y = ES, ylim(0,maxse))) +
        geom_point() +
        xlab('Meta-analytic standard error') + ylab('Study mean effect')+
        geom_hline(yintercept= estimate)+
        geom_hline(yintercept= 0, color='grey')+
        scale_x_reverse()+
        scale_y_continuous(breaks=Axis_ES , limits =c(lim_minimum,lim_maximum))+
        coord_flip()+
        theme_bw()+
        theme(panel.grid.major=element_blank(),
              panel.grid.minor=element_blank(),
              panel.border=element_blank(),
              panel.background = element_blank(),
              axis.line=element_line(),
              axis.title = element_text(size=14),
              axis.text = element_text(size=12, colour = "black"),
              text=element_text(),
              legend.position="none")
    }
  }
  
  e <- e + geom_line(data=CI_Lim, aes(y=lb_total, x=csize), colour="black")+
    geom_line(data=CI_Lim, aes(y=ub_total, x=csize), colour="black")
  print(e)
  
} 


          

# server logic---- 
server <- function(input, output, session) {
  #load metafor
  library(metafor)
  library(ggplot2)
  library(dplyr)
  library(data.table)
  ##ES calc ----  
  resultsVisiblees <- reactiveVal(FALSE)
  
  observeEvent(input$calc_es_c, {
    resultsVisiblees(TRUE)
    print("Run analysis clicked, setting resultsVisibleRVECat to TRUE.")
  })
  
  observeEvent(input$cesfile, {
    resultsVisiblees(FALSE)
  })

  output$dynamicResultses <- renderUI({
    if (resultsVisiblees()) {
      tagList(
          h2("Understand the results"),
          p("Your results appear below. You will see your entire data file, but at the end are appended columns yi and vi, which are your effect size (Hedge's g) and variance, respectively. You are now ready to move forward to data analysis."), strong("You must download the result of this analysis to use for the rest of your analysis. Simply click 'download data' and save the file, then proceed to the next step."),
          p(""), 
          downloadButton("download_button_c", label = "Download Data"),
          verbatimTextOutput("esresultc_output")
        )
    }
  })
  
  # Read the CSV file
  sampledata1 <- read.csv("mydatac.csv")
  
  # Render the table
  output$sampledata1 <- renderTable({
    sampledata1})
  
  # Reactive expression to read uploaded file and return result
  esresultc <- eventReactive(input$calc_es_c, {
    req(input$cesfile)  # Check if a file is uploaded
    
    # Read the uploaded CSV file
    data <- read.csv(input$cesfile$datapath)
    # Assuming the file contains columns 'n1', 'n2', and 'r'
    escalc(measure="SMD", m1i=Exp_mean, sd1i=Exp_sd, n1i=Exp_n,
           m2i=Ctrl_mean, sd2i=Ctrl_sd, n2i=Ctrl_n, data=data)
  })
  # Render the result
  output$esresultc_output <- renderPrint({
    esresultc()
    
  })
  #download button
  # Download handler for the button
  output$download_button_c <- downloadHandler(
    filename = function() {
      "mydata.csv"  
    },
    content = function(file) {
      savedata <- esresultc()
      # Write data to CSV file
      write.csv(savedata, file)
    })
  
  ##Conventional MA----- 
  
  resultsVisiblecma <- reactiveVal(FALSE)
  
  observeEvent(input$run_cmac, {
    resultsVisiblecma(TRUE)
    print("Run analysis clicked, setting resultsVisibleRVECat to TRUE.")
  })
  
  observeEvent(input$cmafilec, {
    resultsVisiblecma(FALSE)
  })
  
  output$dynamicResultscma <- renderUI({
    if (resultsVisiblecma()) {
      tagList(
          h3("Random Effects Meta-Analysis of Standardized Mean Differences Results"),
          downloadButton("cmadownload_buttonc", label = "Download Results"),
          verbatimTextOutput("cmaresultc_output"),
          h4("Methods Notes"),
          p("This random effects conventional meta-analysis used Restricted Maximum Likelihood Estimation (REML) (the default in metafor package)."),
          h3("Forest Plot"),
          box( title = "*Important Note*", width = 12, status = "primary",
               p("The plot is rendered at 'half-page' height (800px by 600px) within the software. You may choose to download at this size (about half a standard page in height) or full-page size. If you need a taller image (most typical when there are many comparisons) than the 'full-page' download size may be helpful.",
               )),
          # Button to copy forest plot
          downloadButton("forestplotdownload_buttonc", label = "Download Forest Plot (half-page)"), downloadButton("forestplotdownload_buttoncf", label = "Download Forest Plot (full-page)"),
          # Render the forest plot
          plotOutput("forest_plotc", width = 800, height = 600),
          h3("R Script"),
          downloadButton("download_script_cma", "Download R Script"),
          verbatimTextOutput("generated_script_cma"),
          h3("Need Help Understanding The Results?"),
          p("If you want help interpreting these results, please see ", HTML("<a href='https://noah-schroeder.github.io/reviewbook/meta.html#interpreting-the-results'>my open book</a>"),
          ),
      )
    }
  })
  
  
  # Reactive expression to run conventional MA
  cmaresultc <- eventReactive(input$run_cmac, {
    req(input$cmafilec)  # Check if a file is uploaded
    
    # Read the uploaded CSV file
    data <- read.csv(input$cmafilec$datapath)
    
    # Check if required columns exist in the data
    req(c("yi", "vi") %in% names(data), message = "Columns 'yi' and 'vi' are required.")
    
    # Run random effects MA
    result <- rma(yi, vi, data=data)
    return(result)
    
  })
  
  # Render the result
  output$cmaresultc_output <- renderPrint({
    cmaresultc()
  })
  #download button
  # Download handler for the button
  output$cmadownload_buttonc <- downloadHandler(
    filename = function() {
      "myoverallresult.txt"  
    },
    content = function(file) {
      # Retrieve the result
      result <- cmaresultc()
      
      # Convert result to text
      result_textc <- capture.output(result)
      
      # Write result to text file
      writeLines(result_textc, file)
    }
  )
  ### Forest plot----
  # Reactive value to store forest plot
  forest_plotc <- reactive({
    # Read the uploaded CSV file
    data <- read.csv(input$cmafilec$datapath)
    # Check if the "run_cma" button has been clicked
    if (input$run_cmac > 0) {
      # Perform meta-analysis and create forest plot
      result <- cmaresultc()
      forest(result, slab = data$Study, main = "Forest Plot of Observed Effects", header="Author(s) and Year")
    }
  })
  
  
  # Render the forest plot
  output$forest_plotc <- renderPlot({
    # Check if the "run_cma" button has been clicked
    if (input$run_cmac > 0) {
      # Display the forest plot
      print(forest_plotc())
    }
  })
  # Download handler for the forest plot button
  output$forestplotdownload_buttonc <- downloadHandler(
    filename = function() {
      "forestplot.png"  # Specify file name
    },
    content = function(file) {
      # Capture the plot as a PNG file
      png(file, width = 800, height = 600)
      data <- read.csv(input$cmafilec$datapath)
      if (input$run_cmac > 0) {
        # Perform meta-analysis and create forest plot
        result <- cmaresultc()
        forest(result, slab = data$Study, main = "Forest Plot of Observed Effects", header="Author(s) and Year")
      }
      dev.off()
    }
  )
  
  # Download handler for the forest plot button
  output$forestplotdownload_buttoncf <- downloadHandler(
    filename = function() {
      "forestplot.png"  # Specify file name
    },
    content = function(file) {
      # Capture the plot as a PNG file
      png(file, width = 800, height = 1056)
      data <- read.csv(input$cmafilec$datapath)
      if (input$run_cmac > 0) {
        # Perform meta-analysis and create forest plot
        result <- cmaresultc()
        forest(result, slab = data$Study, main = "Forest Plot of Observed Effects", header="Author(s) and Year")
      }
      dev.off()
    }
  )

  
  # Define a reactive expression for generating R script
  generated_script_cma <- reactive({
    req(input$run_cmac, input$cmafilec)  # Ensure file and button are active
    
    script_content <- capture.output({
      cat("# Conventional Random Effects Meta-Analysis Script\n\n")
      cat("# Load required package\n")
      cat( "library(metafor)\n\n")
      cat( "# Read data\n")
      # Data loading step
      cat("df <- read.csv(\"", gsub("[^a-zA-Z0-9._-]", "_", input$cmafilec$name), "\")\n\n", sep = "")
      
      # Run random effects MA
      cat("# Run random effects meta-analysis\n")
      cat("result <- rma(yi, vi, data = df)\n\n")
      
      # Return result
      cat("# Display result\n")
      cat("result\n")
      
      cat("# Create and display forest plot\n")
      cat("forest(result, slab = data$Study, main = \"Forest Plot of Observed Effects\", header=\"Author(s) and Year\")\n\n")
    })
    
    # Remove empty lines and return the script content
    script_content_clean <- script_content[script_content != ""]
    paste(script_content_clean, collapse = "\n")
  })
  
  # Output to display the generated R script
  output$generated_script_cma <- renderText({
    if (!is.null(input$run_cmac) && !is.null(input$cmafilec)) {
      generated_script_cma()
    }
  })
  
  
  # Download handler for downloading generated R script as .txt file
  output$download_script_cma <- downloadHandler(
    filename = function() {
      "script.cma.txt"
    },
    content = function(file) {
      # Write generated script content to a file
      script_content <- generated_script_cma()
      cat(script_content, file = file)
    }
  )
  
  
  
  
  ###Outlier and Influence Analysis----
  resultsVisiblecinf <- reactiveVal(FALSE)
  
  observeEvent(input$run_infc, {
    resultsVisiblecinf(TRUE)
    print("Run analysis clicked, setting resultsVisibleRVECat to TRUE.")
  })
  
  observeEvent(input$inffilec, {
    resultsVisiblecinf(FALSE)
  })
  
  output$dynamicResultscmainf <- renderUI({
    if (resultsVisiblecinf()) {
      tagList(
          h3("Outlier and Influence Results"),
          p("Check the 'inf' column. If you see an asterisk (*), it means at least one criteria exceeded a critical value and the study should be examined."),
          downloadButton("infdownload_buttonc", label = "Download Infuence Analysis Results"),
          verbatimTextOutput("resultforinf_outputc"),
          h3("R Script"),
          downloadButton("download_script_inf", "Download R Script"),
          verbatimTextOutput("generated_script_inf"),
          h3("Need Help Understanding The Results?"),
          p("If you want help interpreting these results, please see ", HTML("<a href='https://noah-schroeder.github.io/reviewbook/meta.html#checking-for-outliers-and-influence'>my open book</a>"),
          ),
        )
    }
  })
  
  
  
  # Reactive expression to run conventional MA and influence analysis
  infresult <- eventReactive(input$run_infc, {
    req(input$inffilec)  # Check if a file is uploaded
    
    # Read the uploaded CSV file
    infdata <- read.csv(input$inffilec$datapath)
    
    # Run random effects MA
    resultforinf <- rma(yi, vi, data=infdata)
    
    # Perform influence analysis
    infresc <- influence(resultforinf)
    
    # Return both the MA result and influence analysis result
    return(list(resultforinf = resultforinf, infres = infresc))
  })
  
  # Render the result on the screen
  output$resultforinf_outputc <- renderPrint({
    # Check if the "run_inf" button has been clicked
    if (input$run_infc > 0) {
      # Display the influence analysis result
      inf_result <- infresult()
      print(inf_result$infres)
    }
  })
  
  #inf download button
  # Download handler for the button
  output$infdownload_buttonc <- downloadHandler(
    filename = function() {
      "influenceresult.csv"  
    },
    content = function(file) {
      # Retrieve the influence analysis result from the reactive expression
      inf_result <- infresult()
      
      # Extract the influence statistics from the infres object
      inf_statistics <- inf_result$infres$inf
      
      # Convert the influence statistics into a data frame
      inf_table <- as.data.frame(inf_statistics)
      
      # Write data to CSV file
      write.csv(inf_table, file, row.names = FALSE)
    })
  
  # Define a reactive expression for generating R script
  generated_script_inf <- reactive({
    req(input$run_infc, input$inffilec)  # Ensure file and button are active
    
    script_content <- capture.output({
      cat("# Random Effects Meta-Analysis and Influence Analysis Script\n\n")
      
      cat("# Load required package\n")
      cat( "library(metafor)\n\n")
      cat( "# Read data\n")
      # Data loading step
      cat("df <- read.csv(\"", gsub("[^a-zA-Z0-9._-]", "_", input$inffilec$name), "\")\n\n", sep = "")
      
      # Run random effects meta-analysis
      cat("# Run random effects analysis\n\n")
      cat("resultforinf <- rma(yi, vi, data = df)\n\n")
      
      # Perform influence analysis
      cat("# Run influence analysis\n\n")
      cat("infres <- influence(resultforinf)\n\n")
      
      # Display both meta-analysis and influence analysis results
      cat("# Display results\n\n")
      cat("print(infres)\n")
    })
    
    # Remove empty lines and return the script content
    script_content_clean <- script_content[script_content != ""]
    paste(script_content_clean, collapse = "\n")
  })
  
  # Output to display the generated R script
  output$generated_script_inf <- renderText({
    if (!is.null(input$run_infc) && !is.null(input$inffilec)) {
      generated_script_inf()
    }
  })
  
  # Define a download handler for downloading generated R script as .txt file
  output$download_script_inf <- downloadHandler(
    filename = function() {
      paste("script.inf_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      # Write generated script content to a file
      script_content <- generated_script_inf()
      cat(script_content, file = file)
    }
  )
  
  
  
  
  
  ###Cat Mod Analysis----
  # Initialize a reactive value for displaying results
  resultsvisibleCcat <- reactiveVal(FALSE)
  
  observeEvent(input$run_analysisCcat, {
    resultsvisibleCcat(TRUE)
  })
  
  observeEvent(input$dropdownc, {
    resultsvisibleCcat(FALSE)
  })
  
  observeEvent(input$modfilec, {
    resultsvisibleCcat(FALSE)
  })
  
  output$dynamicResultsCcat <- renderUI({
    if (resultsvisibleCcat()) {
      tagList(
        # Display the results
        box(title = "Important Note", width = 12, status = "primary",
        p("In metafor there are two different tests of the moderator. The table below presents the omnibus test of moderators from the model with an intercept. This is the same statistic as you may be used to seeing as Qbetween in conventional meta-analysis. The effect sizes etc. provided in the moderator table below are from the model without an intercept. In a conventional meta-analysis, this is a presentation consistent with what you may expect to see from other software packages such as Comprehensive Meta-Analysis."),
        ),
        h3("Model Results"),
        p("Below is our table that shows the effect sizes and accompanying statistics for each level of the moderator. Remember, your Qbetween tells you if there are significant differences between levels."),
        downloadButton("download_resultsc", "Download Results"),
        tableOutput("modtable_outputc"),
        h3("Basic Interpretation Tips"),
        p("First you should look at the", strong("Test_of_Moderator"), "column (commonly called Qbetween). If this is significant, it means there are significant differences between levels of your moderator."),
        p("Next you should look at the", strong("Residual_Heterogeneity"), "column (commonly called Qwithin). If this is significant, it means there is significant hetereogenity that the moderator does", strong("not"), "explain."),
        p("Note the following columns I have created to aid in interpretation:"),
        p(strong("nexp"),"is the sample size of the intervention group, and", strong("nctrl"), "is the sample size of the control group."),
        p(strong("kcomp"),"is the number of comparisons examined. The total number of kcomp in the table should correspond to the number of rows in your dataset."),
        h3("R Script"),
        downloadButton("download_scriptc", "Download R Script"),
        verbatimTextOutput("generated_scriptc"),
        h3("Need Help Understanding The Results?"),
        p("If you want help interpreting these results, please see ", HTML("<a href='https://noah-schroeder.github.io/reviewbook/meta.html#moderator-analysis'>my open book</a>"),
        ),
      )
    }
  })
  
  
  # Reactive expression to read uploaded CSV file for rma analysis
  uploaded_datacc_convc <- reactive({
    req(input$modfilec)
    read.csv(input$modfilec$datapath)
  })
  
  # Update selectInput choices when file is uploaded
  observeEvent(input$modfilec, {
    updateSelectInput(session, "dropdownc", choices = colnames(uploaded_datacc_convc()))
  })
  
  # Reactive expression to run rma analysis without intercept
  modfortable_convc <- reactive({
    req(input$run_analysisCcat, input$dropdownc)  # Ensure dropdown choice is available
    
    # Construct the moderator formula without the intercept
    mod_formula_convc <- as.formula(paste("~ -1 + factor(", input$dropdownc, ")"))
    
    # Run the moderator analysis with rma
    mod_result_convc <- rma(yi, vi, mods = mod_formula_convc, data = uploaded_datacc_convc())
    
    # Extract coefficients and standard errors
    coef_table_convc <- coef(summary(mod_result_convc))
    
    # Summarize participants in each group
    participants_summary_convc <- uploaded_datacc_convc() %>%
      group_by(!!sym(input$dropdownc)) %>%
      summarise(nexp = sum(Exp_n, na.rm = TRUE),
                nctrl = sum(Ctrl_n, na.rm = TRUE),
                kcomp = n())
    
    # Combine participants summary with coefficient table
    result_table_convc <- cbind(participants_summary_convc, coef_table_convc)
    
    # Convert numeric columns to numeric (in case they were read as factors)
    numeric_cols_convc <- c("estimate", "se", "zval", "pval", "ci.lb", "ci.ub")
    result_table_convc[, numeric_cols_convc] <- lapply(result_table_convc[, numeric_cols_convc], as.numeric)
    
    # Round numeric columns to 3 decimal places
    result_table_convc[, numeric_cols_convc] <- round(result_table_convc[, numeric_cols_convc], 3)
    
    result_table_convc
  })
  
  # Reactive expression for capturing the "Test of Moderators" information
  mod_summary1_convc <- reactive({
    req(input$run_analysisCcat, input$dropdownc)  # Ensure dropdown choice is available
    
    # Construct the moderator formula with intercept
    mod_formula1_convc <- as.formula(paste("~ factor(", input$dropdownc, ")"))
    
    # Run the moderator analysis with intercept
    mod_result_with_intercept1r_convc <- rma(yi, vi, mods = mod_formula1_convc, data = uploaded_datacc_convc())
    
    # Extract the QM statistic and p-value for the "Test of Moderators"
    QM_convc <- round(mod_result_with_intercept1r_convc$QM, 3)
    QMp_convc <- round(mod_result_with_intercept1r_convc$QMp, 3)
    
    # Extract the QE statistic and degrees of freedom for the residual heterogeneity
    QE_convc <- round(mod_result_with_intercept1r_convc$QE, 3)
    QEdf_convc <- df.residual(mod_result_with_intercept1r_convc)
    QE_convcp <- round(mod_result_with_intercept1r_convc$QEp, 3)
    
    # Create a data frame with both the "Test of Moderators" and "Test of Residual Heterogeneity" information
    data.frame(
      Qbetween = paste("Qb(", mod_result_with_intercept1r_convc$QMdf[1], ") =", QM_convc, ", p-val =", QMp_convc),
      Qbetween <- ifelse(QMp_convc < 0.001, 
                        paste("Qb(", mod_result_with_intercept1r_convc$QMdf[1], ") =", QM_convc, ", p-val < .001"),
                        paste("Qb(", mod_result_with_intercept1r_convc$QMdf[1], ") =", QM_convc, ", p-val =", QMp_convc)),
      Qwithin <- ifelse(QE_convcp < 0.001, 
                        paste("Qw(", QEdf_convc, ") =", QE_convc, ", p-val < .001"),
                        paste("Qw(", QEdf_convc, ") =", QE_convc, ", p-val =", QE_convcp))
    )
  })
  
  # Display results of the moderator analysis in a table
  output$modtable_outputc <- renderTable({
    # Check if the "Run Moderator Analysis" button has been pressed
    if (input$run_analysisCcat > 0) {
      # Get the result table without intercept
      result_table_data_convc <- modfortable_convc()
      
      # Add the "Test of Moderators" row to the result table
      mod_summary_data_convc <- mod_summary1_convc()
      
      # Convert the result to a data frame
      result_table_df_convc <- as.data.frame(result_table_data_convc)
      
      # Create a new data frame for the "Test of Moderators" and "Test of Residual Heterogeneity" information
      test_of_moderators_and_residual_df_convc <- data.frame(
        Test_of_Moderator = mod_summary_data_convc$Qbetween,
        Residual_Heterogeneity = mod_summary_data_convc$Qwithin
      )
      
      # Combine the result table and the new row
      final_table_convc <- bind_rows(result_table_df_convc, test_of_moderators_and_residual_df_convc)
      
      # Replace NA values with empty strings in the final table
      final_table_convc <- final_table_convc %>%
        mutate_all(~ ifelse(is.na(.), "", .))
       # Replace NA values with empty strings in the final table
    final_table_convc <- final_table_convc %>%
      mutate_all(~ ifelse(is.na(.), "", .))
      
      # Return the final table
      final_table_convc
    }
  })
  
  
  
  # Download handler for the results
  output$download_resultsc <- downloadHandler(
    filename = function() {
      paste("mod.", input$dropdownc, Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      # Get the result table with "Test of Moderators" row for download
      result_table_data_convc <- modfortable_convc()
      mod_summary_data_convc <- mod_summary1_convc()
      
      # Create a new data frame for the "Test of Moderators" row
      test_of_moderators_and_residual_df_convc <- data.frame(
        Test_of_Moderator = mod_summary_data_convc$Qbetween,
        Residual_Heterogeneity = mod_summary_data_convc$Qwithin
      )      
      # Combine the result table and the "Test of Moderators" row
      final_table_convc <- bind_rows(result_table_data_convc, test_of_moderators_and_residual_df_convc)
      
      # Replace NA values with empty strings in the final table
      final_table_convc <- final_table_convc %>%
        mutate_all(~ ifelse(is.na(.), "", .))
      
      # Write csv file
      write.csv(final_table_convc, file, row.names = FALSE)
    }
  )
  
  
  # Define a reactive expression for generating R script
  generated_scriptc <- reactive({
    req(input$run_analysisCcat, input$dropdownc)  # Ensure dropdown choice is available
    
    script_content <- capture.output({
      cat("# Categorical Moderator Conventional Meta-Analysis Script\n\n")
      
      cat("# Load required package\n")
      cat( "library(metafor)\n\n")
      cat( "# Read data\n")
      # Data loading step
      cat("df <- read.csv(\"", gsub("[^a-zA-Z0-9._-]", "_", input$modfilec$name), "\")\n\n", sep = "")
      
      # Generate moderator formula with intercept for test of moderator value
      cat("#  Moderator formula with intercept for test of moderator value\n\n")
      cat("mod_formulaq <- as.formula(paste(\"~ factor(\", \"", input$dropdownc, "\")\"))\n\n")
      # Perform meta-analysis with intercept for table
      cat("#  Perform meta-analysis with intercept for Test of Moderator Value\n\n")
      cat("mod_resultq <- rma(yi, vi, mods = mod_formulaq, data = df)\n\n")
      cat("mod_resultq\n\n")
      
      # Generate moderator formula without intercept
      cat("#  Moderator formula without intercept for table\n\n")
      cat("mod_formula_t <- as.formula(paste(\"~ -1 + factor(\", \"", input$dropdownc, "\")\"))\n\n")
      
      # Perform meta-analysis without intercept for table
      cat("#  Perform meta-analysis without intercept for table\n\n")
      cat("mod_result <- rma(yi, vi, mods = mod_formula_t, data = df)\n\n")
      
      # Summary of results
      cat("summary(mod_result)\n")
    })
    
    # Remove empty lines and return the script content
    script_content_clean <- script_content[script_content != ""]
    paste(script_content_clean, collapse = "\n")
  })
  
  
  # Output to display the generated R script
  output$generated_scriptc <- renderText({
    if (!is.null(input$run_analysisCcat) && !is.null(input$dropdownc)) {
      generated_scriptc()
    }
  })
  # Define a download handler for downloading generated R script as .txt file
  output$download_scriptc <- downloadHandler(
    filename = function() {
      # Extract the variable name selected by the user
      selected_variable <- input$dropdownc
      
      # Generate the filename with the format "mod.[variable]_YYYY-MM-DD.txt"
      paste("script.mod.", selected_variable, "_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      # Write generated script content to a file
      script_content <- generated_scriptc()
      cat(script_content, file = file)
    }
  )
  
  
  
  
  
  ### Cont Mod Analysis ----
  
  # Initialize a reactive value for displaying results
  resultsvisibleCc <- reactiveVal(FALSE)
  
  observeEvent(input$run_analysiscc, {
    resultsvisibleCc(TRUE)
  })
  
  observeEvent(input$dropdowncc, {
    resultsvisibleCc(FALSE)
  })
  observeEvent(input$modfilecc, {
    resultsvisibleCc(FALSE)
  })
  
  output$dynamicResultsCc <- renderUI({
    if (resultsvisibleCc()) {
      tagList(
        # Display the results
        h3("Effect Size Table"),
        p("Below is our table that shows the relevant statistics for your single-variable meta-regression. Remember, your Test of Moderators above tells you if the moderator is significant."),
        downloadButton("download_resultscc", "Download Results"),
        tableOutput("modtable_outputcc"),
        h3("Basic Interpretation Tips"),
        p("First you should look at the", strong("Test_of_Moderator"), "column (commonly called Qbetween). If this is significant, it means the variable examined is a significant moderator."),
        p("Next you should look at the", strong("Residual_Heterogeneity"), "column (commonly called Qwithin). If this is significant, it means there is significant hetereogenity that the moderator does", strong("not"), "explain."),
        h3("R Script"),
        downloadButton("download_scriptcc", "Download R Script"),
        verbatimTextOutput("generated_scriptcc"),
        h3("Need Help Understanding The Results?"),
        p("If you want help interpreting these results, please see ", HTML("<a href='https://noah-schroeder.github.io/reviewbook/meta.html#moderator-analysis'>my open book</a>"),
        ),
      )
    }
  })
  
  
  
  
  
  
  
  uploaded_datacc <- reactive({
    req(input$modfilecc)
    read.csv(input$modfilecc$datapath)
  })
  
  observeEvent(input$modfilecc, {
    updateSelectInput(session, "dropdowncc", choices = colnames(uploaded_datacc()))
  })
  
  # This reactive generates the complete table including the test of moderators and residual heterogeneity
  complete_table <- reactive({
    req(input$run_analysiscc, input$dropdowncc)
    mod_formula <- as.formula(paste("~", input$dropdowncc))
    mod_result <- rma(yi, vi, mods = mod_formula, data = uploaded_datacc())
    model_summary <- summary(mod_result)
    summary_table <- coef(model_summary)
    
    # Initialize result table with the number of columns needed
    result_table <- data.frame(
      Term = rownames(summary_table),
      Estimate = round(summary_table[, "estimate"], 3),
      StdError = round(summary_table[, "se"], 3),
      ZValue = round(summary_table[, "zval"], 3),
      PValue = round(summary_table[, "pval"], 3),
      CI_Lower = round(summary_table[, "ci.lb"], 3),
      CI_Upper = round(summary_table[, "ci.ub"], 3),
      Test_of_Moderator = "",  # Initialize with empty strings
      Residual_Heterogeneity = ""  # Initialize Qwithin column
    )
    
    # Calculate Test of Moderator and Qwithin
    if (!is.null(mod_result$QE)) {
      q_within_val <- round(mod_result$QE, 3)
      
      if (mod_result$QEp < 0.001) {
        q_within_pval <- paste("p-val < .001")
      } else {
        q_within_pval <- paste("p-val =", round(mod_result$QEp, 3))
      }
      
      if (!is.null(model_summary$QMp)) {
        modelsumqmp <- round(model_summary$QMp, 3)
        
        if (modelsumqmp < 0.001) {
          q_betweencc_pval <- paste("p-val < .001")
        } else {
          q_betweencc_pval <- paste("p-val =", round(model_summary$QMp, 3))
        }
      }
      
      # Combine Test of Moderator and Residual Heterogeneity into one row
      combined_row <- data.frame(
        Term = "",
        Estimate = "",
        StdError = "",
        ZValue = "",
        PValue = "",
        CI_Lower = "",
        CI_Upper = "",
        Test_of_Moderator = paste("Qb(", model_summary$QMdf[1], ") =", round(model_summary$QM, 3), q_betweencc_pval),
        Residual_Heterogeneity = paste("Qw(", mod_result$k - mod_result$p, ") =", q_within_val, q_within_pval)  # Include both q_within_val and q_within_pval
      )
      
      # Bind the combined row to the result table
      final_table <- rbind(result_table, combined_row)
    } else {
      # If QE is NULL, only include Test of Moderator row
      test_mod_info <- paste("Qb(", model_summary$QMdf[1], ") =", round(model_summary$QM, 3), ", p-val =", round(model_summary$QMp, 3))
      final_table <- data.frame(
        Term = "Test of Moderator",
        Estimate = "",
        StdError = "",
        ZValue = "",
        PValue = "",
        CI_Lower = "",
        CI_Upper = "",
        Test_of_Moderator = test_mod_info,
        Residual_Heterogeneity = ""
      )
    }
    
    # Replace NA values with empty strings in the final table
    final_table <- final_table %>%
      mutate_all(~ ifelse(is.na(.), "", .))
    
    # Return the final table
    final_table
  })
  
  
  
  
  
  
  
  
  output$modtable_outputcc <- renderTable({
    complete_table()  # Display the complete table
  })
  
  #download handler
  output$download_resultscc <- downloadHandler(
    filename = function() {
      paste("mod.", input$dropdowncc, Sys.Date(), ".csv", sep = "")
      },
    content = function(file) {
      write.csv(complete_table(), file, row.names = FALSE)  # Use the complete table for download
    }
  )
  
  
  # Define a reactive expression for generating R script
  generated_scriptcc <- reactive({
    req(input$run_analysiscc, input$dropdowncc)
    
    script_content <- capture.output({
      cat("# Continuous Moderator Conventional Meta-Analysis Script\n\n")
      cat("# Load required package\n")
      cat( "library(metafor)\n\n")
      cat( "# Read data\n")
      # Data loading step
      cat("df <- read.csv(\"", gsub("[^a-zA-Z0-9._-]", "_", input$modfilecc$name), "\")\n\n", sep = "")
      
      # Generate moderator formula
      mod_formula <- as.formula(paste("~", input$dropdowncc))
      cat("mod_formula <- as.formula(paste(\"~\", \"", input$dropdowncc, "\"))\n\n", sep = "")
      
      # Perform meta-analysis
      cat("mod_result <- rma(yi, vi, mods = mod_formula, data = df)\n\n")
      
      # Summary of results
      cat("summary(mod_result)\n")
    })
    
    # Remove empty lines and return the script content
    script_content_clean <- script_content[script_content != ""]
    paste(script_content_clean, collapse = "\n")
  })
  
  
  # Output to display the generated R script
  output$generated_scriptcc <- renderText({
    if (!is.null(input$run_analysiscc) && !is.null(input$dropdowncc)) {
      generated_scriptcc()
    }
  })
  
  # Define a download handler for downloading generated R script as .txt file
  output$download_scriptcc <- downloadHandler(
    filename = function() {
      # Extract the variable name selected by the user
      selected_variable <- input$dropdowncc
      
      # Generate the filename with the format "mod.[variable]_YYYY-MM-DD.txt"
      paste("script.mod.", selected_variable, "_", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      # Write generated script content to a file
      script_content <- generated_scriptcc()
      cat(script_content, file = file)
    }
  )
  
  
  
  
## Multiple Meta-Regression ---- 
  
  # Initialize a reactive value for displaying results
  resultsvisibleCmreg <- reactiveVal(FALSE)
  resultsvisiblecmregall <- reactiveVal(FALSE)
  
  observeEvent(input$render_variables, {
    resultsvisibleCmreg(TRUE)
  })
  
  observeEvent(input$run_analysismregc, {
    resultsvisiblecmregall(TRUE)
  })
  
  observeEvent(input$num_continuous, {
    resultsvisibleCmreg(FALSE)
    resultsvisiblecmregall(FALSE)
  })
  observeEvent(input$num_categorical, {
    resultsvisibleCmreg(FALSE)
    resultsvisiblecmregall(FALSE)
  })
  
  # Observe changes in individual variable selections to reset results visibility
  observe({
    lapply(seq_len(input$num_continuous), function(i) {
      input[[paste0("continuous_var_", i)]]
    })
    lapply(seq_len(input$num_categorical), function(i) {
      input[[paste0("categorical_var_", i)]]
    })
    resultsvisiblecmregall(FALSE)
  })
  
  output$dynamicResultsCmregVar <- renderUI({
    if (resultsvisibleCmreg()) {
      tagList(
        # Display the results
        # Display variable selection
        uiOutput("variable_selection"),
        # Run analysis button
        actionButton("run_analysismregc", "Run Analysis"),
      )
    }
  })
  
    output$dynamicResultsCmregRes <- renderUI({
      if (resultsvisiblecmregall()) {
        tagList(
          h3("Results"),
        # Display meta-analysis results
        box(title = "Important Note", width = 12, status = "primary",
            p("Note that categorical moderators are treated as factors with distinct levels. The results below are from the model including an intercept."),
        ),
        downloadButton("mregc_resdl", "Download Results"),
        verbatimTextOutput("meta_results"),
        h3("R Script"),
        downloadButton("download_code", "Download R Script"),
        # Display generated R script
        verbatimTextOutput("generated_script"),
        h3("Need Help Understanding The Results?"),
        p("If you want help interpreting these results, please see ", HTML("<a href='https://noah-schroeder.github.io/reviewbook/meta.html#moderator-analysis'>my open book</a>"),
        ),
      )
    }
  })
  
  
  
  
  
  
  # Reactive function to read uploaded CSV data
  uploaded_datamreg <- reactive({
    req(input$mregc)
    read.csv(input$mregc$datapath)
  })
  
  # Reactive expression to construct the meta-analysis model formula
  meta_formula <- reactive({
    continuous_mods <- paste(sapply(seq_len(input$num_continuous), function(i) {
      input[[paste0("continuous_var_", i)]]
    }), collapse = " + ")
    
    categorical_mods <- paste(sapply(seq_len(input$num_categorical), function(i) {
      paste0("factor(", input[[paste0("categorical_var_", i)]], ")")
    }), collapse = " + ")
    
    # Construct the formula based on user inputs
    if (input$num_continuous > 0 && input$num_categorical > 0) {
      formula <- as.formula(paste("yi ~", continuous_mods, "+", categorical_mods))
    } else if (input$num_continuous > 0) {
      formula <- as.formula(paste("yi ~", continuous_mods))
    } else if (input$num_categorical > 0) {
      formula <- as.formula(paste("yi ~", categorical_mods))
    } else {
      formula <- as.formula("yi ~ 1")  # Default formula if no variables are selected
    }
    
    formula
  })
  
  # Event observer for rendering variable selection
  observeEvent(input$render_variables, {
    # Render UI for variable selection
    output$variable_selection <- renderUI({
      req(uploaded_datamreg())  # Ensure uploaded data is available
      
      continuous_inputs <- lapply(seq_len(input$num_continuous), function(i) {
        selectInput(paste0("continuous_var_", i), 
                    label = paste("Continuous Variable", i),
                    choices = colnames(uploaded_datamreg()))
      })
      
      categorical_inputs <- lapply(seq_len(input$num_categorical), function(i) {
        selectInput(paste0("categorical_var_", i), 
                    label = paste("Categorical Variable", i),
                    choices = colnames(uploaded_datamreg()))
      })
      
      fluidRow(
        column(6, tagList(continuous_inputs)),
        column(6, tagList(categorical_inputs))
      )
    })
  })
  
  # Reactive expression for running the meta-analysis
  run_analysismregc <- eventReactive(input$run_analysismregc, {
    req(uploaded_datamreg(), meta_formula())
    
    # Perform meta-analysis
    mod_result <- rma(yi, vi, mods = meta_formula(), data = uploaded_datamreg())
    mod_result
  })
  
  
  # Output for displaying results
  output$meta_results <- renderPrint({
    req(run_analysismregc())
    summary(run_analysismregc())
  })
  
  # Download handler for analysis results
  output$mregc_resdl <- downloadHandler(
    filename = function() {
      paste("meta_analysis_results", Sys.Date(), ".txt", sep = "")
    },
    content = function(file) {
      req(run_analysismregc())
      result_summary <- capture.output(summary(run_analysismregc()))
      writeLines(result_summary, con = file)
    }
  )
  
  
  # Event observer for generating R script
  observeEvent(input$run_analysismregc, {
    # Generate R script content dynamically
    script_content <- capture.output({
      cat("# Meta-Regression Script\n\n")
      cat("# Load required package\n")
      cat( "library(metafor)\n\n")
       cat( "# Read data\n")
       # Data loading step
      cat("df <- read.csv(\"", gsub("[^a-zA-Z0-9._-]", "_", input$mregc$name), "\")\n\n", sep = "")
      
      # Generate formulas for continuous and categorical variables
      if (input$num_continuous > 0) {
        cat("# Continuous Variables\n")
        for (i in seq_len(input$num_continuous)) {
          cat(paste("continuous_var_", i, " <- \"", input[[paste0("continuous_var_", i)]], "\"\n", sep = ""))
        }
        cat("\n")
      }
      
      if (input$num_categorical > 0) {
        cat("# Categorical Variables\n")
        for (i in seq_len(input$num_categorical)) {
          cat(paste("categorical_var_", i, " <- \"", input[[paste0("categorical_var_", i)]], "\"\n", sep = ""))
        }
        cat("\n")
      }
      
      # Meta-analysis modeling step
      cat("# Meta-Analysis Model\n")
      if (input$num_continuous > 0 && input$num_categorical > 0) {
        cat("formula <- yi, vi, mods = ~ ", paste(paste0("continuous_var_", seq_len(input$num_continuous)), collapse = " + "), 
            " + ", paste(paste0("factor(categorical_var_", seq_len(input$num_categorical), ")", collapse = " + ")))
      } else if (input$num_continuous > 0) {
        cat("formula <- yi, vi, mods = ~ ", paste(paste0("continuous_var_", seq_len(input$num_continuous)), collapse = " + "))
      } else if (input$num_categorical > 0) {
        cat("formula <- yi, vi, mods = ~ ", paste(paste0("factor(categorical_var_", seq_len(input$num_categorical), ")", collapse = " + ")))
      } else {
        cat("formula <- yi, vi")  # Default formula if no variables are selected
      }
      
      cat("\n\n")
      
      # Perform meta-analysis
      cat("mod_result <- rma(formula, data = df)\n\n")
      
      # Print summary of results
      cat("summary(mod_result)\n")
    })
    
    
    # Remove empty lines
    script_content_clean <- script_content[script_content != ""]  # Filter out empty lines
    
    # Store the cleaned script content in a reactive value
  output$generated_script <- renderPrint({
    cat(script_content_clean, sep = "\n")
  })
})
  
  # Download handler for downloading the generated R script
  output$download_code <- downloadHandler(
    filename = function() {
      "script.metareg.txt"
    },
    content = function(file) {
      # Begin writing the script content
      script_content <- "# R Script for Meta-Analysis\n\n"
      script_content <- "# Load required package\n"
      script_content <- "library(metafor)\n\n"
      script_content <- "# Read data\n"
         # Sanitize filename by replacing special characters
        sanitized_filename <- gsub("[^a-zA-Z0-9._-]", "_", input$mregc$name)

      # Read uploaded CSV data
      script_content <- paste0(script_content, "uploaded_data <- read.csv(\"", sanitized_filename, "\")\n\n")
      
      # Generate formulas for continuous and categorical variables
      if (input$num_continuous > 0) {
        script_content <- paste0(script_content, "# Continuous Variables\n")
        for (i in seq_len(input$num_continuous)) {
          script_content <- paste0(script_content, paste("continuous_var_", i, " <- \"", input[[paste0("continuous_var_", i)]], "\"\n"))
        }
        script_content <- paste0(script_content, "\n")
      }
      
      if (input$num_categorical > 0) {
        script_content <- paste0(script_content, "# Categorical Variables\n")
        for (i in seq_len(input$num_categorical)) {
          script_content <- paste0(script_content, paste("categorical_var_", i, " <- \"", input[[paste0("categorical_var_", i)]], "\"\n"))
        }
        script_content <- paste0(script_content, "\n")
      }
      
      # Meta-analysis modeling step
      script_content <- paste0(script_content, "# Meta-Analysis Model\n")
      if (input$num_continuous > 0 && input$num_categorical > 0) {
        script_content <- paste0(script_content, "formula <- yi, vi, mods = ~ ", paste(paste0("continuous_var_", seq_len(input$num_continuous)), collapse = " + "), 
                                 " + ", paste(paste0("factor(categorical_var_", seq_len(input$num_categorical), ")", collapse = " + ")))
      } else if (input$num_continuous > 0) {
        script_content <- paste0(script_content, "formula <- yi, vi, mods = ~ ", paste(paste0("continuous_var_", seq_len(input$num_continuous)), collapse = " + "))
      } else if (input$num_categorical > 0) {
        script_content <- paste0(script_content, "formula <- yi, vi, mods = ~ ", paste(paste0("factor(categorical_var_", seq_len(input$num_categorical), ")", collapse = " + ")))
      } else {
        script_content <- paste0(script_content, "formula <- yi, vi")  # Default formula if no variables are selected
      }
      
      script_content <- paste0(script_content, "\n\n")
      
      # Perform meta-analysis
      script_content <- paste0(script_content, "mod_result <- rma(formula, data = uploaded_data)\n\n")
      
      # Print summary of results
      script_content <- paste0(script_content, "summary(mod_result)\n")
      
      # Write the script content to the file
      writeLines(script_content, file)
    }
  )
  
  
  

  
  
  
  ### Publication bias ----
  
  # Initialize a reactive value for displaying results
  resultsvisibleCpub <- reactiveVal(FALSE)
  
  observeEvent(input$run_pubc, {
    resultsvisibleCpub (TRUE)
  })
  
  observeEvent(input$pubbiasfilec, {
    resultsvisibleCpub (FALSE)
  })
  
  output$dynamicResultsCpub  <- renderUI({
    if (resultsvisibleCpub()) {
      tagList(
          h3("Funnel Plot"),
          downloadButton("download_funnelc", "Download Funnel Plot"),
          plotOutput("funnel_plotc", width = 800, height = 600),
          h3("Trim and Fill Analysis"),
          downloadButton("download_trim_fill", "Download Trim and Fill Analysis"),
          verbatimTextOutput("trim_fill_output"),
          h3("Egger's Regression"),
          downloadButton("download_eggers", "Download Egger's Regression"),
          verbatimTextOutput("eggers_output"),
          h3("Rosenthal's Fail Safe N"),
          downloadButton("download_rosenthal", "Download Rosenthal's Fail Safe N"),
          verbatimTextOutput("rosenthal_output"),
          h3("Orwin's Fail Safe N"),
          downloadButton("download_orwin", "Download Orwin's Fail Safe N"),
          verbatimTextOutput("orwin_output"),
          h3("Rosenberg's Fail Safe N"),
          downloadButton("download_rosenberg", "Download Rosenberg's Fail Safe N"),
          verbatimTextOutput("rosenberg_output"),
          h3("R Script"),
          downloadButton("download_codepubbias"),
          verbatimTextOutput("generated_codepubbias"),
          h3("Need Help Understanding The Results?"),
          p("If you want help interpreting these results, please see ", HTML("<a href='https://noah-schroeder.github.io/reviewbook/meta.html#publication-bias'>my open book</a>")),
        )
    }
  })
  
  
  
  
  # Reactive expression to perform meta-analysis and diagnostics
  perform_meta_analysis <- eventReactive(input$run_pubc, {
    req(input$pubbiasfilec)  # Check if a file is uploaded
    
    # Read data
    data <- read.csv(input$pubbiasfilec$datapath)
    
    # Run meta-analysis
    res <- tryCatch({
      rma(yi, vi, data = data)
    }, error = function(e) {
      NULL  # Return NULL if there's an error
    })
    
    if (!is.null(res)) {
      # Perform funnel plot
      funnel_plotc <- tryCatch({
        funnel(res)
      }, error = function(e) {
        NULL
      })
      
      # Perform Egger's regression
      eggers <- tryCatch({
        regtest(res)
      }, error = function(e) {
        NULL
      })
      
      # Perform Rosenthal's fail-safe N
      rosenthal <- tryCatch({
        fsn(yi, vi, data = data)
      }, error = function(e) {
        NULL
      })
      
      # Perform Orwin's fail-safe N
      orwin <- tryCatch({
        fsn(yi, vi, data = data, type = "Orwin")
      }, error = function(e) {
        NULL
      })
      
      # Perform Rosenberg's fail-safe N
      rosenberg <- tryCatch({
        fsn(yi, vi, data = data, type = "Rosenberg")
      }, error = function(e) {
        NULL
      })
      # Generate clean R code with line breaks
      codepubbias <- paste(
        "# Load required package\n",
        "library(metafor)\n\n",
        "# Read data\n",
        paste("df <- read.csv(\"", gsub("[^a-zA-Z0-9._-]", "_", input$pubbiasfilec$name), "\")\n\n", sep = ""),
        "# Run meta-analysis\n",
        "res <- rma(yi, vi, data = df)\n\n",
        "# Funnel plot\n",
        "funnel(res)\n\n",
        "# Egger's regression test\n",
        "eggers <- regtest(res)\n\n",
        "# Rosenthal's fail-safe N\n",
        "rosenthal <- fsn(yi, vi, data = df)\n\n",
        "# Orwin's fail-safe N\n",
        "orwin <- fsn(yi, vi, data = df, type = \"Orwin\")\n\n",
        "# Rosenberg's fail-safe N\n",
        "rosenberg <- fsn(yi, vi, data = df, type = \"Rosenberg\")\n\n"
      )
      
      # Return the results
      list(
        funnel_plot = funnel_plotc,
        eggers = eggers,
        rosenthal = rosenthal,
        orwin = orwin,
        rosenberg = rosenberg,
        codepubbias = codepubbias
      )
    } else {
      # Return NULL if meta-analysis fails
      NULL
    }
  })
  
  # Reactive expression to perform trim and fill analysis
  perform_trim_fill <- eventReactive(input$run_pubc, {
    req(input$pubbiasfilec)  # Check if a file is uploaded
    
    # Read data
    data <- read.csv(input$pubbiasfilec$datapath)
    
    # Run meta-analysis
    res <- tryCatch({
      rma(yi, vi, data = data)
    }, error = function(e) {
      NULL  # Return NULL if there's an error
    })
    
    if (!is.null(res)) {
      # Perform trim and fill analysis
      trim_fill_result <- tryCatch({
        trimfill(res)
      }, error = function(e) {
        NULL
      })
      
      # Return the trim and fill result
      trim_fill_result
    } else {
      # Return NULL if meta-analysis fails
      NULL
    }
  })
  
  
  # Render the UI components
  output$funnel_plotc <- renderPlot({
    perform_meta_analysis()$funnel_plotc
  })
  
  
  output$trim_fill_output <- renderPrint({
    perform_trim_fill()
  })
  
  output$eggers_output <- renderPrint({
    perform_meta_analysis()$eggers
  })
  
  output$rosenthal_output <- renderPrint({
    perform_meta_analysis()$rosenthal
  })
  output$orwin_output <- renderPrint({
    perform_meta_analysis()$orwin
  })
  
  output$rosenberg_output <- renderPrint({
    perform_meta_analysis()$rosenberg
  })

  # Download handler for the funnel plot button
  output$download_funnelc <- downloadHandler(
    filename = function() {
      "funnelplot.png"  # Specify file name
    },
    content = function(file) {
      # Capture the plot as a PNG file
      png(file, width = 2800, height = 2400, res = 300)
      if (!is.null(perform_meta_analysis()$funnel_plotc)) {
        # Print the funnel plot to the PNG file
        # Read data
        data <- read.csv(input$pubbiasfilec$datapath)
        
        # Run meta-analysis
        res <- tryCatch({
          rma(yi, vi, data = data)
        }, error = function(e) {
          NULL  # Return NULL if there's an error
        })
        funnel(res)
      }
      dev.off()
    }
  )
  
  
  
  # Download handler for the trim and fill analysis
  output$download_trim_fill <- downloadHandler(
    filename = function() {
      "trim_fill_analysis.txt"
    },
    content = function(file) {
      # Convert result to text
      result_text <- capture.output(perform_trim_fill())
      
      # Write result to text file
      writeLines(result_text, file)
      
    }
  )
  
  # Download handler for Egger's regression
  output$download_eggers <- downloadHandler(
    filename = function() {
      "eggers_regression.txt"
    },
    content = function(file) {
      # Convert result to text
      result_text <- capture.output(perform_meta_analysis()$eggers)
      
      # Write result to text file
      writeLines(result_text, file)
      
    }
  )
  
  
  # Download handler for Rosenthal's fail safe N
  output$download_rosenthal <- downloadHandler(
    filename = function() {
      "rosenthal_failsafe_n.txt"
    },
    content = function(file) {
      # Convert result to text
      result_text <- capture.output(perform_meta_analysis()$rosenthal)
      
      # Write result to text file
      writeLines(result_text, file)
      
    }
  )
  
  # Download handler for Orwin's fail safe N
  output$download_orwin <- downloadHandler(
    filename = function() {
      "orwin_failsafe_n.txt"
    },
    content = function(file) {
      # Convert result to text
      result_text <- capture.output(perform_meta_analysis()$orwin)
      
      # Write result to text file
      writeLines(result_text, file)
      
    }
  )
  
  # Download handler for Rosenberg's fail safe N
  output$download_rosenberg <- downloadHandler(
    filename = function() {
      "rosenberg_failsafe_n.txt"
    },
    content = function(file) {
      # Convert result to text
      result_text <- capture.output(perform_meta_analysis()$rosenberg)
      
      # Write result to text file
      writeLines(result_text, file)
      
    }
  )
  
  

  
  
  output$generated_codepubbias <- renderText({
    perform_meta_analysis()$codepubbias
  })
  
  # Download handler for the R code
  output$download_codepubbias <- downloadHandler(
    filename = function() {
      "script.pubbias.txt"
    },
    content = function(file) {
      code <- perform_meta_analysis()$codepubbias
      writeLines(code, file)
    }
  )
  
  
  
  
  
  

  ## 3LMA code----
  ###About ---- example coding form
  # Read the CSV file
  sampledata <- read.csv("mydata.csv")
  
  # Render the table
  output$sampledatatable <- renderTable({
    sampledata})
  
  # Read the CSV file
  sampledata3lma <- read.csv("mydata.csv")
  
  # Render the table
  output$sampledatatable3lma <- renderTable({
    sampledata3lma})
###ES calc  
  # Reactive expression to read uploaded file and return result
  esresult <- eventReactive(input$calc_es, {
    req(input$file)  # Check if a file is uploaded
    
    # Read the uploaded CSV file
    data <- read.csv(input$file$datapath)
    
    # Assuming the file contains columns 'n1', 'n2', and 'r'
    escalc(measure="SMD", m1i=Exp_mean, sd1i=Exp_sd, n1i=Exp_n,
           m2i=Ctrl_mean, sd2i=Ctrl_sd, n2i=Ctrl_n, data=data)
  })
  # Render the result
  output$esresult_output <- renderPrint({
    esresult()
    
  })
    #download button
    # Download handler for the button
    output$download_button <- downloadHandler(
      filename = function() {
        "mydata.csv"  
      },
      content = function(file) {
        savedata <- esresult()
        # Write data to CSV file
        write.csv(savedata, file)
      })


### Three-Level MA ----
    resultsvisible3lmaoverall <- reactiveVal(FALSE)
    
    observeEvent(input$run_cma, {
      resultsvisible3lmaoverall(TRUE)
    })
    
    observeEvent(input$cmafile, {
      resultsvisible3lmaoverall(FALSE)
    })
    
    output$dynamicResults3lmaoverall <- renderUI({
      if (resultsvisible3lmaoverall()) {
        tagList(
          # Display the results
            h3("Random Effects Meta-Analysis of Standardized Mean Differences Results"),
            downloadButton("cmadownload_button", label = "Download Results"),
            verbatimTextOutput("cmaresult_output"),
            h4("Methods Notes"),
            p("This random effects three-level meta-analysis used Restricted Maximum Likelihood Estimation (REML) (the default in metafor package). We used a three-level structure with ES_number nested within Study. We also used a t distribution rather than a z distribution. You can read about t distributions here:", HTML("<a href='https://wviechtb.github.io/metafor/reference/rma.mv.html'>metafor documentation about rma.mv</a>")),
            h3("R Script"),
            downloadButton("rscript3lma"),
            verbatimTextOutput("generated_code_3lma"),
            h3("Need Help Understanding The Results?"),
            p("If you want help interpreting these results, please see ", HTML("<a href='https://noah-schroeder.github.io/reviewbook/meta.html#interpreting-the-results'>my open book</a>"),
            ),
          )
      }
    })
    
    
    
    
    
    # Reactive expression to run 3LMA
    cmaresult <- eventReactive(input$run_cma, {
      req(input$cmafile)  # Check if a file is uploaded
      
      # Check if the input file exists
      if (is.null(input$cmafile)) {
        return(NULL)
      }
      
      # Read the uploaded CSV file
      data <- read.csv(input$cmafile$datapath)
      
      # Check if required columns are present
      required_cols <- c("yi", "vi")
      if (!all(required_cols %in% names(data))) {
        return(NULL)
      }
      
      # Run random effects MA
      result <- tryCatch({
        rma.mv(yi, vi,
               random = ~ 1 | Study/ES_number,
               method = "REML",
               test = "t",
               dfs = "contain",
               data = data) 
      }, error = function(e) {
        return(NULL)
      })
      
      return(result)
    })
    
    # Render the result
    output$cmaresult_output <- renderPrint({
      cmaresult()
    })
    
    # Download handler for the button
    output$cmadownload_button <- downloadHandler(
      filename = function() {
        "myoverallresult.txt"  
      },
      content = function(file) {
        # Retrieve the result
        result <- cmaresult()
        
        # Check if result is NULL or an error occurred
        if (is.null(result)) {
          return()
        }
        
        # Convert result to text
        result_text <- capture.output(result)
        
        # Write result to text file
        writeLines(result_text, file)
      }
    )
    
    
    # Reactive expression to generate R script content
    tlmascript_content <- reactive({
      paste(
        "# Load required package\n",
        "library(metafor)\n\n",
        "# Read data\n",
        paste("data <- read.csv(\"", gsub("[^a-zA-Z0-9._-]", "_", input$cmafile$name), "\")\n\n", sep = ""),
        "# Run 3-level meta-analysis\n",
        "result <- rma.mv(yi, vi,\n",
        "                  random = ~ 1 | Study/ES_number,\n",
        "                  method = \"REML\",\n",
        "                  test = \"t\",\n",
        "                  dfs = \"contain\",\n",
        "                  data = data)\n\n",
        "# Print the result\n",
        "print(result)\n"
      )
    })
    
    # Render the R script content
    output$generated_code_3lma <- renderText({
      tlmascript_content()
    })
    
    # Download handler for the R script
    output$rscript3lma <- downloadHandler(
      filename = function() {
        "script.3lma.txt"  # Set the name of the downloaded file
      },
      content = function(file) {
        # Write the R script content to the file
        writeLines(tlmascript_content(), file)
      }
    )

    


    
    
###Variance 3lma----
    
    resultsvisible3lmai2 <- reactiveVal(FALSE)
    
    observeEvent(input$run_i2, {
      resultsvisible3lmai2(TRUE)
    })
    
    observeEvent(input$i2file, {
      resultsvisible3lmai2(FALSE)
    })
    
    output$dynamicResults3lmai2 <- renderUI({
      if (resultsvisible3lmai2()) {
        tagList(
            #display result
            h3("I2 Results"),
            downloadButton("download_i2_results", label = "Download Results"),
            verbatimTextOutput("i2result_output"),
            verbatimTextOutput("totalI2_output"),
            h3("R Script"),
            downloadButton("rscript_i2tlma"),
            verbatimTextOutput("generated_code_i2tlma"),
            h3("Need Help Understanding The Results?"),
            p("If you want help interpreting these results, please see ", HTML("<a href='https://noah-schroeder.github.io/reviewbook/meta.html#interpreting-the-results'>my open book</a>"),
            ),
         )}
    })
    
    
    
    
    # Create an eventReactive to read the uploaded CSV file and run the code when action button is pressed
    i2_result <- eventReactive(input$run_i2, {
      req(input$i2file)  # Check if a file is uploaded
      
      # Read the uploaded CSV file and rename as data
      data <- read.csv(input$i2file$datapath)
      
      # Run the model
      m_multi <- rma.mv(yi, vi,
                        random = ~ 1 | Study/ES_number,
                        method = "REML",
                        test = "t",
                        dfs = "contain",
                        data = data)
      
      # Calculate I-squared values and variance distribution
      i2 <- mlm.variance.distribution(m_multi)
      
      # Extract the results and total I2
      results <- i2$results
      totalI2 <- i2$totalI2
      
      # Return the results and total I2 for rendering in the UI
      return(list(results = results, totalI2 = totalI2))
    })
    
    # Render the results output in the UI
    output$i2result_output <- renderPrint({
      i2_result()$results
    })
    
    # Render the totalI2 output in the UI with label
    output$totalI2_output <- renderText({
      paste("Total I2:", i2_result()$totalI2)
    })
    
    # Add a download button to download both the i2 results and the total i2 results as a .txt file
    output$download_i2_results <- downloadHandler(
      filename = function() {
        paste("i2_results_", Sys.Date(), ".txt", sep = "")
      },
      content = function(file) {
        # Retrieve the i2 results and total i2 results
        i2_results <- i2_result()$results
        total_i2 <- i2_result()$totalI2
        
        # Convert i2 results and total i2 results to text
        i2_results_text <- capture.output(i2_results)
        total_i2_text <- paste("Total I2:", total_i2)
        
        # Combine i2 results and total i2 results text
        combined_text <- c(i2_results_text, total_i2_text)
        
        # Write combined text to a .txt file
        writeLines(combined_text, file)
      }
    )

    
    # Reactive expression to generate R script content
    script_content_i2tlma <- reactive({
      paste(
        "# Load required package\n",
        "library(metafor)\n\n",
        "# Read data\n",
        paste("data <- read.csv(\"", gsub("[^a-zA-Z0-9._-]", "_", input$i2file$name), "\")\n\n", sep = ""),
        "# Run 3-level meta-analysis\n",
        "m_multi <- rma.mv(yi, vi,\n",
        "                  random = ~ 1 | Study/ES_number,\n",
        "                  method = \"REML\",\n",
        "                  test = \"t\",\n",
        "                  dfs = \"contain\",\n",
        "                  data = data)\n\n",
        "# Teach R i-squared functions\n",
        "# Please visit the following URL, copy the code, and paste it into your R console and hit enter before proceeding with your code:\n",
        "# https://raw.githubusercontent.com/MathiasHarrer/dmetar/master/R/mlm.variance.distribution.R\n\n",
        "# Calculate I-squared values and variance distribution\n",
        "i2 <- mlm.variance.distribution(m_multi)\n\n",
        "# Print results and total I2\n",
        "i2\n"
      )
    })
    
    # Render the R script content
    output$generated_code_i2tlma <- renderText({
      script_content_i2tlma()
    })
    
    # Download handler for the R script
    output$rscript_i2tlma <- downloadHandler(
      filename = function() {
        "script.i2.3lma.txt"  # Set the name of the downloaded file
      },
      content = function(file) {
        # Write the R script content to the file
        writeLines(script_content_i2tlma(), file)
      }
    )

    
    
    
    
    
###Outlier and Influence Analysis 3lma ----

    resultsvisible3lmaout <- reactiveVal(FALSE)
    
    observeEvent(input$run_inf, {
      resultsvisible3lmaout(TRUE)
    })
    
    observeEvent(input$inffile, {
      resultsvisible3lmaout(FALSE)
    })
    
    output$dynamicResults3lmaout <- renderUI({
      if (resultsvisible3lmaout()) {
        tagList(
          #progress bar
            conditionalPanel(
              condition = "output.progressActive",
              uiOutput("progress")),
            h3("Outlier Results"),
            downloadButton("download_outliers", "Download Outlier Plot"),
            plotOutput("outlier_plot"),
            h3("Influence Results"),
            p("It is important to see if the outliers significantly influence our results. We'll examine three metrics: Cook's distance, DFBETAS, and hat values. You will see that there are columns with these names _flag. If that column says TRUE, that means that study had significant influence according to that metric."),
            downloadButton("download_influence", "Download .csv with Influence Results"),
            h3("Influence Statistics"),
            tableOutput("influence_table"),
            h3("R Script"),
            downloadButton("download_r_script_inftlma"),
            verbatimTextOutput("script_display_inftlma"),
            h3("Need Help Understanding The Results?"),
            p("If you want help interpreting these results, please see ", HTML("<a href='https://noah-schroeder.github.io/reviewbook/meta.html#checking-for-outliers-and-influence'>my open book</a>"),
            ),
          )
    }
    })
    
    
    
    
    
    
    # Define a reactive value to hold the influence data
    influence_data <- reactiveVal(NULL)
    infprog <- reactiveVal(0)
    progress_active <- reactiveVal(FALSE)
    # Reactive function to perform analysis and update influence data
    observeEvent(input$run_inf, {
      req(input$inffile)
      infprog(0)
      progress_active(TRUE)  # Activate progress bar
      withProgress(message = "Running Meta-Analysis...", value = 0, {
        # Read uploaded CSV file
        data <- read.csv(input$inffile$datapath)
        
        # Run the meta-analysis
        m_multi <- rma.mv(yi, vi,
                          random = ~ 1 | Study/ES_number,
                          method = "REML",
                          test = "t",
                          dfs = "contain",
                          data = data)
        incProgress(1/8, message = "Performing outlier analysis") 
        # Perform outlier and influence analysis
        data$upperci <- data$yi + 1.96 * sqrt(data$vi)
        data$lowerci <- data$yi - 1.96 * sqrt(data$vi)
        data$outlier <- data$upperci < m_multi$ci.lb | data$lowerci > m_multi$ci.ub
        
        incProgress(2/8, message = "Calculating Cook's Distance") 
        # Calculate Cook's distance
        cooks <- cooks.distance(m_multi)
        incProgress(3/8, message = "Calculating DFBETAS") 
        # Calculate dfbetas
        dfbetas <- dfbetas(m_multi)
        incProgress(4/8, message = "Calculating hat values") 
        # Calculate hat values
        hatvalues <- hatvalues(m_multi)
        
        # Calculate p/k
        p <- length(coef(m_multi))
        k <- nrow(data)
        incProgress(5/8, message = "Checking for influential studies") 
        # Check if there are more predictors than just the intercept
        if (length(coef(m_multi)) > 1) {
          # Remove the intercept term from the count of coefficients
          p <- p - 1
        }
        
        p_over_k <- 3 * (p / k)
        
        # Calculate hat_flag
        hat_flag <- ifelse(hatvalues > p_over_k, "TRUE", "")
        
        incProgress(6/8, message = "Building table...")
        # Combine influence metrics with correct column names
        influence <- data.frame(Study = data$Study,
                                effect_size = data$yi,
                                outlier = ifelse(data$outlier == FALSE, "", "TRUE"),  # Include outlier column                                cooks = cooks,
                                cooks = cooks,
                                cooks_flag = ifelse(cooks > 0.5, "TRUE", ""),
                                dfbetas = dfbetas,
                                dfbetas_flag = ifelse(abs(dfbetas) > 1, "TRUE", ""),
                                hatvalues = hatvalues,
                                hat_flag = hat_flag)
        
        # Rename the columns with proper names
        colnames(influence)[which(colnames(influence) == "intrcpt")] <- "dfbetas"
        colnames(influence)[which(colnames(influence) == "intrcpt.1")] <- "dfbetas_flag"
        
        
        # Update influence data
        influence_data(influence)
        incProgress (7/8, detail = "Analyses complete, building plots...")
        progress_active(FALSE)  # Deactivate progress bar
      })
    })
    
    # Make progressActive available to JavaScript
    output$progressActive <- reactive({
      progress_active()
    })
    outputOptions(output, "progressActive", suspendWhenHidden = FALSE)
    
    
    output$progressActive <- reactive({
      progress_active()
    })
    
    # Render outlier plot
    output$outlier_plot <- renderPlot({
      analysis_data <- influence_data()
      
      ggplot(data = analysis_data, aes(x = effect_size, colour = outlier, fill = outlier)) +
        geom_histogram(alpha = 0.2) +
        geom_vline(xintercept = analysis_data$m_multi$b[1]) +
        theme_bw()
    })
    
    
    # Render influence table
    output$influence_table <- renderTable({
      influence_data()
    })
    
    # Download influence table as CSV
    output$download_influence <- downloadHandler(
      filename = function() {
        "influence_table.csv"
      },
      content = function(file) {
        # Write the influence table to a CSV file
        write.csv(influence_data(), file, row.names = FALSE)
      }
    )
    
    
    
    # Download handler for the outlier plot button
    output$download_outliers <- downloadHandler(
      filename = function() {
        "outlier_plot.png"  # Specify file name
      },
      content = function(file) {
        # Capture the plot as a PNG file
        png(file, width = 2800, height = 2400, units = "px", res = 300)
        # Create the outlier plot
        p <- ggplot(data = influence_data(), aes(x = effect_size, colour = outlier, fill = outlier)) +
          geom_histogram(alpha = 0.2) +
          geom_vline(xintercept = mean(influence_data()$effect_size)) +  # Example line at mean effect size
          theme_bw()
        print(p)  # Print the plot
        dev.off()  # Turn off the PNG device
      }
    )
    
    
    
    script_content_inftlma <- reactive({
      paste(
        "# Load required packages\n",
        "library(metafor)\n",
        "library(ggplot2)\n\n",
        "# Read uploaded CSV file\n",
        paste("data_inftlma <- read.csv(\"", gsub("[^a-zA-Z0-9._-]", "_", input$inffile$name), "\")\n\n", sep = ""),
        "# Run the meta-analysis\n",
        "m_multi_inftlma <- rma.mv(yi, vi,\n",
        "                          random = ~ 1 | Study/ES_number,\n",
        "                          method = \"REML\",\n",
        "                          test = \"t\",\n",
        "                          dfs = \"contain\",\n",
        "                          data = data_inftlma)\n\n",
        "# Perform outlier and influence analysis\n",
        "data_inftlma$upperci <- data_inftlma$yi + 1.96 * sqrt(data_inftlma$vi)\n",
        "data_inftlma$lowerci <- data_inftlma$yi - 1.96 * sqrt(data_inftlma$vi)\n",
        "data_inftlma$outlier <- data_inftlma$upperci < m_multi_inftlma$ci.lb | data_inftlma$lowerci > m_multi_inftlma$ci.ub\n\n",
        "# Calculate Cook's distance\n",
        "cooks_inftlma <- cooks.distance(m_multi_inftlma)\n\n",
        "# Calculate dfbetas\n",
        "dfbetas_inftlma <- dfbetas(m_multi_inftlma)\n\n",
        "# Calculate hat values\n",
        "hatvalues_inftlma <- hatvalues(m_multi_inftlma)\n\n",
        "# Calculate p/k\n",
        "p_inftlma <- length(coef(m_multi_inftlma))\n",
        "k_inftlma <- nrow(data_inftlma)\n",
        "if (length(coef(m_multi_inftlma)) > 1) {\n",
        "  p_inftlma <- p_inftlma - 1\n",
        "}\n",
        "p_over_k_inftlma <- 3 * (p_inftlma / k_inftlma)\n\n",
        "# Calculate hat_flag\n",
        "hat_flag_inftlma <- ifelse(hatvalues_inftlma > p_over_k_inftlma, \"TRUE\", \"\")\n\n",
        "# Combine influence metrics with correct column names\n",
        "influence_inftlma <- data.frame(Study = data_inftlma$Study,\n",
        "                                effect_size = data_inftlma$yi,\n",
        "                                outlier = ifelse(data_inftlma$outlier == FALSE, \"\", \"TRUE\"),\n",
        "                                cooks = cooks_inftlma,\n",
        "                                cooks_flag = ifelse(cooks_inftlma > 0.5, \"TRUE\", \"\"),\n",
        "                                dfbetas = dfbetas_inftlma,\n",
        "                                dfbetas_flag = ifelse(abs(dfbetas_inftlma) > 1, \"TRUE\", \"\"),\n",
        "                                hatvalues = hatvalues_inftlma,\n",
        "                                hat_flag = hat_flag_inftlma)\n\n",
        "# Rename the columns with proper names\n",
        "colnames(influence_inftlma)[which(colnames(influence_inftlma) == \"intrcpt\")] <- \"dfbetas\"\n",
        "colnames(influence_inftlma)[which(colnames(influence_inftlma) == \"intrcpt.1\")] <- \"dfbetas_flag\"\n\n",
        "# Print the results and plots\n",
        "print(influence_inftlma)\n",
        "ggplot(data = influence_inftlma, aes(x = effect_size, colour = outlier, fill = outlier)) +\n",
        "  geom_histogram(alpha = 0.2) +\n",
        "  geom_vline(xintercept = m_multi_inftlma$b[1]) +\n",
        "  theme_bw()\n"
      )
    })
    
    # Display the R script on the UI
    output$script_display_inftlma <- renderText({
      script_content_inftlma()
    })
    
    # Download R script as a file
    output$download_r_script_inftlma <- downloadHandler(
      filename = function() {
        "script.inftlma.txt"
      },
      content = function(file) {
        writeLines(script_content_inftlma(), file)
      }
    )
    
    
    
    
    
    
    
    
    
### Cat Mod Analysis 3lma----
    # Initialize a reactive value for displaying results
    resultsvisible3lCcat <- reactiveVal(FALSE)
    
    observeEvent(input$run_analysis, {
      resultsvisible3lCcat(TRUE)
    })
    
    observeEvent(input$dropdown, {
      resultsvisible3lCcat(FALSE)
    })
    
    observeEvent(input$modfile, {
      resultsvisible3lCcat(FALSE)
    })
    
    output$dynamicResults3lCcat <- renderUI({
      if (resultsvisible3lCcat()) {
        tagList(
          # Display the results
          box(title="Important Note", width = 12, status = "primary",
          p("In metafor there are two different tests of the moderator. The table below presents the omnibus test of moderators from the model with an intercept. This is the same statistic as you may be used to seeing as Qbetween in conventional meta-analysis. The effect sizes etc. provided in the moderator table below are from the model without an intercept. In a conventional meta-analysis, this is a presentation consistent with what you may expect to see from other software packages such as Comprehensive Meta-Analysis.")),
          downloadButton("download_results", "Download Results"),
          tableOutput("modtable_output"),
          h3("Basic Interpretation Tips"),
          p("First you should look at the", strong("TestOfModerators"), "column. If this is significant, it means there are significant differences between levels of your moderator."),
          p("Note the following columns I have created to aid in interpretation:"),
          p(strong("nexp"),"is the sample size of the intervention group, and", strong("nctrl"), "is the sample size of the control group. Note that this is not calculating the actual number of unique participants, because this code is simply conditionally summing the sample size columns in our data set. For example, if a study had one experimental group (n = 10) and one control group (n = 10), and had three outcomes that were included in the analysis (meaning, each appears as its own row in the data set), this code will say there were 30 participants in each group rather than 10. While this is expected in dependent data such as this, it is something to be aware of so you do not make a claim such as, “our analysis of 60 participants” when in reality, your analysis is only 20 unique participants. So, please be careful of your wording when you describe the participant numbers to ensure strict accuracy."),
          p(strong("kcomp"),"is the number of comparisons examined. The total number of kcomp in the table should correspond to the number of rows in your dataset."),
          p(strong("kstudies"),"is the number of studies providing comparisons in the analysis. Note that it is possible for this not to sum to the same number as appears in your data set. For example, if Study A provided 4 comparisons and 1 or more were at different levels of this moderator variable, kstudies will not equal the total number of unique studies in the dataset because it is being counted in multiple moderator levels."),
          h3("R Script"),
          downloadButton("download_script_catmodtlma"),
          verbatimTextOutput("script_display_catmodtlma"),
          h3("Need Help Understanding The Results?"),
          p("If you want help interpreting these results, please see ", HTML("<a href='https://noah-schroeder.github.io/reviewbook/meta.html#moderator-analysis'>my open book</a>"),
          ),
        )
      }
    })
    
    
    
    # Reactive expression to read uploaded CSV file
    uploaded_data3lc <- reactive({
      req(input$modfile)
      read.csv(input$modfile$datapath)
    })
    
    # Update selectInput choices when file is uploaded
    observeEvent(input$modfile, {
      updateSelectInput(session, "dropdown", choices = colnames(uploaded_data3lc()))
    })
    
    # Define a reactive expression for running the moderator analysis without intercept
    mod_without_intercept_3lc <- reactive({
      req(input$run_analysis, input$dropdown)  # Ensure dropdown choice is available
      mod_formula_3lc <- as.formula(paste("~ -1 + factor(", input$dropdown, ")"))
      mod_result_no_intercept_3lc <- rma.mv(yi, vi, random = ~ 1 | Study/ES_number, mods = mod_formula_3lc, 
                                            test = "t", dfs = "contain", data = uploaded_data3lc())
      model_summary_no_intercept_3lc <- summary(mod_result_no_intercept_3lc)
      summary_table_no_intercept_3lc <- coef(model_summary_no_intercept_3lc)
      
      if(is.null(summary_table_no_intercept_3lc) || nrow(summary_table_no_intercept_3lc) == 0) {
        stop("The summary table for the model without intercept has no data.")
      }
      
      # Summarize participants and compute kstudies
      participants_summary_3lc <- uploaded_data3lc() %>%
        group_by(!!sym(input$dropdown)) %>%
        summarise(
          nexp = sum(Exp_n, na.rm = TRUE),
          ctrl = sum(Ctrl_n, na.rm = TRUE),
          kcomp = n(),
          kstudies = n_distinct(Study),
          .groups = 'drop'
        )
      
      # Remove "factor(variable_name)" prefix from the first column
      participants_summary_convc$level_name <- gsub("^factor\\((.+)\\)", "\\1", levels(participants_summary_convc[[1]]))
      
      # Add participant summary data directly to summary_table_no_intercept_3lc
      summary_table_no_intercept_3lc$nexp <- participants_summary_3lc$nexp
      summary_table_no_intercept_3lc$ctrl <- participants_summary_3lc$ctrl
      summary_table_no_intercept_3lc$kcomp <- participants_summary_3lc$kcomp
      summary_table_no_intercept_3lc$kstudies <- participants_summary_3lc$kstudies
      
      # Add row names as a column called "Term"
      summary_table_no_intercept_3lc$Term <- NA
      
      # Reorder columns to start with Term, followed by participant summaries, then model results
      summary_table_no_intercept_3lc <- summary_table_no_intercept_3lc %>%
        select(Term, nexp, ctrl, kcomp, kstudies, everything())
      
      summary_table_no_intercept_3lc
    })
    
    # Define a reactive expression for capturing only the "Test of Moderators" information from a model with intercept
    mod_with_intercept_3lc <- reactive({
      req(input$run_analysis, input$dropdown)
      mod_formula_with_intercept_3lc <- as.formula(paste("~ factor(", input$dropdown, ")"))
      mod_result_with_intercept_3lc <- rma.mv(yi, vi, random = ~ 1 | Study/ES_number, mods = mod_formula_with_intercept_3lc, 
                                              test = "t", dfs = "contain", data = uploaded_data3lc())
      
      if(is.null(mod_result_with_intercept_3lc$QM)) {
        return(NA)  # Return NA if there is no Test of Moderators data available
      }
      
      # Format Test of Moderators data as a string
      QM_3lc <- mod_result_with_intercept_3lc$QM
      QMp_3lc <- mod_result_with_intercept_3lc$QMp
      df1_3lc <- mod_result_with_intercept_3lc$QMdf[1]
      df2_3lc <- mod_result_with_intercept_3lc$QMdf[2]
      sprintf("F(%d, %d) = %.3f, p-val = %.3f", df1_3lc, df2_3lc, QM_3lc, QMp_3lc)
    })
    
    # Define a reactive expression for running the moderator analysis without intercept
    mod_without_intercept_3lc <- reactive({
      req(input$run_analysis, input$dropdown)  # Ensure dropdown choice is available
      mod_formula_3lc <- as.formula(paste("~ -1 + factor(", input$dropdown, ")"))
      mod_result_no_intercept_3lc <- rma.mv(yi, vi, random = ~ 1 | Study/ES_number, mods = mod_formula_3lc, 
                                            test = "t", dfs = "contain", data = uploaded_data3lc())
      model_summary_no_intercept_3lc <- summary(mod_result_no_intercept_3lc)
      summary_table_no_intercept_3lc <- coef(model_summary_no_intercept_3lc)
      
      if(is.null(summary_table_no_intercept_3lc) || nrow(summary_table_no_intercept_3lc) == 0) {
        stop("The summary table for the model without intercept has no data.")
      }
      
      participants_summary_3lc <- uploaded_data3lc() %>%
        group_by(!!sym(input$dropdown)) %>%
        summarise(
          nexp = sum(Exp_n, na.rm = TRUE),
          ctrl = sum(Ctrl_n, na.rm = TRUE),
          kcomp = n(),
          kstudies = n_distinct(Study),
          .groups = 'drop'
        )
      
      summary_table_no_intercept_3lc$nexp <- participants_summary_3lc$nexp
      summary_table_no_intercept_3lc$ctrl <- participants_summary_3lc$ctrl
      summary_table_no_intercept_3lc$kcomp <- participants_summary_3lc$kcomp
      summary_table_no_intercept_3lc$kstudies <- participants_summary_3lc$kstudies
      summary_table_no_intercept_3lc$Term <- rownames(summary_table_no_intercept_3lc)
      
      # Add row names as a column called "Term"
      summary_table_no_intercept_3lc$Term <- rownames(summary_table_no_intercept_3lc)
      
      # Ensure 'TestOfModerators' column exists
      if (!"TestOfModerators" %in% names(summary_table_no_intercept_3lc)) {
        summary_table_no_intercept_3lc$TestOfModerators <- NA
      }
      
      summary_table_no_intercept_3lc <- summary_table_no_intercept_3lc %>%
        select(Term, nexp, ctrl, kcomp, kstudies, everything())
      
      colnames(summary_table_no_intercept_3lc)[which(colnames(summary_table_no_intercept_3lc) == "Term")] <- input$dropdown
      
      summary_table_no_intercept_3lc
    })
    
    # Display results in a table
    output$modtable_output <- renderTable({
      req(input$run_analysis > 0)
      
      # Define the numeric columns list here to ensure visibility
      numeric_columns <- c("nexp", "ctrl", "kcomp", "kstudies", "estimate", "se", "pval", "tval", "ci.lb", "ci.ub")  # Adjust list as necessary
      
      result_table_data_3lc <- mod_without_intercept_3lc()
      
      # Apply rounding to numeric columns first
      result_table_data_3lc[numeric_columns] <- lapply(result_table_data_3lc[numeric_columns], function(x) {
        # Convert to numeric if not already, then round
        if(!is.numeric(x)) x <- as.numeric(as.character(x))
        round(x, 3)
      })
      
      # Replace NA with empty string after rounding
      result_table_data_3lc <- replace(result_table_data_3lc, is.na(result_table_data_3lc), "")
      
      # Combine ci.lb and ci.ub into one column in the format [ci.lb, ci.ub] and label it "95% CI"
      result_table_data_3lc$`95% CI` <- paste0("[", result_table_data_3lc$ci.lb, ", ", result_table_data_3lc$ci.ub, "]")
      
      # Remove the original ci.lb and ci.ub columns
      result_table_data_3lc <- result_table_data_3lc[, !names(result_table_data_3lc) %in% c("ci.lb", "ci.ub")]
      
      # Modify the factor levels in the first column
      result_table_data_3lc[, 1] <- gsub(paste0("^factor\\(", input$dropdown, "\\)"), "", rownames(result_table_data_3lc))
      
      # Add the "Test of Moderators" after numeric handling to prevent structure mismatch
      test_of_mods_3lc <- mod_with_intercept_3lc()  # Get Test of Moderators
      if (!is.na(test_of_mods_3lc)) {
        # Create a row for the Test of Moderators
        test_mods_row <- setNames(as.data.frame(matrix("", ncol = ncol(result_table_data_3lc), nrow = 1)), names(result_table_data_3lc))
        test_mods_row$TestOfModerators <- test_of_mods_3lc
        result_table_data_3lc <- rbind(result_table_data_3lc, test_mods_row)
      }
      # Determine the order of columns to place '95% CI' second to last, just before 'TestOfModerators'
      all_columns <- names(result_table_data_3lc)
      without_test_of_mods <- all_columns[!all_columns %in% c("95% CI", "TestOfModerators")]
      desired_order <- c(without_test_of_mods, "95% CI", "TestOfModerators")
      
      result_table_data_3lc <- result_table_data_3lc[, desired_order]
      result_table_data_3lc
    })
    
    # Define the server logic for the download handler
    output$download_results <- downloadHandler(
      filename = function() {
        paste("mod.", input$dropdown, Sys.Date(), ".csv", sep = "")
      },
      content = function(file) {
        req(input$run_analysis > 0)  # Ensure the analysis has been run
        
        # Generate the table data using the same logic as for display
        result_table_data_3lc <- mod_without_intercept_3lc()
        
        # Replace NA with empty string for consistent output processing
        result_table_data_3lc <- replace(result_table_data_3lc, is.na(result_table_data_3lc), "")
        
        # Define the numeric columns that should be rounded
        numeric_columns <- c("nexp", "ctrl", "kcomp", "kstudies", "estimate", "se", "pval", "tval", "ci.lb", "ci.ub")  # Adjust list as necessary
        
        # Apply rounding to numeric columns
        result_table_data_3lc[numeric_columns] <- lapply(result_table_data_3lc[numeric_columns], function(x) {
          if(is.numeric(x)) round(x, 3) else x
        })
        
        # Combine ci.lb and ci.ub into one column in the format [ci.lb, ci.ub] and label it "95% CI"
        result_table_data_3lc$`95% CI` <- paste0("[", result_table_data_3lc$ci.lb, ", ", result_table_data_3lc$ci.ub, "]")
        
        # Remove the original ci.lb and ci.ub columns
        result_table_data_3lc <- result_table_data_3lc[, !names(result_table_data_3lc) %in% c("ci.lb", "ci.ub")]
        
        # Get Test of Moderators, if available
        test_of_mods_3lc <- mod_with_intercept_3lc()
        if (!is.na(test_of_mods_3lc)) {
          test_mods_row <- setNames(as.data.frame(matrix("", ncol = ncol(result_table_data_3lc), nrow = 1)), names(result_table_data_3lc))
          test_mods_row$TestOfModerators <- test_of_mods_3lc
          result_table_data_3lc <- rbind(result_table_data_3lc, test_mods_row)
        }
        
        # Determine the order of columns to place '95% CI' second to last, just before 'TestOfModerators'
        all_columns <- names(result_table_data_3lc)
        without_test_of_mods <- all_columns[!all_columns %in% c("95% CI", "TestOfModerators")]
        desired_order <- c(without_test_of_mods, "95% CI", "TestOfModerators")
        
        result_table_data_3lc <- result_table_data_3lc[, desired_order]
        
        # Write the data frame to a CSV file
        write.csv(result_table_data_3lc, file, row.names = FALSE, na = "")
      }
    )
    
    
    
    
    # Generate R script
    r_scripttlma <- renderText({
      req(input$run_analysis)
      
     paste0(
        "# Load necessary libraries\n",
        "library(metafor)\n",
        "library(dplyr)\n\n",
        "# Function to perform the meta-analysis and generate results\n",
        "run_meta_analysis <- function() {\n",
        "  # Read the data\n",
        "  data <- read.csv('", input$modfile$name, "')\n\n",
        "  # Model with intercept (for testing moderators)\n",
        "  mod_with_intercept <- rma.mv(yi, vi, \n",
        "                               random = ~ 1 | Study/ES_number, \n",
        "                               mods = ~ factor(", input$dropdown, "), \n",
        "                               test = \"t\", dfs = \"contain\", \n",
        "                               data = data)\n\n",
        "  # Summary of model with intercept\n",
        "  summary_with_intercept <- summary(mod_with_intercept)\n\n",
        "  # Model without intercept (for building table)\n",
        "  mod_without_intercept <- rma.mv(yi, vi, \n",
        "                                  random = ~ 1 | Study/ES_number, \n",
        "                                  mods = ~ -1 + factor(", input$dropdown, "), \n",
        "                                  test = \"t\", dfs = \"contain\", \n",
        "                                  data = data)\n\n",
        "  # Summary of model without intercept\n",
        "  summary_without_intercept <- summary(mod_without_intercept)\n",
        "  summary_table_without_intercept <- coef(summary_without_intercept)\n\n",
        "  # Summarize participants\n",
        "  participants_summary <- data %>%\n",
        "    group_by(", input$dropdown, ") %>%\n",
        "    summarise(\n",
        "      nexp = sum(Exp_n, na.rm = TRUE),\n",
        "      ctrl = sum(Ctrl_n, na.rm = TRUE),\n",
        "      kcomp = n(),\n",
        "      kstudies = n_distinct(Study),\n",
        "      .groups = 'drop'\n",
        "    )\n\n",
        "  # Add participant summary to the table\n",
        "  summary_table_without_intercept$nexp <- participants_summary$nexp\n",
        "  summary_table_without_intercept$ctrl <- participants_summary$ctrl\n",
        "  summary_table_without_intercept$kcomp <- participants_summary$kcomp\n",
        "  summary_table_without_intercept$kstudies <- participants_summary$kstudies\n\n",
        "  # Rename and reorder columns\n",
        "  summary_table_without_intercept$Term <- rownames(summary_table_without_intercept)\n",
        "  summary_table_without_intercept <- summary_table_without_intercept %>%\n",
        "    select(Term, nexp, ctrl, kcomp, kstudies, everything())\n\n",
        "  # Create results list\n",
        "  results <- list(\n",
        "    model_with_intercept = list(\n",
        "      summary = summary_with_intercept,\n",
        "      TestOfModerators = ifelse(!is.null(mod_with_intercept$QM), \n",
        "                                sprintf(\"F(%d, %d) = %.3f, p-val = %.3f\", \n",
        "                                        mod_with_intercept$QMdf[1], \n",
        "                                        mod_with_intercept$QMdf[2], \n",
        "                                        mod_with_intercept$QM, \n",
        "                                        mod_with_intercept$QMp), \n",
        "                                NA)\n",
        "    ),\n",
        "    model_without_intercept = summary_table_without_intercept\n",
        "  )\n\n",
        "  return(results)\n",
        "}\n\n",
        "# Run and see results\n",
        "run_meta_analysis()\n"
      )
      

    })
    
    # Render the script content as text in the UI
    output$script_display_catmodtlma <- renderText({
      r_scripttlma()
    })
    
    # Define the server logic for the download handler
    output$download_script_catmodtlma <- downloadHandler(
      filename = function() {
        paste("script.mod.", input$dropdown, ".txt", sep = "")
      },
      content = function(file) {
        writeLines(r_scripttlma(), file)
      }
    )

    
    
###Cont Mod Analysis 3lma ----
    
    # Initialize a reactive value for displaying results
    resultsvisibleRVECont3lCcont <- reactiveVal(FALSE)
    
    observeEvent(input$run_analysisa, {
      resultsvisibleRVECont3lCcont(TRUE)
    })
    
    observeEvent(input$dropdowna, {
      resultsvisibleRVECont3lCcont(FALSE)
    })
    
    observeEvent(input$modfilea, {
      resultsvisibleRVECont3lCcont(FALSE)
    })
    
    output$dynamicResults3lCcont <- renderUI({
      if (resultsvisibleRVECont3lCcont()) {
        tagList(
          # Display the results
          box( title= "Important Note", width = 12, status = "primary",
          p("In metafor there are two different tests of the moderator. The table below presents the omnibus test of moderators from the model with an intercept. This is the same statistic as you may be used to seeing as Qbetween in conventional meta-analysis. The effect sizes etc. provided in the moderator table below are from the model without an intercept. In a conventional meta-analysis, this is a presentation consistent with what you may expect to see from other software packages such as Comprehensive Meta-Analysis.")),
          downloadButton("download_resultsa", "Download Results"),
          tableOutput("modtable_outputa"),
          h3("R Script"),
          downloadButton("download_script_contmodtlma"),
          verbatimTextOutput("script_display_contmodtlma"),
          h3("Need Help Understanding The Results?"),
          p("If you want help interpreting these results, please see ", HTML("<a href='https://noah-schroeder.github.io/reviewbook/meta.html#moderator-analysis'>my open book</a>"),
          ),
        )
      }
    })
    
    
    
    
    
    uploaded_dataa <- reactive({
      req(input$modfilea)
      read.csv(input$modfilea$datapath)
    })
    
    observeEvent(input$modfilea, {
      updateSelectInput(session, "dropdowna", choices = colnames(uploaded_dataa()))
    })
    
    mod_summary3lma <- reactive({
      req(input$run_analysisa, input$dropdowna)
      mod_formula <- as.formula(paste("~", input$dropdowna))
      
      mod_result_with_intercept <- rma.mv(yi, vi, random = ~ 1 | Study/ES_number, mods = mod_formula,
                                          test = "t", dfs = "contain", data = uploaded_dataa())
      model_summarya <- summary(mod_result_with_intercept)
      summary_tablea <- coef(model_summarya)
      
      if(is.null(summary_tablea) || nrow(summary_tablea) == 0) {
        stop("The summary table has no data.")
      }
      
      QMa <- round(mod_result_with_intercept$QM, 3)
      QMpa <- round(mod_result_with_intercept$QMp, 3)
      df1a <- mod_result_with_intercept$QMdf[1]
      df2a <- mod_result_with_intercept$QMdf[2]
      
      # Combine CI_Lower and CI_Upper into one column formatted as [CI_Lower, CI_Upper]
      CI_formatted <- paste0("[", round(summary_tablea[, "ci.lb"], 3), ", ", round(summary_tablea[, "ci.ub"], 3), "]")
      
      result_tablea <- data.frame(
        Term = c(rownames(summary_tablea), "Test of Moderator"),
        Estimate = c(round(summary_tablea[, "estimate"], 3), NA),
        SE = c(round(summary_tablea[, "se"], 3), NA),
        tvalue = c(round(summary_tablea[, "tval"], 3), NA),
        pvalue = c(round(summary_tablea[, "pval"], 3), NA),
        `95% CI` = c(CI_formatted, NA),  # New CI column
        TestOfModerator = c(rep(NA, nrow(summary_tablea)), sprintf("F(%d, %d) = %.3f, p-val = %.3f", 
                                                                   df1a, df2a, QMa, QMpa)),
        check.names = FALSE
      )
      # Replace values less than 0.001 with "<0.001" in the "p-value" column
      result_tablea$pvalue[result_tablea$PValue < 0.001] <- "< 0.001"
      return(result_tablea)
    })
    
    output$modtable_outputa <- renderTable({
      req(mod_summary3lma())
      mod_summary3lma()
    }, na = '')  # Display NA as empty cells
    
    # Define a download handler for exporting the results as a CSV file
    output$download_resultsa <- downloadHandler(
      filename = function() {
        paste("mod.", input$dropdowna, Sys.Date(), ".csv", sep = "")
        },
      content = function(file) {
        req(mod_summary3lma())
        write.csv(mod_summary3lma(), file, row.names = FALSE, na = "")  # Write empty strings instead of NA
      }
    )

    
    # Generate R script as a reactive expression
    script_content_contmodtlma <- reactive({
      req(input$modfilea, input$dropdowna)
      # Debugging: Print inputs to console
  
      paste(
        "# Load necessary libraries\n",
        "library(metafor)\n",
        "# Read the data\n",
        paste("data <- read.csv('", input$modfilea$name, "')\n\n", sep = ""),
        "# Model with intercept\n",
        paste(
          "mod_with_intercept <- rma.mv(yi, vi, \n",
          "                               random = ~ 1 | Study/ES_number, \n",
          "                               mods = ~ ", input$dropdowna, ", \n",
          "                               test = \"t\", dfs = \"contain\", \n",
          "                               data = data)\n\n",
          sep = ""
        ),
        "# Results\n",
        "mod_with_intercept\n",
        sep = ""
      )
    })
    
    # Render the script content as text in the UI
    output$script_display_contmodtlma <- renderText({
      script_content_contmodtlma()
    })
    
    # Define the server logic for the download handler
    output$download_script_contmodtlma <- downloadHandler(
      filename = function() {
        paste("script.mod.", input$dropdowna, ".txt", sep = "")
      },
      content = function(file) {
        writeLines(script_content_contmodtlma(), file)
      }
    )

 
    
    
    
    
    
### Publication bias 3lma ---- 

    # Initialize a reactive value for displaying results
    resultsvisibleRVECont3lpub <- reactiveVal(FALSE)
    
    observeEvent(input$run_pub, {
      resultsvisibleRVECont3lpub(TRUE)
    })
    
    observeEvent(input$pubbiasfile, {
      resultsvisibleRVECont3lpub(FALSE)
    })
    
    output$dynamicResults3lpub <- renderUI({
      if (resultsvisibleRVECont3lpub()) {
        tagList(
            h3("Check The Data Preparation"),
            p("The app has added a few columns to your data, so it is imporant to check them before creating our plots. The most important item to check is to make sure that your unique studies are sequentially numbered. The table below presents your data, organized by author name alphabetically. Each unique study should be assigned a unique number in the Study column. If that is correct, the other items should be correct as well. Column 'out' refers to unique outcomes; these are organized by how they are organized in your data file so they may not be sequentially numbered here. That is OK and you generally do not need to check this column. You also generally do not need to check the standard error calculation, however you are of course welcome to if you wish."),
            div(class = "scrollable",  tableOutput("data_summary")),
            h3("Forest Plot"),
            actionButton("forest", "Generate Forest and Funnel Plots"),
            conditionalPanel(
              condition = "input.forest > 0",
              box( title = "*Important Note*", width = 12, status = "primary",
                   p("The plots are rendered at 'half-page' height (800px by 600px) within the software. You may choose to download at this size (about half a standard page in height) or full-page size. If you need a taller image (most typical on the comparison-level forest plot) than the 'full-page' download size may be helpful.",
                   )),
              p("This is a forest plot for dependent data, based on code by Fernández-Castilla et al. (2020). The black boxes represent the average effect size from the comparisons within each study, and the black lines are the study precision. The grey lines are the median precision of one effect size from each study. The size of the effect size box is representative of its weight in the analysis. J represents how many comparisons were analyzed from each study."),
              downloadButton("download_forest", "Download Forest Plot (half-page)"), downloadButton("download_forestf", "Download Forest Plot (full-page)"),
              plotOutput("forest_plot", width = 800, height = 600),
              h3("Funnel Plots"),
              h4("Comparison-Level Funnel Plot"),
              p("This is a funnel plot of every comparison within the analysis."),
              downloadButton("download_compfunnel", "Download Comparison-Level Funnel Plot (half-page)"),  downloadButton("download_compfunnelf", "Download Comparison-Level Funnel Plot (full-page)"), 
              plotOutput("comp_level_funnel", width = 800, height = 600),
              h4("Study-Level Funnel Plot"),
              p("This is a funnel plot of every study that contributed comparisons to the analysis."),
              downloadButton("download_studyfunnel", "Download Study-Level Funnel Plot (half-page)"), downloadButton("download_studyfunnelf", "Download Study-Level Funnel Plot (full-page)"),
              plotOutput("three_level_study_funnel", width = 800, height = 600),
              h3("R Script"),
              p("The script for these plots is very long. You will need to compute some statistics that you do not have in your dataset by default. You can download a template for the plots (but not calculating the statistics)", HTML("<a href='https://github.com/noah-schroeder/simple_meta-analysis/blob/main/3lmaplots.r'>on github</a>"),"."),
              h3("Need Help Understanding The Results?"),
              p("If you want help interpreting these results, please see ", HTML("<a href='https://noah-schroeder.github.io/reviewbook/meta.html#publication-bias'>my open book</a>")),
            ),
          )
      }
    })
    
    
    
    
    
# Reactive function to read the uploaded file
data <- reactive({
  req(input$pubbiasfile)
  read.table(input$pubbiasfile$datapath, header = TRUE, sep = ",", stringsAsFactors = FALSE)
})

# Reactive function to process data upon button click
processed_data <- eventReactive(input$run_pub, {
  req(data())
  data_with_totaln <- data()
  
  # Check if required columns exist
  if ("Exp_n" %in% colnames(data_with_totaln) && "Ctrl_n" %in% colnames(data_with_totaln)) {
    # Calculate total number of participants for each row
    data_with_totaln$totaln <- rowSums(data_with_totaln[c("Exp_n", "Ctrl_n")], na.rm = TRUE)
    
    # Calculate standard error
    data_with_totaln$standard_error <- sqrt(data_with_totaln$vi) / sqrt(data_with_totaln$totaln)
    
    # Create sequentially numbered unique cases within each study
    data_with_totaln <- data_with_totaln %>%
      mutate(study = as.integer(factor(Study))) %>%
      select(study, author = Study, out = ES_number, yi, vi, standard_error)

    # Assign vectors for subsequent analyses
    study <- data_with_totaln$study
    out <- data_with_totaln$out
    ES <- data_with_totaln$yi
    var <- data_with_totaln$vi
    se <- data_with_totaln$standard_error
    author <- data_with_totaln$author
    
    return(data_with_totaln)
  } else {
    # Show error message if required columns are missing
    showModal(modalDialog(
      title = "Error",
      "Required columns 'Exp_n' and/or 'Ctrl_n' are missing in the uploaded file.",
      easyClose = TRUE
    ))
    return(NULL)
  }
})

# Render data summary as a table
output$data_summary <- renderTable({
  req(processed_data())
  processed_data() %>%
    arrange(author)  # Sort the table by 'author' column
})



# Render the forest plot
output$forest_plot <- renderPlot({
  req(input$forest, processed_data())
  processed <- processed_data()
  author <- processed_data()$author
  study <- processed_data()$study
  ES <- processed_data()$yi
  out <- processed_data()$out
  var <- processed_data()$vi
  se <- processed_data()$standard_error

  # Print lengths of vectors for debugging
  print(c(author = length(author), study = length(study), ES = length(ES), out = length(out), var = length(var), se = length(se)))
  
  # Generate forest plot
  # Pass study to the forest_plot_3 function
  forest_plot_3(author, study, ES, out, var, se, size_lines = 1)
})

# Render the comp level funnel plot
output$comp_level_funnel <- renderPlot({
  req(input$forest,processed_data())  # Assuming processed_data() contains the necessary data
  processed <- processed_data()
  study <- processed$study
  ES <- processed$yi
  out <- processed$out
  var <- processed$vi
  se <- processed$standard_error
  
  # Generate comp level funnel plot
  three_funnel(study, ES, out, var, se)
})

# Render the three level study funnel plot
output$three_level_study_funnel <- renderPlot({
  req(input$forest, processed_data())  # Assuming processed_data() contains the necessary data
  processed <- processed_data()
  study <- processed$study
  ES <- processed$yi
  out <- processed$out
  var <- processed$vi
  se <- processed$standard_error
  
  # Generate three level study funnel plot
  three_funnel_study(study, ES, out, var, se, size_dots=1, numbers=0)  # Make sure size_dots and numbers are defined
})

# Download handler for the forest plot
output$download_forest <- downloadHandler(
  filename = function() {
    "forest_plot.png"  # Specify file name
  },
  content = function(file) {
    # Capture the plot as a PNG file
    png(file, width = 800, height = 600)
    
    # Generate the forest plot
    processed <- processed_data()
    author <- processed$author
    study <- processed$study
    ES <- processed$yi
    out <- processed$out
    var <- processed$vi
    se <- processed$standard_error
    forest_plot_3(author, study, ES, out, var, se, size_lines = 1)  # Generate the forest plot
    
    dev.off()  # Turn off the PNG device
  }
)

# Download handler for the forest plot
output$download_forestf <- downloadHandler(
  filename = function() {
    "forest_plot.png"  # Specify file name
  },
  content = function(file) {
    # Capture the plot as a PNG file
    png(file, width = 800, height = 1056)
    
    # Generate the forest plot
    processed <- processed_data()
    author <- processed$author
    study <- processed$study
    ES <- processed$yi
    out <- processed$out
    var <- processed$vi
    se <- processed$standard_error
    forest_plot_3(author, study, ES, out, var, se, size_lines = 1)  # Generate the forest plot
    
    dev.off()  # Turn off the PNG device
  }
)

# Download handler for three_funnel plot
output$download_compfunnel <- downloadHandler(
  filename = function() {
    "three_funnel_plot.png"  # Specify file name
  },
  content = function(file) {
    # Capture the plot as a PNG file
    png(file, width = 800, height = 600)
    
    # Generate the three_funnel plot
    processed <- processed_data()  # Assuming processed_data() contains required data
    study <- processed$study
    ES <- processed$yi
    out <- processed$out
    var <- processed$vi
    se <- processed$standard_error
    three_funnel(study, ES, out, var, se)  # Generate the three_funnel plot
    
    dev.off()  # Turn off the PNG device
  }
)

# Download handler for three_funnel plot
output$download_compfunnelf <- downloadHandler(
  filename = function() {
    "three_funnel_plot.png"  # Specify file name
  },
  content = function(file) {
    # Capture the plot as a PNG file
    png(file, width = 800, height = 1056)
    
    # Generate the three_funnel plot
    processed <- processed_data()  # Assuming processed_data() contains required data
    study <- processed$study
    ES <- processed$yi
    out <- processed$out
    var <- processed$vi
    se <- processed$standard_error
    three_funnel(study, ES, out, var, se)  # Generate the three_funnel plot
    
    dev.off()  # Turn off the PNG device
  }
)

# Download handler for three_funnel_study plot
output$download_studyfunnel <- downloadHandler(
  filename = function() {
    "three_funnel_study_plot.png"  # Specify file name
  },
  content = function(file) {
    # Capture the plot as a PNG file
    png(file, width = 800, height = 600)
    
    # Generate the three_funnel_study plot
    processed <- processed_data()  # Assuming processed_data() contains required data
    study <- processed$study
    ES <- processed$yi
    out <- processed$out
    var <- processed$vi
    se <- processed$standard_error
    three_funnel_study(study, ES, out, var, se, size_dots=1, numbers=0)  # Generate the three_funnel_study plot
    
    dev.off()  # Turn off the PNG device
  }
)

# Download handler for three_funnel_study plot
output$download_studyfunnelf <- downloadHandler(
  filename = function() {
    "three_funnel_study_plot.png"  # Specify file name
  },
  content = function(file) {
    # Capture the plot as a PNG file
    png(file, width = 800, height = 1056)
    
    # Generate the three_funnel_study plot
    processed <- processed_data()  # Assuming processed_data() contains required data
    study <- processed$study
    ES <- processed$yi
    out <- processed$out
    var <- processed$vi
    se <- processed$standard_error
    three_funnel_study(study, ES, out, var, se, size_dots=1, numbers=0)  # Generate the three_funnel_study plot
    
    dev.off()  # Turn off the PNG device
  }
)
















##3LMA CHE RVE----
### Main Analysis----
# Initialize a reactive value for displaying results
resultsVisibleRVE <- reactiveVal(FALSE)

observeEvent(input$run_che, {
  resultsVisibleRVE(TRUE)
})

observeEvent(input$correlation, {
  resultsVisibleRVE(FALSE)
})

observeEvent(input$chefile, {
  resultsVisibleRVE(FALSE)
})

output$dynamicResultsche <- renderUI({
  if (resultsVisibleRVE()) {
    tagList(
      h3("Model Result"),
      downloadButton("downloadRVEvar", "Download RVE Results"),
      verbatimTextOutput("che_resultsrobust"),
      h2("Sensitivity Check"),
      downloadButton("downloadRVE", "Download Sensitivity Results"),
      tableOutput("custom_results"),
      h4("Methods Notes"),
      p("This random effects three-level meta-analysis used correlated and hierarchical effects (CHE) and robust variance estimation. It used the correlation you chose as the correlation for CHE. Robust variance estimation was done using the clubSandwich package and metafor (the code is robust(...clubSandwich = TRUE)). You can read more about metafor and cluster-robust tests and confidence intervals here:", HTML("<a href='https://wviechtb.github.io/metafor/reference/robust.html'>metafor robust() documentation</a>"), "Otherwise, the analysis used Restricted Maximum Likelihood Estimation (REML) (the default in metafor package) and we used a three-level structure with ES_number nested within Study. We also used a t distribution rather than a z distribution. You can read about t distributions here:", HTML("<a href='https://wviechtb.github.io/metafor/reference/rma.mv.html'>metafor documentation about rma.mv</a>")),
    )
  }
})

# Reactive value for rho
rho <- reactive({
  as.numeric(input$correlation)
})

# Reactive data preparation
uploaded_data <- reactive({
  req(input$chefile)
  read.csv(input$chefile$datapath)
})

# Reactive covariance matrix calculation
V <- reactive({
  req(uploaded_data())
  with(uploaded_data(), impute_covariance_matrix(vi = vi, cluster = Study, r = rho()))
})

# Compute CHE results
CHEresult <- eventReactive(input$run_che, {
  tryCatch({
    rma.mv(yi, V(),
           random = ~ 1 | Study/ES_number,
           method = "REML",
           test = "t",
           dfs = "contain",
           data = uploaded_data(),
           Sparse = TRUE)
  }, error = function(e) {
    print(e)  # Print error for debugging
    showNotification(conditionMessage(e), type = "error")
    return(NULL)
  })
})

CHEresultrobust <- eventReactive(input$run_che, {
  tryCatch({
    resultrobust <- CHEresult()  # Retrieve the reactive value
    robust(resultrobust, cluster = Study, clubSandwich = TRUE, digits = 3)
  }, error = function(e) {
    print(e)  # Print error for debugging
    # Display error as a notification
    showNotification(conditionMessage(e), type = "error")
    return(NULL)
  })
})

# Display the robust rma.mv results
output$che_resultsrobust <- renderPrint({
  req(CHEresult(), CHEresultrobust)
  result <- CHEresultrobust()
  print(summary(result))  # For debugging
})

# Download Handler for the RVE Results
output$downloadRVEvar <- downloadHandler(
  filename = function() {
    "myCHERVEresult.txt"  
  },
  content = function(file) {
    # Retrieve the result
    RVEresult <- CHEresultrobust()
    
    # Convert result to text
    RVEresult_textc <- capture.output(RVEresult)
    
    # Write result to text file
    writeLines(RVEresult_textc, file)
  }
)

new_rho1 <- reactive({
  req(uploaded_data(), rho())
  new_rho1 <- min(1.0, rho() + 0.2)  # Ensure rho does not exceed 1.0
})
# Reactive covariance matrix calculation
V_upper <- reactive({
  req(uploaded_data())
  with(uploaded_data(), impute_covariance_matrix(vi = vi, cluster = Study, r = new_rho1()))
})

# Analyses with rho adjusted by +0.2
rhoUpperResult <- eventReactive(input$run_che, {
  tryCatch({
    rma.mv(yi, V_upper(),
           random = ~ 1 | Study/ES_number,
           method = "REML",
           test = "t",
           dfs = "contain",
           data = uploaded_data(),
           rho = new_rho1,
           Sparse = TRUE)
  }, error = function(e) {
    print(e)  # Print error for debugging
    showNotification(conditionMessage(e), type = "error")
    return(NULL)
  })
})

rhoupperrobust <- eventReactive(input$run_che, {
  tryCatch({
    rhoupperresultrobust <- rhoUpperResult()  # Retrieve the reactive value
    robust(rhoupperresultrobust, cluster = Study, clubSandwich = TRUE, digits = 3)
  }, error = function(e) {
    print(e)  # Print error for debugging
    showNotification(conditionMessage(e), type = "error")
    return(NULL)
  })
})


new_rho <- reactive({
  req(uploaded_data(), rho())
  new_rho <- max(-1.0, rho() - 0.2)
})# Ensure rho does not go below -1.0
# Reactive covariance matrix calculation
V_lower <- reactive({
  req(uploaded_data())
  with(uploaded_data(), impute_covariance_matrix(vi = vi, cluster = Study, r = new_rho()))
})

# Analyses with rho adjusted by -0.2
rhoLowerResult <- eventReactive(input$run_che, {
  tryCatch({
    rma.mv(yi, V_lower(),
           random = ~ 1 | Study/ES_number,
           method = "REML",
           test = "t",
           dfs = "contain",
           data = uploaded_data(),
           rho = new_rho,
           Sparse = TRUE)
  }, error = function(e) {
    print(e)  # Print error for debugging
    showNotification(conditionMessage(e), type = "error")
    return(NULL)
  })
})

rholowerrobust <- eventReactive(input$run_che, {
  tryCatch({
    rholowerresultrobust <- rhoLowerResult()  # Retrieve the reactive value
    robust(rholowerresultrobust, cluster = Study, clubSandwich = TRUE, digits = 3)
  }, error = function(e) {
    print(e)  # Print error for debugging
    showNotification(conditionMessage(e), type = "error")
    return(NULL)
  })
})

# Don't forget to define safe_round() and any other necessary components
safe_round <- function(x) {
  if (is.numeric(x)) {
    round(x, 3)
  } else {
    NA  # Return NA if the input is not numeric
  }
}
# Create a globally accessible reactive expression for results_df
results_df <- reactive({
  req(CHEresultrobust(), rholowerrobust(), rhoupperrobust())
  
  robust_model <- data.frame(
    Model = sprintf("Your model rho = %.2f", rho()),
    Estimate = safe_round(CHEresultrobust()$b),
    SE = safe_round(CHEresultrobust()$se),
    `t-value` = safe_round(CHEresultrobust()$zval),
    df = safe_round(CHEresultrobust()$QMdf[2]),
    `p-value` = safe_round(CHEresultrobust()$pval),
    `95% CI` = sprintf("[%s, %s]", safe_round(CHEresultrobust()$ci.lb), safe_round(CHEresultrobust()$ci.ub))
  )
  
  robust_model_lower <- data.frame(
    Model = sprintf("Sensitivity Test rho = %.2f", max(-1.0, rho() - 0.2)),
    Estimate = safe_round(rholowerrobust()$b),
    SE = safe_round(rholowerrobust()$se),
    `t-value` = safe_round(rholowerrobust()$zval),
    df = safe_round(rholowerrobust()$QMdf[2]),
    `p-value` = safe_round(rholowerrobust()$pval),
    `95% CI` = sprintf("[%s, %s]", safe_round(rholowerrobust()$ci.lb), safe_round(rholowerrobust()$ci.ub))
  )
  
  robust_model_upper <- data.frame(
    Model = sprintf("Sensitivity Test rho = %.2f", min(1.0, rho() + 0.2)),
    Estimate = safe_round(rhoupperrobust()$b),
    SE = safe_round(rhoupperrobust()$se),
    `t-value` = safe_round(rhoupperrobust()$zval),
    df = safe_round(rhoupperrobust()$QMdf[2]),
    `p-value` = safe_round(rhoupperrobust()$pval),
    `95% CI` = sprintf("[%s, %s]", safe_round(rhoupperrobust()$ci.lb), safe_round(rhoupperrobust()$ci.ub))
  )
  
  # Combine the rows into a single data frame and explicitly set column names
  combined_results <- rbind(robust_model, robust_model_lower, robust_model_upper)
  colnames(combined_results) <- c("Model", "Estimate", "SE", "t-value", "df", "p-value", "95% CI")
  combined_results
})

output$custom_results <- renderTable({
  req(results_df())
  results_df()
}, digits = 3, align = 'l', sanitize.text.function = function(x) {x})  # Correcting placement of options within renderTable

# Download handler for downloading the results table
output$downloadRVE <- downloadHandler(
  filename = function() {
    paste("CHERVE-Sensitivity-Results-", Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    req(results_df())
    write.csv(results_df(), file, row.names = FALSE)
  }
)
###Variance CHERVE----


# Initialize a reactive value for displaying results
resultsVisibleRVEvar <- reactiveVal(FALSE)

observeEvent(input$run_i2RVE, {
  resultsVisibleRVEvar(TRUE)
})

observeEvent(input$correlationi2, {
  resultsVisibleRVEvar(FALSE)
})

observeEvent(input$i2fileRVE, {
  resultsVisibleRVEvar(FALSE)
})

output$dynamicResultschevar <- renderUI({
  if (resultsVisibleRVEvar()) {
    tagList(
      h3("I2 Results"),
      box(title = "Important Note", width = 12, status = "primary",
          p("The i2 values are based on the CHE model and do not include RVE. At the time I created the app, the functions for i2 do not support robust models, but they do support .rma models with CHE.")),
      downloadButton("download_i2_resultsRVE", label = "Download Results"),
      verbatimTextOutput("i2result_outputRVE"),
      verbatimTextOutput("totalI2_outputRVE"),
      h3("Need Help Understanding The Results?"),
      p("If you want help interpreting these results, please see ", HTML("<a href='https://noah-schroeder.github.io/reviewbook/meta.html#interpreting-the-results'>my open book</a>"),
      ),
    )
  }
})


# Reactive value for rho
rhoi2 <- reactive({
  as.numeric(input$correlationi2)
})

# Data loading and initial processing
i2_datRVE <- eventReactive(input$run_i2RVE, {
  req(input$i2fileRVE)
  read.csv(input$i2fileRVE$datapath)
})

# Reactive covariance matrix calculation
Vi2 <- eventReactive(input$run_i2RVE, {
  req(i2_datRVE())
  datavi2 <- i2_datRVE()
  vii <- with(datavi2, impute_covariance_matrix(vi = vi, cluster = Study, r = rhoi2()))
  vii
})

# i2 results computation
i2_resultRVE <- eventReactive(input$run_i2RVE, {
  req(i2_datRVE())
  datai2RVE <- i2_datRVE()
  # Ensure `yi` and `Study` are part of your dataset
  if (!("yi" %in% names(datai2RVE)) || !("Study" %in% names(datai2RVE))) {
    stop("Data is missing 'yi' or 'Study' columns")
  }
  # Run the model
  m_multii2 <- rma.mv(yi, V = Vi2(),
                      random = ~ 1 | Study/ES_number,
                      method = "REML",
                      test = "t",
                      dfs = "contain",
                      data = datai2RVE,
                      Sparse = TRUE)
  
  # Calculate I-squared values and variance distribution
  i2RVE <- mlm.variance.distribution(m_multii2)
  # Extract the results and total I2, ensure they exist
  resultsi2RVE <- if("results" %in% names(i2RVE)) i2RVE$results else NULL
  totalI2RVE <- if("totalI2" %in% names(i2RVE)) i2RVE$totalI2 else "Empty"
  
  
  # Return the results and total I2 for rendering in the UI
  return(list(results = i2RVE$results, totalI2 = i2RVE$totalI2))
  
})

# Render the results output in the UI
output$i2result_outputRVE <- renderPrint({
  req(i2_resultRVE())  # Ensure the reactive is resolved before accessing
  if (!is.null(i2_resultRVE()$results)) {
    # Rounding numerical columns to two decimal places
    rounded_results <- i2_resultRVE()$results
    numeric_columns <- sapply(rounded_results, is.numeric)  # Identify numeric columns
    rounded_results[numeric_columns] <- lapply(rounded_results[numeric_columns], round, 2)
    print(rounded_results)
  }
})

# Render the totalI2 output in the UI with label
output$totalI2_outputRVE <- renderText({
  req(i2_resultRVE())  # Ensure the reactive is resolved before accessing
  if(!is.na(i2_resultRVE()$totalI2) && !is.null(i2_resultRVE()$totalI2)) {
    paste("Total I2:", sprintf("%.2f", i2_resultRVE()$totalI2))   #  ROund 2 decimal places
  } else {
    "Total I2: Not available"
  }
})

# Add a download button to download both the i2 results and the total i2 results as a .txt file
output$download_i2_resultsRVE <- downloadHandler(
  filename = function() {
    paste("i2_results_", Sys.Date(), ".txt", sep = "")
  },
  content = function(file) {
    # Retrieve the i2 results and total i2 results
    i2_results <- i2_resultRVE()$results
    total_i2 <- i2_resultRVE()$totalI2
    
    # Round numeric columns in the results data frame to two decimal places
    numeric_columns <- sapply(i2_results, is.numeric)  # Identify numeric columns
    i2_results[numeric_columns] <- lapply(i2_results[numeric_columns], round, 2)
    
    # Convert i2 results to text
    i2_results_text <- capture.output(print(i2_results))
    
    # Format total I2 to two decimal places and convert to text
    total_i2_text <- sprintf("Total I2: %.2f", total_i2)
    
    # Combine i2 results and total i2 results text
    combined_text <- c(i2_results_text, total_i2_text)
    
    # Write combined text to a .txt file
    writeLines(combined_text, file)
  }
)
###Outlier and Influence Analysis CHERVE----


# Initialize a reactive value for displaying results
resultsVisibleRVEinf <- reactiveVal(FALSE)

observeEvent(input$run_infrve, {
  resultsVisibleRVEinf(TRUE)
  print("Run analysis clicked, setting resultsVisibleRVECat to TRUE.")
})

observeEvent(input$correlationrve, {
  resultsVisibleRVEinf(FALSE)
})

observeEvent(input$inffilerve, {
  resultsVisibleRVEinf(FALSE)
})

output$dynamicResultsinf <- renderUI({
  if (resultsVisibleRVEinf()) {
    tagList(
      h3("Outlier Results"),
      downloadButton("download_outliersrve", "Download Outlier Plot"),
      plotOutput("outlier_plotrve"),
      h3("Influence Results"),
      p("It is important to see if the outliers significantly influence our results. We'll examine three metrics: Cook's distance, DFBETAS, and hat values. You will see that there are columns with these names _flag. If that column says TRUE, that means that study had significant influence according to that metric."),
      downloadButton("download_influencerve", "Download .csv with Influence Results"),
      h4("Influence Statistics"),
      tableOutput("influence_tablerve"),
      h3("Need Help Understanding The Results?"),
      p("If you want help interpreting these results, please see ", HTML("<a href='https://noah-schroeder.github.io/reviewbook/meta.html#checking-for-outliers-and-influence'>my open book</a>"),
      ),
    )
  }
})



# Reactive value for rhorve
rhorve <- reactive({
  rho_value <- as.numeric(input$correlationrve)
  if (is.na(rho_value)) {
    print("Invalid rho value")
    return(0)  # Default or error handling value
  }
  print(paste("Rho updated:", rho_value))
  rho_value
})

# Reactive covariance matrix calculation 
V_RVErve <- reactive({
  req(input$inffilerve)  # Depend on the filtered data
  # Read uploaded CSV file
  data <- read.csv(input$inffilerve$datapath)
  
  if (!"vi" %in% names(data) || !is.numeric(data$vi)) {
    stop("Column 'vi' is missing or not numeric.")
  }
  if (!"Study" %in% names(data)) {
    stop("Column 'Study' is missing.")
  }
  
  tryCatch({
    result <- impute_covariance_matrix(vi = data$vi, cluster = data$Study, r = rhorve())  # Use rhorve() directly
  }, error = function(e) {
    stop("Error in computing V_RVErve: ", e$message)
  })
})


# Define a reactive value to hold the influence data
influence_datarve <- reactiveVal(NULL)
infprogrve <- reactiveVal(0)
progress_activerve <- reactiveVal(FALSE)
# Reactive function to perform analysis and update influence data
observeEvent(input$run_infrve, {
  req(input$inffilerve)
  infprog(0)
  progress_active(TRUE)  # Activate progress bar
  withProgress(message = "Running Meta-Analysis...", value = 0, {
    # Read uploaded CSV file
    data <- read.csv(input$inffilerve$datapath)
    
    # Run the meta-analysis
    m_multi <- rma.mv(yi, V_RVErve(),
                      random = ~ 1 | Study/ES_number,
                      method = "REML",
                      test = "t",
                      dfs = "contain",
                      data = data)
    robrveinf <- robust(m_multi, cluster = data$Study, clubSandwich = TRUE, digits = 3)
    incProgress(1/8, message = "Performing outlier analysis") 
    # Perform outlier and influence analysis
    data$upperci <- data$yi + 1.96 * sqrt(data$vi)
    data$lowerci <- data$yi - 1.96 * sqrt(data$vi)
    data$outlier <- data$upperci < robrveinf$ci.lb | data$lowerci > robrveinf$ci.ub
    
    incProgress(2/8, message = "Calculating Cook's Distance") 
    # Calculate Cook's distance
    cooks <- cooks.distance(robrveinf)
    
    incProgress(3/8, message = "Calculating DFBETAS") 
    # Calculate hat values
    hatvalues <- hatvalues(robrveinf)
    
    
    # Calculate dfbetas
    dfbetas <- dfbetas(m_multi)
    incProgress(4/8, message = "Calculating hat values") 
    
    # Calculate p/k
    p <- length(coef(robrveinf))
    k <- nrow(data)
    incProgress(5/8, message = "Checking for influential studies") 
    # Check if there are more predictors than just the intercept
    if (length(coef(robrveinf)) > 1) {
      # Remove the intercept term from the count of coefficients
      p <- p - 1
    }
    
    p_over_k <- 3 * (p / k)
    
    # Calculate hat_flag
    hat_flag <- ifelse(hatvalues > p_over_k, "TRUE", "")
    
    incProgress(6/8, message = "Building table...")
    # Combine influence metrics with correct column names
    influence <- data.frame(Study = data$Study,
                            effect_size = data$yi,
                            outlier = ifelse(data$outlier == FALSE, "", "TRUE"),  # # Include outlier column
                            cooks = cooks,
                            cooks_flag = ifelse(cooks > 0.5, "TRUE", ""),
                            dfbetas = dfbetas,
                            dfbetas_flag = ifelse(abs(dfbetas) > 1, "TRUE", ""),
                            hatvalues = hatvalues,
                            hat_flag = hat_flag)
    
    # Rename the columns with proper names
    colnames(influence)[which(colnames(influence) == "intrcpt")] <- "dfbetas*"
    colnames(influence)[which(colnames(influence) == "intrcpt.1")] <- "dfbetas_flag"
    
    new_row <- data.frame(
      Study = "*Note that DFBETAS is based on the three-level CHE model and not the three-level CHE RVE model as the other metrics are.", 
      effect_size = NA, 
      outlier = NA,
      cooks = NA,
      cooks_flag = NA,
      dfbetas = NA,
      dfbetas_flag = NA,
      hatvalues = NA,
      hat_flag = NA
    )
    # Ensure column names of new_row match those of influence exactly
    colnames(new_row) <- colnames(influence)
    
    influence <- rbind(influence, new_row)
    # Update influence data
    influence_datarve(influence)
    #  Only one or a few cells should contain "see this information", adjust like so:
    
    incProgress (7/8, detail = "Analyses complete, building plots...")
    progress_activerve(FALSE)  # Deactivate progress bar
  })
})

# Make progressActive available to JavaScript
output$progressActiverve <- reactive({
  progress_active()
})
outputOptions(output, "progressActiverve", suspendWhenHidden = FALSE)


output$progressActiverve<- reactive({
  progress_activerve()
})


# Debugging for plot data
observeEvent(input$run_infrve, {
  print(str(influence_datarve()))  # Check the structure right after updating
})

# Render influence table
output$influence_tablerve <- renderTable({
  influence_datarve()
}, na = '')

# Adjusted histogram plot
output$outlier_plotrve <- renderPlot({
  req(influence_datarve())  # Ensure data is available
  analysis_data <- influence_datarve()
  
  if("effect_size" %in% names(analysis_data)) {
    ggplot(data = analysis_data, aes(x = effect_size, colour = outlier, fill = outlier)) +
      geom_histogram(alpha = 0.2) +  # Added binwidth to avoid another common error
      geom_vline(xintercept = mean(analysis_data$effect_size, na.rm = TRUE)) +  # Handle possible NAs
      theme_bw()
  } else {
    print("effect_size not found in the data")
  }
})



# Download influence table as CSV
output$download_influencerve <- downloadHandler(
  filename = function() {
    "influence_table.csv"
  },
  content = function(file) {
    # Retrieve the data
    temp_data <- influence_datarve()  # Get the reactive data
    
    # Replace NA with empty strings and round numeric columns to 2 decimal places
    temp_data[] <- lapply(temp_data, function(x) {
      if(is.numeric(x)) {
        x <- round(x, 2)  # Round numeric columns to 2 decimal places
      }
      ifelse(is.na(x), "", x)  # Replace NA with empty string after rounding
    })
    
    # Write the modified data frame to a CSV file
    write.csv(temp_data, file, row.names = FALSE, quote = FALSE)
  }
)



# Download handler for the outlier plot button
output$download_outliersrve <- downloadHandler(
  filename = function() {
    "outlier_plot.png"  # Specify file name
  },
  content = function(file) {
    # Capture the plot as a PNG file
    png(file, width = 2800, height = 2400, units = "px", res = 300)
    # Create the outlier plot
    p <- ggplot(data = influence_datarve(), aes(x = effect_size, colour = outlier, fill = outlier)) +
      geom_histogram(alpha = 0.2) +
      geom_vline(xintercept = mean(influence_datarve()$effect_size)) +  # Example line at mean effect size
      theme_bw()
    print(p)  # Print the plot
    dev.off()  # Turn off the PNG device
  }
)
#####Cat Mod CHERVE ----

# Initialize a reactive value for displaying results
resultsVisibleRVECat <- reactiveVal(FALSE)

observeEvent(input$run_cheCat, {
  resultsVisibleRVECat(TRUE)
  print("Run analysis clicked, setting resultsVisibleRVECat to TRUE.")
})

observeEvent(input$mod_RVECat, {
  resultsVisibleRVECat(FALSE)
})
observeEvent(input$correlationCat, {
  resultsVisibleRVECat(FALSE)
})
observeEvent(input$chefileCat, {
  resultsVisibleRVECat(FALSE)
})

output$dynamicResults <- renderUI({
  if (resultsVisibleRVECat()) {
    tagList(
      box(title = "Important Notes*", width = 12, status = "primary",
           p("In metafor there are two different tests of the moderator. The table below presents the omnibus test of moderators from the model with an intercept. This is the same statistic as you may be used to seeing as Qbetween in conventional meta-analysis. The effect sizes etc. provided in the moderator table below are from the model without an intercept. In a conventional meta-analysis, this is a presentation consistent with what you may expect to see from other software packages such as Comprehensive Meta-Analysis."),
           p("Please also note that since we are using CHE RVE, any levels of the moderator that only have 1 comparison will be automatically removed and noted in the last row of the table."),
           p("Finally, note that it is possible for the Test of Moderators to show a p value of NA. This typically occurs when there are few comparisons for levels of the moderator. If this occurs you can do a few things. First, you may decide to create larger categories of your moderator so you have more comparisons at each level. Alternatively, you may decide that you do not want to use a CHE RVE approach for this analysis (the standard three-level analysis in this app will likely produce a result for you). Finally, you may decide that the NA result is OK because it shows the nature of your sample. There are likely other approaches you can take, as with many things in meta-analysis, it may be somewhat subjective as to what approach to take with NA results from this analysis.")),
      h3("Model Result"),
      downloadButton("downloadRVE_RVE", "Download Results"),
      div(class = "scrollable", tableOutput("custom_results_RVE")),
      h3("Basic Interpretation Tips"),
      p("First you should look at the", strong("Test of Moderators"), "column. If this is significant, it means there are significant differences between levels of your moderator."),
      p("Note the following columns I have created to aid in interpretation:"),
      p(strong("nexp"),"is the sample size of the intervention group, and", strong("nctrl"), "is the sample size of the control group. Note that this is not calculating the actual number of unique participants, because this code is simply conditionally summing the sample size columns in our data set. For example, if a study had one experimental group (n = 10) and one control group (n = 10), and had three outcomes that were included in the analysis (meaning, each appears as its own row in the data set), this code will say there were 30 participants in each group rather than 10. While this is expected in dependent data such as this, it is something to be aware of so you do not make a claim such as, “our analysis of 60 participants” when in reality, your analysis is only 20 unique participants. So, please be careful of your wording when you describe the participant numbers to ensure strict accuracy."),
      p(strong("kcomp"),"is the number of comparisons examined. The total number of kcomp in the table should correspond to the number of rows in your dataset."),
      p(strong("kstudies"),"is the number of studies providing comparisons in the analysis. Note that it is possible for this not to sum to the same number as appears in your data set. For example, if Study A provided 4 comparisons and 1 or more were at different levels of this moderator variable, kstudies will not equal the total number of unique studies in the dataset because it is being counted in multiple moderator levels."),
      h3("Need Help Understanding The Results?"),
      p("If you want help interpreting these results, please see ", HTML("<a href='https://noah-schroeder.github.io/reviewbook/meta.html#moderator-analysis'>my open book</a>"),
      ),
    )
  }
})

# Make sure to correctly trigger and use results_df_RVE
output$custom_results_RVE <- renderTable({
  req(results_df_RVE())
  results_df_RVE()
})


# Reactive value for rhoCat
rhoCat <- reactive({
  rho_value <- as.numeric(input$correlationCat)
  if (is.na(rho_value)) {
    print("Invalid rho value")
    return(0)  # Default or error handling value
  }
  print(paste("Rho updated:", rho_value))
  rho_value
})

# Reactive data preparation
uploaded_dataRVECAT <- reactive({
  req(input$chefileCat)
  data <- read.csv(input$chefileCat$datapath)
  print("Data loaded successfully.")  # Debug print
  data
})

# Correct use of reactive expressions with parentheses
V_RVE <- reactive({
  req(filtered_data())  # Notice the parentheses
  data_filtered <- filtered_data()  # Accessing the reactive result
  tryCatch({
    result <- with(data_filtered, impute_covariance_matrix(vi = vi, cluster = Study, r = rhoCat()))  # Correctly calling rhoCat()
    print("Covariance matrix calculated.")  # Debug print
    result
  }, error = function(e) {
    print(paste("Error in computing V_RVE:", e$message))  # More informative error message
    NULL
  })
})

# Update the select input once the file is uploaded
observe({
  req(input$chefileCat)
  data <- read.csv(input$chefileCat$datapath)
  updateSelectInput(session, "mod_RVECat", choices = names(data))
})




# Reactive value to store excluded levels
excludedLevels <- reactiveVal()

# Reactive for data preparation and identifying excluded levels
filtered_data <- reactive({
  req(input$chefileCat)
  data <- read.csv(input$chefileCat$datapath)
  
  # Group and count
  group_data <- data %>%
    group_by(Moderator = get(input$mod_RVECat)) %>%
    summarise(Count = n(), .groups = 'drop')
  
  # Find excluded levels
  excluded_levels <- group_data %>%
    filter(Count == 1) %>%
    pull(Moderator)
  
  # Update reactive value
  excludedLevels(excluded_levels)
  
  # Filter data
  data %>%
    filter(!(get(input$mod_RVECat) %in% excluded_levels))
})


# Reactive for summarizing data after filtering
summarized_data <- reactive({
  req(filtered_data())  # Ensure filtered data is ready
  data <- filtered_data()
  
  # Summarize the data now that it's been filtered
  data %>%
    group_by(Moderator = get(input$mod_RVECat)) %>%
    summarise(
      nexp = sum(Exp_n, na.rm = TRUE),
      nctrl = sum(Ctrl_n, na.rm = TRUE),
      kcomp = n(),  # Number of comparisons
      kstudies = n_distinct(Study),
      .groups = 'drop'
    )
})

# Reactive covariance matrix calculation using filtered data
V_RVE <- reactive({
  req(filtered_data())  # Depend on the filtered data
  data <- filtered_data()
  
  if (!"vi" %in% names(data) || !is.numeric(data$vi)) {
    stop("Column 'vi' is missing or not numeric.")
  }
  if (!"Study" %in% names(data)) {
    stop("Column 'Study' is missing.")
  }
  
  tryCatch({
    impute_covariance_matrix(vi = data$vi, cluster = data$Study, r = rhoCat())  # Use rhoCat() directly
  }, error = function(e) {
    stop("Error in computing V_RVE: ", e$message)
  })
})

# Meta-analysis computation
CHEresult_RVE <- eventReactive(input$run_cheCat, {  # Triggered by a button
  req(filtered_data(), V_RVE())  # Ensure data and covariance matrix are ready
  data <- filtered_data()
  
  tryCatch({
    rob <- rma.mv(yi = data$yi, V = V_RVE(), mods = ~ -1 + factor(data[[input$mod_RVECat]]),
                  random = ~ 1 | Study/ES_number, method = "REML", test = "t", data = data)
  }, error = function(e) {
    stop("Error in CHE analysis: ", e$message)
  })
  robrob <- robust(rob, cluster = data$Study, clubSandwich = TRUE, digits = 3)
  robrob
})

# Generate a results data frame
results_df_RVE <- reactive({
  req(CHEresult_RVE())
  robust_result <- CHEresult_RVE()
  
  if (is.null(robust_result)) {
    print("Robust result is null.")
    return(data.frame())  # Return an empty data frame if no results
  }
  
  # Ensure the moderator variable is properly factorized
  data <- filtered_data()
  mod_levels <- levels(factor(data[[input$mod_RVECat]]))
  
  # Check for alignment between model results and levels
  if(length(robust_result$b) != length(mod_levels)) {
    print("Mismatch between number of estimates and moderator levels.")
    return(data.frame())  # Return an empty frame if mismatch found
  }
  
  # Prepare a data frame
  df <- data.frame(
    Model = mod_levels,  # Moderator levels
    Estimate = safe_round(robust_result$b),  # Assuming robust_result$b is correctly ordered and corresponds to mod_levels
    SE = safe_round(robust_result$se),
    `t-value` = safe_round(robust_result$zval),
    `p-value` = safe_round(robust_result$pval),
    `95% CI` = sprintf("[%s, %s]", safe_round(robust_result$ci.lb), safe_round(robust_result$ci.ub))
  )
  
  df
})

# Compute CHE results including the moderator variable with intercept
CHEresult_RVECatInt <- eventReactive(input$run_cheCat, {
  req(filtered_data(), V_RVE())  # Ensure filtered data and V_RVE are ready
  data <- filtered_data()  # Use the already filtered data
  
  if (nrow(data) == 0) {
    print("No valid data after filtering out single-comparison levels.")
    return(NULL)
  }
  
  tryCatch({
    resultRVECat <- rma.mv(
      yi = data$yi, 
      V = V_RVE(),  # Use filtered yi and V
      mods = ~ factor(data[[input$mod_RVECat]]),  # Include the intercept
      random = ~ 1 | Study/ES_number,
      method = "REML",
      test = "t",
      dfs = "contain",
      data = data,
      Sparse = TRUE
    )
    robust_modelRVECatInt <- robust(resultRVECat, cluster = data$Study, clubSandwich = TRUE, digits = 3)
    print("CHE analysis with moderator and intercept completed successfully.")
    robust_modelRVECatInt
  }, error = function(e) {
    message <- paste("Error in CHE analysis with moderator and intercept:", e$message)
    print(message)
    NULL
  })
})

results_df_RVEInt <- reactive({
  result_with_intercept <- req(CHEresult_RVECatInt())
  
  # Ensure the result is not null and contains the required fields
  if (!is.null(result_with_intercept) && !is.null(result_with_intercept$QM)) {
    QM_RVECat <- result_with_intercept$QM
    QMp_RVECat <- result_with_intercept$QMp
    df1_RVECat <- result_with_intercept$QMdf[1]
    df2_RVECat <- result_with_intercept$QMdf[2]
    
    # Return the formatted string indicating the results of the test of moderators
    sprintf("F(%.2f, %.2f) = %.3f, p-val = %.3f", df1_RVECat, df2_RVECat, QM_RVECat, QMp_RVECat)
  } else {
    # Return NA if the required data is not available
    NA
  }
})

# Generate a results data frame that includes summarized data
results_df_RVE <- reactive({
  req(CHEresult_RVE(), summarized_data())
  robust_result <- CHEresult_RVE()
  summary_data <- summarized_data()
  
  if (is.null(robust_result)) {
    print("Robust result is null.")
    return(data.frame())  # Return an empty data frame if no results
  }
  
  # Prepare the main results data frame using these levels
  df <- data.frame(
    Model = summary_data$Moderator,  # Use the actual levels from the moderator variable
    nexp = summary_data$nexp,
    nctrl = summary_data$nctrl,
    kcomp = summary_data$kcomp,
    kstudies = summary_data$kstudies,
    Estimate = safe_round(robust_result$b),
    SE = safe_round(robust_result$se),
    `t-value` = safe_round(robust_result$zval),
    `p-value` = safe_round(robust_result$pval),
    `95% CI` = sprintf("[%s, %s]", safe_round(robust_result$ci.lb), safe_round(robust_result$ci.ub)),
    `Test of Moderator` = rep("", nrow(summary_data))  # Initially, fill with empty strings
  )
  # Replace values less than 0.001 with "<0.001" in the "p-value" column
  df$p.value[df$p.value < 0.001] <- "< 0.001"
  
  # Append the test of moderator result if available
  test_of_moderator <- results_df_RVEInt()
  if (!is.na(test_of_moderator) && !is.null(test_of_moderator)) {
    moderator_row <- data.frame(
      Model = "",  # Leave this empty for the Test of Moderator row
      nexp = "",   # Empty string for other cells
      nctrl = "",
      kcomp = "",
      kstudies = "",
      Estimate = "",
      SE = "",
      `t-value` = "",
      `p-value` = "",
      `95% CI` = "",
      `Test of Moderator` = test_of_moderator
    )
    df <- rbind(df, moderator_row)  # Append this row
  }
  
  # Append excluded levels text in the first column of the "Test of Moderator" row
  if (length(excludedLevels()) > 0) {
    excluded_text <- sprintf("The following levels of the moderator were not examined because they only contained one comparison: %s",
                             toString(excludedLevels()))
    df[nrow(df), "Model"] <- excluded_text  # Set the excluded text in the last row's "Model" column
  }
  
  # Set custom column names using colnames()
  colnames(df) <- c(
    input$mod_RVECat,
    "nexp",
    "nctrl",
    "kcomp",
    "kstudies",
    "Estimate",
    "SE",
    "t-value",
    "p-value",
    "95% CI",
    "Test of Moderator"
  )
  df
})


# Update the outputs
output$custom_results_RVE <- renderTable({
  req(results_df_RVE())
  results_df_RVE()
})


# Download handler for downloading the results table
output$downloadRVE_RVE <- downloadHandler(
  filename = function() {
    paste("mod.", input$mod_RVECat, Sys.Date(), ".csv", sep = "") 
    },
  content = function(file) {
    req(results_df_RVE())
    write.csv(results_df_RVE(), file, row.names = FALSE)
  }
)

#####Cont Mod CHERVE ----

# Initialize a reactive value for displaying results
resultsVisibleRVECont <- reactiveVal(FALSE)

observeEvent(input$run_cheCont, {
  resultsVisibleRVECont(TRUE)
  print("Run analysis clicked, setting resultsVisibleRVECat to TRUE.")
})

observeEvent(input$mod_RVECont, {
  resultsVisibleRVECont(FALSE)
})
observeEvent(input$correlationCont, {
  resultsVisibleRVECont(FALSE)
})
observeEvent(input$chefileCont, {
  resultsVisibleRVECont(FALSE)
})

output$dynamicResultsCont <- renderUI({
  if (resultsVisibleRVECont()) {
    tagList(
      h3("Model Result"),
      downloadButton("downloadRVE_RVECont", "Download Results"),
      div(class = "scrollable", tableOutput("custom_results_RVECont"))
    )
  }
})

# Make sure to correctly trigger and use results_df_RVE
output$custom_results_RVECont <- renderTable({
  req(results_df_RVECont())
  results_df_RVECont()
})


# Reactive value for rhoCat
rhoCont <- reactive({
  rho_value <- as.numeric(input$correlationCont)
  if (is.na(rho_value)) {
    print("Invalid rho value")
    return(0)  # Default or error handling value
  }
  print(paste("Rho updated:", rho_value))
  rho_value
})

# Reactive data preparation
uploaded_dataRVECont <- reactive({
  req(input$chefileCont)
  data <- read.csv(input$chefileCont$datapath)
  print("Data loaded successfully.")  # Debug print
  data
})

# Correct use of reactive expressions with parentheses
V_RVE_Cont <- reactive({
  req(uploaded_dataRVECont())  # Notice the parentheses
  data_filtered <- uploaded_dataRVECont()  # Accessing the reactive result
  tryCatch({
    result <- with(data_filtered, impute_covariance_matrix(vi = vi, cluster = Study, r = rhoCont()))  # Correctly calling rhoCat()
    print("Covariance matrix calculated.")  # Debug print
    result
  }, error = function(e) {
    print(paste("Error in computing V_RVE:", e$message))  # More informative error message
    NULL
  })
})

# Update the select input once the file is uploaded
observe({
  req(input$chefileCont)
  data <- read.csv(input$chefileCont$datapath)
  updateSelectInput(session, "mod_RVECont", choices = names(data))
})


# Reactive for summarizing data after filtering
summarized_dataCont <- reactive({
  req(input$chefileCont)
  data <- read.csv(input$chefileCont$datapath)
  
  # Summarize the data now that it's been filtered
  data %>%
    group_by(Moderator = get(input$mod_RVECont)) %>%
    summarise(
      nexp = sum(Exp_n, na.rm = TRUE),
      nctrl = sum(Ctrl_n, na.rm = TRUE),
      kcomp = n(),  # Number of comparisons
      kstudies = n_distinct(Study),
      .groups = 'drop'
    )
})


# Compute CHE results including the moderator variable with intercept
mod_summary <- reactive({
  req(input$run_cheCont, input$mod_RVECont)
  data <- read.csv(input$chefileCont$datapath)
  # Create the formula for the moderator
  mod_formula <- as.formula(paste("~", input$mod_RVECont))
  
  # Perform the analysis
  resultRVECont <- rma.mv(
    yi = yi, 
    V = V_RVE_Cont(), 
    mods = mod_formula,
    random = ~ 1 | Study / ES_number,
    method = "REML",
    test = "t",
    dfs = "contain",
    data = data,
    Sparse = TRUE
  )
  robust_modelRVEContInt <- robust(resultRVECont, cluster = data$Study, clubSandwich = TRUE, digits = 3)
  
  model_summary <- summary(robust_modelRVEContInt)
  summary_table <- coef(model_summary)
  
  if (is.null(summary_table) || nrow(summary_table) == 0) {
    stop("The summary table has no data.")
  }
  
  # Extract statistics
  QM <- round(robust_modelRVEContInt$QM, 3)
  QMp <- round(robust_modelRVEContInt$QMp, 3)
  df1 <- robust_modelRVEContInt$QMdf[1]
  df2 <- robust_modelRVEContInt$QMdf[2]
  
  # Combine CI_Lower and CI_Upper into one column formatted as [CI_Lower, CI_Upper]
  CI_formatted <- paste0("[", round(summary_table[, "ci.lb"], 3), ", ", round(summary_table[, "ci.ub"], 3), "]")
  
  # Prepare the results table
  result_table <- data.frame(
    Term = c(rownames(summary_table), "Test of Moderator"),
    Estimate = c(round(summary_table[, "estimate"], 3), NA),
    StdError = c(round(summary_table[, "se"], 3), NA),
    TValue = c(round(summary_table[, "tval"], 3), NA),
    PValue = c(round(summary_table[, "pval"], 3), NA),
    `95% CI` = c(CI_formatted, NA),  # New CI column
    TestOfModerator = c(rep(NA, nrow(summary_table)), sprintf("F(%.2f, %.2f) = %.3f, p-val = %.3f", 
                                                              df1, df2, QM, QMp)),
    check.names = FALSE
  )
  
  # Replace values less than 0.001 with "<0.001" in the "p-value" column
  result_table$PValue[result_table$PValue < 0.001] <- "< 0.001"
  result_table
})

output$custom_results_RVECont <- renderTable({
  req(mod_summary())
  mod_summary()
}, na = '')  # Display NA as empty cells

# Download handler for exporting the results as a CSV file
output$downloadRVE_RVECont <- downloadHandler(
  filename = function() {
    paste("mod.", input$mod_RVECont, Sys.Date(), ".csv", sep = "")
  },
  content = function(file) {
    req(mod_summary())
    write.csv(mod_summary(), file, row.names = FALSE, na = "")  # Write empty strings instead of NA
  }
)



### Publication Bias CHERVE---- 
# Initialize a reactive value for displaying results
resultsVisibleRVEplots <- reactiveVal(FALSE)

observeEvent(input$run_cheplot, {
  resultsVisibleRVEplots(TRUE)
})

observeEvent(input$chefileplot, {
  resultsVisibleRVEplots(FALSE)
})
observeEvent(input$correlationplot, {
  resultsVisibleRVEplots(FALSE)
})


output$dynamicResultcheplot <- renderUI({
  if (resultsVisibleRVEplots()) {
    tagList(
      box( title = "*Important Note*", width = 12, status = "primary",
           p("The plots are rendered at 'half-page' height (800px by 600px) within the software. You may choose to download at this size (about half a standard page in height) or full-page size. If you need a taller image (most typical on the comparison-level forest plot) than the 'full-page' download size may be helpful.",
           )),
      h4("Comparison-level Forest Plot"),
      p("This plot uses CHE RVE Three-Level Meta-Analysis"),
      downloadButton("downloadcheForest", "Download Comparison-Level Forest Plot (Half-Page)"), downloadButton("downloadcheForestb", "Download Comparison-Level Forest Plot (Full-Page)"),
      plotOutput("cheforest", width = 800, height = 600),
      h4("Study-level Forest Plot"),
      p("This plot includes effect sizes that are by study. In short, we calculated weighted means for each study, then run the CHE RVE Three-Level Meta-Analysis"),
      downloadButton("downloadCheForest2", "Download Study-Level Forest Plot (Half-Page)"), downloadButton("downloadCheForest2b", "Download Study-Level Forest Plot (Full-Page)"),
      plotOutput("cheforest2", width = 800, height = 600),
      h4("Funnel Plot of Robust Effects"),
      p("This funnel plot is based on the three-level meta-analysis with CHE RVE. Each comparison from the analysis is included."),
      downloadButton("downloadCheFunnel", "Download Standard Funnel Plot (Half-Page)"), downloadButton("downloadCheFunnelb", "Download Standard Funnel Plot (Full-Page)"),
      plotOutput("chefunnel", width = 800, height = 600),
    )
  }
})


# Reactive data preparation
uploaded_datacheplot <- reactive({
  req(input$chefileplot)
  read.csv(input$chefileplot$datapath)
})

# Reactive value for rhoCat
rhocheplot <- reactive({
  rho_value <- as.numeric(input$correlationplot)
  if (is.na(rho_value)) {
    showNotification("Invalid rho value", type = "error")
    return(0)  # Default or error handling value
  }
  rho_value
})

# Reactive covariance matrix calculation
Vcheplot <- reactive({
  req(uploaded_datacheplot())
  with(uploaded_datacheplot(), impute_covariance_matrix(vi = vi, cluster = Study, r = rhocheplot()))
})

# Compute CHE results
CHEresultplot <- eventReactive(input$run_cheplot, {
  withCallingHandlers({
    rma.mv(yi, Vcheplot(),
           random = ~ 1 | Study/ES_number,
           method = "REML",
           test = "t",
           dfs = "contain",
           data = uploaded_datacheplot(),
           Sparse = TRUE)
  }, error = function(e) {
    showNotification(conditionMessage(e), type = "error")
    NULL
  })
})

# Compute robust results
CHEresultrobustplot <- eventReactive(input$run_cheplot, {
  withCallingHandlers({
    resultrobust <- CHEresultplot()
    robust(resultrobust, cluster = Study, clubSandwich = TRUE, digits = 3)
  }, error = function(e) {
    showNotification(conditionMessage(e), type = "error")
    NULL
  })
})

# Display the robust rma.mv results
output$cheforest <- renderPlot({
  resultrobust <- CHEresultrobustplot()
  if (!is.null(resultrobust)) {
    rho_value <- rhocheplot()
    study_labels <- uploaded_datacheplot()$Study
    forest(resultrobust, slab = study_labels, mlab = paste("RE CHE RVE Three-Level Model, rho =", rho_value), header = TRUE)
  }
})
# Download handler
output$downloadcheForest <- downloadHandler(
  filename = function() {
    paste("comparison_level_forest_plot", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    # Save the plot as a PNG file
    png(file, width = 800, height = 600)
    resultrobust <- CHEresultrobustplot()
    if (!is.null(resultrobust)) {
      rho_value <- rhocheplot()
      study_labels <- uploaded_datacheplot()$Study
      forest(resultrobust, slab = study_labels, mlab = paste("RE CHE RVE Three-Level Model, rho =", rho_value), header = TRUE)
    }
    dev.off()
  }
)

# Download handler
output$downloadcheForestb <- downloadHandler(
  filename = function() {
    paste("comparison_level_forest_plot_large", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    # Save the plot as a PNG file
    png(file, width = 800, height = 1056)
    resultrobust <- CHEresultrobustplot()
    if (!is.null(resultrobust)) {
      rho_value <- rhocheplot()
      study_labels <- uploaded_datacheplot()$Study
      forest(resultrobust, slab = study_labels, mlab = paste("RE CHE RVE Three-Level Model, rho =", rho_value), header = TRUE)
    }
    dev.off()
  }
)

# Aggregated data by Study
aggregated_datacheplot2 <- reactive({
  req(uploaded_datacheplot())
  dat <- uploaded_datacheplot()
  rho <- rhocheplot()
  
  # Aggregate yi and vi by Study
  aggregatedcheplot2 <- aggregate(cbind(yi, vi) ~ Study, data = dat, function(x) mean(x, na.rm = TRUE))
  
  # Compute new covariance matrix for aggregated data
  V <- impute_covariance_matrix(vi = aggregatedcheplot2$vi, cluster = aggregatedcheplot2$Study, r = rho)
  
  list(data = aggregatedcheplot2, V = V)
})

# Compute CHE results
CHEresultplot2 <- eventReactive(input$run_cheplot, {
  agg <- aggregated_datacheplot2()
  withCallingHandlers({
    rma.mv(yi, agg$V,
           random = ~ 1 | Study,
           method = "REML",
           test = "t",
           dfs = "contain",
           data = agg$data,
           Sparse = TRUE)
  }, error = function(e) {
    showNotification(conditionMessage(e), type = "error")
    NULL
  })
})

# Compute robust results
CHEresultrobustplotcheplot2 <- eventReactive(input$run_cheplot, {
  withCallingHandlers({
    resultrobust <- CHEresultplot2()
    robust(resultrobust, cluster = resultrobust$data$Study, clubSandwich = TRUE, digits = 3)
  }, error = function(e) {
    showNotification(conditionMessage(e), type = "error")
    NULL
  })
})

# Display the robust rma.mv results
output$cheforest2 <- renderPlot({
  resultrobustcheplot2 <- CHEresultrobustplotcheplot2()
  if (!is.null(resultrobustcheplot2)) {
    rho_value <- rhocheplot()
    study_labels <- aggregated_datacheplot2()$data$Study
    forest(resultrobustcheplot2, slab = study_labels, header = TRUE, 
           annotate = TRUE, addfit = FALSE, mlab = "")
    # Add custom label below the plot
    title(sub = paste("RE CHE RVE Three-Level Model, rho =", rho_value), line = 2.5, adj = 0)
    title(sub = "Note that the means above are ", line = 2.5, adj = 1)
    title(sub = "not weighted within studies.", line = 3.25, adj = 1)
  }
})


# Download handler for the forest plot
output$downloadCheForest2 <- downloadHandler(
  filename = function() {
    paste("study_level_forest_plot", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    png(file, width = 800, height = 600)
    resultrobustcheplot2 <- CHEresultrobustplotcheplot2()
    if (!is.null(resultrobustcheplot2)) {
      rho_value <- rhocheplot()
      study_labels <- aggregated_datacheplot2()$data$Study
      forest(resultrobustcheplot2, slab = study_labels, header = TRUE, 
             annotate = TRUE, addfit = FALSE, mlab = "")
      title(sub = paste("RE CHE RVE Three-Level Model, rho =", rho_value), line = 2.5, adj = 0)
      title(sub = "Note that the means above are ", line = 2.5, adj = 1)
      title(sub = "not weighted within studies.", line = 3.25, adj = 1)
      
    }
    dev.off()
  }
)

# Download handler for the forest plot
output$downloadCheForest2b <- downloadHandler(
  filename = function() {
    paste("study_level_forest_plot", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    png(file, width = 800, height = 1056)
    resultrobustcheplot2 <- CHEresultrobustplotcheplot2()
    if (!is.null(resultrobustcheplot2)) {
      rho_value <- rhocheplot()
      study_labels <- aggregated_datacheplot2()$data$Study
      forest(resultrobustcheplot2, slab = study_labels, header = TRUE, 
             annotate = TRUE, addfit = FALSE, mlab = "")
      title(sub = paste("RE CHE RVE Three-Level Model, rho =", rho_value), line = 2.5, adj = 0)
      title(sub = "Note that the means above are ", line = 2.5, adj = 1)
      title(sub = "not weighted within studies.", line = 3.25, adj = 1)
      
    }
    dev.off()
  }
)
# Display the robust funnel results
output$chefunnel <- renderPlot({
  resultrobust <- CHEresultrobustplot()
  if (!is.null(resultrobust)) {
    rho_value <- rhocheplot()
    funnel(resultrobust)
  }
})

# Download handler for the funnel plot
output$downloadCheFunnel <- downloadHandler(
  filename = function() {
    paste("funnel_plot", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    png(file, width = 800, height = 600)
    resultrobust <- CHEresultrobustplot()
    if (!is.null(resultrobust)) {
      rho_value <- rhocheplot()
      funnel(resultrobust)
    }
    dev.off()
  }
)
  
# Download handler for the funnel plot
output$downloadCheFunnelb <- downloadHandler(
  filename = function() {
    paste("funnel_plot", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    png(file, width = 800, height = 1056)
    resultrobust <- CHEresultrobustplot()
    if (!is.null(resultrobust)) {
      rho_value <- rhocheplot()
      funnel(resultrobust)
    }
    dev.off()
  }
)

  




##code for validation----
###conventional MA
output$ccode <- renderUI({
  HTML("<pre><code>
##########preparation
#load metafor
library(metafor)
#name data file and read in .csv
dat1 <- read.csv(&quot;yourdata.csv&quot;)
#calculate overall ES, in this case standardized mean dif hedges g, and variance. 
dat1 <- escalc(measure=&quot;SMD&quot;, m1i=Exp_mean, sd1i=Exp_sd, n1i=Exp_n,
               m2i=Ctrl_mean, sd2i=Ctrl_sd, n2i=Ctrl_n, data=dat1)
#display dataset with ES and variance
dat1



##########analyses

#run overall random effects meta-analysis
overallresult <- rma(yi, vi, data=dat1)
#display overall result
overallresult
#moderator test to calculate qbetween value for categorical moderator
mod.ctrlq <- rma(yi, vi, mods = ~ factor(control.condition), data=dat1)
mod.ctrlq
#moderator test to get mean effect size for each group categorical moderator
mod.ctrl <- rma(yi, vi, mods = ~ factor(control.condition)-1, data=dat1)
#Display moderator result
mod.ctrl
#continuous moderator test
mod.cont <- rma(yi, vi, mods = ~ cont_fake, data=dat1)
mod.cont

#forest plot
forest.rma(overallresult, slab = dat1$Study)


#########publication bias analyses

#standard funnel plot
funnel(overallresult)
# carry out trim-and-fill analysis
trimandfill <- trimfill(overallresult)
trimandfill
#Eggers regression
regtest(overallresult)
#Rosenthal, Orwin, & Rosenberg Fail Safe N test 
fsn(yi, vi, data=dat1)
fsn(yi, vi, data=dat1, type=&quot;Orwin&quot;)
fsn(yi, vi, data=dat1, type=&quot;Rosenberg&quot;)

#influence analysis
influence(overallresult)
    </code></pre>")
})

###conventional MA
output$threecode <- renderUI({
  HTML("<pre><code>
##########preparation

#load metafor
library(metafor)
library(ggplot2)

#name data file and read in .csv. Change the \ to /
df360 <- read.csv(&quot;yourdata.csv&quot;)

#calculate overall ES, in this case standardized mean dif hedges g, and variance.
dat1 <- escalc(measure=&quot;SMD&quot;, m1i=Exp_mean, sd1i=Exp_sd, n1i=Exp_n,
               m2i=Ctrl_mean, sd2i=Ctrl_sd, n2i=Ctrl_n, data=df360)
#display dataset with ES and variance
dat1


###########################
#fitting model#
###########################
#multilevel model
m_multi <- rma.mv(yi,
                  vi,
                  random = ~ 1 | Study/ES_number,
                  test = &quot;t&quot;, 
                  data = dat1) 
m_multi

#calculate i2 for each level-copy paste function from https://raw.githubusercontent.com/MathiasHarrer/dmetar/master/R/mlm.variance.distribution.R into console and hit enter
i2 <- var.comp(m_multi)
summary(i2)
i2

###########################
#Check for outliers#
###########################

#adapting CI calculation and plotting from https://cjvanlissa.github.io/Doing-Meta-Analysis-in-R/detecting-outliers-influential-cases.html
# Calculate CI for all observed effect sizes
dat1$upperci <- dat1$yi + 1.96 * sqrt(dat1$vi)
dat1$lowerci <- dat1$yi - 1.96 * sqrt(dat1$vi)
# Create filter variable
dat1$outlier <- dat1$upperci < m_multi$ci.lb | dat1$lowerci > m_multi$ci.ub
# Count number of outliers:
sum(dat1$outlier)
dat1
# Make a basic plot, based on the data in df, and specify that the x-variable is
# the effect size, 'd', the colour and fill of the histogram bars are based on
# the value of 'outlier':
ggplot(data = dat1, aes(x = yi, colour = outlier, fill = outlier)) +
  # Add a histogram with transparent bars (alpha = .2)
  geom_histogram(alpha = .2) +
  # Add a vertical line at the pooled effect value (m_re$b[1])
  geom_vline(xintercept = m_multi$b[1]) +
  # Apply a black and white theme
  theme_bw()


##########################################
#influence check
#adapting method from https://wviechtb.github.io/metafor/reference/influence.rma.mv.html to calculate influence 
#cook's distance
cooks <- cooks.distance(m_multi)
plot(cooks, type=&quot;o&quot;, pch=19, xlab=&quot;Observed Outcome&quot;, ylab=&quot;Cook's Distance&quot;)

#dfbeta
dfbetas <-dfbetas(m_multi)
dfbetas
#hatvalue
hatvalues <- hatvalues(m_multi)
hatvalues

#########################################
#moderator analyses#
#########################################
#calcualte qb categorical moderator
mod.ctrlq <- rma.mv(yi,
                  vi,
                  data = dat1,
                  random = ~ 1 | Study/ES_number, 
                  test = &quot;t&quot;,
                  method = &quot;REML&quot;,
                  mods = ~ factor(control_c))
summary(mod.ctrlq)

#calcualte ES categorical moderator
mod.ctrl <- rma.mv(yi,
                    vi,
                    data = dat1,
                    random = ~ 1 | Study/ES_number, 
                    test = &quot;t&quot;,
                    method = &quot;REML&quot;,
                    mods = ~ factor(control_c)-1)
summary(mod.ctrl)

#continuous mod
mod.cont_fake <- rma.mv(yi,
                     vi,
                     data = dat1,
                     random = ~ 1 | Study/ES_number, 
                     test = &quot;t&quot;,
                     method = &quot;REML&quot;,
                     mods = ~ cont_fakedata)
summary(mod.cont_fake)
    </code></pre>")
})

###conventional MA
output$chervecode <- renderUI({
  HTML("<pre><code>
###########################
#Preparation#
###########################

#load metafor
library(metafor)
library(ggplot2)
library(dplyr)
library(clubSandwich)


#name data file and read in .csv. 
dat1 <- read.csv(&quot;yourdata.csv&quot;)

#set Rho
rho <- #user sets value 

V <- with(dat1, impute_covariance_matrix(vi = vi, cluster = Study, r = rho))

#multilevel model
m_multi <- rma.mv(yi,
                  V,
                  random = ~ 1 | Study/ES_number,
                  method = &quot;REML&quot;,
                  test = &quot;t&quot;,
                  dfs = &quot;contain&quot;,
                  data = dat1) 
m_multi
#robust model
robrob <- robust(m_multi, cluster = Study, clubSandwich = TRUE)
robrob
#calculate i2 for each level- this is for CHE model not CHERVE model as it won't work with robust model - copy paste function from https://raw.githubusercontent.com/MathiasHarrer/dmetar/master/R/mlm.variance.distribution.R into console and hit enter
i2 <- var.comp(m_multi)
summary(i2)
i2

###########################
#Check for outliers#
###########################

#adapting CI calculation and plotting from https://cjvanlissa.github.io/Doing-Meta-Analysis-in-R/detecting-outliers-influential-cases.html
# Calculate CI for all observed effect sizes
dat1$upperci <- dat1$yi + 1.96 * sqrt(dat1$vi)
dat1$lowerci <- dat1$yi - 1.96 * sqrt(dat1$vi)
# Create filter variable
dat1$outlier <- dat1$upperci < m_multi$ci.lb | dat1$lowerci > m_multi$ci.ub
# Count number of outliers:
sum(dat1$outlier)
dat1
# Make a basic plot, based on the data in df, and specify that the x-variable is
# the effect size, 'd', the colour and fill of the histogram bars are based on
# the value of 'outlier':
ggplot(data = dat1, aes(x = yi, colour = outlier, fill = outlier)) +
  # Add a histogram with transparent bars (alpha = .2)
  geom_histogram(alpha = .2) +
  # Add a vertical line at the pooled effect value (m_re$b[1])
  geom_vline(xintercept = m_multi$b[1]) +
  # Apply a black and white theme
  theme_bw()


##########################################
#influence check
#adapting method from https://wviechtb.github.io/metafor/reference/influence.rma.mv.html to calculate influence 
#cook's distance
cooks <- cooks.distance(robrob)
plot(cooks, type=&quot;o&quot;, pch=19, xlab=&quot;Observed Outcome&quot;, ylab=&quot;Cook's Distance&quot;)

#note dfbetas doesn't work with robust results so we use the 3 level model CHE model
       dfbetas <-dfbetas(m_multi)
       dfbetas
       
       #hatvalues
       hatvalues <- hatvalues(robrob)
       hatvalues
       
       #################################################
       #Moderator Analyses#########
       ################################################
       ####Control Condition
       #calculate qb for categorical moderator
       mod.ctrlq <- rma.mv(yi,
                           V,
                           data = dat1,
                           random = ~ 1 | Study/ES_number, 
                           method = &quot;REML&quot;,
                           test = &quot;t&quot;,
                           dfs = &quot;contain&quot;,
                           mods = ~ factor(control_c))
       robust_q <- robust(mod.ctrlq, cluster = Study, clubSandwich = TRUE, digits = 3)
       summary(robust_q)
       
       #calculate levels for categorical moderator
       mod.ctrl <- rma.mv(yi,
                          V,
                          data = dat1,
                          random = ~ 1 | Study/ES_number, 
                          method = &quot;REML&quot;,
                          test = &quot;t&quot;,
                          dfs = &quot;contain&quot;,
                          mods = ~ -1 + factor(control_c))
       robust <- robust(mod.ctrl, cluster = Study, clubSandwich = TRUE, digits = 3)
       summary(robust)
       
       #run continuous moderator
       mod.cont <- rma.mv(yi,
                          V,
                          data = dat1,
                          random = ~ 1 | Study/ES_number, 
                          method = &quot;REML&quot;,
                          test = &quot;t&quot;,
                          dfs = &quot;contain&quot;,
                          mods = ~ cont_fakedata)
       robust_cont <- robust(mod.cont, cluster = Study, clubSandwich = TRUE, digits = 3)
       summary(robust_cont)
       
       ################################################
       #forest plot
       forest(robrob, slab = dat1$Study)
    </code></pre>")
})

    
    
    
# end don't change below this line'   ----      
    
}
# Run the application 
shinyApp(ui = ui, server = server)