#' @title Retention index predictions
#'
#' @description
#' \code{DeepReI} (Deep Learning-based Retention Index predictor) is gas chromatographic retention index predictor based on convolutional neural network.
#'
#' @details
#' It is designed to predict retention index of molecules (represented as SMILES strings) on SEMI-STANDARD NON-POLAR stationary
#' phases (e.g. 5% phenyl(dimethyl)polysiloxane). Data input, prediction and export of results is performed through GUI of shiny based app.
#'
#' @param ... function has no parameters (data input is performed by GUI)
#'
#' @return shiny.appobj
#'
#' @export
#'
#' @author Tomas Vrzal <tomas.vrzal@@beerresearch.cz>
#'
#' @references reference will be added
DeepReI <- function(){
  library(shiny)
  library(tidyverse)
  library(shinybusy)
  library(shinyjs)
  library(ChemmineR)
  library(stringr)
  library(keras)
  library(writexl)


shinyApp(
  ui = fluidPage(
    useShinyjs(),
    navbarPage(p("DeepReI - Deep Learning-based Retention Index Predictor",style = "font-family: 'times'; font-si16pt"),
               # SMILES input card ----
               tabPanel("SMILES input", icon = icon("file-import"), sidebarPanel(
                 fileInput("smiles", "Select SMILES (txt file)",buttonLabel = icon("folder-open"),
                           multiple = TRUE,
                           accept = c("text/csv",
                                      "text/comma-separated-values,text/plain",
                                      ".csv")),
                 div("Input has to have only one column."),
                 # Horizontal line
                 tags$hr(),

                 # Input: Checkbox if file has header
                 checkboxInput("header", "Header", FALSE),

                 # Horizontal line
                 tags$hr(),

                 # Input: Select number of rows to display
                 radioButtons("disp", "Display",
                              choices = c(Head = "head",
                                          All = "all"),
                              selected = "head"),
                 tags$hr(),
                 actionButton("clear_smile", label = "Clear", icon = icon("trash"))

               ),
               mainPanel(textOutput("text_smiles"),
                         tableOutput("smiles_table"))
               ),

               # Prediction card ----
               tabPanel("Prediction", icon = icon("angle-double-right"), sidebarPanel(
                 actionButton("predict", label = "Predict Retention Index", icon = icon("cogs")),
                 align = "center"),


                 mainPanel(textOutput("text_prediction"),
                           tableOutput("prediction"))
               ),
               # Export card ----
               tabPanel("Export results", icon = icon("table"), sidebarPanel(
                 downloadButton("export", label = "Export results", icon = icon("download")), align = "center",
                 tags$hr(),
                 actionButton("copy", label = "Copy results to clipboard", icon = icon("copy"))
               )),
               # Info card ----
               tabPanel("Info", icon = icon("info-circle"), mainPanel(div("DeepReI is a gas chromatographic retention index predictor based on convolutional neural network
                                                                            It is designed to predict retention index of a molecule on SEMI-STANDARD NON-POLAR stationary
                                                                            phases (e.g. 5% phenyl(dimethyl)polysiloxane)."),
                                                                      tags$hr(),
                                                                      div("INSTRUCTIONS:"),
                                                                      div("The required input to the DeepReI is SMILES string (Simplified Molecular-Input Line-Entry System) representation of a molecule."),
                                                                      div("SMILES can be loaded in SMILES input tab (.txt or .csv file) - the file have to consist of one column. If the first row represents header of the column, tick the Header box."),
                                                                      div("Retention index prediction could be performed by clicking on ?Predict Retention Index? button in the Prediction tab."),
                                                                      div("When prediction is done, table with predicted value of retention index, together with reprective SMILES, will appear in the Prediction tab."),
                                                                      div("Export or copy of the resulted table is avalaible in Export results tab."),
                                                                      tags$hr(),
                                                                      div("Developed by Tomas Vrzal and Michaela Maleckova at Research Institute of Brewing and Malting, Prague, Czech Republic (www.beerresearch.cz)."),
                                                                      div("Development was suported by the Ministry of agriculture of the Czech republic within the institutional support MZE-RO1918."),
                                                                      tags$hr(),
                                                                      div("Please cite as follows:"),
                                                                      div("reference will be added"),
                                                                      tags$hr(),
                                                                      div("link and DOI will be added; https://github.com/TomasVrzal/DeepReI"),
                                                                      tags$hr(),
                                                                      div("For more info and  troubleshooting please contact the author: tomas.vrzal@beerresearch.cz")
               )


               )
    )

  ),
  server = function(input, output) {
    # SMILES table output ----
    output$smiles_table <- renderTable({
      req(input$smiles)
      sm <- read.csv(input$smiles$datapath,
                     header = input$header
      )

      if(input$disp == "head") {
        return(head(sm))
      }
      else {
        return(sm)
      }

    })
    # Imported data text for flags ----
    output$text_flags <- renderText({

      req(input$flags)

      return("Imported data")

    })
    # Imported data text fo SMILES ----
    output$text_smiles <- renderText({

      req(input$smiles)

      return("Imported data")

    })
    # Flags table output ----
    output$flags_table <- renderTable({

      req(input$flags)

      fl <- read.csv(input$flags$datapath,
                     header = input$header_flags,
                     sep = input$sep,
                     quote = input$quote,
                     dec = input$dec)

      if(input$disp_flags == "head") {
        return(head(fl))
      }
      else {
        return(fl)



      }

    })

    # RI prediction after "predict" button click ----
    observeEvent(input$predict, {
      req(input$smiles)

      show_modal_spinner(spin = "fading-circle", text = "DeepReI in progress")

      # DeepReI body

      tokenizer_loaded <- load_text_tokenizer(system.file("extdata", "tokenizer_train_aug", package = "DeepReI"))
      model_DeepReI <- load_model_hdf5(system.file("extdata", "model_DeepReI.h5", package = "DeepReI"), compile = FALSE)

      smiles_r <- read.SMIset(input$smiles$datapath)
      sdf <- ChemmineR::smiles2sdf(smiles_r)
      prop <- ChemmineR::propOB(sdf)
      t1_r <- str_replace_all(prop[,1], "Si", "G")
      t2_r <- str_replace_all(t1_r, "c", "a")
      t3_r <- str_replace_all(t2_r, "o", "b")
      t4_r <- str_replace_all(t3_r, "n","d")
      t5_r <- str_replace_all(t4_r, "Cl","w")
      t6_r <- str_replace_all(t5_r, "Br","Q")
      t7 <- texts_to_sequences(tokenizer_loaded, t6_r)
      vectorize_sequences <- function(sequences, dimension = 32) {
        results <- matrix(as.integer(0), nrow = 183, ncol = dimension)
        for (i in 1:length(sequences))
          results[i, sequences[[i]]] <- as.integer(1)
        t(results)
      }
      t8 <- lapply(t7, vectorize_sequences)
      t9 <- array_reshape(t8, c(length(smiles_r),32, 183,1), order = "F")
      t10 <- array_reshape(t9, c(length(smiles_r),32,183, 1))

      predicted <- model_DeepReI %>% predict(t10) %>% round(digits = 0)
      sm <- read.csv(input$smiles$datapath,
                     header = input$header)
      predicted <- cbind(sm, predicted)
      colnames(predicted) <- c("SMILES", "RI")
      predicted <- data.frame(predicted)

      #
      # Function for retention time calculation
      rt <- function(flag, pred){
        fl <- read.csv(flag,
                       header = TRUE,
                       sep = "\t", dec = ",", colClasses = "double")

        reg <- lm(fl[,2] ~ poly(fl[,1],3, raw = TRUE))
        pred_plus <- pred %>% mutate(
          retention_time = coef(reg)[1] + coef(reg)[2]*pred$RI + coef(reg)[3]*pred$RI^2 + coef(reg)[4]*pred$RI^3)
        return(pred_plus)
      }
      # Output table with predicted values
      output$prediction <- renderTable({

        if(is.null(isolate(input$flags$datapath)) == TRUE) {
          return(predicted)
        }
        else {

          rt_comp <- rt(flag = isolate(input$flags$datapath), pred = predicted)
          return(rt_comp)
        }

      },
      digits = 2)


      remove_modal_spinner()
      # Export or copy results ----
      output$export <- downloadHandler(

        filename = function() {
          paste(input$smiles, Sys.Date(), "-", format(Sys.time(), "%R"), ".xlsx", sep="")
        },
        content = function(file) {
          if(is.null(isolate(input$flags$datapath)) == TRUE){
            write_xlsx(predicted, path = file)
          }
          else {
            write_xlsx(rt(flag = isolate(input$flags$datapath), pred = predicted), path = file)
          }
        })

      observeEvent(input$copy, {
        req(predicted)
        if(is.null(isolate(input$flags$datapath)) == TRUE){
          write.table(predicted, "clipboard-16384", sep="\t", col.names=TRUE, row.names = FALSE)
        }
        else {
          write.table(rt(flag = isolate(input$flags$datapath), pred = predicted), "clipboard-16384", sep="\t", col.names=TRUE, row.names = FALSE)
        }

      })

    })

    # Reset flags input (delete flags file from input) ----
    observeEvent(input$clear_flags, {
      reset("flags")
    })
    # Reset SMILES input (delete SMILES file from input) ----
    observeEvent(input$clear_smile, {
      reset("smiles")
    })

  },
  options = list(launch.browser = TRUE)

)
}

