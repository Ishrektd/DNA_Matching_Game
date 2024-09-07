# List of required packages
required_packages <- c("shiny", "sortable")

# Function to check and install missing packages
install_if_missing <- function(packages) {
  for (pkg in packages) {
    if (!require(pkg, character.only = TRUE)) {
      install.packages(pkg, dependencies = TRUE)
      library(pkg, character.only = TRUE)
    }
  }
}

# Install missing packages
install_if_missing(required_packages)

# Libraries
library(shiny)
library(sortable)

# Function to color-code each nucleotide with the new colors
color_code <- function(sequence) {
  sequence <- gsub("A", "<span style='color:#D81B60; font-weight:bold;'>A</span>", sequence) # D81B60
  sequence <- gsub("T", "<span style='color:#1E88E5; font-weight:bold;'>T</span>", sequence) # 1E88E5
  sequence <- gsub("C", "<span style='color:#FFC107; font-weight:bold;'>C</span>", sequence) # FFC107
  sequence <- gsub("G", "<span style='color:#008871; font-weight:bold;'>G</span>", sequence) # 008871
  return(sequence)
}

# Sequences
sequences <- list(
  "ATCGGCTAAGCTTAGCGGCTAAGCT",
  "TAGCCGATTCGAATCGCCGATTCGA",
  "GCTTAGCGGCTAAGCTTAGCGTCG",
  "CGAATCGCCGATTCGAATCGCAGC",
  "TCGGCTAAGCTTAGCGGCTAAGCTT",
  "AGCCGATTCGAATCGCCGATTCGA"
)

# Shuffle sequences
set.seed(Sys.time()) # Ensure randomness
shuffled_sequences <- sample(sequences)

matching_ui <- fluidPage(
  titlePanel("DNA matching game"),
  fluidRow(
    column(12, 
           h4(HTML("Now we will match our DNA strands to their complementary strands. Click and drag strands that are complementary to each other in order. Remember, <span style='color:#D81B60; font-weight:bold;'>A</span> matches <span style='color:#1E88E5; font-weight:bold;'>T</span>, <span style='color:#FFC107; font-weight:bold;'>C</span> matches <span style='color:#008871; font-weight:bold;'>G</span>!"))
    ),
    column(6,
           bucket_list(
             header = "Drag the sequences to match their complementary strands",
             group_name = "bucket_list_group",
             orientation = "horizontal",
             add_rank_list(
               text = "Sequences",
               labels = lapply(shuffled_sequences, function(seq) HTML(color_code(seq))),
               input_id = "sequences",
               options = sortable_options(style = "font-size: 20px; font-weight: bold;")
             ),
             add_rank_list(
               text = "Complementary Strands",
               labels = NULL,
               input_id = "complements",
               options = sortable_options(style = "font-size: 20px; font-weight: bold;")
             )
           )
    ),
    column(6,
           div(htmlOutput("result"), style = "font-size: 20px;")
    )
  )
)

matching_server <- function(input, output, session) {
  output$result <- renderUI({
    sequences <- input$sequences
    complements <- input$complements
    
    correct_pairs <- list(
      "ATCGGCTAAGCTTAGCGGCTAAGCT" = "TAGCCGATTCGAATCGCCGATTCGA",
      "TAGCCGATTCGAATCGCCGATTCGA" = "ATCGGCTAAGCTTAGCGGCTAAGCT",
      "GCTTAGCGGCTAAGCTTAGCGTCG" = "CGAATCGCCGATTCGAATCGCAGC",
      "CGAATCGCCGATTCGAATCGCAGC" = "GCTTAGCGGCTAAGCTTAGCGTCG",
      "TCGGCTAAGCTTAGCGGCTAAGCTT" = "AGCCGATTCGAATCGCCGATTCGA",
      "AGCCGATTCGAATCGCCGATTCGA" = "TCGGCTAAGCTTAGCGGCTAAGCTT"
    )
    
    if (length(sequences) == length(complements) && length(sequences) > 0) {
      matches <- mapply(function(seq, comp) correct_pairs[[gsub("<.*?>", "", seq)]] == gsub("<.*?>", "", comp), sequences, complements)
      if (all(matches)) {
        HTML("<span style='color:green; font-weight:bold;'>Congratulations! All pairs are correct!</span>")
      } else {
        HTML("<span style='color:red; font-weight:bold;'>Some pairs are incorrect. Try again.</span>")
      }
    } else {
      HTML("<span style='color:orange; font-weight:bold;'>Please match all sequences with their complements.</span>")
    }
  })
}

shinyApp(ui = matching_ui, server = matching_server)
