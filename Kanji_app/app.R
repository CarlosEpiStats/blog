# Loading necessary libraries ####
library(tidyverse)
# library(here)
library(janitor)
library(readxl)
library(writexl)
#library(shiny)
library(DT)

# Loading datasets ####

file_kanji <- "Kanji_20240227_081842.csv"
file_jukugo <- "Jukugo_20240227_081908.csv"
file_words <- "Optimized Kore - Sheet1.csv"
file_sentences1 <- "Sentences.xlsx"
file_sentences2 <- "Sentences_core.xlsx"

## Kanji list ####
data_kanji <- read.csv2(file_kanji) %>% 
  clean_names()

## Jukugo list ####
data_jukugo <- read.csv2(file_jukugo) %>% 
  clean_names() %>% 
  arrange(desc(frequency)) %>% 
  select(comp_word, pronunciation, english_translation) %>%
  rename(word = comp_word,
         meaning = english_translation) %>% 
  distinct(word, .keep_all = TRUE) %>% 
  # Change romaji
  mutate(
    pronunciation = gsub("zi", "ji", pronunciation),
    pronunciation = gsub("zy", "jy", pronunciation),
    pronunciation = gsub("ti", "chi", pronunciation),
    pronunciation = gsub("ty", "ch", pronunciation),
    pronunciation = gsub("si", "shi", pronunciation),
    pronunciation = gsub("sy", "shy", pronunciation),
    pronunciation = gsub("tu", "tsu", pronunciation),
    pronunciation = gsub("hu", "fu", pronunciation)
    )

## word list ####
data_words <- read.csv(file_words) %>% 
  clean_names() %>% 
  arrange(core_index) %>% 
  select(vocab_expression, vocab_kana, vocab_meaning) %>% 
  rename(word = vocab_expression,
         pronunciation = vocab_kana,
         meaning = vocab_meaning) %>% 
  distinct(word, .keep_all = TRUE) 

## Number list ####

data_numbers <- data.frame(
  word = c("零", "一", "二", "三", "四", "五", "六", "七", "八", "九", "十", 
     "十一", "十二", "十三", "十四", "十五", "十六", "十七", "十八", "十九", 
     "二十", "二十一", "二十二", "二十三", "二十四", "二十五", "二十六", "二十七", 
     "二十八", "二十九", "三十", "三十一", "三十二", "三十三", "三十四", "三十五", 
     "三十六", "三十七", "三十八", "三十九", "四十", "四十一", "四十二", "四十三", 
     "四十四", "四十五", "四十六", "四十七", "四十八", "四十九", "五十", "五十一", 
     "五十二", "五十三", "五十四", "五十五", "五十六", "五十七", "五十八", "五十九", 
     "六十", "六十一", "六十二", "六十三", "六十四", "六十五", "六十六", "六十七", 
     "六十八", "六十九", "七十", "七十一", "七十二", "七十三", "七十四", "七十五", 
     "七十六", "七十七", "七十八", "七十九", "八十", "八十一", "八十二", "八十三", 
     "八十四", "八十五", "八十六", "八十七", "八十八", "八十九", "九十", "九十一", 
     "九十二", "九十三", "九十四", "九十五", "九十六", "九十七", "九十八", "九十九", "百"),
  pronunciation = c("れい", "いち", "に", "さん", "し", "ご", "ろく", "しち", "はち", "きゅう", "じゅう",
                    "じゅういち", "じゅうに", "じゅうさん", "じゅうし", "じゅうご", "じゅうろく", "じゅうしち", "じゅうはち", "じゅうきゅう",
                    "にじゅう", "にじゅういち", "にじゅうに", "にじゅうさん", "にじゅうし", "にじゅうご", "にじゅうろく", "にじゅうしち", "にじゅうはち", "にじゅうきゅう",
                    "さんじゅう", "さんじゅういち", "さんじゅうに", "さんじゅうさん", "さんじゅうし", "さんじゅうご", "さんじゅうろく", "さんじゅうしち", "さんじゅうはち", "さんじゅうきゅう",
                    "よんじゅう", "よんじゅういち", "よんじゅうに", "よんじゅうさん", "よんじゅうし", "よんじゅうご", "よんじゅうろく", "よんじゅうしち", "よんじゅうはち", "よんじゅうきゅう",
                    "ごじゅう", "ごじゅういち", "ごじゅうに", "ごじゅうさん", "ごじゅうし", "ごじゅうご", "ごじゅうろく", "ごじゅうしち", "ごじゅうはち", "ごじゅうきゅう",
                    "ろくじゅう", "ろくじゅういち", "ろくじゅうに", "ろくじゅうさん", "ろくじゅうし", "ろくじゅうご", "ろくじゅうろく", "ろくじゅうしち", "ろくじゅうはち", "ろくじゅうきゅう",
                    "しちじゅう", "しちじゅういち", "しちじゅうに", "しちじゅうさん", "しちじゅうし", "しちじゅうご", "しちじゅうろく", "しちじゅうしち", "しちじゅうはち", "しちじゅうきゅう",
                    "はちじゅう", "はちじゅういち", "はちじゅうに", "はちじゅうさん", "はちじゅうし", "はちじゅうご", "はちじゅうろく", "はちじゅうしち", "はちじゅうはち", "はちじゅうきゅう",
                    "きゅうじゅう", "きゅうじゅういち", "きゅうじゅうに", "きゅうじゅうさん", "きゅうじゅうし", "きゅうじゅうご", "きゅうじゅうろく", "きゅうじゅうしち", "きゅうじゅうはち", "きゅうじゅうきゅう",
                    "ひゃく"),
  meaning = as.character(0:100)
  )

# Sentence list ####
data_sentences1 <- read_excel(file_sentences1) 
data_sentences2 <- read_excel(file_sentences2)

data_sentences <- rbind(data_sentences1, data_sentences2)
rm(data_sentences1, data_sentences2)


# Extract kana ####

hiragana_chars <- intToUtf8(seq(12353, 12438)) # Unicode range for hiragana characters
katakana_chars <- intToUtf8(seq(12448, 12543)) # Unicode range for katakana characters

kana <- paste0(hiragana_chars, katakana_chars) %>% 
  str_split_1(pattern = "") %>% 
  paste(collapse = "|")

kana


# All data ####
data_bind <- data_words %>% 
  rbind(data_sentences %>% select(word, pronunciation, meaning),
        data_numbers,
        data_jukugo) %>% 
  mutate(word = str_remove_all(word, "[ a-zA-Z]")) %>% 
  distinct(word, .keep_all = TRUE)

data_bind

# Extract kanji ####

all_kanji <- data_kanji %>% 
  select(kanji) %>% 
  rbind(data_bind %>% 
          mutate(kanji = word, .keep = "none")) %>% 
  mutate(kanji = str_remove_all(kanji, kana)) %>% 
  filter(kanji != "") %>% 
  pull() %>% 
  paste(collapse = "") %>% 
  str_split_1(pattern = "") %>% 
  unique()
  
## Separate individual kanji from all words ####
data_all <- data_bind %>% 
  mutate(kanji = str_remove_all(word, kana)) %>% 
  separate_wider_position(kanji, widths = c("kanji_1" = 1, 
                                            "kanji_2" = 2,
                                            "kanji_3" = 3,
                                            "kanji_4" = 4,
                                            "kanji_5" = 5),
                          too_few = "align_start") %>% 
  filter(!is.na(kanji_1))

# Functions ####

make_study_blocks <- function(kanji_learning) {
  
  # Select words with only those kanji
  kanji_combinations <- data_all %>% 
    filter(kanji_1 %in% kanji_learning, 
           kanji_2 %in% kanji_learning | is.na(kanji_2),
           kanji_3 %in% kanji_learning | is.na(kanji_3),
           kanji_4 %in% kanji_learning | is.na(kanji_4),
           kanji_5 %in% kanji_learning | is.na(kanji_5)) 
  
  learning_length <- length(kanji_learning)
  
  # Separate the list in blocks by kanji
  kanji_list <- list()
  
  for (i in 1:learning_length) {
    
    tmp <- kanji_combinations %>% 
      filter(kanji_1 %in% kanji_learning[i] | 
               kanji_2 %in% kanji_learning[i] | 
               kanji_3 %in% kanji_learning[i] |
               kanji_4 %in% kanji_learning[i] | 
               kanji_5 %in% kanji_learning[i])
    tmp2 <- rbind(data.frame(word = "***", pronunciation = "***", meaning = "***",
                             kanji_1 = "***", kanji_2 = "***", kanji_3 = "***", kanji_4 = "***", kanji_5 = "***"),
                  tmp) %>% 
      select(!starts_with("kanji_"))
    
    kanji_list[[i]] <- tmp2
    
  }
  
  study_words <- bind_rows(kanji_list)
  
  # Return the final dataframe
  
  study_words
}

make_study_list <- function(kanji_learning) {
  
  # Select words with only those kanji
  study_words <- data_all %>% 
    filter(kanji_1 %in% kanji_learning,  
             kanji_2 %in% kanji_learning | is.na(kanji_2),
             kanji_3 %in% kanji_learning | is.na(kanji_3),
             kanji_4 %in% kanji_learning | is.na(kanji_4),
             kanji_5 %in% kanji_learning | is.na(kanji_5)) %>% 
    select(!starts_with("kanji_"))
  
  study_words
}


make_sentences_list <- function(kanji_learning) {
  
  kanji_list <- paste(kanji_learning, collapse = "|")
  
  letters_lower <- paste(letters, collapse = "|")
  letters_upper <- paste(LETTERS, collapse = "|")
  numbers <- paste(0:9, collapse = "|")
  symbols <- "。|、|「|」|？|!|%|！|々"
  
  other_characters <- paste(c(letters_lower, letters_upper, numbers, symbols), collapse = "|")
  
  sentences_list <- data_sentences %>% 
    mutate(only_kanji = str_remove_all(sentence, paste(kana, other_characters, sep = "|")),
           unknown_kanji = str_remove_all(only_kanji, kanji_list)) %>% 
    filter(unknown_kanji == "", grepl(kanji_list, sentence)) %>% 
    select(sentence, sentence_hiragana, sentence_meaning)
  
  sentences_list
  
}



# Shiny App ####

ui <- fluidPage(
  
  titlePanel("List Japanese Words Including Selected Kanji"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      downloadButton("downloadExcel", "Download Excel file"),
      downloadButton("downloadCSV", "Download CSV file"),
      selectInput ("kanji_selected", "Select Kanji:",  
                  choices = all_kanji, multiple = TRUE)

    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          "List of words",
          textOutput("n_kanji"),
          textOutput("n_words"),
          DTOutput("table_words")
          ),
        
        tabPanel(
          "List of sentences",
          textOutput("n_sentences"),
          DTOutput("table_sentences")
        ),
        
        tabPanel(
          "Stats",
          tableOutput("number_words_per_kanji")
        )
      )
      
      
    )
  )
)

server <- function(input, output) {
  
  # Reactive table words ####
  rval_table_words <- reactive({
    study_table <- make_study_list(input$kanji_selected)
    study_table
  })
  
  
  # Reactive table sentences ####
  rval_table_sentences <- reactive({
    study_table <- make_sentences_list(input$kanji_selected)
    study_table
  })
  
  
  # Reactive table counter ####
  rval_table_counter <- reactive({
    
    study_words <- data_all %>% 
      filter(kanji_1 %in% input$kanji_selected, 
             kanji_2 %in% input$kanji_selected | is.na(kanji_2),
             kanji_3 %in% input$kanji_selected | is.na(kanji_3),
             kanji_4 %in% input$kanji_selected | is.na(kanji_4),
             kanji_5 %in% input$kanji_selected | is.na(kanji_5))
    
    learning_length <- length(input$kanji_selected)
    
    kanji_counter <- c()
    
    for (i in 1:learning_length){
      
      kanji_counter[i] <- study_words %>% 
        filter(kanji_1 %in% input$kanji_selected[i] | 
                 kanji_2 %in% input$kanji_selected[i] | 
                 kanji_3 %in% input$kanji_selected[i] |
                 kanji_4 %in% input$kanji_selected[i] | 
                 kanji_5 %in% input$kanji_selected[i] ) %>% 
        nrow()
      
    }
    
    table_counter <- data.frame(kanji = input$kanji_selected,
                                times = kanji_counter)
    
    table_counter
    
  })
  
  # Reactive number kanji ####
  rval_n_kanji <- reactive({
    number_words <- length(input$kanji_selected)
    number_words
  }) 
  
  
  # Reactive number words ####
  rval_n_words <- reactive({
    study_table <- rval_table_words()
    number_words <- nrow(study_table)
    number_words
  })
  
  # Reactive number sentences ####
  rval_n_sentences <- reactive({
    study_table <- rval_table_sentences()
    number_sentences <- nrow(study_table)
    number_sentences
  })
  
  # output number words ####
  output$n_words <- renderText({
    n_words <- rval_n_words()
    paste0("Number of words: ", n_words)
  })
  
  # output number Kanji ####
  output$n_kanji <- renderText({
    number_kanji <- rval_n_kanji()
    paste0("Number of kanji: ", number_kanji)
  })
  
  # output number sentences ####
  output$n_sentences <- renderText({
    number_sentences <- rval_n_sentences()
    paste0("Number of sentences: ", number_sentences)
  })
  
  # output table words ####
  output$table_words <- renderDT({
    study_table <- rval_table_words()
    study_table
  })
  
  # output table sentences ####
  output$table_sentences <- renderDT({
    study_table <- rval_table_sentences()
    study_table
  })
  
  # output table counter ####
  
  output$number_words_per_kanji <- renderTable({
    table_counter <- rval_table_counter()
    table_counter
  })
  
  # download Excel ####
  
  output$downloadExcel <- downloadHandler(
    
    filename = function() {
      paste0("study_list_k", rval_n_kanji(), "_w", rval_n_words(),".xlsx")
    },
    content = function(file) {
      write_xlsx(rval_table_words(), file)
      
    })
  
  # download CSV ####
  
  output$downloadCSV <- downloadHandler(
    
    filename = function() {
      paste0("study_list_k", rval_n_kanji(), "_w", rval_n_words(),".csv")
    },
    content = function(file) {
      write_csv(rval_table_words(), file)
      
    })

}


shinyApp(ui = ui, server = server)




