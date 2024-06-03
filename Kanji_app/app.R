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

make_study_list <- function(kanji_learning) {
  
  # Select words with only those kanji
  study_words <- data_all %>% 
    filter(kanji_1 %in% kanji_learning,  
             kanji_2 %in% kanji_learning | is.na(kanji_2),
             kanji_3 %in% kanji_learning | is.na(kanji_3),
             kanji_4 %in% kanji_learning | is.na(kanji_4),
             kanji_5 %in% kanji_learning | is.na(kanji_5)) %>% 
    mutate(order = row_number()) %>% 
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

# Kanji in order by Genetic Kanji

genetic_kanji_grade1 <- "一二三丨王玉五十七上下八亅小匸匹四亠六木林森本人入亻休体千九丸田力男丁町日月早艸草倝朝匕化花茶屮又友口右工左目儿見貝員負糸細白百曰大太天犬立音暗出土生星年青晴先寸村水川氵也池地他文子宀字学女好穴空中虫交校夕卜外名多夜止正足走歩火灬赤雨気車耳手毛个竹厂石山岩" %>% 
  str_split_1(pattern = "")

genetic_kanji_grade2 <- "汽占点广店電雪云雲買言士売読続舌話活市姉未妹味心必意思厶禾私公広台始回己記起氏紙谷魚黒戸角計算委秋斗科春矢知西北南母毎海父至室肀聿書筆画亼今巾帚婦帰豕豚家鳥島鳴馬舟船古苦丂可欠哥歌何皀食飲刀切分刂前明東隹羽翟曜習旦尸尺昼駅米番半来麦面支攴攵牧毋婁娄数老孝考教豆自頁頭首辵辶道用甬通周週調弓引弱弘強弔弟第泉線原願源且組助声直真植夂冬終夊夏当同銅凡風万方放斿遊旅族衣表長巾帳門問聞間開閉里理予野肉有亲親斤新所近遠光囗国図園夭笑高喬橋吾語会亼合答午許寺時彳亍行後黄横卩邑色冂内丙病" %>% 
  str_split_1(pattern = "")

genetic_kanji_grade3 <- "具歯卩令命
                        服丂号世枼
                        葉安全育
                        充流辰晨
                        曲農濃畑品
                        酉乍作昨酒
                        配非悲相想
                        箱亜悪業反
                        坂板返皮波
                        兔免勉晩自
                        鼻息勿物樂
                        楽薬由油笛
                        羊美着差洋
                        羕様和亏于
                        平少秒壬重
                        動働童夂各
                        洛落路客易
                        昜阜阝陽湯
                        咅倍部殳役
                        投両柬練录
                        緑豕豚彖縁
                        歹死列例皿
                        血丶主注註
                        柱住氷永泳
                        州庫転軍運
                        揮短爪 受
                        授艮根銀眼
                        失鉄及級汲
                        進灷送束速
                        敕整取趣最
                        扌打元完院
                        次央英映尸
                        句局屋昷温
                        界比皆階咸
                        感減堇漢難
                        弋代式試茻
                        寒巷港者都
                        暑匚匸医区
                        県縣懸史吏
                        使事守癶発
                        登荷対肖消
                        去廴廷庭章
                        商官館
                        管其期急呂
                        宮求球救追
                        尹君郡群
                        系係胡湖向
                        幸仕旨指実
                        与舄寫写宿
                        券巻朕勝
                        乗身罙深昔
                        借定是題厂
                        屵炭干岸炎
                        談庶度席
                        服報圣軽径
                        経召昭照招
                        夬決快寺待
                        持詩等特" %>% 
  str_remove_all("\n") %>% 
  str_split_1(pattern = "")

genetic_kanji_grade4 <- "拾給成功改
                        紀孫好料札
                        漁衣類胃腸
                        芸侖輪論倫
                        連器信塩季
                        節覚関巽
                        選挙散治
                        祝初臣帯
                        隊墜達置仲
                        宁貯兆典徒
                        灯得夫規婦
                        掃辺変包法
                        満民眠無舞
                        要厤歴暦老
                        若彡参勇痛
                        井囲億憶戒
                        械亭停勺的
                        約責積績債
                        付府愛念案
                        果課巣松吉
                        結壴喜建健
                        種氐低底象
                        像奴努告造
                        圭街佳固個
                        然燃熒栄営
                        蛍養飯版仮
                        弗費佛仏加
                        賀貨尚賞堂
                        常党雚観眾
                        众衆権勧戈
                        我幾機義議
                        共供彥産吅
                        単戦敗争静
                        清情精亢航
                        訓順昌唱堯
                        焼梅毐毒則
                        側測升昇飛
                        競竟鏡境采
                        採菜察際隶
                        康畐副富
                        亡望忘専伝
                        甫補尃博侯
                        候兵丘別利
                        末抹亟極僉
                        験険検牙芽
                        卒雑冫冷領
                        票標更便派
                        脈戔浅残銭
                        旗基旧児位
                        泣粉貧印布
                        希刷量录
                        録刑型良限
                        朗网置値朮
                        杀殺坴陸埶
                        熱勢享熟害
                        割憲損不否
                        省砂辛辞
                        乱折断労協
                        呆保浴容欲
                        以似倉創" %>% 
  str_remove_all("\n") %>% 
  str_split_1(pattern = "")

genetic_kanji_grade5 <- "解件圧厚久
                        移兑税往徳
                        祖禁査攸修
                        條条効效鉱
                        拡弁益演液
                        瞢夢妻妾接
                        櫻桜隺確屰
                        逆夋酸師師
                        現在舎舍忄
                        性素応忩総
                        總統蜀獨独
                        犯屬属居故
                        率筑築張提
                        過迷退導団
                        團扁編破判
                        刊幹俵武豊
                        豐防暴帛綿
                        錦刖俞輸評
                        余除戠職識
                        織韋衛偉司
                        飼詞因恩述
                        術复復複腹
                        帝啻適敵冉
                        再冓構講耕
                        卯卵留貿質
                        賛贊任呈程
                        賃貸証政蒦
                        護肥絶曾曽
                        増層匀均臼
                        興額略格閣
                        序預矛務河
                        奇寄技枝制
                        製准準備価
                        契喫絜潔
                        蒸承片爿状
                        壮装能態" %>% 
  str_remove_all("\n") %>% 
  str_split_1(pattern = "")



kanji_grade1 <- genetic_kanji_grade1[genetic_kanji_grade1 %in% all_kanji]
kanji_grade2 <- genetic_kanji_grade2[genetic_kanji_grade2 %in% all_kanji]
kanji_grade3 <- genetic_kanji_grade3[genetic_kanji_grade3 %in% all_kanji]
kanji_grade4 <- genetic_kanji_grade4[genetic_kanji_grade4 %in% all_kanji]
kanji_grade5 <- genetic_kanji_grade5[genetic_kanji_grade5 %in% all_kanji]

kanji_grade_other <- all_kanji[!all_kanji %in% c(kanji_grade1, kanji_grade2, kanji_grade3, kanji_grade4, kanji_grade5)]

kanji_order_grade <-c(kanji_grade1, kanji_grade2, kanji_grade3, kanji_grade4, kanji_grade5, kanji_grade_other)



# Shiny App ####

ui <- fluidPage(
  
  titlePanel("List Japanese Words Including Selected Kanji"),
  
  sidebarLayout(
    
    sidebarPanel(
      p("Created by CarlosEpiStats"),
      uiOutput("blog_link"),
      downloadButton("downloadExcel", "Download Excel file"),
      downloadButton("downloadCSV", "Download CSV file"),
      checkboxGroupInput("kanji_selected1", "Select Kanji (Grade 1):",  
                  choices = kanji_grade1, inline = TRUE),
      
      actionButton("select_all_kanji1", "Select All Kanji Grade 1"),
      actionButton("deselect_all_kanji1", "Deselect All Kanji Grade 1"),
      
      checkboxGroupInput("kanji_selected2", "Select Kanji (Grade 2):",  
                         choices = kanji_grade2, inline = TRUE),
      
      actionButton("select_all_kanji2", "Select All Kanji Grade 2"),
      actionButton("deselect_all_kanji2", "Deselect All Kanji Grade 2"),
      
      selectInput("kanji_selected3", "Select Kanji (Grade 3):",  
                  choices = kanji_grade3, multiple = TRUE, ),
      selectInput("kanji_selected4", "Select Kanji (Grade 4):",  
                   choices = kanji_grade4, multiple = TRUE, ),
      selectInput("kanji_selected5", "Select Kanji (Grade 5):",  
                   choices = kanji_grade5, multiple = TRUE, )

    ),
    
    mainPanel(
      
      tabsetPanel(
        
        tabPanel(
          "List of words",
          p("Number of Kanji and Words in the Study Table"),
          textOutput("n_kanji"),
          textOutput("n_words"),
          p("\nWords with last Kanji added"),
          textOutput("last_kanji"),
          DTOutput("words_last_kanji"),
          p("\nAll words"),
          DTOutput("table_words")
          ),
        
        tabPanel(
          "List of sentences",
          textOutput("n_sentences"),
          DTOutput("table_sentences")
        ),
        
        tabPanel(
          "Stats",
          DTOutput("number_words_per_kanji")
        )
      )
      
      
    )
  )
)

server <- function(input, output, session) {
  
  # Blog link ####
  url <- a("CarlosEpiStats", href="https://carlosepistats.github.io/blog/")
 
  output$blog_link <- renderUI({
    tagList("Blog:", url)
  })
  
  # Reactive input_kanji ####
  rval_input_kanji <- reactive({
    c(input$kanji_selected1, input$kanji_selected2, input$kanji_selected3, input$kanji_selected4, input$kanji_selected5)
  })
  
  # This observer will update checkboxes of Kanji grade 1 to TRUE whenever select_all_kanji1 is clicked
  observeEvent(input$select_all_kanji1, {
      updateCheckboxGroupInput(session = session, 
                                     inputId = "kanji_selected1", 
                                     selected = kanji_grade1 )
    }
  )
  
  # This observer will uncheck checkboxes of Kanji grade 1 to TRUE whenever deselect_all_kanji1 is clicked
  observeEvent(input$deselect_all_kanji1, {
    updateCheckboxGroupInput(session = session, 
                             inputId = "kanji_selected1", 
                             selected = character(0))
  }
  ) 
  
  # This observer will update checkboxes of Kanji grade 2 to TRUE whenever select_all_kanji2 is clicked
  observeEvent(input$select_all_kanji2, {
    updateCheckboxGroupInput(session = session, 
                             inputId = "kanji_selected2", 
                             selected = kanji_grade2 )
  }
  )
  
  # This observer will uncheck checkboxes of Kanji grade 2 to TRUE whenever deselect_all_kanji2 is clicked
  observeEvent(input$deselect_all_kanji2, {
    updateCheckboxGroupInput(session = session, 
                             inputId = "kanji_selected2", 
                             selected = character(0))
  }
  )   
  
  # Reactive table words ####
  rval_table_words <- reactive({
    study_table <- make_study_list(rval_input_kanji())
    study_table
  })
  
  
  # Reactive table sentences ####
  rval_table_sentences <- reactive({
    study_table <- make_sentences_list(rval_input_kanji())
    study_table
  })
  
  
  # Reactive table counter ####
  rval_table_counter <- reactive({
    
    study_words <- data_all %>% 
      filter(kanji_1 %in% rval_input_kanji(), 
             kanji_2 %in% rval_input_kanji() | is.na(kanji_2),
             kanji_3 %in% rval_input_kanji() | is.na(kanji_3),
             kanji_4 %in% rval_input_kanji() | is.na(kanji_4),
             kanji_5 %in% rval_input_kanji() | is.na(kanji_5))
    
    learning_length <- length(rval_input_kanji())
    
    kanji_counter <- c()
    
    for (i in 1:learning_length){
      
      kanji_counter[i] <- study_words %>% 
        filter(kanji_1 %in% rval_input_kanji()[i] | 
                 kanji_2 %in% rval_input_kanji()[i] | 
                 kanji_3 %in% rval_input_kanji()[i] |
                 kanji_4 %in% rval_input_kanji()[i] | 
                 kanji_5 %in% rval_input_kanji()[i] ) %>% 
        nrow()
      
    }
    
    table_counter <- data.frame(kanji = rval_input_kanji(),
                                times = kanji_counter)
    
    table_counter
    
  })
  
  # Reactive number kanji ####
  rval_n_kanji <- reactive({
    number_words <- length(rval_input_kanji())
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
  
  output$number_words_per_kanji <- renderDT({
    table_counter <- rval_table_counter()
    table_counter
  })
  
  # download Excel ####
  
  output$downloadExcel <- downloadHandler(
    
    filename = function() {
      paste0("study_list_k", rval_n_kanji(), "_w", rval_n_words(),".xlsx")
    },
    content = function(file) {
      write_xlsx(rval_table_words(), file, col_names = FALSE)
      
    })
  
  # download CSV ####
  
  output$downloadCSV <- downloadHandler(
    
    filename = function() {
      paste0("study_list_k", rval_n_kanji(), "_w", rval_n_words(),".csv")
    },
    content = function(file) {
      write_csv(rval_table_words(), file, col_names = FALSE)
      
    })
  
  # Show last kanji added ####
  
  output$last_kanji <- renderText({
    
    last_kanji <- rval_input_kanji()[rval_n_kanji()]
    
    paste0("Last kanji selected: ", last_kanji)
    
  })
  
  # Words of last kanji selected ####
  
  output$words_last_kanji <- renderDT({
    
    if (length(rval_input_kanji()) > 0 ){
    last_kanji <- rval_input_kanji()[rval_n_kanji()]
    
    rval_table_words() %>% 
      filter(grepl(last_kanji, word)) %>% 
      mutate(order = row_number())
    } else {
      matrix(0)
    }
    
    
  })

}


shinyApp(ui = ui, server = server)




