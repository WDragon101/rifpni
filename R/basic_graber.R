#' basic_graber returns the information of each items according to retrieved result.
#'
#' @param basic_url should be URL path returned by basic methods.
#'
#' @return A dataframe with detailed information of result.
#' @export
#' @examples
#' # df <- basic_graber(basic_supragenus("cla",searchOnlyByFirstSymbols = "on"))
#' @import stringr
#' @import cli
#' @import rvest
#' @import dplyr
basic_graber <- function(basic_url) {
  f <- read_html(basic_url)

  pages_url <- c()
  if (!is.na(html_node(f, "a.paging_toEnd"))) {
    end_page <- as.numeric(f %>% html_node("a.paging_toEnd") %>% html_attr("title"))
    pages_url <- paste0(basic_url, paste0("page=", c(1:end_page)))
    }
  cli_alert_success(paste0("There are ",length(pages_url)," pages found."))

  items_df <- data.frame()

  cli_progress_bar("Collecting pages: ", total = length(pages_url), clear = FALSE,
                   format = "{pb_name} {pb_bar} {pb_current}/{pb_total} | ETA: {pb_eta}",
                   format_done = "Collected {pb_total} pages from IFPNI. Elapsed Time: {pb_elapsed_clock}")
  items_url <- c()
  for (l in c(1:length(pages_url))) {
    h <- read_html(pages_url[l])
    i <- h %>% html_nodes("h1.lead.list-group-item-heading a") %>% html_attr("href")
    n <- h %>% html_nodes("span.nameInList") %>% html_text2()
    n <- sub(".*? ","", n )
    items_url <- append(items_url, i)
    cli_progress_update()
    Sys.sleep(1/3)
  }
  items_url <- paste0(c("http://ifpni.org"), items_url)
  cli_progress_done()
  cli_alert_success(paste0("There are ",length(items_url), " items found."))

  cli_progress_bar("Collecting items:",
                   total = length(items_url),
                   clear = FALSE,
                   format = "{pb_name} {pb_bar} {pb_current}/{pb_total} | ETA: {pb_eta}",
                   format_done = "Collected {pb_total} items from IFPNI. Elapsed Time: {pb_elapsed_clock}"
                   )
  temp_df <- data.frame()
  for (i in c(1:length(items_url))){
    item <- items_url[i]
    dl <- read_html(item) %>% html_node("dl.dl-horizontal")
    dt <- dl %>% html_nodes("dt") %>% html_text2()
    dd <- dl %>% html_nodes("dd") %>% html_text2()
    dd <- gsub("\r", "", dd)
    names(dd) <- dt
    df <- data.frame(as.list(dd))
    df$ID <- strsplit(items_url[i],"=")[[1]][-1]
    if (i>1){
      suppressMessages(
        temp_df <- full_join(temp_df, df))
    }else{
      temp_df <- df}
    cli_progress_update()
    Sys.sleep(1/3)
  }
  cli_progress_done()
  cli_rule("Done")
  return(temp_df)
}

#' Get brief information return by basic_url, a dataframe contains "Name" and "URL".
#'
#' @param basic_url created by basic methods, such as basic_supragenus.
#'
#' @return A dataframe contains "Name" and "URL" fields.
#' @export
#' @examples
#' # Build initial URL
#' cla_basic_url <- basic_supragenus("cla", searchOnlyByFirstSymbols = "on")
#' # Get the result entry based on the initial URL and generate a dataframe containing Name and URL.
#' cla_brief_information <- brief_information(cla_basic_url)
#' # View entries
#' cla_brief_information$Name
#' # [1] "Cladophoraceae"       "Cladophorales"        "Cladoxylaceae"        "Cladoxylaceae"        "Cladoxylales"
#' # [6] "Cladoxyleae"          "Cladoxylidae"         "Cladoxylophyta"       "Cladoxylopsida"       "Claracrustoideae"
#' # [11] "Clavaphysoporelleae"  "Clavaphysoporellinae" "Clavatoraceae"        "Clavatorales"         "Clavatorales"
#' # [16] "Clavatoreae"          "Clavatoritoideae"     "Clavatoroideae"
#'
#' # Select entries and build a vector of URLs. Note: there are two "Clavatorales".
#' slt_cla_URLs <- cla_brief_information$URL[cla_brief_information$Name%in%c("Cladophorales", "Clavatorales", "Clavatoroideae")]
#' slt_cla_URLs
#' # [1] "http://ifpni.org/supragenus.htm?id=ABBBCF19-1A47-D7EE-18B4-88EB8B277D84"
#' # [2] "http://ifpni.org/supragenus.htm?id=51571A09-482E-4A6D-8EB8-9B753EB1E397"
#' # [3] "http://ifpni.org/supragenus.htm?id=8EDFC4C2-38B3-4E5C-AC5E-202AE551D872"
#' # [4] "http://ifpni.org/supragenus.htm?id=E151FA37-059F-497D-9565-97D2427CC8D4"
#'
#' # Pass slt_cla_URLs to .detail_graber.
#' detail_slt_cla <- detail_graber(slt_cla_URLs)
#' str(detail_slt_cla)
#'
#' #'data.frame':	4 obs. of  14 variables:
#' #$ Name             : chr  "Cladophorales" "Clavatorales" "Clavatorales" "Clavatoroideae"
#' #$ Rank             : chr  "Order" "Order" "Order" "Subfamily"
#' #$ Non.fossil       : chr  "Yes" NA NA NA
#' #$ Original.spelling: chr  "Cladophorinae" NA "Clavaterales" NA
#' #$ Authors..Name.   : chr  "Prantl K. A. E. " NA NA NA
#' #$ Page.number      : chr  "Lehrb. Bot. ed. 5: 131. 1884" "47" "177" "880"
#' #$ Parent.Taxon     : chr  "[Class] Siphonocladophyceae" "[Class] Charophyceae" "[Class] Charophyceae" "[Family] Clavatoraceae"
#' #$ Type             : chr  "[Genus] Cladophora" "[Genus] Clavator" "[Genus] Clavator" "[Genus] Clavator"
#' #$ Authors..Pub..   : chr  NA "Maslov V. P. " "Pojarkov B. V. " "Grambast L. "
#' #$ Publication      : chr  NA "Vvedenie v izuchenie iskopaemykh kharovykh vodoroslej [1963/5]" "Devonskie kharofity Tjan'-Shanja [1966/5]" "La symétrie de l'utricule chez les Clavatoracées et sa signification phylogénétique [1969/9]"
#' #$ Journal          : chr  NA "Trudy Geologicheskogo Instituta Akademii Nauk SSSR" "Trudy Geologicheskogo Instituta Akademii Nauk SSSR" "Compte rendu hebdomadaire des séances de l’Académie des Sciences, Paris"
#' #$ Issue            : chr  NA "82" "143" NA
#' #$ Year             : chr  NA "1963" "1966" "1969"
#' #$ Volume           : chr  NA NA NA "269"
#'
brief_information <- function(basic_url){
  pages_url <- c(basic_url)
  f <- read_html(basic_url)

  if (!is.na(html_node(f, "a.paging_toEnd"))) {
    end_page <-
      as.numeric(f %>% html_node("a.paging_toEnd") %>% html_attr("title"))
    pages_url <-
      paste0(basic_url, paste0("page=", c(1:end_page)))
  }

  items_url <- c()
  items_name <- c()
  for (u in pages_url) {
    h <- read_html(u)
    i <- h %>% html_nodes("h1.lead.list-group-item-heading a") %>% html_attr("href")
    n <- h %>% html_nodes("span.nameInList") %>% html_text2()
    n <- sub(".*? ","", n )

    items_url <- append(items_url, i)
    items_name <- append(items_name, n)
  }
  items_url <- paste0(c("http://ifpni.org"), items_url)

  return(data.frame("Name" = items_name, "URL" = items_url))
}

#' Get detail information of selected items' URLs that should be prepared by brief_information.
#'
#' @param items_url Selected items' URLs.
#'
#' @return A dataframe contains detail information
#' @export
#'
#' @examples ###no example.
#'
detail_graber <- function(items_url){
  temp_df <- data.frame()
  for (i in c(1:length(items_url))){
    item <- items_url[i]
    dl <- read_html(item) %>% html_node("dl.dl-horizontal")
    dt <- dl %>% html_nodes("dt") %>% html_text2()
    dd <- dl %>% html_nodes("dd") %>% html_text2()
    dd <- gsub("\r", "", dd)
    names(dd) <- dt
    df <- data.frame(as.list(dd))
    if (i>1){
      temp_df <- full_join(temp_df, df)
    }else{temp_df <- df}
  }
  return(temp_df)
}
