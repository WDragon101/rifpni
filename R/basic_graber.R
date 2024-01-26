#' basic_graber returns the information of each items according to retrieved result.
#'
#' @param basic_url should be URL path returned by basic methods.
#'
#' @return A dataframe with detailed information of result.
#' @export
#' @examples
#' # supra_cla <- basic_graber(basic_supragenus("cla",searchOnlyByFirstSymbols = "on"))
#' # genus_cla <- basic_graber(basic_genus("cla",searchOnlyByFirstSymbols = "on"))
#' @import stringr
#' @import cli
#' @import rvest
#' @import dplyr
basic_graber <- function(basic_url) {
  f <- read_html(basic_url, timeout = 10)

  pages_url <- c()
  if (!is.na(html_node(f, "a.paging_toEnd"))) {
    end_page <-
      as.numeric(f %>% html_node("a.paging_toEnd") %>% html_attr("title"))
    pages_url <- paste0(basic_url, paste0("page=", c(1:end_page)))
  }
  cli_alert_success(paste0("There are ", length(pages_url), " pages found."))

  items_df <- data.frame()

  cli_progress_bar(
    "Collecting pages: ",
    total = length(pages_url),
    clear = FALSE,
    format = "{pb_name} {pb_bar} {pb_current}/{pb_total} | ETA: {pb_eta}",
    format_done = "Collected {pb_total} pages from IFPNI. Elapsed Time: {pb_elapsed_clock}"
  )
  items_url <- c()
  for (l in c(1:length(pages_url))) {
    h <- read_html(pages_url[l], timeout = 10)
    i <-
      h %>% html_nodes("h1.lead.list-group-item-heading a") %>% html_attr("href")
    n <- h %>% html_nodes("span.nameInList") %>% html_text2()
    n <- sub(".*? ", "", n)
    items_url <- append(items_url, i)
    cli_progress_update()
    Sys.sleep(1 / 3)
  }
  items_url <- paste0(c("http://ifpni.org"), items_url)
  cli_progress_done()
  cli_alert_success(paste0("There are ", length(items_url), " items found."))

  cli_progress_bar(
    "Collecting items:",
    total = length(items_url),
    clear = FALSE,
    format = "{pb_name} {pb_bar} {pb_current}/{pb_total} | ETA: {pb_eta}",
    format_done = "Collected {pb_total} items from IFPNI. Elapsed Time: {pb_elapsed_clock}"
  )
  temp_df <- data.frame()
  for (i in c(1:length(items_url))) {
    item <- items_url[i]
    dl <-
      read_html(item, timeout = 10) %>% html_node("dl.dl-horizontal")
    dt <- dl %>% html_nodes("dt") %>% html_text2()
    dd <- dl %>% html_nodes("dd") %>% html_text2()
    dd <- gsub("\r", "", dd)
    names(dd) <- dt
    df <- data.frame(as.list(dd))
    df$ID <- strsplit(items_url[i], "=")[[1]][-1]
    if (i > 1) {
      suppressMessages(temp_df <- full_join(temp_df, df))
    } else{
      temp_df <- df
    }
    cli_progress_update()
    Sys.sleep(1 / 3)
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
#'

brief_information <- function(basic_url, output_file = "") {
  f <- read_html(basic_url, timeout = 10)

  pages_url <- c()
  if (!is.na(html_node(f, "a.paging_toEnd"))) {
    end_page <-
      as.numeric(f %>% html_node("a.paging_toEnd") %>% html_attr("title"))
    pages_url <- paste0(basic_url, paste0("page=", c(1:end_page)))
  }
  cli_alert_success(paste0("There are ", length(pages_url), " pages found."))
  cli_progress_bar(
    "Collecting pages: ",
    total = length(pages_url),
    clear = FALSE,
    format = "{pb_name} {pb_bar} {pb_current}/{pb_total} | ETA: {pb_eta}",
    format_done = "Collected {pb_total} pages from IFPNI. Elapsed Time: {pb_elapsed_clock}"
  )

  temp_df <- data.frame()
  all_urls <- c()
  all_names <- c()
  exist_items <- c()
  done_page <- 0
  if (!file.exists(output_file)) {
    file.create(output_file)
  }
  if (file.info(output_file)$size > 0) {
    temp_df <- read.csv(output_file)
    temp_df <-
      as.data.frame(lapply(temp_df, as.character), stringAsFactors = FALSE)
    exist_items <- temp_df$URL
    all_urls <- temp_df$URL
    all_names <- temp_df$Name
    done_page <- trunc(length(exist_items) / 10)
  }


  for (i in 1:length(pages_url)) {
    if (i > done_page) {
      h <- read_html(pages_url[i], timeout = 10)
      hrefs <-
        h %>% html_nodes("h1.lead.list-group-item-heading a") %>% html_attr("href")
      page_names <-
        h %>% html_nodes("span.nameInList") %>% html_text2()
      # page_names <- sub(".*? ","", page_names)

      for (j in 1:length(hrefs)) {
        item_url <- paste0(c("http://ifpni.org"), hrefs[j])

        if (!item_url %in% exist_items) {
          all_urls <- append(all_urls, item_url)
          all_names <- append(all_names, page_names[j])
          all_df <- data.frame("Name" = all_names, "URL" = all_urls)
          write.csv(all_df,
                    output_file,
                    row.names = FALSE)
        }
      }
      Sys.sleep(1 / 3)
    }
    cli_progress_update()
  }
  cli_progress_done()
  cli_alert_success(paste0("There are ", length(pages_url), " pages collected."))

}

#' Get detail information of selected items' URLs that should be prepared by brief_information.
#'
#' @param items_url Selected items' URLs.
#'
#' @return A dataframe contains detail information
#' @export
#'
#' @examples
#' brief_all_genus_urls <- read.csv("./data/brief_all_genus.csv")$URL
#'
#' while (TRUE) {
#' tryCatch({
#' all_genus_info <- detail_graber(brief_all_genus_urls, output_file = "./data/all_genus_info.csv")
#' }, warning = function(w){
#' cat("warning")
#' Sys.sleep(100)
#' }, error = function(e){
#' cat("error")
#' Sys.sleep(100)
#' })
#' }

detail_graber <- function(items_url, output_file = "") {
  temp_df <- data.frame()
  exist_ids <- c()
  if (!file.exists(output_file)) {
    file.create(output_file)
  }
  if (file.info(output_file)$size > 0) {
    temp_df <- read.csv(output_file)
    temp_df <-
      as.data.frame(lapply(temp_df, as.character), stringAsFactors = FALSE)
    exist_ids <- temp_df$ID
  }

  cli_progress_bar(
    "Collecting items:",
    total = length(items_url),
    clear = FALSE,
    format = "{pb_name} {pb_bar} {pb_current}/{pb_total} | ETA: {pb_eta}",
    format_done = "Collected {pb_total} items from IFPNI. Elapsed Time: {pb_elapsed_clock}"
  )
  for (i in c(1:length(items_url))) {
    item <- items_url[i]
    item_id <- strsplit(as.character(items_url[i]), "=")[[1]][-1]
    if (!item_id %in% exist_ids) {
      dl <-
        read_html(item, timeout = 10) %>% html_node("dl.dl-horizontal")
      dt <- dl %>% html_nodes("dt") %>% html_text2()
      dd <- dl %>% html_nodes("dd") %>% html_text2()
      dd <- gsub("\r", "", dd)
      names(dd) <- dt
      df <- data.frame(as.list(dd))
      df$ID <- strsplit(items_url[i], "=")[[1]][-1]
      if (i > 1) {
        suppressMessages(temp_df <- full_join(temp_df, df))
      } else{
        temp_df <- df
      }
      write.csv(temp_df, output_file, row.names = FALSE)
      Sys.sleep(1 / 3)
    }
    cli_progress_update()
  }
  cli_progress_done()
  cli_rule("Done")
  return(temp_df)
}
