brief_all_genus_urls <- read.csv("./data/brief_all_genus.csv")$URL

while (TRUE) {
  tryCatch({
    all_genus_info <- detail_graber(brief_all_genus_urls, output_file = "./data/all_genus_info.csv")
  }, warning = function(w){
    cat("warning")
    Sys.sleep(10)
  }, error = function(e){
    cat("error")
    Sys.sleep(10)
  })
}
