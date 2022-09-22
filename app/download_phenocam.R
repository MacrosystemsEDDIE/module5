download_phenocam <- function(url) {
  require(rvest)
  web <- "https://phenocam.sr.unh.edu"
  
  imgsrc <- url %>% 
    httr::GET(config = httr::config(ssl_verifypeer = FALSE)) %>% 
    read_html() %>%
    html_node(xpath = '//*/img') %>%
    html_attr('src')

  destfile <- file.path("www",basename(imgsrc))
  
  # 
  img_url <- paste0(web, imgsrc)
  options(download.file.method="curl", download.file.extra="-k -L") # ignore SSL certificates
  download.file(img_url, destfile = destfile, mode = "wb", quiet = TRUE)
  
  if(file.exists(destfile)) {
    message("Phenocam downloaded!")
  } else {
    "Phenocam download failed!"
  }
  return(destfile)
}

