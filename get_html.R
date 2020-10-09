require(rvest)
get_html <- function(site_id) {
  url <- "https://www.neonscience.org/field-sites/field-sites-map/"
  myurl <- read_html(paste0(url, site_id))
  
  body_nodes <- myurl %>% 
    html_nodes("article") %>%
    html_children() %>%
    html_nodes("fieldset")
  write_html(body_nodes[1], "data/site.html")
  if(!file.exists("data.html")) {
    return(htmltools::htmlTemplate("data/site.html"))
  } else {
    warning("HTML not downloaded")
  }
}



