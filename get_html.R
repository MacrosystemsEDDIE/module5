require(rvest)
get_html <- function(site_id) {
  url <- "https://www.neonscience.org/field-sites/"
  myurl <- read_html(paste0(url, site_id))
  
  tst <- myurl %>% 
    html_nodes(".field-site__withSidebar-content") 
  tst2 <- html_children(tst[2])[2]
  write_html(tst2, "data/site.html")

  # body_nodes <- myurl %>% 
  #   html_nodes("field-site__twocol-wrapper") %>%
  #   html_children() #%>%
  #   # html_nodes("fieldset")
  # write_html(body_nodes[1], "data/site.html")
  if(!file.exists("data.html")) {
    return(htmltools::htmlTemplate("data/site.html"))
  } else {
    warning("HTML not downloaded")
  }
}
