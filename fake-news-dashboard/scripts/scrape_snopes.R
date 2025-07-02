# scrape_snopes.R
library(rvest)
library(tidyverse)

base_url <- "https://www.snopes.com/tag/philippines/ "

scrape_snopes_articles <- function() {
  page <- read_html(base_url, httr::add_headers(`User-Agent` = "Mozilla/5.0"))
  
  links <- page %>%
    html_nodes(".post-list__item .entry-title a") %>%
    html_attr("href")
  
  links <- head(links, 10)
  
  articles <- tibble(
    title = character(),
    claim = character(),
    rating = character(),
    summary = character(),
    url = character()
  )
  
  for (link in links) {
    Sys.sleep(2)
    full_url <- link
    
    tryCatch({
      article_page <- read_html(full_url, httr::add_headers(`User-Agent` = "Mozilla/5.0"))
      
      title <- article_page %>% html_node("h1") %>% html_text()
      claim <- article_page %>% html_node(".claim") %>% html_text()
      rating <- article_page %>% html_node(".rating") %>% html_text()
      summary_text <- article_page %>% html_node(".article-text") %>% html_text()
      
      articles <<- add_row(articles,
                           title = title,
                           claim = claim,
                           rating = rating,
                           summary = substr(summary_text, 1, 1500),
                           url = full_url
      )
    }, error = function(e) {
      message("Error scraping ", full_url, ": ", e$message)
    })
  }
  
  return(articles)
}

# Run scraper
live_news_data <- scrape_snopes_articles()

# Save raw scraped data
write.csv(live_news_data, "data/live_scraped_news.csv", row.names = FALSE)

cat("✅ Scraped and saved", nrow(live_news_data), "articles.\n")