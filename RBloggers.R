### Counts number of blogs on R Bloggers

library(rvest)

wbpg <- read_html("https://www.r-bloggers.com/blogs-list/")

blog_name <- wbpg %>%
  html_nodes(".entry a") %>%
  html_text()

blog_count <- length(blog_name)