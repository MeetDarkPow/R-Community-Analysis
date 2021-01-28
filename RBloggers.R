### Counts number of blogs on R Bloggers

library(rvest)

wbpg <- read_html("https://www.r-bloggers.com/blogs-list/")

blog_name <- wbpg %>%
  html_nodes(".entry a") %>%
  html_text()

blog_count <- length(blog_name)

### Count of Blog posts - Monthwise, Yearwise

library(stringr)
library(ggplot2)
library(rvest)

wbpg <- read_html("https://www.r-bloggers.com/blogs-list/")

blog_text <- wbpg %>%
  html_nodes(xpath = "//*[@id='archives-dropdown-3']/option") %>%
  html_text() %>%
  stringr::str_squish()

blog_text <- blog_text[2:length(blog_text)]

# hyperlink extraction
blog_monthwise_hyperlink <- wbpg %>%
  html_nodes(xpath = "//*[@id='archives-dropdown-3']/option") %>%
  html_attr("value")

blog_monthwise_hyperlink <- blog_monthwise_hyperlink[2:length(blog_monthwise_hyperlink)]

# month extraction
month <- gsub("\\d|[[:punct:]]", " ", blog_text) %>%
  stringr::str_squish()

# year extraction
year <- str_extract(blog_text, "(?<=\\s)(.*)(?=\\s)")

# blogs count
Rcount <- as.numeric(gsub('.*\\((\\d+).*', '\\1', blog_text))

date <- paste(month, year, sep = ", ")

# creating dataframe for R Bloggers
Rbloggers_df <- data.frame(
  Hyperlink = blog_monthwise_hyperlink, Date = date, Month = month, Year = year, Blog_Count = Rcount
)

# creating a combined plot using ggplot2
ggplot(data=Rbloggers_df, aes(x=Date, y=Blog_Count)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Blog_Count), vjust=-0.3, size=2.5)+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  labs(title = "R Bloggers Count",
       x = "Date - Month and Year", y = "Number of Blogs Post")
