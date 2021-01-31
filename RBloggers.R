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
month_names <- gsub("\\d|[[:punct:]]", " ", blog_text) %>%
  stringr::str_squish()

# year extraction
year_number <- str_extract(blog_text, "(?<=\\s)(.*)(?=\\s)")

# blogs count
Rcount <- as.numeric(gsub('.*\\((\\d+).*', '\\1', blog_text))

date <- paste(month_names, year_number, sep = ", ")

# creating data-frame for R Bloggers
Rbloggers_df <- data.frame(
  Hyperlink = blog_monthwise_hyperlink, Date = date, Month = month_names, Year = year_number, Blog_Count = Rcount
)
Rbloggers_df$Date <- factor(Rbloggers_df$Date, levels = unique(Rbloggers_df$Date))

# creating a combined bar-plot using ggplot2
ggplot(data=Rbloggers_df, aes(x=Date, y=Blog_Count)) +
  geom_bar(stat="identity", fill="steelblue")+
  geom_text(aes(label=Blog_Count), vjust=-0.3, size=2.5)+
  theme_minimal()+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  labs(title = "R Bloggers Count",
       x = "Date - Month and Year", y = "Number of Blogs Post")

# creating a line chart
rev_date <- Rbloggers_df$Date[181:1]
rev_blog_count <- Rbloggers_df$Blog_Count[181:1]
rev_blog_df <- data.frame(Dt=rev_date, bc=rev_blog_count)
rev_blog_df$Dt <- factor(rev_blog_df$Dt, levels = unique(rev_blog_df$Dt))

ggplot(data=rev_blog_df, aes(x=Dt, y=bc, group=1)) +
  geom_line(color="red")+
  geom_text(aes(label=bc), vjust=-1.0, size=2.5)+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  scale_x_discrete(breaks = rev_blog_df$Dt[c(T,F,F)]) +
  geom_point()

# yearly bar-plot
yearly_blog_count <- tapply(Rbloggers_df$Blog_Count, Rbloggers_df$Year, FUN=sum)
yearly_df <- data.frame(Year=names(yearly_blog_count), Count=yearly_blog_count)

ggplot(data=yearly_df, aes(x=Year, y=Count)) +
  geom_bar(stat="identity", fill="darkred")+
  geom_text(aes(label=Count), vjust=-0.3, size=3.5)+
  theme_minimal()+
  labs(title = "R Bloggers Yearly Count",
       x = "Year", y = "Number of Blogs Posted")

# monthly bar-plot
x <- factor(Rbloggers_df$Month, levels = month.name)
monthly_blog_count <- tapply(Rbloggers_df$Blog_Count, x, FUN=sum)
monthly_df <- data.frame(Month=names(monthly_blog_count), Count=monthly_blog_count)
monthly_df$Month <- factor(monthly_df$Month, levels = unique(monthly_df$Month))

ggplot(data=monthly_df, aes(x=Month, y=Count)) +
  geom_bar(stat="identity", fill="darkgreen")+
  geom_text(aes(label=Count), vjust=-0.3, size=3.5)+
  theme_minimal()+
  labs(title = "R Bloggers Monthly Count",
       x = "Month", y = "Number of Blogs Posted")
