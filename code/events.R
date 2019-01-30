library(rvest)
library(stringr)



baseurl <- 'https://www.nycgovparks.org'

pages_add <- '/events/p'

page_count <- as.character(seq(1,3))

park_events <- data.frame(event = character(), location = character(), month = character(), 
                          day = integer(), start_time = character(), end_time = character(), desc = character())

data.frame(event = character(), park = character(), location = character(),
           start_time = character(),end_time = character(), accessible = character())


for (i in 1:length(page_count)){
  page = read_html(paste(baseurl,pages_add,page_count[[i]], sep = ''))
  urls <- page %>% 
    html_nodes(".event-title a") %>% 
    html_attr('href')
  month <- page %>%
    html_nodes('.cal_month') %>%
    html_text()
  day <- page %>%
    html_nodes('.cal_day') %>%
    html_text() %>%
    as.numeric()
  location <- page %>%
    html_nodes('.location') %>%
    html_text()
  categories <- page %>%
    html_nodes("#events_leftcol span a , strong:nth-child(6)") %>%
    html_text() %>%
    paste(., collapse = ", ") %>%
    strsplit(., 'Category: ')%>%
    unlist(.)
  categories <- categories[categories != '']
  start_time <- page %>%
    html_nodes('#events_leftcol strong:nth-child(1)') %>%
    html_text()
  end_time <- page %>%
    html_nodes('strong:nth-child(2)') %>%
    html_text()
  desc <- page %>%
    html_nodes('.description') %>%
    html_text()
  event <- page %>%
    html_nodes('.event-title a') %>%
    html_text()
  
  new_events_row <- data.frame(event, location, month, day, start_time, end_time, desc)
  park_events <- rbind(park_events, new_events_row)
}


df

df <- 0
df <- data.frame(event = character(), park = character(), location = character(),
                 start_time = character(),end_time = character(), accessible = character())
for (i in 1:length(urls)){
  webpage <- read_html(paste(baseurl,urls[[i]], sep = ''))
  event_org <- webpage %>%
    html_nodes('p:nth-child(9)') %>%
    html_text()
  park <- webpage %>%
    html_nodes('.location div a') %>%
    html_text()
  accessible <- webpage %>%
    html_nodes('#single_event_body img') %>%
    html_attr('src')
  if (!is.null(accessible)){
    accessible <- 'yes'
  } else { accessible <- 'no'}
  newrow <- data.frame(event, park, location,start_time,end_time, accessible)
  df <- rbind(df, newrow)
  #print(c(event,park, location, start_time, end_time, event_org, accessible))
  i <- i+1
  }



df




tempo <- read_html('https://www.nycgovparks.org/events/2019/01/09/6th-annual-whimsical-winter-wonder-exhibition')




df <- data.frame(event, park, location, event_org, accessible, phone, email, start_time, end_time, accessible)
df

length(free)

for (i in 1:length(free)) {
  print(free[[i]])}


i <- 13
for (i in 1:length(categories)) {
  if (categories[[i]] == "Category: "){
    if (categories[[i+1]] == "Free!") {
      print(categories[[i]])
    }
  } else if (categories[[i]] == "Category: "){
    if (categories[[i+1]] == "Category: ") {
      print(categories[[i]])
    }
  } else if (categories[[i]] != "Category: ") {
      print(categories[[i]])
  } else {
    print("OH NO")
  }
    i=i+1
}
