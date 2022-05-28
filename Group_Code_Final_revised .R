######################################################
###### Created by Team 5
###### MBAN2 HULT2021
###### Subject: Text Mining
###### Version 0.3
###################################################

#installing and loading the mongolite library to download the Airbnb data
#install.packages("mongolite") #need to run this line of code only once and then you can comment out
library(mongolite)
library(jsonlite)

# This is the connection_string. You can get the exact url from your MongoDB cluster screen
#replace the <<user>> with your Mongo user name and <<password>> with the mongo password
#lastly, replace the <<server_name>> with your MongoDB server name
connection_string <- 'mongodb+srv://dpolar96:918o412o@cluster0.prsct.mongodb.net/sample_airbnb?retryWrites=true&w=majority'
airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)

#Here's how you can download all the Airbnb data from Mongo
## keep in mind that this is huge and you need a ton of RAM memory

airbnb_all <- airbnb_collection$find()
#######################################################
#if you know or want to learn MQL (MongoQueryLanguage), that is a JSON syntax, feel free to use the following:::
######################################################
#1 subsetting your data based on a condition:
Name <- airbnb_all$name
Descritpion <- airbnb_all$description
Property_type <- airbnb_all$property_type
Room_type <- airbnb_all$room_type
Guest.number <- airbnb_all$accommodates
Bedrooms <- airbnb_all$bedrooms
Beds <- airbnb_all$beds
Bathrooms <- airbnb_all$bathrooms
Reviews_scores_value <- airbnb_all$review_scores$review_scores_value
airbnb_verified <- airbnb_all$host$host_verifications
Superhost <- airbnb_all$host$host_is_superhost
Host <- airbnb_all$host$host_name
ID <- airbnb_all$host$host_id
Country <- airbnb_all$address$country
City <- airbnb_all$address$market
Cancellation <- airbnb_all$cancellation_policy
Price <- airbnb_all$price
Weekly_price <- airbnb_all$weekly_price
Monthly_price <- airbnb_all$monthly_price

airbnb_work <- cbind(Name, Descritpion, Property_type, Room_type,
                     Guest.number, Bedrooms, Beds, Bathrooms, Reviews_scores_value,
                     Superhost, Host, Country, City, Cancellation, ID, Price, 
                     Weekly_price, Monthly_price)

write.csv(airbnb_work, "C:/Users/polar/Downloads/airbnb.csv")

#Downloading necessary packages 
library(tidytext)
library(tidyverse)
library(tidyr)
library(tidytuesdayR)
library(stringr)
library(textreadr)
library(pdftools)
library(textshape)
library(twitteR)
library(tm)
library(ggplot2)
library(scales)
library(magrittr)
library(dplyr)
library(gutenbergr)
library(Matrix)
library(textdata)
library(igraph)
library(ggraph)
library(widyr)
library(topicmodels)
library(gutenbergr)
library(quanteda)
library(quanteda.textmodels)
library(RColorBrewer)
library(tibble)
library(stringr)

airbnb <- read_csv("/Users/tsztinviviansoo/Desktop/combined project/airbnb.csv")
View(airbnb)

data <- c(airbnb[1],airbnb[3],airbnb[13])
data<- data.frame(data)
data
airbnb<-data.frame(airbnb)
colnames(airbnb)[1] <- "IDme"
colnames(airbnb)[3] <- "text"

#######################################

airbnb_token <- airbnb %>%
  unnest_tokens(word, text)

nrcpositive <- get_sentiments("nrc") %>%
  filter(sentiment == "positive")

mean(airbnb$Price)
summary(airbnb$Price)

#mean price is $279 but Q3 is $280 too which means the data is skewed right- median price $129
#let us see if a higher price has more positive sentiment


high<-airbnb_token %>%
  filter(Price >= 279) %>%        #taking $279 as a reference point allows us to look at the highest sentiment words for the top 25% listing prices
  inner_join(nrcpositive) %>%
  count(word, sort=T)
high

low<- airbnb_token %>%
  filter(Price <= 129) %>%        #taking $129 as a reference point allows us to look at the highest sentiment words for the bottom 25% listing prices
  inner_join(nrcpositive) %>%
  count(word, sort=T)
low

#R <=129                  >=279
#1 building    686      #1 Building          
#2 quiet       678      #2 enjoy      
#3 fully       659      #3 shopping
#4 center      582      #4 fully
#5 full        567      #5 quiet
#6 beautiful   566      #6 spacious
#7 enjoy       564      #7 full
#8 spacious    431      #8 pool
#9 perfect     405      #9 beautiful
#10 clean      379      #10 perfect

#we noticed that for the higher price range, there is an added need for top sentiment, which are shopping and pool 
###############################################

###############Sentiment Analysis###################

### For the sentiment analysis we will be looking at English speaking countries

#Filtering by country - Australia
Australia <- airbnb_token %>%
  filter(Country == "Australia") %>% 
  anti_join(stop_words) %>% 
  count(word, sort=T) 

Australia

# Top 10 words and frequency 
#             word   n
#1       apartment 693
#2            walk 530
#3           beach 476 
#4         bedroom 472
#5          sydney 459 #Listings in Sydney are more than any other city 
#6         kitchen 453
#7           house 391
#8             bed 375
#9            city 353
#10              2 347

#Filtering by country - Australia and prices above average ($279)
Australia_high <- airbnb_token %>%
  filter(Country == "Australia" & Price >= 279) %>% 
  anti_join(stop_words) %>% 
  count(word, sort=T)

Australia_high

afinn_Australia_high <- Australia_high %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_Australia_high <- bind_rows(
  Australia_high%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  Australia_high %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_Australia_high, bing_and_nrc_Australia_high)

## AFINN = 198 
## Bing = 145 (Positive = 186 and Negative = 41)
## NRC = 164 (Positive = 234 and Negative = 70)

# Overall more positive than negative sentiments for Airbnbs above the average price point 

#Top 10 words for Australia_high
#word   n
#1           beach 142 #people are willing to pay more if the Airbnb is closer to the beach 
#2            walk 111
#3            home  99
#4         bedroom  98
#5           house  95
#6               2  82
#7          living  82
#8          sydney  82
#9       apartment  81
#10        kitchen  79

#######

#Filtering by country - Australia and prices below $129
Australia_low <- airbnb_token %>%
  filter(Country == "Australia" & Price <= 129) %>%  
  anti_join(stop_words) %>% 
  count(word, sort=T)

Australia_low

afinn_Australia_low <- Australia_low %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_Australia_low <- bind_rows(
  Australia_low%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  Australia_low %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_Australia_low, bing_and_nrc_Australia_low)

## AFINN = 265
## Bing = 170 (Positive = 245 and Negative = 75)
## NRC = 239 (Positive = 343 and Negative = 104)

# Overall more positive than negative sentiments for Airbnbs below the average price point 
## More positive sentiments in the lower price range than high, suggesting that more people book cheaper Airbnbs 
### Low cost apartments by the beach seem to be enticing for customers in Australia 

#Top 10 words for Australia_low
#             word   n
#1       apartment 281
#2            walk 250
#3          sydney 222
#4         kitchen 219 #Basic amenities are reviewed more in cheaper airbnbs to see if needs aren't compensated for 
#5           house 209
#6         bedroom 189
#7            city 181
#8           beach 180
#9             bed 179
#10       bathroom 168

#######

#Filtering by country - Canada
Canada <- airbnb_token %>%
  filter(Country == "Canada") %>% 
  anti_join(stop_words) %>% 
  count(word, sort=T) 
Canada

#Looking at Top 10 frequent words after ignoring French stop words - de, la, le, du, est, vous and des 
# Top 10 words and frequency 
#             word   n
#1       apartment 471
#2        montreal 400 #Listings in Montreal more than any other city
#3               2 377 
#4         minutes 351
#5     restaurants 333
#6         located 310  
#7           metro 299
#8         kitchen 294
#9            walk 267
#10            bed 257 
             
#Filtering by country - Canada and prices above average ($279)  
Canada_high <- airbnb_token %>%
  filter(Country == "Canada" & Price >= 279)%>%  
  anti_join(stop_words) %>% 
  count(word, sort=T)

Canada_high

afinn_Canada_high <- Canada_high %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_Canada_high <- bind_rows(
  Canada_high%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  Canada_high %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_Canada_high, bing_and_nrc_Canada_high)

## AFINN = 67
## Bing = 54 (Positive = 65 and Negative = 11)
## NRC = 69 (Positive = 80 and Negative = 11)

# Overall more positive than negative sentiments for Airbnbs above the average price point 

#Top 10 words for Canada_high

#word  n
#1        montreal 20
#2        downtown 18
#3               3 16
#4             bed 16
#5         bedroom 16
#6       apartment 15
#7               2 14
#8           house 12
#9         located 12
#10          floor 11

#######

#Filtering by country - Canada and prices prices below $129 
Canada_low <- airbnb_token %>%
  filter(Country == "Canada" & Price <= 129)%>%  
  anti_join(stop_words) %>% 
  count(word, sort=T)

Canada_low

afinn_Canada_low <- Canada_low  %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

bing_and_nrc_Canada_low <- bind_rows(
  Canada_low %>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  Canada_low  %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(afinn_Canada_low, bing_and_nrc_Canada_low)

## AFINN = 261
## Bing = 193 (Positive = 267 and Negative = 74)
## NRC = 266 (Positive = 375 and Negative = 109)

# Overall more positive than negative sentiments for Airbnbs below the average price point 
## More positive sentiments in lower price range than high, suggesting that more people booked cheaper Airbnbs 
### Canada had the lowest score for Airbnbs at the high price range 
#### Low cost apartments in Montreal seem enticing to customers 

#Top 10 words for Canada_low 
#              word   n
#1        apartment 357
#2          minutes 293
#3         montreal 286
#4                2 280
#5      restaurants 268 
#6            metro 258
#7          located 236
#8          kitchen 231    
#9             walk 220
#10         station 207     

#######

#Filtering by country - United States 
United_States <- airbnb_token %>%
  filter(Country == "United States") %>% 
  anti_join(stop_words) %>% 
  count(word, sort=T) 
United_States

# Top 10 words and frequency 
#              word    n
#1          bedroom 1112
#2        apartment 1023
#3          kitchen  978
#4            beach  838
#5              bed  818
#6                2  778
#7          private  764
#8           living  743
#9          located  690
#10            home  617

#Filtering by country - United States and prices above average ($279)  
United_States_high <- airbnb_token %>%
  filter(Country == "United States" & Price >= 279)%>%  
  anti_join(stop_words) %>% 
  count(word, sort=T)

United_States_high 

United_States_afinn_high <- United_States_high %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

United_States_bing_and_nrc_high <- bind_rows(
  United_States_high%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  United_States_high %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(United_States_afinn_high, United_States_bing_and_nrc_high)

## AFINN = 305
## Bing = 200 (Positive = 260 and Negative = 60)
## NRC = 289 (Positive = 382 and Negative = 93)

# Overall more positive than negative sentiments for Airbnbs above the average price point 

#Top 10 words for United_States_high 
#             word   n
#1           beach 211
#2         bedroom 207
#3           ocean 176
#4               2 172
#5          living 145
#6            home 139
#7         kitchen 138
#8         located 127
#9           views 115
#10              3 103

#######

#Filtering by country - United States and prices below $129 
United_States_low <- airbnb_token %>%
  filter(Country == "United States" & Price <= 129)%>%  
  anti_join(stop_words) %>% 
  count(word, sort=T)

United_States_low

United_States_afinn_low <- United_States_low %>%
  inner_join(get_sentiments("afinn"))%>%
  summarise(sentiment=sum(value)) %>%
  mutate(method="AFINN")

United_States_bing_and_nrc_low <- bind_rows(
  United_States_low%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al."),
  United_States_low %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC")) %>%
  count(method,  sentiment) %>%
  spread(sentiment, n, fill=0) %>%
  mutate(sentiment = positive-negative)

bind_rows(United_States_afinn_low, United_States_bing_and_nrc_low )

## AFINN = 346
## Bing = 214 (Positive = 358 and Negative = 144)
## NRC = 343 (Positive = 522 and Negative = 179)

# Overall more positive than negative sentiments for Airbnbs below the average price point 
## More positive sentiments in lower price range than high, suggesting that more people booked cheaper Airbnbs 
### Many seem to opt for low cost bedrooms than entire apartments in the United States 

#Top 10 words for United_States_low 
#word   n
#1        apartment 566
#2          bedroom 469
#3          kitchen 463
#4          private 447 
#5              bed 429
#6         bathroom 355
#7                2 342
#8           living 339
#9             walk 339
#10         located 327

#######

###############BIGRAM###################
#Creating bigram of comments
airbnb_bigrams <- data %>%
  unnest_tokens(bigram, Descritpion, token = "ngrams", n=2) 

airbnb_bigrams 

airbnb_bigrams %>%
  count(bigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words we need to separate each word then remove:

bigrams_separated <- airbnb_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ") 

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#creating the new bigram, "no-stop-words":
bigram_counts <- bigrams_filtered %>%
  count(word1, word2, Country, sort = TRUE)
bigram_counts%>%
  head(25)

bigram_counts <- bigrams_filtered %>%
  count(word1, word2, Country, sort = TRUE)%>%
  head(25)
bigram_counts%>%
  head(25)

bigram_counts_US <- bigrams_filtered %>%
  count(word1, word2, Country, sort = TRUE)%>%
  filter(Country=="United States") %>%
head(25)

bigram_counts_Canada <- bigrams_filtered %>%
  count(word1, word2, Country, sort = TRUE)%>%
  filter(Country=="Canada") %>%
  head(25)

bigram_counts_Australia <- bigrams_filtered %>%
  count(word1, word2, Country, sort = TRUE)%>%
  filter(Country=="Australia") %>%
  head(25)

bigram_counts_Brazil <- bigrams_filtered %>%
  count(word1, word2, Country, sort = TRUE)%>%
  filter(Country=="Brazil") %>%
  head(25)

bigram_counts_Portugal <- bigrams_filtered %>%
  count(word1, word2, Country, sort = TRUE)%>%
  filter(Country=="Portugal") %>%
  head(25)

bigram_counts_HK <- bigrams_filtered %>%
  count(word1, word2, Country, sort = TRUE)%>%
  filter(Country=="Hong Kong") %>%
  head(25)


bigram_counts_Spain <- bigrams_filtered %>%
  count(word1, word2, Country, sort = TRUE)%>%
  filter(Country=="Spain") %>%
  head(25)

bigram_counts_China <- bigrams_filtered %>%
  count(word1, word2, Country, sort = TRUE)%>%
  filter(Country=="China") %>%
  head(25)

bigram_counts_Turkey <- bigrams_filtered %>%
  count(word1, word2, Country, sort = TRUE)%>%
  filter(Country=="Turkey") %>%
  head(25)
###############TRIGRAM###################

airbnb_trigrams <- data %>%
  unnest_tokens(trigram, Descritpion, token = "ngrams", n=3)

airbnb_trigrams 

airbnb_trigrams %>%
  count(trigram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words we need to separate each word then remove:

trigrams_separated <- airbnb_trigrams %>%
  separate(trigram, c("word1", "word2","word3"), sep = " ") 

trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

#creating the new trigram, "no-stop-words":
trigram_counts <- trigrams_filtered %>%
  count(word1, word2,word3, Country, sort = TRUE)
trigram_counts%>%
  head(25)

###############QUADROGRAM###################

airbnb_quadrograms <- data %>%
  unnest_tokens(quadrogram, Descritpion, token = "ngrams", n=4)

airbnb_quadrograms 

airbnb_quadrograms %>%
  count(quadrogram, sort = TRUE) #this has many stop words, need to remove them 

#to remove stop words we need to separate each word then remove:

quadrograms_separated <- airbnb_quadrograms %>%
  separate(quadrogram, c("word1", "word2","word3","word4"), sep = " ") 

quadrograms_filtered <- quadrograms_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word) %>%
  filter(!word4 %in% stop_words$word)

#creating the new quadrogram, "no-stop-words":
quadrogram_counts <- quadrograms_filtered %>%
  count(word1, word2,word3,word4, Country, sort = TRUE)
quadrogram_counts%>%
  head(25)

#highest quadrogram count is n=34 / no high business insight / not making more sense






### creating a tidy format for US  apartments
usa <- airbnb %>%
  filter(Country== "United States" & Property_type=="Apartment")

tidy_usa <- usa %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_usa)

### creating a tidy format for Spain apartments
spain <- airbnb %>%
  filter(Country== "Spain" & Property_type=="Apartment")

tidy_spain <- spain %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_spain)

### creating a tidy format for Canada apartments
canada <- airbnb %>%
  filter(Country== "Canada" & Property_type=="Apartment")

tidy_canada <- canada %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words)
print(tidy_canada)

#############################################
####We want to combine all the datasets and do frequencies 
#############################################
library(tidyr)
library(stringr)
frequency <- bind_rows(mutate(tidy_usa, author="United States"),
                       mutate(tidy_canada, author= "Canada"),
                       mutate(tidy_spain, author="Spain")
)%>%#closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(author, word) %>%
  group_by(author) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(author, proportion) %>%
  gather(author, proportion, `Canada`, `Spain`)

#let's plot the correlograms:
library(scales)
library(ggplot2)
ggplot(frequency, aes(x=proportion, y=`United States`, 
                      color = abs(`United States`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~author, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "United States", x=NULL)

##########################################
##doing the cor.test() ################
##########################################

cor.test(data=frequency[frequency$author == "Canada",],
         ~proportion + `United States`)

cor.test(data=frequency[frequency$author == "Spain",],
         ~proportion + `United States`)
