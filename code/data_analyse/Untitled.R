library(tidyverse)

###get pop_people(people_list)
movie1 = read.csv('./data/get_people/1.csv', fileEncoding = 'utf-8')
movie2 = read.csv('./data/get_people/2.csv', fileEncoding = 'utf-8')
movie3 = read.csv('./data/get_people/3.csv', fileEncoding = 'utf-8')

pop_movie1 = filter(movie1, user_movie>1000, user_movie < 3000, user_pop > 10000)[,-1]
pop_movie2 = filter(movie2, user_movie>1000, user_movie < 3000, user_pop > 10000)[,-1]
pop_movie3 = filter(movie3, user_movie>1000 ,user_movie < 3000, user_pop > 10000)[,-1]

pop_user = pop_movie1 %>% rbind(pop_movie2) %>% rbind(pop_movie3) 
names(table(as.character(pop_user$user_id)))

###people_inspect mining

###deriving people_all
setwd('./data/people_inspect')
people_all = data_frame()
for(i in list.files('./')){
  people_inspect = read.csv(i, fileEncoding = 'utf-8')
  people_all = rbind(people_all, people_inspect)
}
hist(table(people_all$movie_id))

###get movie_list
movie_list = names(table(people_all$movie_id)[table(people_all$movie_id) >= 3])
movie_list
