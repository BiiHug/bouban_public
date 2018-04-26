###Load library
library(shiny)
library(shinyjs)
library(tidyverse)
library(plyr)
library(data.table)

###Data and environment preparation
movie_all = read.table("./data/movie_all.rdata")
header1 = read.table("./data/movie_cor.rdata", header = TRUE, nrow = 1)
movie_cor = fread("./data/movie_cor.rdata", skip=1, header=FALSE)[,-1]
setnames(movie_cor, colnames(header1))
colnames(movie_cor) = gsub("X(\\d+)","\\1",colnames(movie_cor)) 
rownames(movie_cor) = colnames(movie_cor)
header2 = read.table("./data/movie_cor2.rdata", header = TRUE, nrow = 1)
movie_cor2 = fread("./data/movie_cor2.rdata", skip=1, header=FALSE)[,-1]
setnames(movie_cor2, colnames(header2))
colnames(movie_cor2) = gsub("X(\\d+)","\\1",colnames(movie_cor2)) 
rownames(movie_cor2) = colnames(movie_cor2)
people_all = fread("./data/people_all.csv")
settle_cluster = read_rds("./data/settle_cluster.rdata")
movie_cluster = read.table("./data/movie_cluster.rdata")
docu =data.frame(score=c(1), id = c(1)) ###Initialization the user-docu-vector!!!
write.table(docu,"./data/docu.rdata") ###Initialization the user-docu-vector!!!

create_movies = function(){
  docu2 <- read.table("/Applications/学习/UWM/new479/douban_public/douban_public/shiny_2/data/docu.rdata")
  docu3 = docu2[-1,]
  
  scored = docu3$score
  scored_id = docu3$id
  
  movie_recon <- function(movie_cor){
    n = length(movie_cor[,1])
    names_in = as.numeric(colnames(movie_cor)[colnames(movie_cor) %in% scored_id])
    score_frame = data.frame(id = names_in) %>% left_join(docu3, by = "id")
    score_vector = numeric(n)
    score_vector[which(colnames(movie_cor) %in% scored_id)] = score_frame$score
    Movie_1 = (movie_cor - diag(rep(1,n))) %*% t(t(score_vector))
    Location = as.numeric(colnames(movie_cor) %in% scored_id)
    Movie_2 = (movie_cor - diag(rep(1,n))) %*% t(t(Location))
    Movie = Movie_1/Movie_2
    
    Movie_pool = Movie[!(rownames(Movie) %in% scored_id),]
    recon_id = as.numeric(names(Movie_pool))[tail(order(Movie_pool), n=3)]
    return(recon_id)
  }
  
  return(c(movie_recon(movie_cor), movie_recon(movie_cor2)))
}

function(input, output, session) {
  observeEvent(input$refresh, {
    input$save_inputs =0
    docu2 <- read.table("./data/docu.rdata")
      if (length(docu2) <=10 ){
        id = sample(c(settle_cluster[[1]],settle_cluster[[2]],settle_cluster[[3]]),1)
      }else if((length(docu2)-10)%%6 == 1){
        id_list = create_movies()
        id = sample(id_list,1)
        id_list = id_list[id_list!=id]
      }else{
        id = sample(id_list,1)
        id_list = id_list[id_list!=id]
      }
      poster = movie_all[movie_all$id==id, 'poster']
      output$movie_info_main <- renderUI({
        type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
        type = type[!is.na(type)]
        type = paste(type,collapse = "/")
        score = movie_all[movie_all$id==id, 'score']
        pop = movie_all[movie_all$id==id, 'pop']
        rate = paste(score," (",pop,"人评价)",sep = "")
        director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
        director = director[!is.na(director)]
        director = paste(director,collapse = "/")
        director = paste("导演:",director)
        actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
        actor = actor[!is.na(actor)]
        actor = paste(actor,collapse = "/")
        actor = paste("演员",actor)
        tags$div(
          img(src = poster),
          h3(movie_all[movie_all$id==id, 'English_name']),
          h4(movie_all[movie_all$id==id, 'name']),
          h4(rate),
          h5(type),
          h6(director),
          h6(actor)
        )
      })
      observeEvent(input$save_inputs, {
        user_rate = as.numeric(gsub('(\\d).+?','\\1',input$user_score))
        print(user_rate)
        docu2 = rbind(docu2, data.frame(score=user_rate, id = id))
        write.table(docu2, "./data/docu.rdata")
      })
      user_rate <- eventReactive(input$save_inputs, as.numeric(gsub('(\\d).+?','\\1',input$user_score)))
      output$real_rate <- renderUI(
        if(user_rate()<=2){
          tags$h3(user_rate())
        }
      )
      
      ###Comment_block
      observeEvent(input$side, {
        comment = people_all[people_all$movie_id == id,]
        comment = comment[!is.na(comment$movie_score),]
        comment$movie_comment = as.character(comment$movie_comment)
        comment = comment[comment$movie_comment!="",]
        output$movie_inspect5<-renderTable({
          comment[comment$movie_score==5,c('movie_comment','movie_time','user_id')]
        })
        output$movie_inspect4<-renderTable({
          comment[comment$movie_score==4,c('movie_comment','movie_time','user_id')]
        })
        output$movie_inspect3<-renderTable({
          comment[comment$movie_score==3,c('movie_comment','movie_time','user_id')]
        })
        output$movie_inspect2<-renderTable({
          comment[comment$movie_score==2,c('movie_comment','movie_time','user_id')]
        })
        output$movie_inspect1<-renderTable({
          comment[comment$movie_score==1,c('movie_comment','movie_time','user_id')]
        })
      })
      
      ###Dist_block
      observeEvent(input$side, {
          x = 1:5
          y = select(movie_all[movie_all$id==id,],starts_with('rating'))
          y = sapply(y, function(x) as.numeric(sub("%", "", x))/100)
          y = rev(y)
          a = data.frame(x = x, y = y)
          output$user_dist <- renderPlot({
              ggplot(a, mapping = aes(x = x, y = y)) + geom_smooth(col='royalblue')+
                theme_hc() + geom_point(x=user_rate(),y=0, col='tomato',
                                        alpha = 0.25, cex=5)
          })
      })
      
      ###Movie_might_block
      observeEvent(input$side, {
        ###BY_movie_block
        poster_list=c()
        cor_list = movie_cor[id==rownames(movie_cor),]
        movie_might = names(cor_list)[tail(order(cor_list))][1:5]
        
        output$movie_might1 <- renderUI({
          tags$img(src = movie_all[movie_all$id==movie_might[1],'poster'])
        })
        observeEvent(input$movie_select1,{
          id = movie_might[1]
          output$movie_might_info1 <- renderUI({
            type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
            type = type[!is.na(type)]
            type = paste(type,collapse = "/")
            score = movie_all[movie_all$id==id, 'score']
            pop = movie_all[movie_all$id==id, 'pop']
            rate = paste(score," (",pop,"人评价)",sep = "")
            director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
            director = director[!is.na(director)]
            director = paste(director,collapse = "/")
            director = paste("导演:",director)
            actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
            actor = actor[!is.na(actor)]
            actor = paste(actor,collapse = "/")
            actor = paste("演员",actor)
            tags$div(
              h3(movie_all[movie_all$id==id, 'English_name']),
              h4(movie_all[movie_all$id==id, 'name']),
              h4(rate),
              h5(type),
              h6(director),
              h6(actor)
            )
        })
        })
        observeEvent(input$save_movie_might1, {
          #docu3 <- read.table("./data/docu.rdata")
          user_rate = as.numeric(gsub('(\\d).+?','\\1',input$movie_might_score1))
          print(user_rate)
          docu2 <<- rbind(docu2, data.frame(score=user_rate, id = movie_might[1]))
          write.table(docu2, "./data/docu.rdata")
        })
        user_rate <- eventReactive(input$save_movie_might1, as.numeric(gsub('(\\d).+?','\\1',input$movie_might_score1)))
        
        output$movie_might2 <- renderUI({
          tags$img(src = movie_all[movie_all$id==movie_might[2],'poster'])
        })
        observeEvent(input$movie_select2,{
          id = movie_might[2]
          output$movie_might_info2 <- renderUI({
            type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
            type = type[!is.na(type)]
            type = paste(type,collapse = "/")
            score = movie_all[movie_all$id==id, 'score']
            pop = movie_all[movie_all$id==id, 'pop']
            rate = paste(score," (",pop,"人评价)",sep = "")
            director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
            director = director[!is.na(director)]
            director = paste(director,collapse = "/")
            director = paste("导演:",director)
            actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
            actor = actor[!is.na(actor)]
            actor = paste(actor,collapse = "/")
            actor = paste("演员",actor)
            tags$div(
              h3(movie_all[movie_all$id==id, 'English_name']),
              h4(movie_all[movie_all$id==id, 'name']),
              h4(rate),
              h5(type),
              h6(director),
              h6(actor)
            )
          })
        })
        observeEvent(input$save_movie_might2, {
          #docu3 <- read.table("./data/docu.rdata")
          user_rate = as.numeric(gsub('(\\d).+?','\\1',input$movie_might_score2))
          print(user_rate)
          docu2 <<- rbind(docu2, data.frame(score=user_rate, id = movie_might[2]))
          write.table(docu2, "./data/docu.rdata")
        })
        
        output$movie_might3 <- renderUI({
          tags$img(src = movie_all[movie_all$id==movie_might[3],'poster'])
        })
        observeEvent(input$movie_select3,{
          id = movie_might[3]
          output$movie_might_info3 <- renderUI({
            type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
            type = type[!is.na(type)]
            type = paste(type,collapse = "/")
            score = movie_all[movie_all$id==id, 'score']
            pop = movie_all[movie_all$id==id, 'pop']
            rate = paste(score," (",pop,"人评价)",sep = "")
            director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
            director = director[!is.na(director)]
            director = paste(director,collapse = "/")
            director = paste("导演:",director)
            actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
            actor = actor[!is.na(actor)]
            actor = paste(actor,collapse = "/")
            actor = paste("演员",actor)
            tags$div(
              h3(movie_all[movie_all$id==id, 'English_name']),
              h4(movie_all[movie_all$id==id, 'name']),
              h4(rate),
              h5(type),
              h6(director),
              h6(actor)
            )
          })
        })
        observeEvent(input$save_movie_might3, {
          user_rate = as.numeric(gsub('(\\d).+?','\\1',input$movie_might_score3))
          docu2 = rbind(docu2, data.frame(score=user_rate, id = movie_might[3]))
          write.table(docu2, "./data/docu.rdata")
        })
        
        output$movie_might4 <- renderUI({
          tags$img(src = movie_all[movie_all$id==movie_might[4],'poster'])
        })
        observeEvent(input$movie_select4,{
          id = movie_might[4]
          output$movie_might_info4 <- renderUI({
            type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
            type = type[!is.na(type)]
            type = paste(type,collapse = "/")
            score = movie_all[movie_all$id==id, 'score']
            pop = movie_all[movie_all$id==id, 'pop']
            rate = paste(score," (",pop,"人评价)",sep = "")
            director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
            director = director[!is.na(director)]
            director = paste(director,collapse = "/")
            director = paste("导演:",director)
            actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
            actor = actor[!is.na(actor)]
            actor = paste(actor,collapse = "/")
            actor = paste("演员",actor)
            tags$div(
              h3(movie_all[movie_all$id==id, 'English_name']),
              h4(movie_all[movie_all$id==id, 'name']),
              h4(rate),
              h5(type),
              h6(director),
              h6(actor)
            )
          })
        })
        observeEvent(input$save_movie_might4, {
          user_rate = as.numeric(gsub('(\\d).+?','\\1',input$movie_might_score4))
          docu2 = rbind(docu2, data.frame(score=user_rate, id = movie_might[4]))
          write.table(docu2, "./data/docu.rdata")
        })
        
        output$movie_might5 <- renderUI({
          tags$img(src = movie_all[movie_all$id==movie_might[5],'poster'])
        })
        observeEvent(input$movie_select5,{
          id = movie_might[5]
          output$movie_might_info5 <- renderUI({
            type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
            type = type[!is.na(type)]
            type = paste(type,collapse = "/")
            score = movie_all[movie_all$id==id, 'score']
            pop = movie_all[movie_all$id==id, 'pop']
            rate = paste(score," (",pop,"人评价)",sep = "")
            director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
            director = director[!is.na(director)]
            director = paste(director,collapse = "/")
            director = paste("导演:",director)
            actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
            actor = actor[!is.na(actor)]
            actor = paste(actor,collapse = "/")
            actor = paste("演员",actor)
            tags$div(
              h3(movie_all[movie_all$id==id, 'English_name']),
              h4(movie_all[movie_all$id==id, 'name']),
              h4(rate),
              h5(type),
              h6(director),
              h6(actor)
            )
          })
        })
        observeEvent(input$save_movie_might5, {
          user_rate = as.numeric(gsub('(\\d).+?','\\1',input$movie_might_score5))
          docu2 = rbind(docu2, data.frame(score=user_rate, id = movie_might[5]))
          write.table(docu2, "./data/docu.rdata")
        })
      })
      
      ###User_might_block
      observeEvent(input$side, {
        poster_list2=c()
        cor_list2 = movie_cor2[id==rownames(movie_cor2),]
        user_might = names(cor_list2)[tail(order(cor_list2))][1:5]
        output$user_might1 <- renderUI({
          tags$img(src = movie_all[movie_all$id==user_might[1],'poster'])
        })
        observeEvent(input$user_select1,{
          id = user_might[1]
          output$user_might_info1 <- renderUI({
            type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
            type = type[!is.na(type)]
            type = paste(type,collapse = "/")
            score = movie_all[movie_all$id==id, 'score']
            pop = movie_all[movie_all$id==id, 'pop']
            rate = paste(score," (",pop,"人评价)",sep = "")
            director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
            director = director[!is.na(director)]
            director = paste(director,collapse = "/")
            director = paste("导演:",director)
            actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
            actor = actor[!is.na(actor)]
            actor = paste(actor,collapse = "/")
            actor = paste("演员",actor)
            tags$div(
              h3(movie_all[movie_all$id==id, 'English_name']),
              h4(movie_all[movie_all$id==id, 'name']),
              h4(rate),
              h5(type),
              h6(director),
              h6(actor)
            )
          })
        })
        observeEvent(input$save_user_might1, {
          user_rate = as.numeric(gsub('(\\d).+?','\\1',input$user_might_score1))
          print(user_rate)
          docu2 = rbind(docu2, data.frame(score=user_rate, id = user_might[1]))
          write.table(docu2, "./data/docu.rdata")
        })
        
        output$user_might2 <- renderUI({
          tags$img(src = movie_all[movie_all$id==user_might[2],'poster'])
        })
        observeEvent(input$user_select2,{
          id = user_might[2]
          output$user_might_info2 <- renderUI({
            type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
            type = type[!is.na(type)]
            type = paste(type,collapse = "/")
            score = movie_all[movie_all$id==id, 'score']
            pop = movie_all[movie_all$id==id, 'pop']
            rate = paste(score," (",pop,"人评价)",sep = "")
            director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
            director = director[!is.na(director)]
            director = paste(director,collapse = "/")
            director = paste("导演:",director)
            actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
            actor = actor[!is.na(actor)]
            actor = paste(actor,collapse = "/")
            actor = paste("演员",actor)
            tags$div(
              h3(movie_all[movie_all$id==id, 'English_name']),
              h4(movie_all[movie_all$id==id, 'name']),
              h4(rate),
              h5(type),
              h6(director),
              h6(actor)
            )
          })
        })
        observeEvent(input$save_user_might2, {
          docu2 = read.table("./data/docu.rdata")
          user_rate = as.numeric(gsub('(\\d).+?','\\1',input$user_might_score2))
          docu2 = rbind(docu2, data.frame(score=user_rate, id = user_might[2]))
          write.table(docu2, "./data/docu.rdata")
        })
        observeEvent(input$save_user_might2, {
          user_rate = as.numeric(gsub('(\\d).+?','\\1',input$user_might_score2))
          print(user_rate)
          docu2 = rbind(docu2, data.frame(score=user_rate, id = user_might[2]))
          write.table(docu2, "./data/docu.rdata")
        })
        
        output$user_might3 <- renderUI({
          tags$img(src = movie_all[movie_all$id==user_might[3],'poster'])
        })
        observeEvent(input$user_select3,{
          id = user_might[3]
          output$user_might_info3 <- renderUI({
            type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
            type = type[!is.na(type)]
            type = paste(type,collapse = "/")
            score = movie_all[movie_all$id==id, 'score']
            pop = movie_all[movie_all$id==id, 'pop']
            rate = paste(score," (",pop,"人评价)",sep = "")
            director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
            director = director[!is.na(director)]
            director = paste(director,collapse = "/")
            director = paste("导演:",director)
            actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
            actor = actor[!is.na(actor)]
            actor = paste(actor,collapse = "/")
            actor = paste("演员",actor)
            tags$div(
              h3(movie_all[movie_all$id==id, 'English_name']),
              h4(movie_all[movie_all$id==id, 'name']),
              h4(rate),
              h5(type),
              h6(director),
              h6(actor)
            )
          })
        })
        observeEvent(input$save_user_might3, {
          docu2 = read.table("./data/docu.rdata")
          user_rate = as.numeric(gsub('(\\d).+?','\\1',input$user_might_score3))
          docu2 = rbind(docu2, data.frame(score=user_rate, id = user_might[3]))
          write.table(docu2, "./data/docu.rdata")
        })
        
        output$user_might4 <- renderUI({
          tags$img(src = movie_all[movie_all$id==user_might[4],'poster'])
        })
        observeEvent(input$user_select4,{
          id = user_might[4]
          output$user_might_info4 <- renderUI({
            type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
            type = type[!is.na(type)]
            type = paste(type,collapse = "/")
            score = movie_all[movie_all$id==id, 'score']
            pop = movie_all[movie_all$id==id, 'pop']
            rate = paste(score," (",pop,"人评价)",sep = "")
            director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
            director = director[!is.na(director)]
            director = paste(director,collapse = "/")
            director = paste("导演:",director)
            actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
            actor = actor[!is.na(actor)]
            actor = paste(actor,collapse = "/")
            actor = paste("演员",actor)
            tags$div(
              h3(movie_all[movie_all$id==id, 'English_name']),
              h4(movie_all[movie_all$id==id, 'name']),
              h4(rate),
              h5(type),
              h6(director),
              h6(actor)
            )
          })
        })
        observeEvent(input$save_user_might4, {
          docu2 = read.table("./data/docu.rdata")
          user_rate = as.numeric(gsub('(\\d).+?','\\1',input$user_might_score4))
          docu2 = rbind(docu2, data.frame(score=user_rate, id = user_might[4]))
          write.table(docu2, "./data/docu.rdata")
        })
        
        output$user_might5 <- renderUI({
          tags$img(src = movie_all[movie_all$id==user_might[5],'poster'])
        })
        observeEvent(input$user_select5,{
          id = user_might[5]
          output$user_might_info5 <- renderUI({
            type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
            type = type[!is.na(type)]
            type = paste(type,collapse = "/")
            score = movie_all[movie_all$id==id, 'score']
            pop = movie_all[movie_all$id==id, 'pop']
            rate = paste(score," (",pop,"人评价)",sep = "")
            director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
            director = director[!is.na(director)]
            director = paste(director,collapse = "/")
            director = paste("导演:",director)
            actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
            actor = actor[!is.na(actor)]
            actor = paste(actor,collapse = "/")
            actor = paste("演员",actor)
            tags$div(
              h3(movie_all[movie_all$id==id, 'English_name']),
              h4(movie_all[movie_all$id==id, 'name']),
              h4(rate),
              h5(type),
              h6(director),
              h6(actor)
            )
          })
        })
        observeEvent(input$save_user_might5, {
          docu2 = read.table("./data/docu.rdata")
          user_rate = as.numeric(gsub('(\\d).+?','\\1',input$user_might_score5))
          docu2 = rbind(docu2, data.frame(score=user_rate, id = user_might[5]))
          write.table(docu2, "./data/docu.rdata")
        })
      })
      
      ###sever_user_analyse_block
      observeEvent(input$side, {
        docu3 = docu2[-1,]
        test2 = plyr::join(docu3, movie_all, by = "id")
        output$hist <- renderPlot({
          hist(test2$score)
        })
        names(test2)[1] = "docu_score"
        test3 = select(test2, docu_score, starts_with("type"))
        test3 = gather(test3, "type1":"type5",key = "index", value = "type")
        test3 = test3[!is.na(test3$type),c(1,3)]
        output$table<-renderTable({
          as.data.frame(t(tapply(test3$docu_score, test3$type, mean, na.rm = T)))})
        table(test3$type)
      }) 
  })
  
  observeEvent(input$refresh1,{
    docu2 <- read.table("./data/docu.rdata")
    id = sample(settle_cluster[[1]],1) 
    poster = movie_all[movie_all$id==id, 'poster']
    output$cluster1_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"人评价)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("导演:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("演员",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh2,{
    docu2 <- read.table("./data/docu.rdata")
    id = sample(settle_cluster[[2]],1) 
    poster = movie_all[movie_all$id==id, 'poster']
    output$cluster2_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"人评价)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("导演:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("演员",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh3,{
    docu2 <- read.table("./data/docu.rdata")
    id = sample(settle_cluster[[3]],1) 
    poster = movie_all[movie_all$id==id, 'poster']
    output$cluster3_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"人评价)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("导演:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("演员",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh4,{
    docu2 <- read.table("./data/docu.rdata")
    id = sample(settle_cluster[[4]],1) 
    poster = movie_all[movie_all$id==id, 'poster']
    output$cluster4_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"人评价)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("导演:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("演员",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh5,{
    docu2 <- read.table("./data/docu.rdata")
    id = sample(settle_cluster[[5]],1) 
    poster = movie_all[movie_all$id==id, 'poster']
    output$cluster5_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"人评价)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("导演:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("演员",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh6,{
    docu2 <- read.table("./data/docu.rdata")
    id = sample(settle_cluster[[6]],1) 
    poster = movie_all[movie_all$id==id, 'poster']
    output$cluster6_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"人评价)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("导演:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("演员",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh7,{
    docu2 <- read.table("./data/docu.rdata")
    id = sample(settle_cluster[[7]],1) 
    poster = movie_all[movie_all$id==id, 'poster']
    output$cluster7_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"人评价)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("导演:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("演员",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh8,{
    docu2 <- read.table("./data/docu.rdata")
    id = sample(settle_cluster[[8]],1) 
    poster = movie_all[movie_all$id==id, 'poster']
    output$cluster8_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"人评价)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("导演:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("演员",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh9,{
    docu2 <- read.table("./data/docu.rdata")
    id = sample(settle_cluster[[9]],1) 
    poster = movie_all[movie_all$id==id, 'poster']
    output$cluster9_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"人评价)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("导演:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("演员",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh10,{
    docu2 <- read.table("./data/docu.rdata")
    id = sample(settle_cluster[[10]],1) 
    poster = movie_all[movie_all$id==id, 'poster']
    output$cluster10_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"人评价)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("导演:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("演员",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh11,{
    docu2 <- read.table("./data/docu.rdata")
    id = sample(settle_cluster[[11]],1) 
    poster = movie_all[movie_all$id==id, 'poster']
    output$cluster11_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"人评价)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("导演:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("演员",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh12,{
    docu2 <- read.table("./data/docu.rdata")
    id = sample(settle_cluster[[12]],1) 
    poster = movie_all[movie_all$id==id, 'poster']
    output$cluster12_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"人评价)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("导演:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("演员",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh13,{
    docu2 <- read.table("./data/docu.rdata")
    id = sample(settle_cluster[[13]],1) 
    poster = movie_all[movie_all$id==id, 'poster']
    output$cluster13_info <- renderUI({
      type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
      type = type[!is.na(type)]
      type = paste(type,collapse = "/")
      score = movie_all[movie_all$id==id, 'score']
      pop = movie_all[movie_all$id==id, 'pop']
      rate = paste(score," (",pop,"人评价)",sep = "")
      director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
      director = director[!is.na(director)]
      director = paste(director,collapse = "/")
      director = paste("导演:",director)
      actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
      actor = actor[!is.na(actor)]
      actor = paste(actor,collapse = "/")
      actor = paste("演员",actor)
      tags$div(
        img(src = poster),
        h3(movie_all[movie_all$id==id, 'English_name']),
        h4(movie_all[movie_all$id==id, 'name']),
        h4(rate),
        h5(type),
        h6(director),
        h6(actor)
      )
    })
  })
  
  observeEvent(input$refresh_cluster,{
    id_list = movie_cluster[,sample(1:50,1)]
    id_list = rev(id_list)
    
    output$cluster_might1 <- renderUI({
      tags$img(src = movie_all[movie_all$id==id_list[1],'poster'])
    })
    observeEvent(input$cluster_select1,{
      id = id_list[1]
      output$cluster_might_info1 <- renderUI({
        type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
        type = type[!is.na(type)]
        type = paste(type,collapse = "/")
        score = movie_all[movie_all$id==id, 'score']
        pop = movie_all[movie_all$id==id, 'pop']
        rate = paste(score," (",pop,"人评价)",sep = "")
        director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
        director = director[!is.na(director)]
        director = paste(director,collapse = "/")
        director = paste("导演:",director)
        actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
        actor = actor[!is.na(actor)]
        actor = paste(actor,collapse = "/")
        actor = paste("演员",actor)
        tags$div(
          h3(movie_all[movie_all$id==id, 'English_name']),
          h4(movie_all[movie_all$id==id, 'name']),
          h4(rate),
          h5(type),
          h6(director),
          h6(actor)
        )
      })
    })
    
    output$cluster_might2 <- renderUI({
      tags$img(src = movie_all[movie_all$id==id_list[2],'poster'])
    })
    observeEvent(input$cluster_select2,{
      id = id_list[2]
      output$cluster_might_info2 <- renderUI({
        type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
        type = type[!is.na(type)]
        type = paste(type,collapse = "/")
        score = movie_all[movie_all$id==id, 'score']
        pop = movie_all[movie_all$id==id, 'pop']
        rate = paste(score," (",pop,"人评价)",sep = "")
        director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
        director = director[!is.na(director)]
        director = paste(director,collapse = "/")
        director = paste("导演:",director)
        actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
        actor = actor[!is.na(actor)]
        actor = paste(actor,collapse = "/")
        actor = paste("演员",actor)
        tags$div(
          h3(movie_all[movie_all$id==id, 'English_name']),
          h4(movie_all[movie_all$id==id, 'name']),
          h4(rate),
          h5(type),
          h6(director),
          h6(actor)
        )
      })
    })
    
    output$cluster_might3 <- renderUI({
      tags$img(src = movie_all[movie_all$id==id_list[3],'poster'])
    })
    observeEvent(input$cluster_select3,{
      id = id_list[3]
      output$cluster_might_info3 <- renderUI({
        type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
        type = type[!is.na(type)]
        type = paste(type,collapse = "/")
        score = movie_all[movie_all$id==id, 'score']
        pop = movie_all[movie_all$id==id, 'pop']
        rate = paste(score," (",pop,"人评价)",sep = "")
        director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
        director = director[!is.na(director)]
        director = paste(director,collapse = "/")
        director = paste("导演:",director)
        actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
        actor = actor[!is.na(actor)]
        actor = paste(actor,collapse = "/")
        actor = paste("演员",actor)
        tags$div(
          h3(movie_all[movie_all$id==id, 'English_name']),
          h4(movie_all[movie_all$id==id, 'name']),
          h4(rate),
          h5(type),
          h6(director),
          h6(actor)
        )
      })
    })
    
    output$cluster_might4 <- renderUI({
      tags$img(src = movie_all[movie_all$id==id_list[4],'poster'])
    })
    observeEvent(input$cluster_select4,{
      id = id_list[4]
      output$cluster_might_info4 <- renderUI({
        type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
        type = type[!is.na(type)]
        type = paste(type,collapse = "/")
        score = movie_all[movie_all$id==id, 'score']
        pop = movie_all[movie_all$id==id, 'pop']
        rate = paste(score," (",pop,"人评价)",sep = "")
        director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
        director = director[!is.na(director)]
        director = paste(director,collapse = "/")
        director = paste("导演:",director)
        actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
        actor = actor[!is.na(actor)]
        actor = paste(actor,collapse = "/")
        actor = paste("演员",actor)
        tags$div(
          h3(movie_all[movie_all$id==id, 'English_name']),
          h4(movie_all[movie_all$id==id, 'name']),
          h4(rate),
          h5(type),
          h6(director),
          h6(actor)
        )
      })
    })
    
    output$cluster_might5 <- renderUI({
      tags$img(src = movie_all[movie_all$id==id_list[5],'poster'])
    })
    observeEvent(input$cluster_select5,{
      id = id_list[5]
      output$cluster_might_info5 <- renderUI({
        type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
        type = type[!is.na(type)]
        type = paste(type,collapse = "/")
        score = movie_all[movie_all$id==id, 'score']
        pop = movie_all[movie_all$id==id, 'pop']
        rate = paste(score," (",pop,"人评价)",sep = "")
        director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
        director = director[!is.na(director)]
        director = paste(director,collapse = "/")
        director = paste("导演:",director)
        actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
        actor = actor[!is.na(actor)]
        actor = paste(actor,collapse = "/")
        actor = paste("演员",actor)
        tags$div(
          h3(movie_all[movie_all$id==id, 'English_name']),
          h4(movie_all[movie_all$id==id, 'name']),
          h4(rate),
          h5(type),
          h6(director),
          h6(actor)
        )
      })
    })
    
    output$cluster_might6 <- renderUI({
      tags$img(src = movie_all[movie_all$id==id_list[6],'poster'])
    })
    observeEvent(input$cluster_select6,{
      id = id_list[6]
      output$cluster_might_info6 <- renderUI({
        type = movie_all[movie_all$id==id, grep('type\\d',names(movie_all))]
        type = type[!is.na(type)]
        type = paste(type,collapse = "/")
        score = movie_all[movie_all$id==id, 'score']
        pop = movie_all[movie_all$id==id, 'pop']
        rate = paste(score," (",pop,"人评价)",sep = "")
        director = movie_all[movie_all$id==id, grep('director\\d',names(movie_all))]
        director = director[!is.na(director)]
        director = paste(director,collapse = "/")
        director = paste("导演:",director)
        actor = movie_all[movie_all$id==id, grep('actor\\d',names(movie_all))]
        actor = actor[!is.na(actor)]
        actor = paste(actor,collapse = "/")
        actor = paste("演员",actor)
        tags$div(
          h3(movie_all[movie_all$id==id, 'English_name']),
          h4(movie_all[movie_all$id==id, 'name']),
          h4(rate),
          h5(type),
          h6(director),
          h6(actor)
        )
      })
    })
    
  })
  
}
