###Load library
library(shiny)
library(shinyjs)
library(tidyverse)
library(plyr)
library(V8)

###Data and environment preparation
movie_all = read.table("./data/movie_all.rdata")
movie_cor = read.table("./data/movie_cor.rdata")
colnames(movie_cor) = gsub("X(\\d+)","\\1",colnames(movie_cor)) 
docu =data.frame(score=c(1), id = c(1)) ###Initialization the user-docu-vector!!!
write.table(docu,"./data/docu.rdata") ###Initialization the user-docu-vector!!!

function(input, output, session) {
  observeEvent(input$refresh, {
    docu2 <- read.table("./data/docu.rdata")
      ###Select the out put movie
      id = sample(as.character(movie_all$id),1)
      poster = movie_all[movie_all$id==id, 'poster']
      output$image <- renderUI({
        tags$img(src = poster)
      })
      ###Get the score from main block
      observeEvent(input$save_inputs, {
        docu2 = rbind(docu2, data.frame(score=input$n, id = id))
        write.table(docu2, "./data/docu.rdata")
       })
      
      ###sever_YOU_MIGHT_LIKE_block
      observeEvent(input$YOU_MIGHT_LIKE, {
        ###BY_movie_block
        poster_list=c()
        cor_list = movie_cor[id==rownames(movie_cor),]
        movie_might = colnames(cor_list)[tail(order(cor_list))][1:5]
        output$movie_might1 <- renderUI({
          tags$img(src = movie_all[movie_all$id==movie_might[1],'poster'])
        })
        
        observeEvent(input$rate_movie1,{
          movie_might1_score = input$n_might1
        }) ###Small block!!!
        
        output$movie_might2 <- renderUI({
          tags$img(src = movie_all[movie_all$id==movie_might[2],'poster'])
        })
        output$movie_might3 <- renderUI({
          tags$img(src = movie_all[movie_all$id==movie_might[3],'poster'])
        })
        output$movie_might4 <- renderUI({
          tags$img(src = movie_all[movie_all$id==movie_might[4],'poster'])
        })
        output$movie_might5 <- renderUI({
          tags$img(src = movie_all[movie_all$id==movie_might[5],'poster'])
        })
      })
      
      ###sever_user_analyse_block
      observeEvent(input$index, {
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
}