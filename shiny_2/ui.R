fluidPage(
  navbarPage(
    title="Welcome!",
    id = 'index',
    tabPanel("New_user", icon=icon("home")),
    tabPanel("Who_are_you"),
    tabPanel("Douban_Guess")
  ),
  ###main block
  
  ###Douban_Guess block
  conditionalPanel(
    condition = 'input.index == "Douban_Guess"',
    navlistPanel(
      well = FALSE,
      id = 'side',
      widths = c(3, 8),
      tabPanel("详情"),
      tabPanel("评论"),
      tabPanel("评分分布"),
      tabPanel("你可能也感兴趣"),
      conditionalPanel(
        condition = 'input.side == "详情"',
        uiOutput(outputId = "movie_info_main"),
        uiOutput(outputId = "real_rate"),
        radioButtons('user_score', '', choices = c('5星','4星','3星','2星','1星'), 
                     inline = T, selected='5星'), 
        actionButton("refresh", "Refresh movie"),
        actionButton('save_inputs', 'Save inputs')
      ),
      
      conditionalPanel(
        condition = 'input.side == "评论"',
        radioButtons('comment_star', '', choices = c('5星','4星','3星','2星','1星'), 
                     inline = T, selected='5星'),
          conditionalPanel(
              condition = 'input.comment_star == "5星"',
              tableOutput(outputId = "movie_inspect5")
            ),
          conditionalPanel(
              condition = 'input.comment_star == "4星"',
              tableOutput(outputId = "movie_inspect4")
            ),
          conditionalPanel(
              condition = 'input.comment_star == "3星"',
              tableOutput(outputId = "movie_inspect3")
            ),
          conditionalPanel(
              condition = 'input.comment_star == "2星"',
              tableOutput(outputId = "movie_inspect2")
            ),
          conditionalPanel(
              condition = 'input.comment_star == "1星"',
              tableOutput(outputId = "movie_inspect1")
            )
      ),
      
      conditionalPanel(
        condition = 'input.side == "评分分布"',
        plotOutput('user_dist')
      ),
      
      conditionalPanel(
        condition = 'input.side == "你可能也感兴趣"',
        radioButtons('by_way', '', choices = c("电影相似度","用户相似度"), 
                     inline = T, selected='电影相似度'),
        conditionalPanel(
          condition = 'input.by_way == "电影相似度"',
          uiOutput(outputId = "movie_might1"),
          radioButtons('movie_select1', '', choices = c("评分","详细信息"), 
                       inline = T, selected="详细信息"), 
          conditionalPanel(
            condition = 'input.movie_select1=="评分"',
            radioButtons('movie_might_score1', '', choices = c('5星','4星','3星','2星','1星'), 
                         inline = T, selected='5星'),
            actionButton('save_movie_might1', '保存')
            ),
          conditionalPanel(
            condition = 'input.movie_select1=="详细信息"',
            uiOutput(outputId = "movie_might_info1")
          ),
          
          uiOutput(outputId = "movie_might2"),
          radioButtons('movie_select2', '', choices = c("评分","详细信息"), 
                       inline = T, selected="详细信息"), 
          conditionalPanel(
            condition = 'input.movie_select2=="评分"',
            radioButtons('movie_might_score2', '', choices = c('5星','4星','3星','2星','1星'), 
                         inline = T, selected='5星'),
            actionButton('save_movie_might2', '保存')
          ),
          conditionalPanel(
            condition = 'input.movie_select2=="详细信息"',
            uiOutput(outputId = "movie_might_info2")
          ),
          
          uiOutput(outputId = "movie_might3"),
          radioButtons('movie_select3', '', choices = c("评分","详细信息"), 
                       inline = T, selected="详细信息"), 
          conditionalPanel(
            condition = 'input.movie_select3=="评分"',
            radioButtons('movie_might_score3', '', choices = c('5星','4星','3星','2星','1星'), 
                         inline = T, selected='5星'),
            actionButton('save_movie_might3', '保存')
          ),
          conditionalPanel(
            condition = 'input.movie_select3=="详细信息"',
            uiOutput(outputId = "movie_might_info3")
          ),
          
          uiOutput(outputId = "movie_might4"),
          radioButtons('movie_select4', '', choices = c("评分","详细信息"), 
                       inline = T, selected="详细信息"), 
          conditionalPanel(
            condition = 'input.movie_select4=="评分"',
            radioButtons('movie_might_score4', '', choices = c('5星','4星','3星','2星','1星'), 
                         inline = T, selected='5星'),
            actionButton('save_movie_might4', '保存')
          ),
          conditionalPanel(
            condition = 'input.movie_select4=="详细信息"',
            uiOutput(outputId = "movie_might_info4")
          ),
          
          uiOutput(outputId = "movie_might5"),
          radioButtons('movie_select5', '', choices = c("评分","详细信息"), 
                       inline = T, selected="详细信息"), 
          conditionalPanel(
            condition = 'input.movie_select5=="评分"',
            radioButtons('movie_might_score5', '', choices = c('5星','4星','3星','2星','1星'), 
                         inline = T, selected='5星'),
            actionButton('save_movie_might5', '保存')
          ),
          conditionalPanel(
            condition = 'input.movie_select5=="详细信息"',
            uiOutput(outputId = "movie_might_info5")
          )
        ),
        
        
        conditionalPanel(
          condition = 'input.by_way == "用户相似度"',
          uiOutput(outputId = "user_might1"),
          radioButtons('user_select1', '', choices = c("评分","详细信息"), 
                       inline = T, selected="详细信息"), 
          conditionalPanel(
            condition = 'input.user_select1=="评分"',
            radioButtons('user_might_score1', '', choices = c('5星','4星','3星','2星','1星'), 
                         inline = T, selected='5星'),
            actionButton('save_user_might1', '保存')
          ),
          conditionalPanel(
            condition = 'input.user_select1=="详细信息"',
            uiOutput(outputId = "user_might_info1")
          ),
          
          uiOutput(outputId = "user_might2"),
          radioButtons('user_select2', '', choices = c("评分","详细信息"), 
                       inline = T, selected="详细信息"), 
          conditionalPanel(
            condition = 'input.user_select2=="评分"',
            radioButtons('user_might_score2', '', choices = c('5星','4星','3星','2星','1星'), 
                         inline = T, selected='5星'),
            actionButton('save_user_might2', '保存')
          ),
          conditionalPanel(
            condition = 'input.user_select2=="详细信息"',
            uiOutput(outputId = "user_might_info2")
          ),
          
          uiOutput(outputId = "user_might3"),
          radioButtons('user_select3', '', choices = c("评分","详细信息"), 
                       inline = T, selected="详细信息"), 
          conditionalPanel(
            condition = 'input.user_select3=="评分"',
            radioButtons('user_might_score3', '', choices = c('5星','4星','3星','2星','1星'), 
                         inline = T, selected='5星'),
            actionButton('save_user_might3', '保存')
          ),
          conditionalPanel(
            condition = 'input.user_select3=="详细信息"',
            uiOutput(outputId = "user_might_info3")
          ),
          
          uiOutput(outputId = "user_might4"),
          radioButtons('user_select4', '', choices = c("评分","详细信息"), 
                       inline = T, selected="详细信息"), 
          conditionalPanel(
            condition = 'input.user_select4=="评分"',
            radioButtons('user_might_score4', '', choices = c('5星','4星','3星','2星','1星'), 
                         inline = T, selected='5星'),
            actionButton('save_user_might4', '保存')
          ),
          conditionalPanel(
            condition = 'input.user_select4=="详细信息"',
            uiOutput(outputId = "user_might_info4")
          ),
          
          uiOutput(outputId = "user_might5"),
          radioButtons('user_select5', '', choices = c("评分","详细信息"), 
                       inline = T, selected="详细信息"), 
          conditionalPanel(
            condition = 'input.user_select5=="评分"',
            radioButtons('user_might_score5', '', choices = c('5星','4星','3星','2星','1星'), 
                         inline = T, selected='5星'),
            actionButton('save_user_might5', '保存')
          ),
          conditionalPanel(
            condition = 'input.user_select5=="详细信息"',
            uiOutput(outputId = "user_might_info5")
          )
        )
      )
    )
  ),

  ###user_analyse block(connect with server!)
  conditionalPanel(
    condition = 'input.index == "Who_are_you"',
    plotOutput('hist'),
    tableOutput('table')
  ),
  
  conditionalPanel(
    condition = 'input.index == "New_user"',
    navlistPanel(
      well = FALSE,
      id = 'cluster',
      widths = c(3, 8),
      tabPanel("时间"),
      tabPanel("类型"),
      tabPanel("智能分类"),
      
      conditionalPanel(
        condition = 'input.cluster == "时间"',
        radioButtons('cluster_time', '', choices = c('80年代','90年代','现代'), 
                     inline = T, selected='80年代'),
        
        conditionalPanel(
          condition = 'input.cluster_time == "80年代"',
          actionButton('refresh1', "来一个！"),
          uiOutput(outputId = 'cluster1_info'),
          radioButtons('cluster_score_1', '', choices = c('5星','4星','3星','2星','1星'), 
                       inline = T, selected='5星'),
          actionButton('save_cluster1', '保存')
        ),
        
        conditionalPanel(
          condition = 'input.cluster_time == "90年代"',
          actionButton('refresh2', "来一个！"),
          uiOutput(outputId = 'cluster2_info'),
          radioButtons('cluster_score_2', '', choices = c('5星','4星','3星','2星','1星'), 
                       inline = T, selected='5星'),
          actionButton('save_cluster2', '保存')
        ),
        
        conditionalPanel(
          condition = 'input.cluster_time == "现代"',
          actionButton('refresh3', "来一个！"),
          uiOutput(outputId = 'cluster3_info'),
          radioButtons('cluster_score_3', '', choices = c('5星','4星','3星','2星','1星'), 
                       inline = T, selected='5星'),
          actionButton('save_cluster3', '保存')
        )
      ),
      
      conditionalPanel(
        condition = 'input.cluster == "类型"',
        radioButtons('cluster_type', '', choices = c('传记','动作','动画','喜剧','奇幻',
                        '惊悚','爱情','科幻','纪录片','同性'), 
                     inline = T, selected='传记'),
        
        conditionalPanel(
          condition = 'input.cluster_type == "传记"',
          actionButton('refresh4', "来一个！"),
          uiOutput(outputId = 'cluster4_info'),
          radioButtons('cluster_score_4', '', choices = c('5星','4星','3星','2星','1星'), 
                       inline = T, selected='5星'),
          actionButton('save_cluster4', '保存')
        ),
        
        conditionalPanel(
          condition = 'input.cluster_type == "动作"',
          actionButton('refresh5', "来一个！"),
          uiOutput(outputId = 'cluster5_info'),
          radioButtons('cluster_score_5', '', choices = c('5星','4星','3星','2星','1星'), 
                       inline = T, selected='5星'),
          actionButton('save_cluster5', '保存')
        ),
        
        conditionalPanel(
          condition = 'input.cluster_type == "动画"',
          actionButton('refresh6', "来一个！"),
          uiOutput(outputId = 'cluster6_info'),
          radioButtons('cluster_score_6', '', choices = c('5星','4星','3星','2星','1星'), 
                       inline = T, selected='5星'),
          actionButton('save_cluster6', '保存')
        ),
        
        conditionalPanel(
          condition = 'input.cluster_type == "喜剧"',
          actionButton('refresh7', "来一个！"),
          uiOutput(outputId = 'cluster7_info'),
          radioButtons('cluster_score_7', '', choices = c('5星','4星','3星','2星','1星'), 
                       inline = T, selected='5星'),
          actionButton('save_cluster7', '保存')
        ),
        
        conditionalPanel(
          condition = 'input.cluster_type == "奇幻"',
          actionButton('refresh8', "来一个！"),
          uiOutput(outputId = 'cluster8_info'),
          radioButtons('cluster_score_8', '', choices = c('5星','4星','3星','2星','1星'), 
                       inline = T, selected='5星'),
          actionButton('save_cluster8', '保存')
        ),
        
        conditionalPanel(
          condition = 'input.cluster_type == "惊悚"',
          actionButton('refresh9', "来一个！"),
          uiOutput(outputId = 'cluster9_info'),
          radioButtons('cluster_score_9', '', choices = c('5星','4星','3星','2星','1星'), 
                       inline = T, selected='5星'),
          actionButton('save_cluster9', '保存')
        ),
        
        conditionalPanel(
          condition = 'input.cluster_type == "爱情"',
          actionButton('refresh10', "来一个！"),
          uiOutput(outputId = 'cluster10_info'),
          radioButtons('cluster_score_10', '', choices = c('5星','4星','3星','2星','1星'), 
                       inline = T, selected='5星'),
          actionButton('save_cluster10', '保存')
        ),
        
        conditionalPanel(
          condition = 'input.cluster_type == "科幻"',
          actionButton('refresh11', "来一个！"),
          uiOutput(outputId = 'cluster11_info'),
          radioButtons('cluster_score_11', '', choices = c('5星','4星','3星','2星','1星'), 
                       inline = T, selected='5星'),
          actionButton('save_cluster11', '保存')
        ),
        
        conditionalPanel(
          condition = 'input.cluster_type == "纪录片"',
          actionButton('refresh12', "来一个！"),
          uiOutput(outputId = 'cluster12_info'),
          radioButtons('cluster_score_12', '', choices = c('5星','4星','3星','2星','1星'), 
                       inline = T, selected='5星'),
          actionButton('save_cluster12', '保存')
        ),
        
        conditionalPanel(
          condition = 'input.cluster_type == "同性"',
          actionButton('refresh13', "来一个！"),
          uiOutput(outputId = 'cluster13_info'),
          radioButtons('cluster_score_13', '', choices = c('5星','4星','3星','2星','1星'), 
                       inline = T, selected='5星'),
          actionButton('save_cluster13', '保存')
        )
      ),
      
      conditionalPanel(
        condition = 'input.cluster == "智能分类"',
        actionButton('refresh_cluster', "来一组！"),
        
        uiOutput(outputId = "cluster_might1"),
        radioButtons('cluster_select1', '', choices = c("评分","详细信息"), 
                     inline = T, selected="详细信息"), 
        conditionalPanel(
          condition = 'input.cluster_select1=="评分"',
          radioButtons('cluster_might_score1', '', choices = c('5星','4星','3星','2星','1星'), 
                       inline = T, selected='5星'),
          actionButton('save_cluster_might1', '保存')
        ),
        conditionalPanel(
          condition = 'input.cluster_select1=="详细信息"',
          uiOutput(outputId = "cluster_might_info1")
        ),
        
        uiOutput(outputId = "cluster_might2"),
        radioButtons('cluster_select2', '', choices = c("评分","详细信息"), 
                     inline = T, selected="详细信息"), 
        conditionalPanel(
          condition = 'input.cluster_select2=="评分"',
          radioButtons('cluster_might_score2', '', choices = c('5星','4星','3星','2星','1星'), 
                       inline = T, selected='5星'),
          actionButton('save_cluster_might2', '保存')
        ),
        conditionalPanel(
          condition = 'input.cluster_select2=="详细信息"',
          uiOutput(outputId = "cluster_might_info2")
        ),
        
        uiOutput(outputId = "cluster_might3"),
        radioButtons('cluster_select3', '', choices = c("评分","详细信息"), 
                     inline = T, selected="详细信息"), 
        conditionalPanel(
          condition = 'input.cluster_select3=="评分"',
          radioButtons('cluster_might_score3', '', choices = c('5星','4星','3星','2星','1星'), 
                       inline = T, selected='5星'),
          actionButton('save_cluster_might3', '保存')
        ),
        conditionalPanel(
          condition = 'input.cluster_select3=="详细信息"',
          uiOutput(outputId = "cluster_might_info3")
        ),
        
        uiOutput(outputId = "cluster_might4"),
        radioButtons('cluster_select4', '', choices = c("评分","详细信息"), 
                     inline = T, selected="详细信息"), 
        conditionalPanel(
          condition = 'input.cluster_select4=="评分"',
          radioButtons('cluster_might_score4', '', choices = c('5星','4星','3星','2星','1星'), 
                       inline = T, selected='5星'),
          actionButton('save_cluster_might4', '保存')
        ),
        conditionalPanel(
          condition = 'input.cluster_select4=="详细信息"',
          uiOutput(outputId = "cluster_might_info4")
        ),
        
        uiOutput(outputId = "cluster_might5"),
        radioButtons('cluster_select5', '', choices = c("评分","详细信息"), 
                     inline = T, selected="详细信息"), 
        conditionalPanel(
          condition = 'input.cluster_select5=="评分"',
          radioButtons('cluster_might_score5', '', choices = c('5星','4星','3星','2星','1星'), 
                       inline = T, selected='5星'),
          actionButton('save_cluster_might5', '保存')
        ),
        conditionalPanel(
          condition = 'input.cluster_select5=="详细信息"',
          uiOutput(outputId = "cluster_might_info5")
        )
      )
    )
  ),
  
  tags$head(
    tags$style(".tab-content .tab-content {border: 1px solid gray; min-height:200px;}")
  )
)