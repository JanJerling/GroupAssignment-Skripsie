# GroupAssignment-Skripsie
This is my final year project for Industrial Engineering at Stellenbosch University. It may be used to divide students into groups in a mathematically optimal manner according to their academic averages.

Please follow the following link to see the program in RShiny: https://janjerlingshiny.shinyapps.io/GroupAssignment/

Otherwise, please peruse the code:

library(shiny)
library(ggplot2)
library(shinyIncubator)
library(shinyjs)

ui <- fluidPage(
  titlePanel(title="Welcome to Group Assignment using DBMOSA"),
  useShinyjs(),
  sidebarLayout(
    sidebarPanel(verbatimTextOutput("Readme"),
                 br(),
                 fileInput("file", label = NULL, multiple = FALSE, accept = NULL, width = NULL, buttonLabel = "Browse...", placeholder = "No file selected"),
                 numericInput("gs", "Enter desired maximum group size", ""),
                 numericInput("mc", "Specify column number containing academic averages", ""),
                 numericInput("nc", "Specify column number containing student numbers", ""),
                 actionButton("button1", "Start"),
                 br(),
                 br(),
                 conditionalPanel(condition = "input.button1 != 0",
                                  fluidRow(column(width = 5, downloadButton("downloadData2", "Download as shown")),
                                           column(width = 7,
                                                  downloadButton("downloadData1", "Download as list"))),
                                  "Downloaded file will be of type CommaSeparated.csv")
                 ),
    mainPanel(
      tabsetPanel(type = "tab",
                  tabPanel("Group Assignment",
      conditionalPanel(condition = "input.button1 == 0", tableOutput("studentdata")),
      
      fluidRow(
        column( width = 7,
                plotOutput("plot1", click = "plot1_click"),
                br()),
        column(width = 5, 
               tableOutput("marksmatrix"),
               verbatimTextOutput("chose"),
               verbatimTextOutput("cl"))
      ),
      
      htmlOutput("head"),
      tableOutput("num")
      
                  ),
      tabPanel("Information", br(), a("         Full Project Description",target="_blank",href="Skripsie soft copy.pdf"), br(), tags$video(src = 'Demonstration video.mp4', type = "video/mp4", height ="360px", width = "540px", controls = 'controls'))
     )
    )
  )
)

server <- function(input, output){
  
  R <- reactiveValues()
  
  observe({
    
    
    inFile <- input$file
    
    if (is.null(inFile)) return(NULL) 
    
    
    
    ######## read student data from tab delimited text file. #####
    R$stdntdata<-read.table(inFile$datapath, na.strings=c("", "NA"), sep="\t",header=T)
    if(input$button1 == 0) return(NULL)
    withProgress(message = "DBMOSA initiated", value = 0, {   
      
      R$groupsize<-as.numeric(input$gs)
      R$markcolumn<-as.numeric(input$mc)
      R$numbercolumn<-as.numeric(input$nc)
      
      NAs<-c(which(is.na(R$stdntdata[,R$markcolumn])))
      incProgress(0.05, message = "DBMOSA initiated")
      
      
      if (length(NAs) != 0){      #if empty cells: clean empty cells with which are not applicable
        cleanset<-R$stdntdata[-NAs,]
        fill<-dim(cleanset)
        nogroups<-ceiling(fill[1]/R$groupsize)
        markmatrix<-rep(NA,c(R$groupsize*nogroups))
        
        for (i in 1:(R$groupsize*nogroups)){ #create matrix of marks, with NA operator for uneven multiples
          markmatrix[i]<-cleanset[i,R$markcolumn]
        }
        dim(markmatrix)<-c(nogroups,R$groupsize)
      } else {                  #If data is clean create mark matrix, with NA operator for uneven multiples
        cleanset<-R$stdntdata
        fill<-dim(R$stdntdata)
        nogroups<-ceiling(fill[1]/R$groupsize)
        markmatrix<-rep(NA,c(R$groupsize*nogroups))
        for (i in 1:(R$groupsize*nogroups)){
          markmatrix[i]<-cleanset[i,R$markcolumn]
        }
        dim(markmatrix)<-c(nogroups,R$groupsize)
      }
      
      
      
      Numbers<-rep(NA,c(R$groupsize*nogroups))
      for (i in 1:(R$groupsize*nogroups)){
        Numbers[i]<-cleanset[i,R$numbercolumn]
      }
      dim(Numbers)<-c(nogroups,R$groupsize)
      
      globalaverage<-mean(markmatrix, na.rm = T)
      
      
      
      #####################################################################
      cursol<-c(0,0)  # Current solution
      archive<-c(0,0)  # Archive
      nebsol<-c(0,0) # Neighbouring solution
      increment<-seq(1,50000,2) # Generates 1, 3, 5, 7,....
      groupaverage<-rep(0,nogroups) # holds the averages of all groups for a single group
      difromglobal<-rep(0,nogroups) # holds the difference bteween global and group averages
      grouprange<-rep(0,R$groupsize) # holds group ranges for a single group
      size<-nogroups*R$groupsize
      
      if ( fill[1] <= 100){
        Epochmax<-10 # specify max number of epochs to execute
        Iterations<-100
      }
      if ( fill[1] > 100 & fill[1] < 500){
        Epochmax<-20 # specify max number of epochs to execute
        Iterations<-250
      }
      if ( fill[1] >= 500 & fill[1] < 1000){
        Epochmax<-30 # specify max number of epochs to execute
        Iterations<-500
      }
      Reheatmax<-3
      
      
      
      
      ################################################################
      # START DBMOSA #
      ################################################################
      
      ############## Intitial current solution ###################
      
      for (i in 1:nogroups){
        groupaverage[i]<-mean(markmatrix[i,],na.rm = T)
      }
      for (i in 1:nogroups){
        difromglobal[i]<-abs(globalaverage-groupaverage[i])
      }
      cursol[1]<-max(difromglobal)
      
      #### objective 2: range in group
      for (i in 1:nogroups){
        grouprange[i]<-(max(markmatrix[i,], na.rm = T)-min(markmatrix[i,], na.rm = T))
      }
      cursol[2]<-max(grouprange)
      ############## Start archive ################
      archive[1]<-cursol[1]
      archive[2]<-cursol[2]
      hold<-Numbers
      dim(hold)<-c(size,1)
      Numholder<-rep(0,size*length(archive)/2)
      dim(Numholder)<-c(size,length(archive)/2)
      Numholder[,1]<-hold
      reheat<-0
      while (reheat < Reheatmax){
        ############ Intialise Temperature ###################
        #random walk algorithm
        if (reheat == 0){
          incProgress(0.15, message = "Relax, go grab a coffee")
          prog<-1
        }
        if (reheat == 1){
          incProgress(0.15, message = "Have you remembered to phone your mom this week?")
          prog<-3
        }
        if (reheat == 2){
          incProgress(0.15, message = "Almost there!")
          prog<-5
        }
        
        
        temp<-(Epochmax/10)+1
        ##############################################################
        #################### START RECURSIVE PROCESS ###############
        Epoch<-1 #tracks number of epochs
        
        while (Epoch <= Epochmax){
          counter<-1 #tracks number of iterations
          accept<-0
          while (counter <= Iterations){
            ########################## Neighbouring Solution ###################
            
            randomrow<-floor(runif(2, min=1, max=nogroups+1))     # generate neighbouring solution
            randomcol<-floor(runif(2, min=1, max=R$groupsize+1))    # by finding randomly 2 column numbers and 2 row number
            # take element in row number 1 and column number 1
            holdera<-markmatrix[randomrow[1],randomcol[1]]        # and swap with element in row numer 2 and column nuber 2
            holderb<-markmatrix[randomrow[2],randomcol[2]] 
            
            markmatrix[randomrow[1],randomcol[1]]<-holderb        # |
            markmatrix[randomrow[2],randomcol[2]]<-holdera        # |
            # 
            holderc<-Numbers[randomrow[1],randomcol[1]]        # and swap with element in row numer 2 and column nuber 2
            holderd<-Numbers[randomrow[2],randomcol[2]]        # |
            Numbers[randomrow[1],randomcol[1]]<-holderd        # |
            Numbers[randomrow[2],randomcol[2]]<-holderc
            
            R$rand<-c(randomcol, randomrow)
            R$hldr<-c(holdera, holderb)
            
            #### objective 1: diff from global
            for (j in 1:nogroups){
              groupaverage[j]<-mean(markmatrix[j,], na.rm = T)
            }
            for (j in 1:nogroups){
              difromglobal[j]<-abs(globalaverage-groupaverage[j])
            }
            nebsol[1]<-max(difromglobal)
            
            #### objective 2: range in group
            for (j in 1:nogroups){
              grouprange[j]<-(max(markmatrix[j,], na.rm = T)-min(markmatrix[j,], na.rm = T))
            }
            nebsol[2]<-max(grouprange)
            
            
            ###################################################################
            ################# Energy Difference ##########################
            c1<-0
            c2<-0
            
            checkcurrent<-c(archive,nebsol)
            checkneighbour<-c(archive,cursol)
            
            ################# Check current energy ############
            
            i<-1
            track<-0
            
            while (i <= length(checkcurrent)/2){
              
              c1<-checkcurrent[increment[i]]-cursol[1]
              c2<-checkcurrent[increment[i]+1]-cursol[2]
              
              if (c1 == 0){
                if(c2 == 0){
                  track[i]<-"X"
                }
                if(c2 > 0){
                  track[i]<-"Y"
                }
                if(c2 < 0){
                  track[i]<-"X"
                }
              }
              
              if (c1 > 0){
                if(c2 == 0){
                  track[i]<-"Y"
                }
                if(c2 > 0){
                  track[i]<-"Y"
                }
                if(c2 < 0){
                  track[i]<-"Z"
                }
              }
              
              if (c1 < 0){
                if(c2 == 0){
                  track[i]<-"X"
                }
                if(c2 > 0){
                  track[i]<-"Z"
                }
                if(c2 < 0){
                  track[i]<-"X"
                }
              }
              i<-i+1
            }
            
            Ecurrent<-length(which(track == "X"))
            
            ################# Check neighbour energy ############
            
            i<-1
            track<-0
            
            
            while (i <= length(checkneighbour)/2){
              
              c1<-checkneighbour[increment[i]]-nebsol[1]
              c2<-checkneighbour[increment[i]+1]-nebsol[2]
              
              if (c1 == 0){
                if(c2 == 0){
                  track[i]<-"X"
                }
                if(c2 > 0){
                  track[i]<-"Y"
                }
                if(c2 < 0){
                  track[i]<-"X"
                }
              }
              
              if (c1 > 0){
                if(c2 == 0){
                  track[i]<-"Y"
                }
                if(c2 > 0){
                  track[i]<-"Y"
                }
                if(c2 < 0){
                  track[i]<-"Z"
                }
              }
              
              if (c1 < 0){
                if(c2 == 0){
                  track[i]<-"X"
                }
                if(c2 > 0){
                  track[i]<-"Z"
                }
                if(c2 < 0){
                  track[i]<-"X"
                }
              }
              i<-i+1
            }
            Eneighbour<-length(which(track == "X"))
            
            DEnergy<-(Eneighbour-Ecurrent)/(length(archive)/2)
            
            ################### Accept or Reject ####################  
            
            if (DEnergy >=0){
              cursol[1]<-nebsol[1]
              cursol[2]<-nebsol[2]
              accept<-accept+1
            }
            if (DEnergy <  0){
              metropolis<-exp(-DEnergy/temp)
              if (runif(1) < metropolis){
                cursol[1]<-nebsol[1]
                cursol[2]<-nebsol[2]
                accept<-accept+1
              }
            }
            
            ############### Archive sweep #########################  
            
            i<-1
            same<-0
            keep<-0
            reject<-0
            track<-0
            
            ##            ###              
            if (prog == 1){
              incProgress(0.15, message = "Relax, go grab a coffee")
              prog<-2
            }
            if (prog == 3){
              incProgress(0.15, message = "Have you remembered to phone your mom this week?")
              prog<-4
            }
            if (prog == 5){
              incProgress(0.15, message = "Almost there!")
              prog<-6
            }
            
            while (i <= length(archive)/2){
              
              c1<-archive[increment[i]]-cursol[1]
              c2<-archive[increment[i]+1]-cursol[2]
              
              if (c1 == 0){
                if(c2 == 0){
                  track[i]<-"X"
                  reject<-reject+1
                }
                if(c2 > 0){
                  track[i]<-"Y"
                  keep<-keep+1
                }
                if(c2 < 0){
                  track[i]<-"X"
                  reject<-reject+1
                }
              }
              
              if (c1 > 0){
                if(c2 == 0){
                  track[i]<-"Y"
                  keep<-keep+1
                }
                if(c2 > 0){
                  track[i]<-"Y"
                  keep<-keep+1
                }
                if(c2 < 0){
                  track[i]<-"Z"
                  same<-same+1
                }
              }
              
              if (c1 < 0){
                if(c2 == 0){
                  track[i]<-"X"
                  reject<-reject+1
                }
                if(c2 > 0){
                  track[i]<-"Z"
                  same<-same+1
                }
                if(c2 < 0){
                  track[i]<-"X"
                  reject<-reject+1
                }
              }
              i<-i+1
            }
            y<-0 #y check stops duplicates 
            if (keep > 0 & reject == 0){
              change<-which(track == "Y")
              rem<-sort(c(increment[change],increment[change]+1))
              archive<-archive[-rem]
              l<-length(archive)
              archive[l+1]<-cursol[1]
              archive[l+2]<-cursol[2]
              y<-1
              
              Numholder<-Numholder[,-change]
              l2<-length(archive)/2
              hold<-Numbers
              dim(hold)<-c(size,1)
              Numholder<-c(Numholder, rep(0,size))
              dim(Numholder)<-c(size,l2)
              Numholder[,l2]<-hold
              
            }
            if (same > 0 & reject == 0 & y == 0){
              l<-length(archive)
              archive[l+1]<-cursol[1]
              archive[l+2]<-cursol[2]
              
              l2<-length(archive)/2
              hold<-Numbers
              dim(hold)<-c(size,1)
              Numholder<-c(Numholder, rep(0,size))
              dim(Numholder)<-c(size,l2)
              Numholder[,l2]<-hold
            }
            
            counter<-counter+1
            
          }
          Epoch<-Epoch+1
          temp<-0.8*temp
        }
        reheat<-reheat+1
      }
      incProgress(0.05, message = "Finished")
      
      dim(archive)<-c(2,length(archive)/2)
      arc<-rep(0,length(archive))
      dim(arc)<-c(length(archive)/2,2)
      arc[,1]<-archive[1,]
      arc[,2]<-archive[2,]
      A<-as.data.frame(arc)
      colnames(A)<-c("xvar","yvar")
      A1<-A
      colnames(A1)<-c("Performance decline (%)","Contribution decline (%)")
      R$markmatrixpoints<-A1
      R$markmatrix<-A
      R$Numbers<-Numholder
      R$groupamount<-nogroups
      
      
      ###End progress bar
    })
    ###End code observe
  })
  
  observeEvent(input$plot1_click, {
    chosen<-row.names(nearPoints(R$markmatrix, input$plot1_click))
    Numero<-unlist(R$Numbers)
    listing<-rep(0,2*R$groupsize*R$groupamount)
    studentout<-Numero[,as.numeric(chosen)]   # unlist(R$Numbers[,as.numeric(chosen)])
    incre<-seq(1,5000,(R$groupamount))
    increts<-seq(1,5000,R$groupsize)
    for (i in 1:R$groupamount){
      for (j in 1:R$groupsize){
        listing[increts[i]+j-1]<-studentout[incre[j]+i-1]
      }
    }
    
    
    dim(listing)<-c(R$groupsize*R$groupamount,2)
    increms<-seq(1,5000,(R$groupsize))
    for (i in 1:R$groupamount){
      for (j in 1:(R$groupsize-1)){
        listnam<-paste("Group", i)
        listing[increms[i],2]<-listnam
        listing[increms[i]+j,2]<-listnam
      }
    }
    
    listings<-as.data.frame(listing, row.names = FALSE)
    colnames(listings)[1]<-"Member"
    colnames(listings)[2]<-"Group"
    R$studentoutlist<-listings
    
    dim(studentout)<-c(R$groupamount,R$groupsize)
    seq
    st<-as.data.frame(studentout)
    for (i in 1:R$groupsize){
      head<-paste("Member", i)
      colnames(st)[i]<-head
    }
    for (i in 1:R$groupamount){
      left<-paste("Group", i)
      rownames(st)[i]<-left
    }
    
    
    R$studentout<-st
    R$chosen<-chosen
  })
  
  
  
  #####################################################################   
  
  output$Readme <- renderText({paste0("When using the program please note the following:\n - The uploaded file must be of type TabDelimited.txt\n - Only the 'Student Number' and 'Academic Average' columns are used, all other columns are disregarded by the program and may contain any values\n - The data in the used columns must be clean, blank cells will be rejected, cells containing unwanted characters will cause the program to crash\n - All academic averages must be written as percentages e.g. 50.00% (ensure comma [,] is not used as decimal separator)")})
  
  output$studentdata <- renderTable({R$stdntdata})
  
  observeEvent(input$button1, {
    
    output$marksmatrix <- renderTable({R$markmatrixpoints}, rownames = TRUE)
    
    
    output$plot1 <- renderPlot({
      ggplot(data = R$markmatrix, aes(x=xvar, y=yvar)) + geom_point(shape=21, color = "black", fill = "darkblue", size = 5) + labs(x = "Performance decline (%)", y = "Contribution decline (%)") + theme(axis.text=element_text(size=12),
                                                                                                                                                                                                          axis.title=element_text(size=14,face="bold"))
    })
    output$chose <- renderText({paste0("Chosen solution: \n", R$chosen)})
    output$cl <- renderText({paste0(nearPoints(R$markmatrix, input$plot1_click))})
    
    output$head <- renderText({paste0('<font size = "+1"><u><B>Chosen group assignment:</B></u></font>')})
    output$num <- renderTable({R$studentout}, rownames = TRUE, digits = 0)
    
  })  
  
  dataset <- reactive({R$studentoutlist})
  dataseta <- reactive({R$studentout})
  
  
  output$downloadData2 <- downloadHandler(
    filename = function() {
      paste("groupassignment", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(dataseta(), file)
    }
  )
  
  output$downloadData1 <- downloadHandler(
    filename = function() {
      paste("groupassignment", "csv", sep = ".")
    },
    content = function(file) {
      write.csv(dataset(), file)
    }
  )
  
  
}

shinyApp(ui, server)

