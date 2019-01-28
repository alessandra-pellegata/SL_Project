library(shiny)
library(leaflet)

library(magrittr)
library(shinythemes)
library(shinyjs)
library(sp)
library(lattice)

library(shinythemes)

library(plotrix)
library(ggplot2)
library(plotly)
library(DiagrammeR)

library(RGraphics)
library(gridExtra)
library(htmltools)

data5 <- read.csv("https://github.com/alessandra-pellegata/SL_Project/raw/master/meteorite_definitivo.csv")
data5 <- data5[,-1]

comp_conf <- read.csv("https://raw.githubusercontent.com/alessandra-pellegata/SL_Project/master/composizione_meteoriti.csv")
rownames(comp_conf) <- comp_conf$X
comp_conf <- comp_conf[,-1]
comp_conf2<-as.data.frame(t(comp_conf))
rownames(comp_conf2)<-c("O","Fe","Si","Mg","S","Ca","Ni","Al",
                        "Na","Cr","C","Altro")

daticlass <- read.csv("https://github.com/alessandra-pellegata/SL_Project/raw/master/descrizioni_classi_meteoriti.csv", sep=";", encoding="UTF-8")




p1<-shinyUI(
  navbarPage("METEORITI, IL CIELO SULLA TERRA",theme = shinytheme("flatly"),
# UI PRIMA PAGINA ----
             tabPanel("Panoramica",
                        tabsetPanel(
                          tabPanel("Mappa",
                                   br(),
                                   fluidRow(
                                     column(12,textOutput('text1'), tags$head(tags$style("#text1{color: black;
                                                                                          font-size: 18px;
                                                                                          font-style: italic;
                                                                                          text-align:center}")))),
                                   fluidRow(
                                     column(12,textOutput('text2'), tags$head(tags$style("#text2{color: darkgray;
                                                                                                font-size: 15px;
                                                                                                font-style: italic;
                                                                                                text-align:center}")))
                                     
                                     ),#chiuso fluidrow
                                   br(),
                                   fluidRow(
                                     column(2),
                                     column(8,leafletOutput("map0")), 
                                     column(2)),
                                   fluidRow(
                                     column(8),
                                     column(4, align="right", "Alessandra Pellegata", br(), "Isabella Bessone",br(), "Lorenzo Gregori", br(), "Università degli Studi di Milano - Bicocca")
                                   )),
                          
                          ####
             
                          tabPanel("Dataset",
                                   # Application title
                                   #tags$h2("Informations"),
                                   p("Il ",
                                     tags$a(href="https://www.kaggle.com/nasa/meteorite-landings/home", "dataset"),
                                     "è disponibile online e raccoglie dati forniti dalla",
                                     tags$a(href="https://www.nasa.gov/", "NASA"), "riguardo i meteoriti caduti negli ultimi 269 anni."),
                                   hr(),
                                   #fluidRow(
                                   #column(12,align="center",textOutput('info'), tags$head(tags$style("#info{color: hsl(40%, 120, 100%);
                                   #                                                       font-size: 25px
                                   #                                                     }")))),
                                   p(textOutput('info_des'), tags$head(tags$style("#info_des{color: black;
                                                                                  font-size: 15px
                                                                                  }"))),
                                   fluidRow(
                                     column(12,textOutput('info_des2'), tags$head(tags$style("#info_des2{color: black;
                                                                                             font-size: 15px
                                                                                             }")))),
                                   br(),
                                   fluidRow(
                                     column(12,align="center",textOutput('desvars'), tags$head(tags$style("#desvars{color: hsl(40%, 120, 100%);
                                                                                                          font-size: 25px
                                                                                                          }")))),
                                   fluidRow(
                                     column(12,textOutput('frase'), tags$head(tags$style("#frase{color: black;
                                                                                         font-size: 15px;
                                                                                         font-weight: ligther                                                                                              font-weight: ligther
                                                                                         }")))),
                                   br(),
                                   
                                   fluidRow(
                                     column(3,align="center",textOutput('var1'), tags$head(tags$style("#var1{color: black;
                                                                                                      font-size: 15px;
                                                                                                      font-weight: bold
                                                                                                      }"))),
                                     column(9,textOutput('desvar1'), tags$head(tags$style("#desvar1{color: black;
                                                                                          font-size: 15px;
                                                                                          font-weight: ligther                                                                                              font-weight: ligther
                                                                                          }")))),
                                    br(),
                                   fluidRow(
                                     column(3,align="center",textOutput('var2'), tags$head(tags$style("#var2{color: black;
                                                                                                      font-size: 15px;
                                                                                                      font-weight: bold
                                                                                                      }"))),
                                     column(9,textOutput('desvar2'), tags$head(tags$style("#desvar2{color: black;
                                                                                          font-size: 15px;
                                                                                          font-weight: ligther                                                                                          font-weight: ligther
                                                                                          }")))),
                                   br(),
                                   fluidRow(
                                     column(3,align="center",textOutput('var3'), tags$head(tags$style("#var3{color: black;
                                                                                                      font-size: 15px;
                                                                                                      font-weight: bold
                                                                                                      }"))),
                                     column(9,textOutput('desvar3'), tags$head(tags$style("#desvar3{color: black;
                                                                                          font-size: 15px;
                                                                                          font-weight: ligther                                                                                          font-weight: ligther
                                                                                          }")))),
                                   br(),
                                   fluidRow(
                                     column(3,align="center",textOutput('var4'), tags$head(tags$style("#var4{color: black;
                                                                                                      font-size: 15px;
                                                                                                      font-weight: bold
                                                                                                      }"))),
                                     column(9,textOutput('desvar4'), tags$head(tags$style("#desvar4{color: black;
                                                                                          font-size: 15px;
                                                                                          font-weight: ligther                                                                                          font-weight: ligther
                                                                                          }")))),
                                   br(),
                                   fluidRow(
                                     column(3,align="center",textOutput('var5'), tags$head(tags$style("#var5{color: black;
                                                                                                      font-size: 15px;
                                                                                                      font-weight: bold
                                                                                                      }"))),
                                     column(9,textOutput('desvar5'), tags$head(tags$style("#desvar5{color: black;
                                                                                          font-size: 15px;
                                                                                          font-weight: ligther
                                                                                          }")))),
                                   br(),
                                   fluidRow(
                                     column(3,align="center",textOutput('var6'), tags$head(tags$style("#var6{color: black;
                                                                                                      font-size: 15px;
                                                                                                      font-weight: bold
                                                                                                      }"))),
                                     column(9,textOutput('desvar6'), tags$head(tags$style("#desvar6{color: black;
                                                                                          font-size: 15px;
                                                                                          font-weight: ligther
                                                                                          }")))),
                                   br(),
                                   fluidRow(
                                     column(3,align="center",textOutput('var7'), tags$head(tags$style("#var7{color: black;
                                                                                                      font-size: 15px;
                                                                                                      font-weight: bold
                                                                                                      }"))),
                                     column(9,textOutput('desvar7'), tags$head(tags$style("#desvar7{color: black;
                                                                                          font-size: 15px;
                                                                                          font-weight: ligther
                                                                                          }")))),
                                   br(),
                                   fluidRow(
                                     column(3,align="center",textOutput('var8'), tags$head(tags$style("#var8{color: black;
                                                                                                      font-size: 15px;
                                                                                                      font-weight: bold
                                                                                                      }"))),
                                     column(9,textOutput('desvar8'), tags$head(tags$style("#desvar8{color: black;
                                                                                          font-size: 15px;
                                                                                          font-weight: ligther
                                                                                          }")))),
                                   br(),
                                   fluidRow(
                                     column(3,align="center",textOutput('var9'), tags$head(tags$style("#var9{color: black;
                                                                                                      font-size: 15px;
                                                                                                      font-weight: bold
                                                                                                      }"))),
                                     column(9,textOutput('desvar9'), tags$head(tags$style("#desvar9{color: black;
                                                                                          font-size: 15px;
                                                                                          font-weight: ligther
                                                                                          }")))),
                                   br(),
                                   fluidRow(
                                     column(3,align="center",textOutput('var10'), tags$head(tags$style("#var10{color: black;
                                                                                                       font-size: 15px;
                                                                                                       font-weight: bold
                                                                                                       }"))),
                                     column(9,textOutput('desvar10'), tags$head(tags$style("#desvar10{color: black;
                                                                                           font-size: 15px;
                                                                                           font-weight: ligther
                                                                                           }")))),
                                   br(),
                                   br()
                                  
                                ),
                          # ui III sottopagina ----
                          
                          tabPanel("Analisi",
                                   fluidRow(
                                     column(3,align="center",textOutput('prec'), tags$head(tags$style("#prec{color: hsl(40%, 120, 100%);
                                                                                                      font-size: 25px
                                                                                                      }"))),
                                 column(9,
                                        tags$div(
                                          tags$ol(
                                            tags$li("Analisi dei missing value"),
                                            tags$li("Arco temporale considerato: da 1900 a 2012"),
                                            tags$li("Eliminate osservazioni con masse negative o nulle"),
                                            tags$li("Considerati punti spaziali con valori di latitudine e longitudine validi"),
                                            tags$li("Creazione di una nuova variabile con informazioni circa il numero di frammenti in cui si sono scomposti i meteoriti più grandi nell’impatto con l’atmosfera terrestre")
                                          ),style = c("color: black","font-size: 15px","font-weight: ligther"))
                                     )),
                                 
                                 fluidRow(
                                   column(3),
                                   column(9, textOutput("finepreprocess"), tags$head(tags$style("#finepreprocess{color: black;
                                                                                      font-size: 15px;
                                                                                      font-weight: ligther
                                                                                      }")))
                                 ),
                                 br(),
                                 fluidRow(
                                   column(3,align="center",textOutput('an1'), tags$head(tags$style("#an1{color: hsl(40%, 120, 100%);
                                                                                                   font-size: 25px
                                                                                                   }"))),
                                 column(9,textOutput('an1_des'), tags$head(tags$style("#an1_des{color: black;
                                                                                      font-size: 15px;
                                                                                      font-weight: ligther
                                                                                      }")))
                               ),
                               br(),
                               fluidRow(
                                 column(3),
                                 column(9, align="center",plotlyOutput('stats_year'))
                               ),
                               br(),
                               fluidRow(
                                 column(3),
                                 column(9, textOutput('an1_des2'), tags$head(tags$style("#an1_des2{color: black;
                                                                                      font-size: 15px;
                                                                                       font-weight: ligther
                                                                                       }")))
                               ),
                               br(),
                               fluidRow(
                                 column(3,align="center",textOutput('an2'), tags$head(tags$style("#an2{color: hsl(40%, 120, 100%);
                                                                                                 font-size: 25px
                                                                                                 }"))),
                                 column(9,textOutput('an2_des'), tags$head(tags$style("#an2_des{color: black;
                                                                                      font-size: 15px;
                                                                                      font-weight: ligther
                                                                                      }")))
                               ),
                               br(),
                               fluidRow(
                                 column(3),
                                 column(9, align="center",plotOutput('stats_mass'))
                               ),
                               br(),
                               fluidRow(
                                 column(3),
                                 column(9, textOutput('an2_des2'), tags$head(tags$style("#an2_des2{color: black;
                                                                                        font-size: 15px;
                                                                                        font-weight: ligther
                                                                                        }")))),
                               br(),
                               fluidRow(
                                 column(3,align="center",textOutput('an3'), tags$head(tags$style("#an3{color: hsl(40%, 120, 100%);
                                                                                                 font-size: 25px
                                                                                                 }"))),
                                 column(9,textOutput('an3_des'), tags$head(tags$style("#an3_des{color: black;
                                                                                      font-size: 15px;
                                                                                      font-weight: ligther
                                                                                      }")))
                               ),
                               br(),
                               fluidRow(
                                 column(3),
                                 column(9, grVizOutput("diagramme"))),
                               br(),
                               fluidRow(
                                 column(3),
                                 column(9,textOutput('an3_des2'), tags$head(tags$style("#an3_des2{color: black;
                                                                                      font-size: 15px;
                                                                                      font-weight: ligther
                                                                                      }")))
                               ),
                               br(),
                               fluidRow(
                                 column(3),
                                 column(9, align="center",plotOutput('stats_compT'))
                               ),
                               br(),
                               fluidRow(
                                 column(3),
                                 column(9,textOutput('data_new'), tags$head(tags$style("#data_new{color: black;
                                                                                       font-size: 15px;
                                                                                       font-weight: ligther
                                                                                       }")))
                               ),
                               br(),
                               fluidRow(
                                 column(3),
                                 column(9, align="center",tableOutput('an3_data'))
                               ),
                               br(),
                               fluidRow(
                                 column(3),
                                 column(9, textOutput('an3_des3'),tags$head(tags$style("#an3_des3{color: black;
                                                                                                        font-size: 15px;
                                                                                                      font-weight: ligther
                                                                                                      }")))
                               ),
                               br(),
                               br()
                               
                                 )#chiudo tabpanel
                      )),#chiudo tabset/tabpanel
                      
# UI SECONDA PAGINA ----
             
             tabPanel("Scegli l'anno", 
                  fluidPage(
                      fluidRow(
                        column(4,
                               fluidRow(h3("Help text"),
                                        helpText("In questa pagina seleziona una fascia annua per visualizzare sulla mappa i meteoriti scoperti durante quell'arco
                                                 temporale. Nella parte inferiore della pagina visualizza la distribuzione dei meteoriti nel periodo dal 1900 al 2012.
                                                 Puoi notare qualche cambiamento?")),
                               sliderInput("range", h3("Arco temporale"),
                                           min = min(data5$year) , max = max(data5$year),
                                           value = range(data5$year),step=1)
                               
                      ),
                      
                      column(8,
                             leafletOutput('map2'))
                      ),
                      br(),
                      br(),
                      fluidRow(
                        column(2,
                               fluidRow(plotOutput("pie"))),
                        column(10,plotlyOutput('histanni'))
                      )
                      )),
             
# UI PAGINA MASS ----
             tabPanel("Scegli la massa",
                      tabsetPanel(
                        tabPanel("Analisi",
                                 fluidPage(
                                 fluidRow(
                                   column(4,
                                          fluidRow(h3("Help text"),
                                                   helpText("In questa pagina seleziona la massa dei meteoriti che vuoi rappresentare sulla mappa. Nella sottosezione Top 5 puoi trovare curiosità e informazioni sui 5 meteoriti più grandi dell'ultimo secolo")),
                                          checkboxGroupInput("masss", 
                                                                 h3("Mass"), 
                                                                 choices = list("Meno di 1 kg" = 1 , 
                                                                                "Da 1 a 5 kg" = 2,
                                                                                "Da 5 a 10 kg" = 3 ,
                                                                                "Da 10 a 20 kg"= 4,
                                                                                "Da 20 a 100 kg" = 5,
                                                                                "Più di 100 kg" = 6))),
                                  
                                    column(8, leafletOutput("map_mass")))
                                 
                                 )), #chiude primo
                        tabPanel("Top 5",
                                 fluidRow(
                                   column(12,align="center",textOutput('cur_mass'), tags$head(tags$style("#cur_mass{color: hsl(40%, 120, 100%);
                                                                                                         font-size: 25px
                                                                                                         }")))),
                                 br(),
                                 fluidRow(
                                   column(3,align="center",tags$b("HOBA",tags$br(),"Namibia", tags$br(),"60.000 kg"), style = c("color: hsl(40%, 120, 100%)","font-size: 25px")),
                                   
                                   column(9,textOutput('hoba_des'), tags$head(tags$style("#hoba_des{color: black;
                                                                                         font-size: 15px;
                                                                                         font-weight: ligther
                                                                                         }")),
                                   p("Clicca ",
                                     tags$a(href="https://it.wikipedia.org/wiki/Hoba", "QUI"),
                                     "per saperne di più su Hoba."),style=c("color:black","font-size:15px"))),
                                 
                                 br(),
                                 fluidRow(
                                   column(3,align="center",tags$b("MUNDRABILLA", tags$br(),"Australia",tags$br(),"24.000 kg"), style = c("color: hsl(40%, 120, 100%)","font-size: 25px")),
                                   column(9,textOutput('mund_des'), tags$head(tags$style("#mund_des{color: black;
                                                                                         font-size: 15px;
                                                                                         font-weight: ligther
                                                                                         }")), 
                                          p("Clicca ",
                                            tags$a(href="https://en.wikipedia.org/wiki/Mundrabilla_(meteorite)", "QUI"),
                                            "per saperne di più su Mundrabilla."),style=c("color:black","font-size:15px"))),
                                 br(),
                                 fluidRow(
                                   column(3,align="center",tags$b("SIKHOTE-ALIN",tags$br(),"Siberia",tags$br(),"23.000 kg"), style = c("color: hsl(40%, 120, 100%)","font-size: 25px")),
                                   column(9,textOutput('sic_des'), tags$head(tags$style("#sic_des{color: black;
                                                                                        font-size: 15px;
                                                                                        font-weight: ligther
                                                                                        }")),
                                          p("Clicca ",
                                            tags$a(href="https://it.wikipedia.org/wiki/Sikhote-Alin_(meteorite)", "QUI"),
                                            "per saperne di più su Sikhote-Alin"),style=c("color:black","font-size:15px"))),
                                 
                                 br(),
                                 fluidRow(
                                   column(3,align="center",tags$b("MBOZI",tags$br(),"Tanzania",tags$br(),"16.000 kg"), style = c("color: hsl(40%, 120, 100%)","font-size: 25px")),
                                   column(9,textOutput('mbo_des'), tags$head(tags$style("#mbo_des{color: black;
                                                                                        font-size: 15px;
                                                                                        font-weight: ligther
                                                                                        }")),
                                          p("Clicca ",
                                            tags$a(href="https://en.wikipedia.org/wiki/Mbozi_meteorite", "QUI"),
                                            "per saperne di più su Mbozi."),style=c("color:black","font-size:15px"))),
                                 br(),
                                 fluidRow(
                                   column(3,align="center",tags$b("WILLAMETTE",tags$br(),"Oregon",tags$br(),"15.500 kg"), style = c("color: hsl(40%, 120, 100%)","font-size: 25px")),
                                   column(9,textOutput('wil_des'), tags$head(tags$style("#wil_des{color: black;
                                                                                        font-size: 15px;
                                                                                        font-weight: ligther
                                                                                        }")),
                                          p("Clicca ",
                                            tags$a(href="https://it.wikipedia.org/wiki/Willamette_(meteorite)", "QUI"),
                                            "per saperne di più su Willamette."),style=c("color:black","font-size:15px"))),
                                 br()
                                 
                                 
                                 
                                 )
                        
                                 ) #tabsetpaneÃ²
                                   ),
# UI QUINTA PAGINA ----
             tabPanel("Scegli la categoria",
                      fluidPage(
                        fluidRow(
                          column(4,
                                 fluidRow(h3("Help text"),
                                          helpText("In questa pagina seleziona la tipologia di meteorite per ottenere una breve descrizione della classe e
                                                  rappresentare i punti sulla mappa interattiva. Nella parte inferiore della pagina potrete trovare due piechart
                                                   che comparano la composizione della roccia terrestre con quella della tipologia di meteorite selezionata e
                                                    un istogramma che ne mostra la distribuzione negli anni.
                                                   ")),
                                 
                                 fluidRow(selectInput("class", 
                                                      h3("Tipologia di class"), 
                                                      choices = list("AcondritiVarie" = "AcondritiVarie", 
                                                                     "Carboniose" = "Carboniose",
                                                                     "AltreCondriti" = "AltreCondriti",
                                                                     "Entasiti" = "Entasiti",
                                                                     "Sideriti" = "Sideriti",
                                                                     "Lunar" = "Lunar",
                                                                     "Martian" = "Martian",
                                                                     "OrdinarieH" = "OrdinarieH",
                                                                     "OrdinarieL" = "OrdinarieL",
                                                                     "OrdinarieLL" = "OrdinarieLL",
                                                                     "Sideroliti" = "Sideroliti",
                                                                     "Stone-uncl" = "Stone-uncl"),selectize=F)),
                                 fluidRow(textOutput("descrizione"))),#chiudo sidebarpanel
                          column(8,leafletOutput('map5'))),
                        
                        br(),
                        fluidRow(
                          column(6, plotOutput('torta_tipo')),
                          column(6, plotlyOutput('istogramma_tipo'))
                        ),
                        br()
      )),

# UI SVILUPPI FUTURI

      tabPanel("Sviluppi futuri",
               fluidPage(
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 br(),
                 fluidRow(
                   column(2, br(), br(), tags$img(height=100,width=100,align="right",src="https://cdn4.iconfinder.com/data/icons/natural-disaster/80/Natural_disaster-03-512.png")),
                   column(8,
                          tags$li("Un ulteriore approfondimento dell'analisi potrebbe consistere nel verificare se esiste correlazione tra il luogo dove sono stati ritrovati
                                  i meteoriti e le loro caratteristiche chimico-fisiche."),
                          br(),
                          br(),
                          tags$li("Calcolare un indice di sicurezza per ogni stato o continente basato sull'indice di Moran, che misura l'autocorrelazione spaziale di un fenomeno"),
                          br(),
                          br(),
                          tags$li("L'app può essere utilizzata per analizzare fenomeni che avvengono nello spazio (ad esempio terremoti, eruzioni vulcaniche)")
                   ),
                   column(2, br(), br(), tags$img(height=100,width=100,align="left",src="https://cdn4.iconfinder.com/data/icons/natural-disaster/80/Natural_disaster-03-512.png"))
                 )
               ))
))

####

p2<-shinyServer(
   function(input, output, session) {
# PAGINA INIZIALE ----
     # PRIMA SOTTOPAGINA
     output$map0 <- renderLeaflet({
       datatot = data.frame(
         lat = data5$reclat,
         lng = data5$reclong,
         name = data5$name,
         mass = data5$mass,
         fall = data5$fall,
         year = data5$year,
         recclass = data5$recclass,
         numframmenti = data5$numframmenti
       )
       popInfo=paste("Name:","<b>",datatot$name,"</b>","<br>",
                     "Year:","<b>",datatot$year,"</b>","<br>",
                     "Fall:","<b>",datatot$fall,"</b>","<br>",
                     "Mass:","<b>",datatot$mass,"</b>","<br>",
                     "Class:","<b>",datatot$recclass,"</b>","<br>") %>%
         lapply(htmltools::HTML)
       MeteoriteIcon<-makeIcon(
         iconUrl = "https://cdn4.iconfinder.com/data/icons/natural-disaster/80/Natural_disaster-03-512.png",
         iconWidth = 20,iconHeight = 20
       )
       
       m2 = leaflet(data=datatot) %>%
         addTiles() %>%
         addMarkers(lat = ~ lat, lng = ~ lng , icon = MeteoriteIcon , label= popInfo ,clusterOptions = markerClusterOptions(),
                          labelOptions = labelOptions(noHide=F, direction='auto', textsize="11px")) %>%
         addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
         setView(30,31,zoom = 1)

     })
     
     output$text1 <- renderText({ " \"Ci vediamo domani? Sempre che il cielo non ci cada sulla testa\" " })
     output$text2 <- renderText({ "Saluto dei Galli nel fumetto Asterix di René Goscinny" })
     
     # SECONDA SOTTOPAGINA
     
     output$info <- renderText({ "Informazioni" })
     output$info_des <- renderText({ "Numero di osservazioni: 45716" })
     output$info_des2 <- renderText({ "Numero di variabili: 10" })
     
     output$desvars <- renderText({ "Variabili" })
     output$frase <- renderText({ "Il dataset contiene le seguenti variabili:" })
     
     output$var1 <- renderText({ "name" })
     output$desvar1 <- renderText({ "Il nome del meteorite (tipicamente la posizione di ritrovamento, spesso aggiornata da un numero.)" })
     
     output$var2 <- renderText({ "id" })
     output$desvar2 <- renderText({ "Un numero univoco che identifica il meteorite" })
     
     output$var3 <- renderText({ "nametype" })
     output$desvar3 <- renderText({ "Può assumere 2 classi: -- valid: un classico meteorite -- relict: un meteorite che è stato deteriorato dalle condizioni climatiche terrestri" })
     
     output$var4 <- renderText({ "recclass" })
     output$desvar4 <- renderText({ "La classe del meteorite; la suddivisione è basata su caratteristiche fisiche e chimiche" })
     
     output$var5 <- renderText({ "mass" })
     output$desvar5 <- renderText({ "La massa del meteorite, espressa in grammi" })
     
     output$var6 <- renderText({ "fall" })
     output$desvar6 <- renderText({ "Può assumere 2 classi: -- Fell: la caduta del meteorite è stata osservata -- Found: la caduta del meteorite non è stata osservata" })
     
     output$var7 <- renderText({ "year" })
     output$desvar7 <- renderText({ "L'anno in cui il meteorite è stato scoperto" })
     
     output$var8 <- renderText({ "reclat" })
     output$desvar8 <- renderText({ "La latitudine del luogo dove è stato ritrovato il meteorite" })
     
     output$var9 <- renderText({ "reclong" })
     output$desvar9 <- renderText({ "La longitudine del luogo dove è stato ritrovato il meteorite" })
     
     output$var10 <- renderText({ "GeoLocation" })
     output$desvar10 <- renderText({ "Variabile che combina latitudine e longitudine" })
     
     # III sottopagina ----
     
     output$prec <- renderText({ "Preprocessing" })
     
     output$finepreprocess <- renderText({"Alla fine del preprocessing il dataset contiene 16091 osservazioni e 11 variabili." })
     
     output$an1 <- renderText({ "Analisi temporale" })
     output$an1_des <- renderText({ "Negli anni più recenti, l’attenzione rivolta alle rocce spaziali è aumentata notevolmente. Nella sezione “Scegli l’anno” è possibile selezionare l’arco temporale di interesse e grazie alla mappa interattiva viene visualizzata la posizione dei meteoriti. Grazie agli strumenti tecnologici avanzati a disposizione degli scienziati è possibile segnalare l’arrivo di un meteorite sulla Terra e stimarne l’istante e il luogo dell’impatto. Nel grafico sottostante si osserva la crescita negli anni del numero di meteoriti trovati sul suolo terrestre. " })
     
     output$stats_year <- renderPlotly({ dt5<-data.frame(table(data5$year))
     colnames(dt5)<-c("Year","Frequency")
     f<-ggplot(data=dt5, aes(x=Year, y=Frequency)) +
       geom_bar(stat="identity",fill="steelblue")+ 
       theme(axis.text.x = element_text(angle = 90, hjust = 1))
     ggplotly(f)
     })
     
     output$an1_des2 <- renderText({"Un’altra variabile rilevante ai fini dell’analisi è “fall” che fornisce informazioni in merito alla tipologia del ritrovamento del meteorite. In “Scegli l’anno” è disponibile un grafico a torta che permette di visualizzare la percentuale, per gli anni selezionati, dei meteoriti trovati e di quelli avvistati. Si può notare come, avvicinandosi ai giorni nostri, il numero di meteoriti ritrovati sia cresciuto significativamente. Il fenomeno può essere collegato alla volontà dei ricercatori di studiare e approfondire tutto ciò che concerne l’Universo."})
     
     output$an2 <- renderText({ "Analisi proprietà fisiche" })
     output$an2_des <- renderText({ " Il focus si è poi spostato all’analisi della dimensione dei meteoriti sfruttando la variabile “mass” presente nel dataset. Si è deciso di raggruppare i meteoriti in classi discriminando per la loro massa. Nel grafico sottostante è possibile avere una panoramica sulla numerosità di meteoriti in ogni classe.
A favore di questa decisione vi è la necessità di individuare i meteoriti con massa maggiore e approfondire i danni da loro creati. La maggior parte dei meteoriti ritrovati sulla Terra è infatti di dimensione esigua poiché sono frammenti dovuti all’impatto del corpo celeste con l’atmosfera terrestre.  
       " })
     
     output$stats_mass <- renderPlot({ 
       dt6<-data.frame(table(data5$numMass))
       colnames(dt6)<-c("Mass","Frequency")
       levels(dt6$Mass)<-c("Meno di 1 kg","Tra 1 e 5 kg","Tra 5 e 10 kg","Tra 10 e 20 kg","Tra 20 e 100 kg","Più di 100 kg")
       ggplot(data=dt6, aes(x=Mass, y=Frequency)) +
         geom_bar(stat="identity",fill="steelblue") +
         geom_text(aes(label=Frequency), vjust=-0.3, size=4)+
         theme(axis.text.x = element_text(size=13))
     }) 
     
     output$an2_des2 <- renderText({ "Sono inoltre disponibili curiosità sui 5 meteoriti più grandi ritrovati sul suolo terrestre."})
     
     output$an3 <- renderText({ "Analisi dati composizionali" })
     output$an3_des <- renderText({ "L’analisi dei dati composizionali consente di affrontare problemi che richiedano la conoscenza della composizione elementale e chimica di un generico fenomeno. I meteoriti sono divisi in tre macro-classi, Aeroliti, Sideriti e Sideroliti, le quali a loro volta sono strutturate in sottocategorie. 
Ai fini dell’analisi si è scelto di considerare unicamente le sotto-categorie in cui si suddividono gli Aeroliti, in quanto rappresentano la maggior parte dei meteoriti scoperti sulla Terra. 
       " })
     
     output$diagramme <- renderGrViz({ grViz("
                                                  digraph box_and_circle{
                                                  
                                                  node[shape=box
                                                  penwidth=2.0
                                                  fontname=Arial
                                                  style=filled]
                                                  
                                                  
                                                  node[fillcolor=snow]
                                                  CONDRITI;ACONDRITI;AEROLITI; METEORITI; NON_CLASSIFICATI
                                                  
                                                  node[fillcolor=red]
                                                  AcondritiVarie
                                                  node[fillcolor=orange]
                                                  Carboniose
                                                  node[fillcolor=yellow3]
                                                  AltreCondriti
                                                  node[fillcolor=darkolivegreen2]
                                                  Entasiti
                                                  node[fillcolor=green1]
                                                  SIDERITI
                                                  node[fillcolor=darkseagreen]
                                                  Lunar
                                                  node[fillcolor=mediumpurple3]
                                                  Martian
                                                  node[fillcolor=slateblue]
                                                  OrdinarieH
                                                  node[fillcolor=blueviolet]
                                                  OrdinarieL
                                                  node[fillcolor=mediumorchid3]
                                                  OrdinarieLL
                                                  node[fillcolor=plum]
                                                  SIDEROLITI
                                                  node[fillcolor=lightpink]
                                                  Stoneuncl
                                                  
                                                  edge[arrowhead=vee]
                                                  
                                                  METEORITI->NON_CLASSIFICATI;
                                                  METEORITI->AEROLITI;METEORITI->SIDEROLITI;METEORITI->SIDERITI;
                                                  AEROLITI->CONDRITI; AEROLITI->ACONDRITI;
                                                  CONDRITI->OrdinarieH; CONDRITI->OrdinarieL;CONDRITI->OrdinarieLL;CONDRITI->Carboniose;CONDRITI->AltreCondriti;CONDRITI->Entasiti;
                                                  ACONDRITI->Lunar; ACONDRITI->Martian;ACONDRITI->AcondritiVarie;
                                                  NON_CLASSIFICATI->Stoneuncl
                                                  
                                                  graph[nodesep=0.2
                                                  rankdir=LR]
     }")

   })
     
     output$an3_des2 <- renderText({ "Di seguito la rappresentazione della quantità di meteoriti presenti in ogni classe. OrdinarieH e OrdinarieL costituiscono circa l’80% dei corpi celesti caduti sulla Terra. "}) 
     
     output$stats_compT <- renderPlot({ dt7<-data.frame(table(data5$class))
     names(dt7)<-c("class","Frequency")
     
     pal <- colorNumeric(c("red", "green", "blue","pink"), 1:12)
     dt7$colorsMat<-NA
     for (i in 1:nrow(dt7)){
       if (dt7$class[i]=="AcondritiVarie"){dt7$colorsMat[i]<-pal(1)}
       else if(dt7$class[i]=="Carboniose"){dt7$colorsMat[i]<-pal(2)}
       else if(dt7$class[i]=="AltreCondriti"){dt7$colorsMat[i]<-pal(3)}
       else if(dt7$class[i]=="Entasiti"){dt7$colorsMat[i]<-pal(4)}
       else if(dt7$class[i]=="Sideriti"){dt7$colorsMat[i]<-pal(5)}
       else if(dt7$class[i]=="Lunar"){dt7$colorsMat[i]<-pal(6)}
       else if(dt7$class[i]=="Martian"){dt7$colorsMat[i]<-pal(7)}
       else if(dt7$class[i]=="OrdinarieH"){dt7$colorsMat[i]<-pal(8)}
       else if(dt7$class[i]=="OrdinarieL"){dt7$colorsMat[i]<-pal(9)}
       else if(dt7$class[i]=="OrdinarieLL"){dt7$colorsMat[i]<-pal(10)}
       else if(dt7$class[i]=="Sideroliti"){dt7$colorsMat[i]<-pal(11)}
       else if(dt7$class[i]=="Stone-uncl"){dt7$colorsMat[i]<-pal(12)}
     }
     ggplot(data=dt7, aes(x=class, y=Frequency),cex.axis=2) +
       geom_bar(stat="identity",fill=dt7$colorsMat) +
       geom_text(aes(label=paste(round(Frequency/sum(Frequency)*100,1),"%")), vjust=-0.3, size=5)+
       theme(axis.text.x = element_text(size=13))
     
     })
     
     output$data_new <- renderText({ "La suddivisione dei meteoriti nelle classi avviene sulla base della loro composizione chimica. Per poter evidenziare le differenze nelle composizioni delle classi, è stato creato un nuovo dataset contenente le caratteristiche chimico-fisiche che le contraddistinguono." })
     output$an3_data <- renderTable({ comp_conf2}, rownames =T)
     output$an3_des3 <- renderText({"Nella sezione “Scegli la categoria” è possibile selezionare la categoria che si intende approfondire. Viene fornito di conseguenza il confronto con le rocce terrestri per evidenziarne le differenze e analogie, oltre alla distribuzione di tale tipologia di meteorite negli anni."})
       
     
# PAGINA YEAR
     
     output$map2 <- renderLeaflet({ 
       df21=data5[which(data5$year>=input$range[1] & data5$year<=input$range[2]),]
       df31 = data.frame(
         lat = df21$reclat,
         lng = df21$reclong,
         name = df21$name,
         mass = df21$mass,
         fall = df21$fall,
         year = df21$year,
         recclass = df21$recclass,
         colors = df21$colors)
       popInfo=paste("Name:","<b>",df31$name,"</b>",br(),"Year:","<b>",df31$year,"</b>")
       m = leaflet(df31) %>% 
         addTiles("http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png") %>%
         addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
         addCircleMarkers(lng = df31$lng, lat = df31$lat,  weight=1, opacity=1,
                          fillOpacity=1, radius= 1,color = df31$colors, popup = popInfo) %>% setView(30,31,zoom=1)
         
     })
     
     output$pie <- renderPlot({
       df22=data5[which(data5$year>=input$range[1] & data5$year<=input$range[2]),]
       df32<-data.frame(
         lat = df22$reclat,
         lng = df22$reclong,
         name = df22$name,
         mass = df22$mass,
         fall = df22$fall,
         year = df22$year,
         recclass = df22$recclass)
       slices <- c(length(which(df32$fall=="Fell")), length(which(df32$fall=="Found")))
       lbls <- c("Fell", "Found")
       pct <- round(slices/sum(slices)*100)
       lbls <- paste(lbls, pct) # add percents to labels
       lbls <- paste(lbls,"%",sep="") # ad % to labels
       pie(slices,labels = lbls, col=c("darkred","forestgreen"),
           main="") 
     })
     
     output$histanni <- renderPlotly({
       dt5<-data.frame(table(data5$year,data5$fall))
       colnames(dt5)<-c("Year","Fall","Frequency")
       pal<- colorNumeric(c("red", "green"), 1:2)
       p<-ggplot(data=dt5, aes(x=Year, y=Frequency,fill=Fall)) +
         geom_bar(stat="identity")+ 
         theme(axis.text.x = element_text(angle = 90, hjust = 1))+
         scale_fill_manual(values=c('#8B0000','#228B22'))
       
       ggplotly(p)
     })
     
# PAGINA MASS ----
     
     output$map_mass<- renderLeaflet({
       dt78=data5[which(data5$numMass %in% input$masss),]
       datatot3 = data.frame(
         lat = dt78$reclat,
         lng = dt78$reclong,
         name = dt78$name,
         mass = dt78$mass,
         fall = dt78$fall,
         year = dt78$year,
         recclass = dt78$recclass,
         colors = dt78$colors,
         numMass = dt78$numMass,
         class = dt78$class
       )
       popInfo=paste("Name:","<b>",datatot3$name,"</b>","<br>","Year:","<b>",datatot3$year,"</b>","<br>",
         "Mass in kg:","<b>",datatot3$mass/1000,"</b>",br(),"Categoria:", "<b>", datatot3$class, "</b>")
       m4= leaflet(datatot3) %>% addLegend(position="bottomright",colors=c("darkred","forestgreen"), labels=c("Fell","Found"), opacity=1) %>%
         addTiles("http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png") %>%
         addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
         addCircles(lng = datatot3$lng,lat = datatot3$lat, weight=2, opacity=1,
                    fillOpacity=0.8, col=datatot3$colors,radius= datatot3$mass/50, popup = popInfo) %>%
         setView(30,31,zoom = 1)
     })
     
     # II sottopagina
     
     output$cur_mass <- renderText({ "Top 5" })
     
     output$hoba_des <- renderText({ "Hoba è la meteorite più pesante mai ritrovata. Scoperta nel 1920, prende il nome dal luogo del ritrovamento, una fattoria di Hoba West (in Namibia) ed è la più grande massa di ferro di originale naturale. Si pensa sia caduta nel luogo di ritrovamento più di 80.000 anni fa." })
     
     output$mund_des <- renderText({ "Mundrabilla è una meteorite ferrosa ritrovata nel 1911 in Australia. Prima dell'impatto con l'atmosfera si stima pesasse circa 12 tonnellate" })
     
     output$sic_des<- renderText({ "Sikhote-Alin è un meteorite ferroso caduto nel 1947 sui Monti Sichote-Alin', nella Siberia orientale (Russia). Questa caduta fa parte di una delle più grandi piogge meteoritiche della storia recente. Alle 10:30, nella fredda mattina del 12 febbraio 1947, molte persone nella zona attorno ai Monti Sichote-Alin' della Siberia orientale, videro in cielo un grosso bolide più luminoso del Sole. Proveniva da nord e aveva un angolo discendente, poi stimato, di circa 41 gradi. La luce e il potente tuono del bolide furono percepiti fino a 300 km attorno al punto d'impatto, non lontano da Luaegorsk e circa a 440 km a nordest di Vladivostok. La scia di fumo di una trentina di km, rimase nel cielo siberiano per diverse ore prima di dissolversi."})
     
     output$mbo_des<- renderText({ "Mbozi è una meteorite ferrosa ritrovata a Mbeya, in Tanzania, nel 1930, nonostante fosse già nota ai locali. È lunga circa 3 metri e larga 1 e si stima pesasse circa 18 tonnellate prima dell'impatto con l'atmosfera."})
     
     output$wil_des<- renderText({ "Willamette è una meteorite ferrosa ritrovata in Oregon nel 1902, nonostante si pensi che fosse già nota ai nativi americani. Sul luogo del ritrovamento non sono state trovate tracce da impatto e perciò si pensa che la meteorite sia caduta in Canada per poi essere trasportata dallo scioglimento della calotta glaciale. È la meteorite più grande ritrovata negli Stati Uniti."})
     
# PAGINA CLASS ----
     
     output$descrizione <- renderText({
       dataa1 <- daticlass[which(daticlass$class %in% input$class),]
       datadescr <- data.frame(
         #class = dataa1$class,
         descr = dataa1$description
       )
       toString(datadescr$descr)
     })
     
     
     output$map5 <- renderLeaflet({ 
       df88=data5[which(data5$class %in% input$class),]
       df89 = data.frame(
         lat = df88$reclat,
         lng = df88$reclong,
         name = df88$name,
         mass = df88$mass,
         fall = df88$fall,
         year = df88$year,
         recclass = df88$recclass,
         class = df88$class,
         colorsMat = df88$colorsMat)
       popInfo=paste("Name:", "<b>", df89$name, "</b>", br(), "Year:", "<b>", df89$year, "</b>", br(), "Fall:", "<b>",df89$fall,"</b>")
       m5 = leaflet(df89) %>% 
         addTiles("http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png") %>%
         addProviderTiles(providers$Esri.NatGeoWorldMap) %>%
         addCircles(lng = df89$lng, lat = df89$lat,  weight=5, opacity=1,
                    fillOpacity=1, radius= 1, color = df89$colorsMat ,popup = popInfo) %>%
         setView(30,31,zoom=1)
     })
     
     
     
     
     output$torta_tipo <- renderPlot({
       par(mfrow=c(1,2))
       #### TORTA ROCCE TERRESTRI
       slicest <- comp_conf2$Terra
       lbls <- rownames(comp_conf2)
       pie3D(slicest,labels=lbls,explode=0.1,
             main=" Rocce terrestri ", labelcex=0.8, theta=pi/4)
       
       #### TORTA ALTRE
       ind <- data5[which(data5$class %in% input$class),]
       categoria <- ind$macroclass[1]
       slices <- comp_conf2[,which(colnames(comp_conf2)==categoria)]
       text = paste("Not available")
       if (categoria=="Stone-uncl")( plot(0, xaxt = 'n', yaxt = 'n', bty = 'n', xlab="", pch = '', ylab = '') &
                                     text(x=1,y=0.3,"Not available"))
       else (pie3D(slices,labels=lbls,explode=0.1,
                   main=as.character(categoria), labelcex=0.8, theta=pi/4))
       
       ## STONE-UNCL?
       
       
       
     })
     
     
     output$istogramma_tipo <- renderPlotly({
       ds1 <- data5[which(data5$class %in% input$class),]
       ds2 <- data.frame(
         tipo <- ds1$class,
         anno <- ds1$year,
         colorsMat <- ds1$colorsMat
       )
       ds3 <- data5
       ds4 <- data.frame(year=ds3$year)
       #hist(ds2$anno)
       q<- ggplot(ds2, aes(x=anno)) + 
         geom_histogram(binwidth=3, fill=ds2$colorsMat[1])
       # + geom_density(data=ds4, aes(x=year, color=6))
       #AGGIUNGERE LA DENSITA' DELLE METEORITI GENERALE NON PER ANNO
       ggplotly(q)
       
       
     })
     
     })

shinyApp(p1,p2) 
 