#======================================================================================================
# ADMIN
#======================================================================================================
want = c("tidyverse", "broom",   # Data management
         "rvest",                # Scraping
         "rio", "readtext",      # Leggere/scrivere dati
         "hunspell", "quanteda","tidytext", # QTA
         "dplyr","RCurl","plyr",
         "topicmodels","lda","ldatuning","stm", # librerie per topic models
         "ggrepel", # una miglioria ai grafici ggplot
         "factoextra" # libreria per scree plot CA
         ) 
have = want %in% rownames(installed.packages())
if ( any(!have) ) { install.packages( want[!have] ) }
junk <- lapply(want, library, character.only = TRUE)
rm(have,want,junk)

rm(list = ls()) # Rimuove tutti gli oggetti presenti nel workspace

# Gruppo di funzioni che ci permettono di riordinare i valori di una variabile in un grafico per
# i valori di un'altra variabile, all'interno dei pannelli
# Credits: https://github.com/dgrtwo/drlib/blob/master/R/reorder_within.R
setwd("C:/Users/Plinio/Desktop/QTA_Torino/biblio-analysis/wikiCE2")
source("main_functions5.R")
###########################################################################
###########################################################################
#1) STARTUP PHASE (1.0) THEN FOLLOW
#(1.1) for the first time or 
#(1.2) when you already run the program and you have already a network
###########################################################################
###########################################################################
#1.0) IMPORT WORDS FOR TYPES  
types <- import("./keyword/type-all-def.csv")
wikibaselink <- "https://en.wikipedia.org"

###########################################################################
#1.1) INITIALIZE EMPTY NODES AND EDGES (IF NECESSARY)
###########################################################################
#TO ADD - IMPORT KEYWORD AND WORDS_TO_CONTROL, NODES AND EDGES WITH FLAG

#1.1.1) IMPORT LIST OF KEYWORD FOR NODES
nodes_temp <- import("./keyword/keyword-tot.csv")

#1.1.2) INITIALIZE EMPTY NODES AND EDGES DATAFRAME
#GLOBAL VARIABLE
cos_freq = 0.95

nodes <- data.frame( #INITIALIZE NODES DATA FRAMES
  id = integer(), #ID NODE
  name = character(), #NAME OF THE NODE
  link = character(), #LINK OF THE WIKI PAGE
  flagBL = logical(), #TRUE (node -> ok) - FALSE (node -> blacklist)
  generation =integer(), #GENERATION 0=seedlist 1=1st neighbourhood 2=2nd neighbourhood ...
  type = character(), #type of page (approach, system, methodology, ....)
  text = character(), #first sentence
  pagetext = character(), #Whole page
  yet = logical(), #flag TRUE if already parsed - FALSE if not
  stringsAsFactors=FALSE
  ) 
edges <- data.frame( #INITIALIZE EDGES DATA FRAMES
  source = integer(), #source id of the link
  target = integer(), #target id of the link,
  flagBL = logical(),
  weight = numeric(), #weight of the link -> COSINE SIMILARITY
  generation = integer(),
  stringsAsFactors=FALSE
)

#1.1.3) INITIALIZE NODES FROM THE SEED LIST OF KEYWORDS
#Loop on every keywords -> create keyword seed list

for(i in 1:length(nodes_temp$id)){
#i <- 2
  print(i)
  nodelink <- paste("/wiki/",gsub(" ", "_", nodes_temp$keyword[i]),sep="")
  if(url.exists(paste(wikibaselink,nodelink,sep=""))){ #CHECK IF EXIST A WIKI PAGE
    
    main.page <- read_html(x = paste(wikibaselink,nodelink,sep="")) #READ HTML NEW PAGE
    nodelink <- check_redirect(main.page) #CHECK REDIRECT 
    id_check <- get_node_id_from_link2(nodelink)
    
    if(id_check == -1){ #CHECK IF NODE ALREADY EXISTS

      #CHECK TYPE - DA RIFARE!!!!
      first_phrase <- first_sentence3(main.page)
      type_temp <- types_check2(first_phrase,types$keyword)
      ifelse(type_temp=="null",flag <- FALSE, flag <- TRUE)
      
      all_phrases <- entire_page(main.page)
      
      add_node4(nodes_temp$keyword[i],nodelink,flag,0,type_temp,first_phrase,all_phrases)
    }
  }
}
tail(nodes)


write.csv(edges, file = "./data/edges-init-allw.csv")
write.csv(nodes, file = "./data/nodes-init-allw.csv")
###########################################################################
#1.2) IMPORT PREVIOUS NETWORK - NODES AND EDGES AND BLACKLIST
###########################################################################
#1.2.1) IMPORT PREVIOUS NODES AND EDGES
nodes <- read.csv("./data/nodes-temp-allw.csv",
                  colClasses=c("NULL",
                              "integer",
                              "character",
                              "character",
                              "logical",
                              "integer",
                              "character",
                              "character",
                              "character",
                              "logical"),
                  stringsAsFactors = FALSE)



edges <- read.csv("./data/edges-temp-allw.csv",
                  colClasses=c("NULL",
                              "integer",
                              "integer",
                              "logical",
                              "numeric",
                              "integer"),
                  stringsAsFactors = FALSE)

#1.2.2) IMPORT BLACKLIST NODES
BLnodes <- read.csv("./data/BLnodes300.csv",
                    colClasses=c("NULL",
                                 "integer",
                                 "character",
                                 "character",
                                 "logical",
                                 "integer",
                                 "character",
                                 "logical"),
                    stringsAsFactors = FALSE)


#1.2.3) UPDATE NODES BASED ON a HAND MADE BLACK LIST NODES
nodes_init_BL()
edges_init_BL()

#1.2.4) CHECK EDGES BASED ON a nodes$flagBL=FALSE and label those edges as edges$flagBL = FALSE 
edges_init_check_BL2()
head(edges,n=100)
###########################################################################
###########################################################################
#2) MAIN PROGRAM
###########################################################################
###########################################################################
#NOTE: AGGIUNGERE WEIGHT con FUNZIONE COUNT() -> N LINK DA PAGINA A PAGINA.
#NOTE: MODIFICARE CODICE per velocità -> FIRST SCRAP -> THEN ANALYZE

for(i in 0:0) { #CICLO FOR SU GENERATION STEP
#  i <- 0
  #INITIAL FILTER ON GENERATION, BLACKLIST and ALREADY PARSE (YET)
  nodes_gen_i <- filter(nodes,yet==FALSE,generation == 0) #generation == 0,flagBL == TRUE

  for(j in 1:length(nodes_gen_i$generation)) { #CICLO SU TUTTE LE PAROLE generaTION i
#    j <- 3
    main.page <- read_html(x = paste(wikibaselink,nodes_gen_i$link[j],sep=""))
    urls <- url(main.page)    # Estrarre tutti i link dalla pagina
    links <- url_text(main.page)     # Estrarre il testo del link
    datas <- df_temp(links,urls)  # Mettiamo tutto in un data frame temporaneo

  
    if(length(datas$urls)!=0){ #CHECK IF N OF LINK > 0
      id_source <- nodes_gen_i$id[j]  #TAKE ID_SOURCE
      gen <- get_generation_from_id(id_source)
      for(k in 1:length(datas$urls)){ 
#        k <- 273
        main.page <- read_html(x = paste(wikibaselink,datas$urls[k],sep=""))
        
        #CHECK, IF ANY, REDIRECT
        datas$urls[k] <- check_redirect(main.page) #CHECK REDIRECT 
        nodelink <- datas$urls[k]
        
        #CHECK IF NODE ALREADY EXISTS
        id_check <- get_node_id_from_link2(nodelink)
        if(id_check != -1){ #3.2) YES -> (TAKE ID_TARGET)
          id_target <- get_node_id_from_link(nodelink) 
          flag <- get_flag_from_id(id_target)
          type_temp <- get_type_from_id(id_target)
        }else{ #3.2) NO -> (CREATE NEW NODES, then TAKE ID_TARGET) 
          #CHECK FIRST SENTENCE - DA RIFARE!!!!
          first_phrase <- first_sentence3(main.page)
          all_phrases <- entire_page(main.page)
          
          type_temp <- types_check2(first_phrase,types$keyword)
          ifelse(type_temp=="null",flag <- FALSE, flag <- TRUE)
          
          add_node4(datas$pty[k],nodelink,flag,gen+1,type_temp,first_phrase,all_phrases) #ADD A NEW NODES for the SEED LIST    
          id_target <- get_last_node_id()
        } # END IF CONTROL IF NODE ALREADY EXISTS
        
        #3.2) -> CREATE NEW LINK (SOURCE, TARGET)
        if(flag){
          print(paste("k:",k, "s:", id_source," ",get_link_from_id(id_source),
                      "t:", id_target, " ", get_link_from_id(id_target),
                      "type:",type_temp))
        }
        
        if(edges_check(id_source,id_target)){ # CHECK IF LINK ALREADY EXIST
                 
          corp_en <- corpus_twopages(id_source,id_target)
          dfm_en <- dfm_from_corpus(corp_en)
          weight <- cos_sim(dfm_en,cos_freq)
          #weight <- jac_sim(dfm_en,cos_freq)
          #weight <- ejac_sim(dfm_en,cos_freq)
          
          add_edge2(id_source,id_target,flag,weight,gen+1)
        }
      } #END LOOP FOR INTERNAL LINK IN PAGE j
    } #END IF (CHECK ON N of LINK != 0)
    
    change_yet_from_id(id_source)  
  } #END LOOP FOR NODES of GENERATION i
} #END LOOP FOR GENERATION


head(nodes_list())
tail(edges)
tail(nodes)

write.csv(edges, file = "./data/edges-temp-allw.csv")
write.csv(nodes, file = "./data/nodes-temp-allw.csv")


########################################################################################
#DATA MANIPULATION
########################################################################################
nodes <- read.csv("./data/def/nodes-all-text-6-0a-all.csv",
                  colClasses=c("NULL",
                               "integer",
                               "character",
                               "character",
                               "logical",
                               "integer",
                               "character",
                               "character",
                               "character",
                               "logical"),
                  stringsAsFactors = FALSE)



edges <- read.csv("./data/def/edges-all-text-6-0a-all.csv",
                  colClasses=c("NULL",
                               "integer",
                               "integer",
                               "logical",
                               "numeric",
                               "integer"),
                  stringsAsFactors = FALSE)

write.csv(edges, file = "./data/def/test/edges-all-text-6-0a-all.csv")
write.csv(nodes, file = "./data/def/test/nodes-all-text-6-0a-all.csv")



types_field <- import("./keyword/type-field-def.csv")
types_tech <- import("./keyword/type-tech-def.csv")
types_practice <- import("./keyword/type-practice3.csv")
types_all <- import("./keyword/type-all-def.csv")


reset_type_and_flag()
init_type_and_flag(types_all$keyword)
write.csv(edges, file = "./data/def/edges-all-text-6-0a-all-def.csv")
write.csv(nodes, file = "./data/def/nodes-all-text-6-0a-all-def.csv")

reset_type_and_flag()
init_type_and_flag(types_field$keyword)
write.csv(edges, file = "./data/def/edges-all-text-6-0a-field-def.csv")
write.csv(nodes, file = "./data/def/nodes-all-text-6-0a-field-def.csv")

reset_type_and_flag()
init_type_and_flag(types_tech$keyword)
write.csv(edges, file = "./data/def/edges-all-text-6-0a-tech-def.csv")
write.csv(nodes, file = "./data/def/nodes-all-text-6-0a-tech-def.csv")

reset_type_and_flag()
init_type_and_flag(types_practice$keyword)
write.csv(edges, file = "./data/def/edges-all-text-6-0a-practice3.csv")
write.csv(nodes, file = "./data/def/nodes-all-text-6-0a-practice3.csv")



init_weight_cos(0.95)
write.csv(edges, file = "./data/edges-95a.csv")

init_weight_cos(0.85)
write.csv(edges, file = "./data/edges-85.csv")

init_weight_cos(0.95)
write.csv(edges, file = "./data/edges-95b.csv")


source("main_functions5.R")

corp_en <- corpus_twopages(1,2)
summary(corp_en)
dfm_en <- dfm_from_corpus(corp_en)
n_topics_from_dfm(dfm_en)
n_topics = 6

terms <- terms_topics(dfm_en,n_topics)
beta_tm <- beta_par_tp(terms)
graph_topics(beta_tm)


topics_en <- data.frame(
  page = docvars(corp_en),
  topic = topics(terms)
)

#NON FUNZIONA!
#graphs_topics_per_page(topics_en) 
