#########################################################################################
#SCRAPING FUNCTION
#########################################################################################
#EXTRACT ALL LINKS IN A WIKI WEB PAGE AS A LIST (e.g. /wiki/circular_economy)
#INPUT: page (html object)
#OUTPUT: all links
url <- function(page){
  page %>%    # Parte dall'html della pagina
    html_nodes(".mw-parser-output a") %>% # Naviga al nodo css specificato
    html_attr("href")      # Estrae ogni link che trova
} #OK

#EXTRACT ALL TEXT OF LINKS IN A WIKI WEB PAGE AS A LIST
#INPUT: page (html object)
#OUTPUT: all text links
url_text <- function(page){
  page %>% 
    html_nodes(".mw-parser-output a") %>% 
    html_text()     
} #OK

#CHECK IF from link there is any redirect by checking css class .mw-redirectedfrom a > 0
#INPUT: page (html object)
#OUTPUT: build wiki url from title h1
check_redirect <- function(page){
  #tolower(
  paste("/wiki/",
        str_replace_all(page %>% html_nodes("h1") %>% html_text()," ","_"),
        sep="")
  #)
} #OK


#CHECK IF NODE ALREADY EXISTS in NODES
#INPUT: nodelink (link of new page), nodes (dataframe with all nodes)
#OUTPUT: TRUE -> it doesn't exist; FALSE -> it already exists
nodes_check <- function(node_link){
  link_to_check <- paste("^",node_link,"$",sep="")
  #link_to_check <- node_link
  data_temp <- select(nodes,link) %>% 
    filter(str_detect(nodes$link,
                      regex(link_to_check,
                           ignore_case = F)
                      ))
  if(length(data_temp$link)==0){
    TRUE
  } else{
    FALSE
  }
} #OK

edges_check <- function(id_source_t,id_target_t){
  edges_t <- edges
  data_temp <- edges_t[edges_t$target == id_target_t & edges_t$source == id_source_t,]
  if(length(data_temp$source)==0){
    TRUE
  } else{
    FALSE
  }
} #OK


#READ FIRST SENTENCE OF A WIKIPEDIA PAGE
#INPUT: page (html object)
#OUTPUT: text of first sentence
first_sentence3 <- function(page){
  first_par <- first_paragraph(page)
  a <- 0
  z <- 1
  if(length(first_par)!=0){
    while(a==0 & z <= length(first_par)){
      ifelse(str_detect(first_par[z],'.'),a<-1,z<-z+1)
    }
    ifelse(a==1, first_sentence_t <- strsplit(first_par[z],"[.]")[[1]][1],first_sentence_t <- " ")
    clean_text(first_sentence_t)
      
  }else{
    " "
  }
}

clean_text <- function(text_t){
  doc_t <- gsub('[[:digit:]]+', '', text_t) #REMOVE NUMBER
  doc_t <- gsub("[^[:alnum:]\\-\\.\\s]", " ", doc_t)
  #gsub('([.-])|[[:punct:] ]+',' ',doc_t) #REMOVE ALL PUNCTUATIONS
  
  doc_t %>% 
    str_replace_all("[:control:]" , " ") %>%
    str_squish() %>% 
    str_trim("both") %>%   #REMOVE WHITE SPACE AT THE END OF SENTENCES
    str_replace_all("\\s{2,}\\d+" , "") #REMOVE DOUBLE WHITE SPACES
  
  
  
}



#READ FIRST PARAGRAPHS OF A WIKIPEDIA PAGE
#INPUT: page (html object)
#OUTPUT: text of first paragraph into a list. Generally 1-2 paragraphs (depends on the web page)
first_paragraph <- function(page){
  #first_par <<-
  page %>% #read 1st and 2nd paragraph of wiki page
    html_nodes(".mw-parser-output p:not(.mw-empty-elt):nth-of-type(-n+6)") %>%
    html_text() #".mw-parser-output > p:not(.mw-empty-elt):nth-of-type(-n+3)"
} #OK

entire_page <- function(page){
  page_t <- page %>%
    html_nodes("p") %>%
    html_text()
  
  page_t <- clean_text(page_t)
  paste(page_t,collapse=" ")
  
}

#EXTRACT WORD (no case sensitive) FROM a SENTENCE
#INPUT: text_to_check (sentence text); words_to_check (list of words)
#OUTPUT: list of all words detected
extract_types <- function(text_to_check,words_to_check){
  #words_detected_text <<-
  str_extract(text_to_check, 
             regex(words_to_check, ignore_case = T)) #EXTRACT WORD
} #OK


#EXTRACT WORD (no case sensitive) FROM a SENTENCE
#INPUT: text_to_check (sentence text); words_to_check (list of words)
#OUTPUT: list of all TRUE/FALSE
extract_types_bool <- function(text_to_check,words_to_check){
  #words_detected <<- 
  str_detect(text_to_check, 
             regex(words_to_check, ignore_case = T)) #TRUE/FALSE IF WORD APPEARS
} #OK



#UPDATE TYPE of NODES (temporary) (if more than one word appear -> mixed type)
#INPUT: words_detected,words_detected_text
#OUTPUT: type_temp
types_check2 <- function(text_to_check,words_to_check){
  words_bool <-  str_detect(text_to_check, 
             regex(words_to_check, ignore_case = T)) #TRUE/FALSE IF WORD APPEARS
  
  words_text <- str_extract(text_to_check, 
              regex(words_to_check, ignore_case = T)) #EXTRACT WORD  

  check_t <- length(words_bool[words_bool==TRUE])
  if(check_t==0){
    "null"
  }else{
      type_t <- ""
      for(k in 1:length(words_bool)){
        if(words_bool[k]){
          type_t <- paste0(type_t,tolower(words_text[k]),"|")   
        }
    }
    type_t
  } 
} #OK


df_temp <- function(links_t,urls_t){
 data.frame(
    pty = links, 
    urls = urls,
    stringsAsFactors = FALSE
  ) %>% filter(str_detect(urls, '^/wiki/') 
               & !str_detect(urls,"Citation_needed") 
               & !str_detect(urls,"#") 
               & !str_detect(urls,"Digital_object_identifier")
               & !str_detect(urls,"Special:") 
               & !str_detect(urls,"International_Standard_Book_Number")
               & !str_detect(urls,"International_Standard_Serial_Number")
               & !str_detect(urls,"/wiki/File:")
               & !str_detect(urls,"/wiki/Template:")
               & !str_detect(urls,"/wiki/Template_talk:")
               & !str_detect(urls,"/wiki/Portal:")
               & !str_detect(urls,"/wiki/Wikipedia:")
               & !str_detect(urls,"/wiki/Category:")
               & !str_detect(urls,"/wiki/Help:")
               & !str_detect(urls,"/wiki/Library_of_Congress_Control_Number")
               & !str_detect(urls,"/wiki/CiteSeerX")
               & !str_detect(urls,"/wiki/Bibcode")
               & !str_detect(urls,"/wiki/Handle_System")
               & !str_detect(urls,"Talk:"))
} #OK

####################################################################################
#NODES FUNCTION
####################################################################################
#ADD A NEW NODES from a single node
#INPUT: nodes (list of all nodes); node_temp (single temporary node); gen (generation).
#OUTPUT: nodes updated with the new node
new_node <- function(name_t,link_t,flag_t,generation_t,type_t){
  id_t <- ifelse(length(get_last_node_id())==0, 1, get_last_node_id() + 1)
    data.frame( #INITIALIZE NODES DATA FRAMES
    id = id_t, #ID NODE
    name = name_t, #NAME OF THE NODE
#    label = tolower(link_t)
    link = link_t, #LINK OF THE WIKI PAGE
    flagBL = flag_t, #FLAG ON TYPE
    generation = generation_t, #GENERATION 0=seedlist 1=1st neighbourhood 2=2nd neighbourhood ...
    type = type_t, #type of page (approach, system, methodology, ....)
    yet = FALSE,
    stringsAsFactors=FALSE    
      )   
} #OK

add_node <- function(node_t){
  nodes <<- rbind(nodes,node_t)
} #OK


add_node3 <- function(name_t,link_t,flag_t,generation_t,type_t,phrase_t){
  id_t <- ifelse(length(get_last_node_id())==0, 1, get_last_node_id() + 1)
  nodes <<- rbind(nodes,
                  list(id=id_t, #get_last_node_id(nodes)+1,
                       name=name_t, 
                       #label=tolower(link_t)
                       link=link_t,
                       flagBL=flag_t,
                       generation=generation_t,
                       type=type_t,
                       text=phrase_t,
                       yet = FALSE),
                  stringsAsFactors=FALSE)
} #OK

add_node4 <- function(name_t,link_t,flag_t,generation_t,type_t,phrase_t,pagetext_t){
  id_t <- ifelse(length(get_last_node_id())==0, 1, get_last_node_id() + 1)
  nodes <<- rbind(nodes,
                  list(id=id_t, 
                       name=name_t, 
                       #label=tolower(link_t)
                       link=link_t,
                       flagBL=flag_t,
                       generation=generation_t,
                       type=type_t,
                       text=phrase_t,
                       pagetext = pagetext_t,
                       yet = FALSE),
                  stringsAsFactors=FALSE)
} #OK


delete_node_by_id <- function(id_t){
  nodes <<- nodes[nodes$id != id_t,]
} #OK

delete_node_by_link <- function(link_t){
  nodes <<- nodes[nodes$link != link_t,]
} #OK


#INITIALIZE EMPTY NODES AND EDGES DATA FRAME
reset_nodes <- function(){
  nodes <<- data.frame( #INITIALIZE NODES DATA FRAMES
    id = integer(), #ID NODE
    name = character(), #NAME OF THE NODE
#   label = character(),
    link = character(), #LINK OF THE WIKI PAGE
    flagBL = integer(),
    generation =integer(), #GENERATION 0=seedlist 1=1st neighbourhood 2=2nd neighbourhood ...
    type = character(), #type of page (approach, system, methodology, ....)
    text = character(),
    yet = integer()
      )   
} #OK

get_generation_from_id <- function(id_t){
  nodes[nodes$id == id_t,]$generation
}

get_max_generation <- function(){
  max(nodes$generation)
}


get_flag_from_id <- function(id_t){
  nodes[nodes$id == id_t,]$flagBL
}

get_yet_from_id <- function(id_t){
  nodes[nodes$id == id_t,]$yet
}

change_yet_from_id <- function(id_t){
  nodes[nodes$id == id_t,]$yet <<- !nodes[nodes$id == id_t,]$yet
}


get_node_id_from_link <- function(link_t){
  id_t <- nodes[nodes$link == link_t,]$id
    if(length(id_t)>0) id_t[1] #[1] SOLO PER EVITARE ERRORI NEI DOPPIONI! 
}

get_node_id_from_link2 <- function(link_t){
  id_t <- nodes[nodes$link == link_t,]$id
  ifelse(length(id_t)>0,id_t[1],-1)
}


get_last_node_id <- function(){
  nodes[length(nodes$id),]$id
} #OK


get_link_from_id <- function(id_t){
  nodes[nodes$id == id_t,]$link
}


get_type_from_id <- function(id_t){
  nodes[nodes$id == id_t,]$type
}

nodes_list <- function(){
  nodes[nodes$flagBL == TRUE,]
} #OK

nodesBL_list <- function(){
  nodes[nodes$flagBL == FALSE,]
} #OK
####################################################################################
#EDGES FUNCTION
####################################################################################
#ADD NEW LINK (SOURCE, TARGET)
add_edge <- function(edge_t){
  edges <<- rbind(edges,edge_t)
} #OK

add_edge2 <- function(id_source_t,id_target_t,flagBL_t,weight_t,gen_t){
  edges <<- rbind(edges,
                  list(source = id_source_t, #source id of the link
                       target = id_target_t, #target id of the link,
                       flagBL = flagBL_t,
                       weight = weight_t, #weight of the link
                       generation = gen_t #get_generation_from_id(id_source_t)+1
                       ),
                  stringsAsFactors=FALSE)
} #OK


new_edge <- function(id_source_t,id_target_t,flagBL_t,weight_t,gen_t){
 data.frame( #INITIALIZE EDGES DATA FRAMES
    source = id_source_t, #source id of the link
    target = id_target_t, #target id of the link,
    flagBL = flagBL_t,
    weight = weight_t, #weight of the link
    generation = gen_t, #CHANGE gen in SELECT GENERATION from nodes$id[id_source]
    stringsAsFactors=FALSE
    )
} #OK


#DELETE ALL EDGES WITH SOURCE AND TARGET = TO ID
delete_edges <- function(edges,id){
  edges <<- edges[edges$target != id & edges$source != id,]
} #OK


#DELETE SINGLE EDGE WITH SOURCE = TO ID_SOURCE AND TARGET = TO ID_TARGET 
delete_edge <- function(edges,id_source,id_target){
  edges <<- edges[edges$target != id_target | edges$source != id_source,]
} #OK

reset_edges <- function(edges){
  edges <<- data.frame( #INITIALIZE EDGES DATA FRAMES
    source = integer(), #source id of the link
    target = integer(), #target id of the link
    flagBL = integer(),
    weight = integer(), #weight of the link
    generation = integer()
  )
} #OK



####################################################################################
#INITIAL CHECK ON BLACK LIST
####################################################################################
#CHANGE FLAGBL of nodes based on a separate handmade blacklist!
nodes_init_BL <- function(){
  nodes_t <- nodes
    for(k in 1:length(BLnodes$id)){
#   nodes[nodes$id == BLnodes$id[k],]$BL <- !nodes[nodes$id == BLnodes$id[k],]$BL
    nodes_t[nodes_t$id == BLnodes$id[k],]$flagBL <- FALSE
    }
  nodes <<- nodes_t
} #OK

#CHANGE FLAGBL of edges based on a separate handmade blacklist!
edges_init_BL <- function(){
  edges_t <- edges
  for(k in 1:length(BLnodes$id)){
    edges_t[edges_t$target == BLnodes$id[k] | edges_t$source == BLnodes$id[k],]$flagBL <- FALSE
  }
  edges <<- edges_t
}


edges_init_check_BL2 <- function(){
  edges_t <- edges
  for(l in 1:length(edges$source)){
    print(l)
    id_source_t <- edges$source[l]
    id_target_t <- edges$target[l]
    flag_source <- nodes$flagBL[id_source_t]
    flag_target <- nodes$flagBL[id_target_t]
    if(flag_source & flag_target){
      edges_t[edges_t$target == id_target_t & edges_t$source == id_source_t,]$flagBL <- TRUE
    }else{
      edges_t[edges_t$target == id_target_t & edges_t$source == id_source_t,]$flagBL <- FALSE
    }
  }
  edges <<- edges_t  
}


####################################################################################
#DATA MANIPULATION
####################################################################################

change_flag_sub_branch <- function(id_source_t){
  nodes_t <- nodes
  edges_t <- edges
  
  nodes_t[nodes_t$id == id_source_t,]$flagBL <- FALSE
  edges_t[edges_t$source == id_source_t,]$flagBL <- FALSE
  
  for( l in 1:length(edges_t[edges_t$source == id_source_t,]$target)){
    id_target_t <- edges_t[edges_t$source == id_source_t,]$target[l]
    if(nodes_t$generation[id_target_t]>nodes_t$generation[id_source_t]){
      nodes_t[nodes_t$id == id_target_t,]$flagBL <- FALSE
    }
  }
  nodes <<- nodes_t
  #edges <<- edges_t
  edges_init_check_BL2()
  
}


change_flag_sub_branches <- function(id_source_t){

} #DA FARE



add_word_type_to_nodes <- function(type_t){
  nodes_t <- nodes
  edges_t <- edges
  
  for(l in 1:length(nodes_t$id)){
    words_bool_t <-  str_detect(nodes_t$text[l], 
                              regex(type_t, ignore_case = T))
    if(words_bool_t){
      words_text_t <- str_extract(nodes_t$text[l], 
                                regex(type_t, ignore_case = T)) #EXTRACT WORD  
      check_t <- nodes_t$type[l]
      #nodes_t$type[l] <- paste0(nodes_t$type[l],tolower(words_text_t),"|")   
      if(check_t == "null"){
        nodes_t$flagBL[l] <- TRUE
        nodes_t$type[l] <- paste0(tolower(words_text_t),"|")   
      }else{
        nodes_t$type[l] <- paste0(nodes_t$type[l],tolower(words_text_t),"|")   
      }
    }
  }
  nodes <<- nodes_t
  edges_init_check_BL2()
  
} #OK


add_word_type_to_nodes2 <- function(type_t){
  nodes_t <- nodes
  edges_t <- edges
  
  nodes_t <- mutate(nodes_t,
                    flagBL = ifelse(str_detect(nodes_t$text, 
                                                 regex(type_t, ignore_case = T)),
                                    ifelse(nodes_t$type=="null",
                                           !nodes_t$flagBL,
                                           nodes_t$flagBL
                                           )
                                    ,
                                    nodes_t$flagBL),
                    type = ifelse(str_detect(nodes_t$text, 
                                             regex(type_t, ignore_case = T)),
                                  ifelse(nodes_t$type=="null",
                                         paste0(tolower(type_t),"|"),
                                         paste0(nodes_t$type,tolower(type_t),"|") 
                                  )
                                  ,
                                  nodes_t$type)
  )
                    
  nodes <<- nodes_t
  edges_init_check_BL2()
  
} #OK



delete_word_type_to_nodes <- function(type_t){
  nodes_t <- nodes
  edges_t <- edges
  
  
  string_t <- paste0(type_t,"[|]")
  nodes_t$type <- gsub(string_t, "", nodes_t$type)
  nodes_t <- mutate(
    nodes_t,
    type = ifelse(nchar(nodes_t$type)==0,"null",nodes_t$type),
    flagBL = ifelse(nchar(nodes_t$type)==0,!nodes_t$flagBL,nodes_t$flagBL)
  )
  
  nodes <<- nodes_t
  edges_init_check_BL2()
  
} #OK


reset_type_and_flag <- function(){
  nodes_t <- nodes
  nodes_t$type <- ""
  nodes_t$flagBL <- FALSE
  nodes <<- nodes_t
} #OK (IN COPPIA CON init_type_and_flag)

init_type_and_flag <- function(type_t){
  nodes_t <- nodes
  edges_t <- edges
  
  for(l in 1:length(type_t)){
      
    nodes_t <- mutate(nodes_t,
                      flagBL = ifelse(str_detect(nodes_t$text, 
                                                 regex(type_t[l], ignore_case = T)),
                                      TRUE,
                                      nodes_t$flagBL),
                      type = ifelse(str_detect(nodes_t$text, 
                                               regex(type_t[l], ignore_case = T)),
                                    gsub("null","",paste0(nodes_t$type,tolower(type_t[l]),"|")),
                                    nodes_t$type)
    )
    nodes_t <- mutate(nodes_t,
      type = ifelse(nchar(nodes_t$type)==0,"null",nodes_t$type)
    )
  }
  nodes <<- nodes_t
  edges_init_check_BL2()
} #OK (IN COPPIA CON reset_type_and_flag)


init_weight_cos <- function(freq_t){
  
  edges_t <- edges
  for(l in 1:length(edges$source)){
    print(l)
    
    if(edges$flagBL[l]){
      id_source_t <- edges$source[l]
      id_target_t <- edges$target[l]      
      
      corp_en <- corpus(
        c(nodes$pagetext[id_source_t],nodes$pagetext[id_target_t]),
        docnames = c(nodes$link[id_source_t],nodes$link[id_target_t])
      )
      
      token_en <- corp_en %>%
        tokens(remove_punct = T,
               remove_numbers = T,
               verbose = T)
      
      #STOP WORDS
      stp <- stopwords("english")
      
      #STEMMING
      token_en <- token_en %>%
        tokens_tolower() %>%                     # Porta tutto in minuscolo
        tokens_remove(stp) %>%                   # Rimuove stop words (lista aggiornata da noi)
        tokens_wordstem(language = "english")    # Stemming
      #head(token_en[[1]], n = 10)
      
      #TOKENS AND N-GRAM
      ngr <- textstat_collocations(
        token_en, 
        size = 2:3,                 # Sequence di 2 e 3 parole
        min_count = 3)              # Sequenze che compaiono almeno 3 volte
      
      # Ispezioniamo il risultato
      #ngr %>%
      #  arrange(-count)
      
      token_en <- tokens_compound(
        token_en, 
        phrase(ngr$collocation), 
        join = T
      )
      
      dtm_en <- dfm(
        token_en,
        verbose = T
      )
      
      cos_sim <- dtm_en %>%
        dfm_trim(min_termfreq = freq_t, termfreq_type = "quantile") %>%
        textstat_simil(method = "cosine",
                       margin = "documents")
      
      edges_t[edges_t$target == id_target_t & edges_t$source == id_source_t,]$weight <- cos_sim[1]
      
    }
  }
  edges <<- edges_t
}

init_weight_jac <- function(){
  
  edges_t <- edges
  for(l in 1:length(edges$source)){
    print(l)
    
    if(edges$flagBL[l]){
      id_source_t <- edges$source[l]
      id_target_t <- edges$target[l]      
      
      corp_en <- corpus(
        c(nodes$pagetext[id_source_t],nodes$pagetext[id_target_t]),
        docnames = c(nodes$link[id_source_t],nodes$link[id_target_t])
      )
      
      token_en <- corp_en %>%
        tokens(remove_punct = T,
               remove_numbers = T,
               verbose = T)
      
      #STOP WORDS
      stp <- stopwords("english")
      
      #STEMMING
      token_en <- token_en %>%
        tokens_tolower() %>%                     # Porta tutto in minuscolo
        tokens_remove(stp) %>%                   # Rimuove stop words (lista aggiornata da noi)
        tokens_wordstem(language = "english")    # Stemming
      #head(token_en[[1]], n = 10)
      
      #TOKENS AND N-GRAM
      ngr <- textstat_collocations(
        token_en, 
        size = 2:3,                 # Sequence di 2 e 3 parole
        min_count = 3)              # Sequenze che compaiono almeno 3 volte
      
      # Ispezioniamo il risultato
      #ngr %>%
      #  arrange(-count)
      
      token_en <- tokens_compound(
        token_en, 
        phrase(ngr$collocation), 
        join = T
      )
      
      dtm_en <- dfm(
        token_en,
        verbose = T
      )
      
      jac_sim <- dtm_en %>%
        textstat_simil(method = "jaccard",
                       margin = "documents")
      
      edges_t[edges_t$target == id_target_t & edges_t$source == id_source_t,]$weight <- jac_sim[1]
      
    }
  }
  edges <<- edges_t
}

init_weight_ejac <- function(){
  
  edges_t <- edges
  for(l in 1:length(edges$source)){
    print(l)
    
    if(edges$flagBL[l]){
      id_source_t <- edges$source[l]
      id_target_t <- edges$target[l]      
      
      corp_en <- corpus(
        c(nodes$pagetext[id_source_t],nodes$pagetext[id_target_t]),
        docnames = c(nodes$link[id_source_t],nodes$link[id_target_t])
      )
      
      token_en <- corp_en %>%
        tokens(remove_punct = T,
               remove_numbers = T,
               verbose = T)
      
      #STOP WORDS
      stp <- stopwords("english")
      
      #STEMMING
      token_en <- token_en %>%
        tokens_tolower() %>%                     # Porta tutto in minuscolo
        tokens_remove(stp) %>%                   # Rimuove stop words (lista aggiornata da noi)
        tokens_wordstem(language = "english")    # Stemming
      #head(token_en[[1]], n = 10)
      
      #TOKENS AND N-GRAM
      ngr <- textstat_collocations(
        token_en, 
        size = 2:3,                 # Sequence di 2 e 3 parole
        min_count = 3)              # Sequenze che compaiono almeno 3 volte
      
      # Ispezioniamo il risultato
      #ngr %>%
      #  arrange(-count)
      
      token_en <- tokens_compound(
        token_en, 
        phrase(ngr$collocation), 
        join = T
      )
      
      dtm_en <- dfm(
        token_en,
        verbose = T
      )
      
      ejac_sim <- dtm_en %>%
        textstat_simil(method = "ejaccard",
                       margin = "documents")
      
      edges_t[edges_t$target == id_target_t & edges_t$source == id_source_t,]$weight <- ejac_sim[1]
      
    }
  }
  edges <<- edges_t
}

####################################################################################
#QUANTEDA - TEXT MINING
####################################################################################

initialize_corpus <- function(){
  nodes_t <- nodes
  corpus(
    nodes_t$pagetext,
    docnames = nodes_t$link
  )
}


corpus_twopages <- function(id_source_t,id_target_t){
  corp_temp <- corpus(
    c(nodes$pagetext[id_source_t],nodes$pagetext[id_target_t]),
    docnames = c(nodes$link[id_source_t],nodes$link[id_target_t])
    )
  docvars(corp_temp,"page") <-  str_replace_all(c(nodes$link[id_source_t],nodes$link[id_target_t]), "/wiki/", "")
  corp_temp
} #OK


dfm_from_corpus <- function(corp_en_t){
  
  token_en <- corp_en_t %>%
    tokens(remove_punct = T,
           remove_numbers = T,
           verbose = T)
  #STOP WORDS
  stp <- stopwords("english")
  
  #STEMMING
  token_en <- token_en %>%
    tokens_tolower() %>%                     # Porta tutto in minuscolo
    tokens_remove(stp) %>%                   # Rimuove stop words (lista aggiornata da noi)
    tokens_wordstem(language = "english")    # Stemming
  #head(token_en[[1]], n = 10)

  #TOKENS AND N-GRAM
  ngr <- textstat_collocations(
    token_en, 
    size = 2:3,                 # Sequence di 2 e 3 parole
    min_count = 3)              # Sequenze che compaiono almeno 3 volte
  # Ispezioniamo il risultato
  #ngr %>%
  #  arrange(-count)
  if(length(ngr$collocation)!=0){
    token_en <- tokens_compound(
      token_en, 
      phrase(ngr$collocation), 
      join = T
    )    
  }

  
  dfm(
    token_en,
    verbose = T
  )
} #OK




cos_sim <- function(dfm_en_t,freq_t){

  cos_sim <- dfm_en_t %>%
    dfm_trim(min_termfreq = freq_t, termfreq_type = "quantile") %>%
    textstat_simil(method = "cosine",
                   margin = "documents")
  cos_sim[1]
} #OK - COS SIM BETWEEN TWO PAGES




jac_sim <- function(dfm_en_t){

  jac_sim <- dfm_en_t %>%
    textstat_simil(method = "jaccard",
                   margin = "documents")
  jac_sim[1]
} #OK - JACCARD SIM BETWEEN TWO PAGES



ejac_sim <- function(dfm_en_t){

  ejac_sim <- dfm_en_t %>%
    textstat_simil(method = "ejaccard",
                   margin = "documents")
  ejac_sim[1]
} #OK - EXTENDED JACCARD SIM BETWEEN TWO PAGES



n_topics_from_dfm <- function(dfm_en_t){
  n_topics <- FindTopicsNumber(
    dfm_en_t,
    topics = seq(from = 2, to = 30, by = 1),
    metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
    method = "Gibbs",
    control = list(seed = 4682),
    mc.cores = 2L,
    verbose = TRUE
  )
  FindTopicsNumber_plot(n_topics)
} #OK


terms_topics <- function(dfm_en_t,n_topics_t){
  it_tm <- convert(dfm_en_t, to = "topicmodels")
  set.seed(4682)
  LDA(it_tm, k = n_topics_t, method = "Gibbs")
  #terms(tm, 10)
}

beta_par_tp <- function(terms_t){
  tidytext::tidy(terms_t, "beta") 
}

graph_topics <- function(beta_tm_t){
  beta_tm_t%>%
    group_by(topic) %>%
    top_n(10, beta) %>%
    ungroup() %>%
    arrange(topic, -beta) %>%
    ggplot(., aes(x = reorder_within(term, beta, topic), y = beta)) +
    geom_bar(aes(fill = factor(topic)),
             stat = "identity", 
             col = "black", alpha = 0.5) +
    facet_wrap(~topic, ncol = 2, scales = "free_y") +
    scale_x_reordered() +
    xlab("Parole") + ylab("Beta") +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "none")
}





graphs_topics_per_page <- function(topics_t){
  topics_t %>%
    group_by(page, topic) %>%
    summarize(freq = n()) %>%
    group_by(page) %>%
    mutate(freq = freq/sum(freq)) %>%
    ggplot(., aes(x = reorder_within(topic, freq, page), y = freq)) +
    geom_bar(aes(fill = page),
             stat = "identity", 
             col = "black", alpha = 0.8) +
    facet_wrap(~page, ncol = 1, scales = "free_y") +
    scale_y_continuous(labels = scales::percent) +
    scale_x_reordered() +
    scale_fill_manual(values = c("green4", "yellow", "violet")) +
    xlab("Topics") + ylab("Prevalenza") +
    coord_flip() +
    theme_minimal() +
    theme(legend.position = "bottom")
}

