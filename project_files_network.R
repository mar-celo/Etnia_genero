library(data.table)
library(igraph)
library(stringr)
list.files(pattern = "\\.((R|r)(md|))$",full.names = T,recursive = T) %>% 
  setdiff("./project_files_network.R") %>%
  sapply(function(f){
  lines = readLines(f)
  f_input <- 
    data.table(source =
                 grep("^#",value = T,lines,invert = T) %>%
                 grep(pattern = "(read|load).*\\(",.,value = T, ignore.case = T) %>% 
                 gsub(".*read.*\\(","",.) %>%
                 str_squish() %>%
                 gsub("\\)(.{1,3}|)$","",.) %>%
                 gsub("\\.\\..","",.),
               target = f) %>%
    filter(!grepl("^#",source))
  
  f_output <- 
    data.table(source = f,
               target = 
                 grep("^#",value = T,lines,invert = T) %>%
                 grep(pattern = "(save|write).*\\(",.,value = T, ignore.case = T) %>% 
                 gsub('.*(save|write).*\\(',"",.)  %>%
                 gsub("\\,[[:space:]]*selfcontained.*","",.) %>%
                 gsub("file = ","",.) %>%
                 gsub(".*\\,","",.)  %>%
                 str_squish()  %>%
                 gsub("\\)(.{1,3}|)$","",.) %>%
                 gsub("\\.\\..","",.)
               ) %>%
    filter(!grepl("^#",target))
  
  rbind(f_input,f_output) 
  },
  simplify = F) %>% 
  rbindlist(fill = T) -> links


nodes <- data.table(name = unique(c(links$source,links$target))) %>%
  .[,script := grepl("\\.((R|r)(md|))$",name)]


network <- graph_from_data_frame(d=links, vertices=nodes, directed=T) 


# Create a vector of color
library(RColorBrewer)
coul  <- brewer.pal(3, "Set1") 
my_color <- coul[as.numeric(as.factor(V(network)$script))]

plot(network, vertex.color=my_color,vertex.size = 5,edge.arrow.size = 0.5)



# %>%
#   lapply(grep,pattern = "(save|write).*",value = T, ignore.case = T) %>%
#   lapply(as.data.table) %>%
#   rbindlist(fill = T,idcol = "file") %>%
#   filter()
#   View
