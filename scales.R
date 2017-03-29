# Define scales



scaling = function(x,scale_name){
  qs = unlist(bucket(question_numbers),use.names = F)
  qs = paste0(qs,unlist(bucket(shortnames),use.names = F))
  for(i in 1:length(qs)){
    cat(paste0(qs[i]," <- Belongs to",paste0(deparse(substitute(scale_name))),"? t/f:\n"))
    ans = readline()
    if(ans == "exit"){
      x[[deparse(substitute(scale_name))]][i:length(qs)] = NA
      return(x)
      }
    if(!ans %in% c("t","exit")) {
      x[[deparse(substitute(scale_name))]][i] = NA
      next
    }
    if(ans == "t"){
      x[[deparse(substitute(scale_name))]][i] = qs[i]
    }
  }
  return(x)
}

scales = list()

# sns
scales = scaling(scales,sns)

# get_news
scales = scaling(scales,get_news)

# sharing
scales = scaling(scales,sharing)

# as for opinion
scales = scaling(scales,ask_opinion)

# organizing
scales = scaling(scales,organizing)

# promotion
scales = scaling()

record()

