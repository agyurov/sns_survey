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
scales = scaling(scales,promotion)

# fun
scales = scaling(scales,fun)

# entertaintment == passive, part of social1
scales = scaling(scales,passive)

# social2, same level as above
scales = scaling(scales,social2)

# q11
tmp = grepl("q11.",question_numbers,fixed=T)
tmp[!tmp] = NA
scales[["q11"]] = shortnames[tmp]

# reconnecting
tmp = grepl("q12.",question_numbers,fixed=T)
tmp[!tmp] = NA
scales[["recon"]] = shortnames[tmp]

# q13
tmp = grepl("q13.",question_numbers,fixed=T)
tmp[!tmp] = NA
scales[["q13"]] = shortnames[tmp]

# distance
tmp = grepl("q15.",question_numbers,fixed=T)
tmp[!tmp] = NA
scales[["distance"]] = shortnames[tmp]

# distance local
tmp = grepl("local",scales$distance)
tmp[!tmp] = NA
scales[["dist_local"]] = shortnames[tmp]

# distance dist
tmp = grepl("dist",scales$distance)
tmp[!tmp] = NA
scales[["dist_dist"]] = shortnames[tmp]

# q16
tmp = grepl("q16.",question_numbers,fixed=T)
tmp[!tmp] = NA
scales[["q16"]] = shortnames[tmp]

# q17
tmp = grepl("q17.",question_numbers,fixed=T)
tmp[!tmp] = NA
scales[["q17"]] = shortnames[tmp]

# q18
tmp = grepl("q18.",question_numbers,fixed=T)
tmp[!tmp] = NA
scales[["q18"]] = shortnames[tmp]

# 19
tmp = grepl("q19.",question_numbers,fixed=T)
tmp[!tmp] = NA
scales[["q19"]] = shortnames[tmp]

# q20
tmp = grepl("q20.",question_numbers,fixed=T)
tmp[!tmp] = NA
scales[["q20"]] = shortnames[tmp]

# demographics
scales = scaling(scales,demogr)

record()

