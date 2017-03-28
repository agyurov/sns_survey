# Dictionary, scale and questions grouping and definitions





str(df2,list.len=ncol(df2))


# all info about the survey contained here 

survey.info = function(...){
  argz = as.list(substitute(list(...)))
  names(argz) = unlist(argz)
  if(length(argz) == 1){
    return(print(c("qnum","long","ulong","short","survey")))
  }
  #
  
  #
  data_list = as.list(substitute(list(...)))
  names(data_list) = unlist(deparse(substitute(data_list)))
  numbers_loc = question_numbers
  long_loc = full_questions_names
  ulong_loc = unique(full_questions_names)
  short_loc = shortnames
  survey_loc = questions
  #
  if(!is.null(argz$qnum)){
    print(numbers_loc)
  }
  if(!is.null(argz$long)){
    print(long_loc)
  }
  if(!is.null(argz$ulong)){
    print(ulong_loc)
  }
  if(!is.null(argz$short)){
    print(short_loc)
  }
  if(!is.null(argz$survey)){
    print(survey_loc)
  }
}

.dictionary = new.env()


test = function(...,add = F){
  # Add to environment
  if(add){
    argz = list(...)
    names(argz) = unlist(as.list(substitute(list(...)))[-1])
    # Check if duplicate
    # ---------------------
    print(names(as.list(.dictionary))%in%  names(argz))
    
    # ---------------------
    
    
    list2env(argz,envir=.dictionary)
    return(cat(paste0("Object(s) ",paste0(names(argz),collapse=", ")," added to dictionary." )))
  }
  # Return object from environment
  if(!add){
    print("Enter iff add = F")
    argz = as.list(substitute(list(...)))
    argz = argz[-1]
    names(argz) = unlist(argz)
    
    out = as.list(.dictionary)
    return(out[names(out) %in% names(argz)])
  }
}
