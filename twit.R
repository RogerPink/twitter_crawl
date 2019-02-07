twit<-function(name_id,f_lim=100,ff_lim=100,degree=3,keywords=c("math","professor","research","scientist","engineer")){
  
  library(rtweet)
  library(memoise)
  
  # m_get_friends<-memoise(get_friends)
  # m_lookup_users<-memoise(lookup_users)
  
  #####Find friends of submitted name#####
  one_degree<-get_friends(name_id,retryonratelimit = TRUE)
  one_deg_id<-one_degree$user_id
  one_deg_int_details<-twit_lookup_users(one_deg_id,100)
  
  #####Remove friends missing keywords#####
  desc1<-one_deg_int_details$description
  search_terms<-keywords
  one_deg_keep<-grep(paste(search_terms,collapse="|"),desc1,ignore.case=TRUE)
  one_deg_details<-one_deg_int_details[one_deg_keep,]
  
  #####Remove friends with too many friends####
  one_deg_id_filtered<-one_deg_details[one_deg_details$friends_count<f_lim,]
  ###################Here is where I add the fix#################
  one_deg_filtered_output<-one_deg_int_details[one_deg_int_details$user_id %in% one_deg_id_filtered$user_id,]
  
  #####Find Friend Networks of friends####
  two_degree<-get_friends(one_deg_id_filtered$user_id,retryonratelimit = TRUE)
  
  #####Combine Friends and Friends of Friends into one list####
  # one_two_degree<-rbind(one_degree,two_degree)
  two_degree_details<-twit_lookup_users(two_degree$user_id,100)
  foff<-rbind(one_deg_filtered_output,two_degree_details)
  # foff<-merge(one_two_degree,one_two_degree_details,by=c("user_id","user_id"))
  # foff_int_out<-foff[c(2,1,3:89)]
  
  #####Remove friends of friends missing keywords#####
  desc2<-foff$description
  foff_keep<-grep(paste(search_terms,collapse="|"),desc2,ignore.case=TRUE)
  foff_out<-foff[foff_keep,]
  
  #####If 2 degrees are requested, output friends of friends######
  if(degree==2){
    foff_out
    saveRDS(foff_out,file = paste(name_id,'.RDS',sep=""),compress=TRUE)
    
    # one_two_degree_unique<-unique(one_two_degree$user_id)
    #otdu_d<-one_two_degree[one_two_degree$user_id==one_two_degree_unique,]
    
    #foff_out<-cbind(one_two_degree$user,one_two_degree_details)
    # one_two_degree_details
    
  }else{
    Sys.sleep(1000)
    ###Preparing to get friends networks of friends of friends
    # two_deg_id<-foff_out$user_id
    # two_deg_details<-twit_lookup_users(two_deg_id,80000)
    ###friend limit on friends of friends####
    two_deg_id_filtered<-two_deg_details[two_deg_details$friends_count<ff_lim,]
    ########ADD HERE???$$$###############################
    two_deg_filtered_output<-two_deg_id_filtered
    
    ####getting friends of friends of friends list
    three_degree<-get_friends(two_deg_id_filtered$user_id,retryonratelimit = TRUE)
    ####combinging friends, friends of friends and friends of friends of friends info###
    
    three_degree_details<-twit_lookup_users(three_degree$user_id,80000)
    one_two_output_setup<-rbind(one_deg_filtered_output, two_deg_filtered_output)
    # fofoff<-merge(three_degree,three_degree_details,by=c("user_id","user_id"))
    # fofoff_int_out<-foff[c(2,1,3:89)]
    
    
    desc3<-three_degree_details$description
    three_degree_keep<-grep(paste(search_terms,collapse="|"),desc3,ignore.case=TRUE)
    three_degree_output<-three_degree_details[three_degree_keep,]
    
    
    fofoff_out<-rbind(one_two_output_setup,three_degree_output)
    
    fofoff_out
    saveRDS(fofoff_out,file = paste(name_id,'.RDS',sep=""),compress=TRUE)
  }
  
}

# desc<-fofoffout$description
# search_terms<-c("math","professor","research","scientist","engineer")
# grep(paste(search_terms,collapse="|"),desc,ignore.case=TRUE)


twit_lookup_users<-function(id_list,divisor){
  ###add blank data frame here###
  id_list_details<-data.frame()
  row_count<-length(id_list)
  
  if(row_count>divisor){
    rows_remainder<-(row_count %% divisor)
    rows_quotient<-as.integer(row_count/divisor)
    i<-1
    j<-divisor
    
    for(n in 1:rows_quotient){
      id_list_add<-id_list[i:j]
      id_list_details_add<-lookup_users(id_list_add)
      id_list_details<-rbind(id_list_details,id_list_details_add)
      i<-i+divisor
      j<-j+divisor
      Sys.sleep(1)
    }
    if(rows_remainder>0){
      j<-(j+rows_remainder)-divisor
      id_list_add<-id_list[i:j]
      id_list_details_add<-lookup_users(id_list_add)
      id_list_details<-rbind(id_list_details,id_list_details_add)
    }
  }else{
    
    id_list_details<-lookup_users(id_list)
  }
  
  id_list_details
  
}

# uniq<-a[!duplicated(a$user_id),]
