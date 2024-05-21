#####++++++++++++++++++++++++++++++++++++++++++++++++++#####
##### scRapEU: An R function to scrape data on EU laws #####
##### Author: Michele Scotto di Vettimo                #####
##### https://mscottodivettimo.github.io/              #####
##### Version of 29 April 2024                         #####
#####++++++++++++++++++++++++++++++++++++++++++++++++++#####

scRapEU<-function(exact_procedures=NULL,years,procedures,extract_texts=c('none'),summary_only=FALSE,verbose=FALSE){
  
  use_eurlex=T # This is used to deactivate scraping from EUR-Lex, mostly added for debugging reasons/to allow function to run when EUR-Lex is down
  
  cat('\nStarting:',as.character(Sys.time()),'\n')
  
  ### Import required packages and define other functions ###
  
  '%!in%' <- Negate('%in%')
  
  suppressMessages(library(rvest)) # needed for scraping html
  suppressMessages(library(stringr)) # str_sub
  suppressMessages(library(rlang)) # is_empty 
  suppressMessages(library(plyr))
  suppressMessages(library(dplyr))
  
  procedures_list<-c(
    ### Ordinary legislative procedure
    'COD'='8', # Codecision procedure (ordinary legislative procedure)
    ### Special legislative procedures
    'CNS'='11', # Consultation procedure (special legislative procedure)
    'APP'='41021', # Consent procedure (special legislative procedure)
    ### Historic legislative procedures
    'AVC'='9', # Assent procedure (historic)
    'SYN'='10', # Cooperation procedure (historic)
    
    ### Non legislative procedures
    'NLE'='41020', # Non-legislative enactments (non legislative procedure)
    
    ### Parliament resolutions and initiatives
    'INI'='14',
    'COS'='15',
    'RSP'='18',
    'DCE'='17',
    'INL'='558000',
    
    ### Budgetary procedures
    'BUD'='19', # Budgetary procedure (budget)
    'DEC'='20', # Discharge procedure (budget)
    'BUI'='597000', # Budgetary initiative (budget)
    
    ### Internal parliament organisation procedures
    'REG'='22',
    'IMM'='23',
    'RSO'='24',
    
    ### Quasi-legislative procedures
    'INS'='26',
    'ACI'='12',
    'DEA'='553000',
    'RPS'='41045'
  )
  
  ### Following lines are to figure out user input and check it is correctly provided
  
  if(!is.null(exact_procedures)){
    oeil_searches<-exact_procedures
    years<-9999
  } else {
    procedures<-procedures_list
    oeil_searches<-NULL
  }
  
  ### Here you should have all info needed for scraping ###
  
  ### Define variables to keep along all the scraping process ###
  
  ### Column names for empty dataframe to be created
  variables<-c('procedure_number','date_initiation','title','procedure_type','procedure_subtype',
               'procedure_status','date_end','act_name','proposal_number','proposal_title','revised_proposal',
               'leg_instrument','subject_oeil','eurovoc_desc','subject_matter','directory_code','cap_topics',
               'celex_proposal','celex_multiple','celex_final',
               'ep_cmtee','ep_cmtee_opinion','n_ep_cmtee_opinion','plenary_texts',#'texts_adopted',
               'rapporteur','rapporteur_appnt','rapporteur_party','rapporteur_url',
               'commission_dg','commissioner',
               'council_config','council_session_id','council_session_date','b_item',
               'relationship_acquis','legal_basis','cmtee_dossier','trilogue','early_agreement',
               'leg_priority','leg_priority_list',
               'summary_proposal','summary_final',
               'text_proposal','text_final',
               'key_events','doc_gateway','url_oeil','error_encountered')
  
  ### List of labels to identify key events
  start_procedure_labels<-c(
    'Commission draft budget published',
    'Commission draft budget',
    'Initial legislative proposal',
    'Initial legislative proposal published',
    'Legislative proposal published',
    'Legislative proposal',
    'Non-legislative basic document published',
    'Non-legislative basic document',
    'Preparatory document')
  
  end_procedure_labels<-c(
    'Final act published in Official Journal',
    'Final act signed',
    'Proposal withdrawn by Commission')
  
  ep_votes_labels<-c(
    'Committee report tabled for plenary, single reading',
    'Motion for a resolution',
    'Motion for a resolution objecting delegated act',
    'Text adopted by Parliament, 1st reading/single reading')
  
  revision_proposal_labels<-c(
    'Amended legislative proposal',
    'Amended legislative proposal for reconsultation published',
    'Modified legislative proposal published',
    'Modified legislative proposal')
  
  trilogue_labels<-c(
    'Approval in committee of the text agreed at 1st reading interinstitutional negotiations',
    'Committee decision to open interinstitutional negotiations with report adopted in committee',
    'Committee decision to enter into interinstitutional negotiations announced in plenary (Rule 71)',
    'Committee decision to enter into interinstitutional negotiations confirmed by plenary (Rule 71)',
    'Committee decision to enter into interinstitutional negotiations confirmed by plenary (Rule 71 - vote)',
    'Committee decision to enter into interinstitutional negotiations announced in plenary (Rule 72)')
  
  early_agreement_labels<-c()
  
  no_text_procedures<-c('RSP')
  
  dfs<-list()
  
  cap_schema<-readRDS(url("https://mscottodivettimo.github.io/scrapeu/oeil_thesaurus.rds"))

  for (year in as.character(c(years))){ # Loop over the years
    gc()
    
    if(is.null(oeil_searches)){
      oeil_searches<-c()
      
      ### Adjust procedure list according to year to make search faster
      if((year>2002) & ('COS' %in% names(procedures))){
        if (verbose==TRUE){cat('COS procedure type has been removed from query as they were no longer in use during the year searched\n')}
        procedures<-procedures[!procedures==procedures[which(names(procedures)=='COS')]]}
      
      if((year>2005) & ('SYN' %in% names(procedures))){
        if (verbose==TRUE){cat('SYN procedure type has been removed from query as they were no longer in use during the year searched\n')}
        procedures<-procedures[!procedures==procedures[which(names(procedures)=='SYN')]]}
      
      if((year<2008) & ('APP' %in% names(procedures))){
        if (verbose==TRUE){cat('APP procedure type has been removed from query as it was not yet in use during the year searched\n')}
        procedures<-procedures[!procedures==procedures[which(names(procedures)=='APP')]]}
      
      if((year>2008) & ('AVC' %in% names(procedures))){
        if (verbose==TRUE){cat('AVC procedure type has been removed from query as they were no longer in use during the year searched\n')}
        procedures<-procedures[!procedures==procedures[which(names(procedures)=='AVC')]]}
      
      if((year>2012) & ('DCE' %in% names(procedures))){
        if (verbose==TRUE){cat('DCE procedure type has been removed from query as they were no longer in use during the year searched\n')}
        procedures<-procedures[!procedures==procedures[which(names(procedures)=='DCE')]]}
      
      if((year<2013) & ('DEA' %in% names(procedures))){
        if (verbose==TRUE){cat('DEA procedure type has been removed from query as it was not yet in use during the year searched\n')}
        procedures<-procedures[!procedures==procedures[which(names(procedures)=='DEA')]]}
      
      for (procedure in procedures){
        
        root_link<-paste0('https://oeil.secure.europarl.europa.eu/oeil/search/search.do?searchTab=y&noHeader=false&snippet=true&:procedureType_sid=',
                          procedure,'&:year=',year,'&lang=en&dismax=y&all&limit=500&lang=en')
        
        connection_established<-FALSE
        errors_to_handle<-c("Timeout was reached|Connection refused|HTTP error 500|HTTP error 404|HTTP error 403|HTTP error 504|Couldn't connect to server")
        attempts<-0
        while((connection_established==FALSE) & (attempts<6)){
          if (str_count(as.character(error_encountered<-try(html_search<-read_html(root_link),silent=T)[1]),errors_to_handle)>0){
            attempts<-attempts+1
            Sys.sleep(30)
            cat('Attempting to establish connection\n')
          } else {
            connection_established<-TRUE
          }
        }
        if(connection_established==TRUE){
          
          html_search %>%
            html_elements(xpath="//h2[contains(@class,'ep-a_heading ep-layout_level2 ')]//
                        div[contains(@class,'ep_title')]") %>%
            html_text2() %>% str_trim()->search_results
          
          results<-as.numeric(str_remove_all(search_results[which(grepl('Result found :|Results found :',search_results))],'[^[:digit:]]'))
          
          if(results>0){
            if(results<501){
              
              html_search %>%
                html_elements(xpath="//h3[contains(@class,'p-a_heading ep-layout_level2-large proc_reference')]//
          div[contains(@class,'ep_title')]//div//a//
          span[contains(@class,'ep_name')]") %>%
                as.character() %>%
                str_extract_all('ep_name\">(.*)<span') %>%
                str_remove_all('ep_name\">|<span')->new_searches
              
              oeil_searches<-c(oeil_searches,new_searches)
              
            } else {
              cat('WARNING: Too many results for',procedure,'\n')
            }
          }
        }
      }
    }
    
    i<-0 # Reset iteration number for each year
    as.data.frame(matrix(NA,1,length(variables))) %>% `colnames<-`(variables)->df # Create new dataframe for each year

    cat(length(oeil_searches),'search results found\n\n')
    
    for (procedure_number in sort(oeil_searches)){
      url_oeil<-paste('https://oeil.secure.europarl.europa.eu/oeil/popups/ficheprocedure.do?reference=',procedure_number,'&l=en',sep='')
      i=i+1 # Track iteration number for filling df with results (inner most loop)
      
      if (i %% 50==0){cat('~~~~ Procedure',i,'of',length(oeil_searches),'-',as.character(Sys.time()),'~~~~\n')}
      
      cat(procedure_number,'\n')

      connection_established<-FALSE
      errors_to_handle<-c("Timeout was reached|Connection refused|HTTP error 500|HTTP error 404|HTTP error 403|HTTP error 504")
      attempts<-0
      while((connection_established==FALSE) & (attempts<6)){
        #cat(attempts,'\n')
        if (str_count(as.character(error_encountered<-try(html_search<-read_html(url_oeil),silent=T)[1]),errors_to_handle)>0){
          attempts<-attempts+1
          Sys.sleep(10)
          cat('Attempting to establish connection to',url_oeil,'\n')
        } else {
          if ('procedure either does not exist or is in the process of being initiated' %in% as.character(html_search)){
            connection_established<-FALSE
          } else {
            connection_established<-TRUE
          }
        }
      }
      if (connection_established==TRUE){
        if (TRUE) { ### Following loop allows me to deactivate scraping in case of debugging to save time
          
          if(use_eurlex==FALSE){cat('WARNING: Not scraping from EUR-Lex\n')}
          
          ###################
          ### Page header ###
          ###################
          
          ### Title of procedure in OEIL page (page header)
          html_search %>%
            html_elements(xpath="//div[contains(@id,'procedure-file-header')]//
                    div//
                    following-sibling::div[contains(@class,'ep_gridcolumn ep-m_header ep-layout_underline')]//
                    span[contains(@class,'ep_name')]") %>%
            html_text2() %>% str_trim()->procedure_title
          
          #####################################
          ### Technical information section ###
          #####################################
          
          if (verbose==TRUE){cat('Technical information\n')}
          
          ### Legal instrument
          
          html_search %>%
            html_elements(xpath="//div[contains(@id,'technical_information-data')]//
                    strong[contains(.,'Legislative instrument')]//
                    ancestor::div[contains(@class,'ep-table-cell ep-table-cell-s ep-table-column-head')]//
                    following-sibling::div[contains(@class,'ep-table-cell ep-table-cell-xl ep-table-column-head')]//
                    span[contains(@class,'ep_name')]") %>%
            html_text2() %>% str_trim()->leg_instrument
          if(is_empty(leg_instrument)){
            if(str_count(procedure_title,'Report')>0){
              leg_instrument<-'Report'
            }
          } 
          
          ### Procedure type
          
          html_search %>%
            html_elements(xpath="//div[contains(@id,'technical_information-data')]//
                    strong[contains(.,'Procedure type')]//
                    ancestor::div[contains(@class,'ep-table-cell ep-table-cell-s ep-table-column-head')]//
                    following-sibling::div[contains(@class,'ep-table-cell ep-table-cell-xl ep-table-column-head')]//
                    span[contains(@class,'ep_name')]") %>%
            html_text2() %>% str_trim()->procedure_type
          
          procedure_type=case_when(
            str_count(procedure_type,'AVC - ')>0 ~ 'AVC',
            str_count(procedure_type,'COD - ')>0 ~ 'COD',
            str_count(procedure_type,'CNS - ')>0 ~ 'CNS',
            str_count(procedure_type,'SYN - ')>0 ~ 'SYN',
            str_count(procedure_type,'APP - ')>0 ~ 'APP',
            str_count(procedure_type,'BUD - ')>0 ~ 'BUD',
            str_count(procedure_type,'DEC - ')>0 ~ 'DEC',
            str_count(procedure_type,'BUI - ')>0 ~ 'BUI',
            str_count(procedure_type,'NLE - ')>0 ~ 'NLE',
            str_count(procedure_type,'INL - ')>0 ~ 'INL',
            str_count(procedure_type,'INI - ')>0 ~ 'INI',
            str_count(procedure_type,'RSP - ')>0 ~ 'RSP',
            str_count(procedure_type,'DCE - ')>0 ~ 'DCE',
            str_count(procedure_type,'REG - ')>0 ~ 'REG',
            str_count(procedure_type,'IMM - ')>0 ~ 'IMM',
            str_count(procedure_type,'RSO - ')>0 ~ 'RSO',
            str_count(procedure_type,'COS - ')>0 ~ 'COS',
            str_count(procedure_type,'INS - ')>0 ~ 'INS',
            str_count(procedure_type,'ACI - ')>0 ~ 'ACI',
            str_count(procedure_type,'DEA - ')>0 ~ 'DEA',
            str_count(procedure_type,'RPS - ')>0 ~ 'RPS',
            TRUE ~ procedure_type)
          
          ### Procedure subtype
          
          html_search %>%
            html_elements(xpath="//div[contains(@id,'technical_information-data')]//
                    strong[contains(.,'Procedure subtype')]//
                    ancestor::div[contains(@class,'ep-table-cell ep-table-cell-s ep-table-column-head')]//
                    following-sibling::div[contains(@class,'ep-table-cell ep-table-cell-xl ep-table-column-head')]//
                    span[contains(@class,'ep_name')]") %>%
            html_text2() %>% str_trim()->procedure_subtype
          
          ### Legal basis
          
          html_search %>%
            html_elements(xpath="//div[contains(@id,'technical_information-data')]//
                    strong[contains(.,'Legal basis')]//
                    ancestor::div[contains(@class,'ep-table-cell ep-table-cell-s ep-table-column-head')]//
                    following-sibling::div[contains(@class,'ep-table-cell ep-table-cell-xl ep-table-column-head')]//
                    span[contains(@class,'ep_name')]") %>%
            html_text2() %>% str_trim()->legal_basis
          
          ### Committee dossier
          
          html_search %>%
            html_elements(xpath="//div[contains(@id,'technical_information-data')]//
                    strong[contains(.,'Committee dossier')]//
                    ancestor::div[contains(@class,'ep-table-cell ep-table-cell-s ep-table-column-head')]//
                    following-sibling::div[contains(@class,'ep-table-cell ep-table-cell-xl ep-table-column-head')]//
                    span[contains(@class,'ep_name')]") %>%
            html_text2() %>% str_trim()->cmtee_dossier
          
          #################################
          ### Basic information section ###
          #################################
          
          if (verbose==TRUE){cat('Basic information\n')}
          
          ### Status of the procedure (completed, rejected, withdrawn, pending) from OEIL (basic information section)
          
          html_search %>%
            html_elements(xpath="//div[contains(@id,'basic-information-data')]//
                    div/
                    following-sibling::div[contains(.,'Status')]//p[contains(@class,'procedure-status')]") %>%
            html_text2() %>% str_trim()->stage_reached
          
          procedure_status=case_when(str_count(stage_reached,'Procedure completed')>0 ~ 'Completed',
                                     str_count(tolower(stage_reached),'preparatory phase|awaiting council|awaiting parliament|awaiting committee|awaiting final|awaiting signature|awaiting plenary')>0 ~ 'Pending',
                                     str_count(tolower(stage_reached),'lapsed or withdrawn')>0 ~ 'Withdrawn',
                                     str_count(tolower(stage_reached),'rejected')>0 ~ 'Rejected',
                                     TRUE ~ NA)
          
          
          #################################################################
          ### Text in basic info box (brute force to extract the following)
          html_search %>%
            html_elements(xpath="//div[contains(@id,'basic-information-data')]") %>% html_text2()->basic_info_text
          
          basic_info_text<-str_replace_all(str_remove_all(basic_info_text,'\\r'),"([\\s+\\n+\\s+])\\1+",' \\1 ')
          basic_info_text<-as.list(strsplit(basic_info_text, '\\s+\\n+\\s+'))
          
          #if(grepl('actually rejected',basic_info_text)){procedure_status<-'Rejected'}
          
          for (b in 1:length(basic_info_text[[1]])){
            
            ### Legislative priority (not all acts have it)
            if (str_count(basic_info_text[[1]][b],'Legislative priori')>0){
              leg_priority=1
              leg_priority_list<-str_replace_all(str_remove(basic_info_text[[1]][b],'Legislative priorities\n'),'\\n','; ')
            }
            
            ### Geographic scope (not all acts have it)
            
            ### Relationship with other documents (remove code from below if extracting it here)
            # Generally stored between procedure type and subject
            if (str_count(basic_info_text[[1]][b],procedure_type)>0){ # If current basic_info element has procedure number...
              if((b+2)<(length(basic_info_text[[1]])+1)){
                if (str_count(basic_info_text[[1]][b+2],'Subject')>0){ # If basic_info element + 2 is subject...
                  if (str_detect(basic_info_text[[1]][4],"[0-9]{4}/[0-9]{4}")){ # And if basic_info + 1 has a pattern matching another procedure number...
                    relations<-str_replace_all(basic_info_text[[1]][b+1],'\\n','; ') # Then this is likely to be the field with relationship to other procedures
                  }
                }
              }
            }
            
            ### Subject
            if (str_count(basic_info_text[[1]][b],'Subject\n')>0){
              subject_oeil<-str_replace_all(str_replace_all(str_remove(basic_info_text[[1]][b],'Subject\n'),';',','),'\\n','; ')
              str_trim(subject_oeil)->subject_oeil
              subject_oeil<-ifelse(subject_oeil=='',NA,subject_oeil)
            }
            
          } # End of loop on items taken from the basic info box
          
          ###################
          ### Key players ###
          ###################
          
          ### European Parliament ###
          
          ### EP committee
          
          html_search %>%
            html_elements(xpath="//div[contains(@class,'ep-table keyplayers-table')]//
                    div[contains(@data-column-head,'Committee responsible')]//abbr") %>%
            html_text2() %>% str_trim()->ep_cmtee
          
          ### Committees for opinion
          
          html_search %>%
            html_elements(xpath="//div[contains(@class,'ep-table keyplayers-table')]//
                    div[contains(@data-column-head,'Committee for opinion')]//abbr") %>%
            html_text2() %>% str_trim()->ep_cmtee_opinion
          
          length(ep_cmtee_opinion)->n_ep_cmtee_opinion
          
          paste(as.character(ep_cmtee_opinion),sep='',collapse='; ')->ep_cmtee_opinion
          
          ### Rapporteur
          
          html_search %>%
            html_elements(xpath="//div[contains(@class,'ep-table keyplayers-table')]//
                    div[contains(@data-column-head,'Committee responsible')]//
                    following-sibling::div[contains(@data-column-head,'Rapporteur')]//
                    span[contains(@id,'x3')]") %>%
            html_text2() %>% str_trim()->rapporteur
          # In case there are two rapporteurs, store them in same semi-colon separated objects
          paste(as.character(rapporteur),sep='',collapse='; ')->rapporteur
          rapporteur<-ifelse(rapporteur=='',NA,rapporteur)
          
          html_search %>%
            html_elements(xpath="//div[contains(@class,'ep-table keyplayers-table')]//
                    div[contains(@data-column-head,'Committee responsible')]//
                    following-sibling::div[contains(@data-column-head,'Appointed')]//
                    span[contains(@class,'ep_name')]") %>%
            html_text2() %>% str_trim()->rapporteur_appnt
          # In case there are two rapporteurs, store appointment dates in same semi-colon separated objects
          str_replace_all(paste(as.character(rapporteur_appnt),sep='',collapse='; '),'\r',';')->rapporteur_appnt
          rapporteur_appnt<-ifelse(rapporteur_appnt=='',NA,rapporteur_appnt)
          
          html_search %>%
            html_elements(xpath="//div[contains(@class,'ep-table keyplayers-table')]//
                    div[contains(@data-column-head,'Committee responsible')]//
                    following-sibling::div[contains(@data-column-head,'Rapporteur')]//
                    span[contains(@id,'x2')]//span") %>%
            html_attr('title') %>%
            str_remove_all('\\s+\n+\\s+')->rapporteur_party
          # In case there are two rapporteurs, store parties in same semi-colon separated objects
          paste(as.character(rapporteur_party),sep='',collapse='; ')->rapporteur_party
          rapporteur_party<-ifelse(rapporteur_party=='',NA,rapporteur_party)
          
          html_search %>%
            html_elements(xpath="//div[contains(@class,'ep-table keyplayers-table')]//
                    div[contains(@data-column-head,'Committee responsible')]//
                    following-sibling::div[contains(@data-column-head,'Rapporteur')]//
                    span[contains(@id,'x3')]//
                    ancestor::a") %>% html_attr('href')->rapporteur_url
          # In case there are two rapporteurs, store urls in same semi-colon separated objects
          paste(as.character(rapporteur_url),sep='',collapse='; ')->rapporteur_url
          rapporteur_url<-ifelse(rapporteur_url=='',NA,rapporteur_url)
          
          ### Council of Ministers ###
          
          ### Council configuration
          
          html_search %>%
            html_elements(xpath="//div[contains(@id,'keyplayers_sectionC-content')]//
                    div[contains(@data-column-head,'Council configuration')]") %>%
            html_text2() %>% str_trim()->council_config
          paste(as.character(council_config),sep='',collapse='; ')->council_config
          council_config<-ifelse(council_config=='',NA,council_config)
          
          ### Meeting ID
          
          html_search %>%
            html_elements(xpath="//div[contains(@id,'keyplayers_sectionC-content')]//
                    div[contains(@data-column-head,'Council configuration')]//
                          following-sibling::div//div") %>%
            html_text2() %>% str_trim() %>% str_remove_all(pattern ='[[:digit:]]{2}/[[:digit:]]{2}/[[:digit:]]{4}')->council_session_id
          council_session_id<-ifelse(council_session_id=='',NA,council_session_id)
          paste(as.character(na.omit(council_session_id)),sep='',collapse='; ')->council_session_id
          
          ### Meeting date
          
          html_search %>%
            html_elements(xpath="//div[contains(@id,'keyplayers_sectionC-content')]//
                    div[contains(@data-column-head,'Date')]") %>%
            html_text2() %>% str_trim()->council_session_date
          council_session_date<-ifelse(council_session_date=='',NA,council_session_date)
          paste(as.character(na.omit(council_session_date)),sep='',collapse='; ')->council_session_date
          
          ### European Commission ###
          
          ### Commission DG/Service
          
          html_search %>%
            html_elements(xpath="//div[contains(@id,'keyplayers_sectionEC-content')]//
                    div[contains(@data-column-head,'Commission DG')]") %>%
            html_text2() %>% str_trim()->commission_dg
          
          ### Commissioner
          
          html_search %>%
            html_elements(xpath="//div[contains(@id,'keyplayers_sectionEC-content')]//
                    div[contains(@data-column-head,'Commissioner')]") %>%
            html_text2() %>% str_trim()->commissioner
          
          ##########################
          ### Key events section ###
          ##########################
          
          if (verbose==TRUE){cat('Key events\n')}
          
          html_search %>%
            html_elements(xpath="//div[contains(@id,'key_events-data')]//
                    div[contains(@class,'ep-table-row')]//
                    div[contains(@class,'ep-table-cell ep-table-cell-s ep-table-column-head')]//
                    span") %>%
            html_text2() %>% str_trim()->key_events_date
          
          html_search %>%
            html_elements(xpath="//div[contains(@id,'key_events-data')]//
                    div[contains(@class,'ep-table-row')]//
                    div[contains(@class,'ep-table-cell ep-table-cell-xl ep-table-column-head')]//
                    span") %>%
            html_text2() %>% str_trim()->key_events_event
          
          key_events_colonsep<-str_remove_all(paste(as.character(key_events_event), sep=" ", collapse="; "),',')
          
          # External documents list
          
          html_search %>%
            html_elements(xpath="//div[contains(@id,'key_events-data')]//
                    div[contains(@class,'ep-table-row')]//
                    div[contains(@class,'ep-table-cell ep-table-cell-m center-cell')]//
                    span//a[contains(@class,'externalDocument')]") %>%
            html_text2() %>% str_trim()->documents
          
          grep("^A[A-Z]?[0-9]-|^RC[0-9]-|^B[0-9]-",documents,value=T)->plenary_texts
          
          grep("^T[A-Z]?[0-9]-",documents,value=T)->texts_adopted
          
          if(is_empty(plenary_texts) & !is_empty(texts_adopted)){
            for(ta in texts_adopted){
              
              if(grepl('^T[0-9]{1,2}',ta)){ta<-str_replace(ta,'T','TA-')}
              
              paste0(str_split(ta,'-')[[1]][1],'-',
                     str_split(ta,'-')[[1]][2],'-',
                     str_split(str_remove_all(ta,'TA-[0-9]{1,2}-'),'/')[[1]][2],'-',
                     str_split(str_remove_all(ta,'TA-[0-9]{1,2}-'),'/')[[1]][1])->ta_search
            
              connection_established_votes<-FALSE
              vote_attempts<-0
              while((connection_established_votes==FALSE) & (vote_attempts<6)){
                vote_url<-paste0("https://www.europarl.europa.eu/doceo/document/",ta_search,"_EN.html")
                if (str_count(as.character(error_encountered<-try(vote_html<-read_html(vote_url),silent=T)[1]),errors_to_handle)>0){
                  vote_attempts<-vote_attempts+1
                  Sys.sleep(10)
                  cat('Attempting to establish connection to',vote_url,'\n')
                } else {
                  connection_established_votes<-TRUE
                }
              }
              if (connection_established_votes==TRUE){
                vote_html %>%
                  html_elements(xpath="//p[contains(.,'Texts tabled :')]//
                              following-sibling::a") %>%
                  html_text2() %>% str_trim()->pt
                plenary_texts<-c(plenary_texts,pt)
              }
            }
          }
          
          setdiff(paste(as.character(plenary_texts),sep='',collapse='; '),"")->plenary_texts
          
          setdiff(paste(as.character(texts_adopted),sep='',collapse='; '),"")->texts_adopted
          
          ### Date of initiation of procedure from OEIL (basic information section)
          
          key_events_date[which(key_events_event %in% start_procedure_labels)]->date_initiation
          
          ### Trilogue dummy
          
          trilogue<-ifelse(any(key_events_event %in% trilogue_labels),1,0)
          
          ### Early agreement dummy
          
          early_agreement<-ifelse(any(key_events_event %in% early_agreement_labels),1,0)
          
          ### URL to PDF and number of the proposal instrument
          
          if(!is_empty(which(key_events_event %in% start_procedure_labels))){
            for (lbl in start_procedure_labels){ # Loop over labels used to identify start of procedure and generate string to path
              xpath_string=paste("//div[contains(@id,'key_events-data')]//
                         div[contains(@class,'ep-table-row')]//
                         div[contains(@class,'ep-table-cell ep-table-cell-xl ep-table-column-head')]//
                         span[contains(.,'",lbl,"')]//
                         ancestor::div[contains(@class,'ep-table-cell ep-table-cell-xl ep-table-column-head')]//
                         following-sibling::div[contains(@class,'ep-table-cell ep-table-cell-m center-cell')]//
                         div//span",sep='')
              if (!is.na(html_search %>% html_node(xpath=eval(xpath_string)))){ # string to path identifies a node
                
                html_search %>% html_node(xpath=eval(xpath_string))->proposal_node
                
                ### Proposal number
                
                proposal_node %>% html_text2() %>% str_trim()->proposal_number
                
                ### Summary of the proposal instrument
                
                if((extract_texts %in% c('all','proposals')) & procedure_type %!in% c(no_text_procedures)){
                  html_search %>%
                    html_elements(xpath=paste("//div[contains(@id,'key_events-data')]//
                div[contains(@class,'ep-table-row')]//
                div[contains(@class,'ep-table-cell ep-table-cell-xl ep-table-column-head')]//
                span[contains(.,'",lbl,"')]//
                ancestor::div[contains(@class,'ep-table-cell ep-table-cell-xl ep-table-column-head')]//
                following-sibling::div[contains(@class,'ep-table-cell ep-table-cell-m center-cell')]//
                span[contains(.,'Summary')]//
                parent::button",sep='')) %>%
                    html_attr('onclick')->onclick_attr_proposal
                  onclick_attr_proposal<-ifelse(is_empty(onclick_attr_proposal),NA,onclick_attr_proposal)
                  
                  if ((is_empty(onclick_attr_proposal)==FALSE) & is.na(onclick_attr_proposal)==FALSE){
                    paste('https://oeil.secure.europarl.europa.eu',str_remove(onclick_attr_proposal,"location.href\\=\\'"),sep='')->url_summary_proposal
                    try(read_html(url_summary_proposal) %>%
                          html_node(xpath="//div[contains(@class,'ep-a_text')]") %>% html_text2()->text_summary_proposal,silent=T)
                  }
                }
              }  # End of condition of wrong label used to find a path
            } # End of loop over possible labels used to identify initiating document
            # Note: if no label leads to a path, proposal information is missing
          }
          
          ### Revised proposal 0/1
          
          revised_proposal<-any(key_events_event %in% revision_proposal_labels)
          
          ### URL to PDF and number of the revised proposal instrument
          
          #for (lbl in start_procedure_labels){ # Loop over labels used to identify start of procedure and generate string to path
          #  xpath_string=paste("//div[contains(@id,'key_events-data')]//
          #               div[contains(@class,'ep-table-row')]//
          #               div[contains(@class,'ep-table-cell ep-table-cell-xl ep-table-column-head')]//
          #               span[contains(.,'",lbl,"')]//
          #               ancestor::div[contains(@class,'ep-table-cell ep-table-cell-xl ep-table-column-head')]//
          #               following-sibling::div[contains(@class,'ep-table-cell ep-table-cell-m center-cell')]//
          #               div//span",sep='')
          #  if (!is.na(html_search %>% html_node(xpath=eval(xpath_string)))){ # string to path identifies a node
              
          #    html_search %>% html_node(xpath=eval(xpath_string))->proposal_node
              
              ### Revised proposal number
              
          #    proposal_node %>% html_text2() %>% str_trim()->proposal_number
              
              ### Summary of the proposal instrument
              
          #    html_search %>%
          #      html_elements(xpath=paste("//div[contains(@id,'key_events-data')]//
          #      div[contains(@class,'ep-table-row')]//
          #      div[contains(@class,'ep-table-cell ep-table-cell-xl ep-table-column-head')]//
          #      span[contains(.,'",lbl,"')]//
          #      ancestor::div[contains(@class,'ep-table-cell ep-table-cell-xl ep-table-column-head')]//
          #      following-sibling::div[contains(@class,'ep-table-cell ep-table-cell-m center-cell')]//
          #      span[contains(.,'Summary')]//
          #      parent::button",sep='')) %>%
          #      html_attr('onclick')->onclick_attr_proposal
          #    onclick_attr_proposal<-ifelse(is_empty(onclick_attr_proposal),NA,onclick_attr_proposal)
              
          #    if ((is_empty(onclick_attr_proposal)==FALSE) & is.na(onclick_attr_proposal)==FALSE){
          #      paste('https://oeil.secure.europarl.europa.eu',str_remove(onclick_attr_proposal,"location.href\\=\\'"),sep='')->url_summary_proposal
          #      try(read_html(url_summary_proposal) %>%
          #            html_node(xpath="//div[contains(@class,'ep-a_text')]") %>% html_text2()->text_summary_proposal,silent=T)
          #    }
          #  }  # End of condition of wrong label used to find a path
          #} # End of loop over possible labels used to identify initiating document
          # Note: if no label leads to a path, proposal information is missing
          
          ### Revised proposal date
          
          ### Revised proposal number
          
          ### Number of EP readings
          
          ################################
          ### Document gateway section ###
          ################################
          
          if (verbose==TRUE){cat('Document gateway\n')}
          
          html_search %>%
            html_elements(xpath="//div[contains(@id,'document_gateway-data')]//
                    div[contains(@class,'ep-table-row')]//
                    div[contains(@class,'ep-table-cell')]//
                    span") %>%
            html_text2() %>% str_trim() %>%
            str_extract_all("[0-9]{2}/[0-9]{2}/[0-9]{4}") %>%
            unlist()->doc_gateway_date
          
          if(is.null(doc_gateway_date)){
            html_search %>%
              html_elements(xpath="//div[contains(@id,'document_gateway-data')]//
                    span[contains(@class,'ep-table-row')]//
                    div[contains(@class,'ep-table-cell')]//
                    span") %>%
              html_text2() %>% str_trim() %>%
              str_extract_all("[0-9]{2}/[0-9]{2}/[0-9]{4}") %>%
              unlist()->doc_gateway_date
          }
          
          html_search %>%
             html_elements(xpath="//div[contains(@id,'document_gateway-data')]//
                     div[contains(@class,'ep-table-row')]//
                     div[contains(@class,'ep-table-cell ep-table-cell-s')]//
                     span[contains(@class,'ep_name')]") %>%
             html_text2() %>% str_trim() %>%
             unlist()->doc_gateway_docs
          doc_gateway_docs<-setdiff(doc_gateway_docs,'')
          
          html_search %>%
            html_elements(xpath="//div[contains(@id,'document_gateway-data')]//
                    div[contains(@class,'ep-table-row')]//
                    div[contains(@class,'ep-table-cell ep-table-cell-l')]//
                    span") %>%
            html_text2() %>% str_trim()->doc_gateway_item
          
          doc_gateway_colonsep<-str_remove_all(paste(as.character(doc_gateway_item), sep=" ", collapse="; "),',')
          
          ### Check if missing plenary_texts and if could be filled with document gateway
          
          if(any(ep_votes_labels %in% doc_gateway_item)){
            for (p in intersect(ep_votes_labels,doc_gateway_item)){
              plenary_texts<-c(plenary_texts,doc_gateway_docs[which(doc_gateway_item==p)])
            }
          }
          
          setdiff(paste(sort(as.character(unique(plenary_texts))),sep='',collapse='; '),"")->plenary_texts
          
          plenary_texts<-unique(c(plenary_texts,texts_adopted))
          
          if (exists("proposal_node")==FALSE){ # Repeat actions to find proposal number and text only if there's been failure to find the node before
            for (lbl in start_procedure_labels){ # Loop over labels used to identify start of procedure and generate string to path
              xpath_string=paste("//div[contains(@id,'document_gateway-data')]//
              div[contains(@class,'ep-table-row')]//
              div[contains(@class,'ep-table-cell ep-table-cell-l')]//
              span[contains(.,'",lbl,"')]//
              ancestor::div[contains(@class,'ep-table-cell ep-table-cell-l')]//
              following-sibling::div[contains(@class,'ep-table-cell ep-table-cell-s')]//
              div//span",sep='')
              if (!is.na(html_search %>% html_node(xpath=eval(xpath_string)))){ # string to path identifies a node
                
                html_search %>% html_node(xpath=eval(xpath_string))->proposal_node
                
                ### Proposal number
                
                proposal_node %>% html_text2() %>% str_trim()->proposal_number
                
                ### Proposal date
                
                doc_gateway_date[which(doc_gateway_item %in% start_procedure_labels)] %>% unique()->date_initiation
                
                ### Summary of the proposal instrument
                
                if(extract_texts %in% c('all','proposals')){
                  html_search %>%
                    html_elements(xpath=paste("//div[contains(@id,'document_gateway-data')]//
                  div[contains(@class,'ep-table-row')]//
                  div[contains(@class,'ep-table-cell ep-table-cell-l')]//
                  span[contains(.,'",lbl,"')]//
                  ancestor::div[contains(@class,'ep-table-cell ep-table-cell-l')]//
                  following-sibling::div[contains(@class,'ep-table-cell')]//
                  span[contains(.,'Summary')]//
                  parent::button",sep='')) %>%
                    html_attr('onclick') %>% unique()->onclick_attr_proposal
                  onclick_attr_proposal<-ifelse(is_empty(onclick_attr_proposal),NA,onclick_attr_proposal)
                  
                  if ((is_empty(onclick_attr_proposal)==FALSE) & is.na(onclick_attr_proposal)==FALSE){
                    paste('https://oeil.secure.europarl.europa.eu',str_remove(onclick_attr_proposal,"location.href\\=\\'"),sep='')->url_summary_proposal
                    try(read_html(url_summary_proposal) %>%
                          html_node(xpath="//div[contains(@class,'ep-a_text')]") %>% html_text2()->text_summary_proposal,silent=T)
                  }
                }
              }  # End of condition of wrong label used to find a path
            } # End of loop over possible labels used to identify initiating document
          } # End of condition proposal node was not found
          
          ################################################
          ### Information to be collected from EUR-Lex ###
          ################################################
          
          if(use_eurlex){
            if (exists("proposal_node")==TRUE){
              if (verbose==TRUE){cat('EUR-Lex of proposal\n')}
              
              url_eurlex_proposal<-proposal_node %>% html_node(xpath="//a[contains(@title,'EUR-Lex')]") %>% html_attr('href')
              
              connection_eurlex_proposal_established<-FALSE
              if (is.na(url_eurlex_proposal)){
                if (verbose==TRUE){
                  cat('EUR-Lex link not found\n')
                  url_eurlex_proposal<-proposal_node %>% html_node(xpath=paste("//a[contains(.,'",proposal_number,"')]",sep='')) %>% html_attr('href')
                }
              } else {
                eurlex_attempts<-0
                while((connection_eurlex_proposal_established==FALSE) & (eurlex_attempts<6)){
                  if (str_count(as.character(error_encountered<-try(eurlex_search_proposal<-read_html(url_eurlex_proposal),silent=T)[1]),errors_to_handle)>0){
                    eurlex_attempts<-eurlex_attempts+1
                    Sys.sleep(10)
                    cat('Attempting to establish connection to',url_eurlex_proposal,'\n')
                  } else {
                    #if ('procedure either does not exist or is in the process of being initiated' %in% as.character(eurlex_search_proposal)){
                    if ('procedure either does not exist or is in the process of being initiated' %in% as.character(try(read_html(url_eurlex_proposal),silent=T))){
                      connection_eurlex_proposal_established<-FALSE
                    } else {
                      connection_eurlex_proposal_established<-TRUE
                    }
                  }
                }
              } # End of condition for non-missing EUR-Lex url
              
              if (connection_eurlex_proposal_established==TRUE){
                # Note, it might be the case that a connection is established but there is no real eurlex page for the proposal
                # Example: 1999/0004(AVC): https://eur-lex.europa.eu/search.html?type=expert&qid=1690030654703
                
                ### Celex number of the original proposal instrument
                
                try(eurlex_search_proposal %>%
                      html_node(xpath="//meta[contains(@name,'WT.z_docID')]") %>%
                      html_attr('content') %>% str_trim()->celex_proposal,silent=T)
                
                if(exists("celex_proposal")==FALSE){celex_proposal<-NA}
                
                if (is.na(celex_proposal)){
                  eurlex_search_proposal %>%
                    html_nodes(xpath="//dt[contains(.,'CELEX number:')]//following-sibling::dd") %>% html_text2()->possible_celex
                  # Remove elements without any number in it
                  possible_celex<-possible_celex[grepl("[[:digit:]]", possible_celex)]
                  # Identify and remove corrigenda
                  possible_celex<-possible_celex[!grepl("R\\([[:digit:]]", possible_celex)]
                  # Keep unique remove duplicates if any
                  possible_celex<-unique(possible_celex)
                  # If both DC and PC in celexes, keep only PC
                  if(any(grepl('PC',possible_celex))==T & any(grepl('DC',possible_celex))==T){possible_celex<-grep('PC',possible_celex,value=T)}
                  # Sort
                  possible_celex<-sort(possible_celex)
                  if (length(possible_celex)>1){
                    celex_multiple<-TRUE
                    while(celex_multiple==T){
                      for (cel in possible_celex){
                        # Try to disambiguate
                        if (verbose==TRUE){cat('Trying to disambiguate CELEX of the proposal\n')}
                        
                        ### new 
                        if (str_count(as.character(error_encountered<-try(possible_proposal_page<-read_html(paste('https://eur-lex.europa.eu/legal-content/EN/ALL/?uri=CELEX:',cel,sep='')),silent=T)[1]),errors_to_handle)==0){
                          possible_proposal_page<-read_html(paste('https://eur-lex.europa.eu/legal-content/EN/ALL/?uri=CELEX:',cel,sep=''))
                          possible_proposal_page %>%
                            html_node(xpath="//dt[contains(.,'Procedure number')]//
                                following-sibling::dd//a") %>% html_text2()->procedure_in_celex
                          if(!is.na(procedure_in_celex) & (str_count(procedure_in_celex,substr(procedure_number,1,9))>0)){
                            celex_proposal<-cel
                            celex_multiple<-FALSE
                          } else {
                            possible_proposal_page<-read_html(paste('https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:',cel,sep=''))
                            possible_proposal_page %>%
                              html_node(xpath="//div[contains(@id,'TexteOnly')]//txt_te//p") %>% html_text2()->eurlex_proposal_title
                            if(grepl(str_remove_all(substr(procedure_number,6,14),'[^[:alpha:]|^[:digit:]]'),str_remove_all(eurlex_proposal_title,'[^[:alpha:]|^[:digit:]]'))){
                              celex_proposal<-cel
                              celex_multiple<-FALSE
                            } else{
                              if(cel==possible_celex[length(possible_celex)]){
                                celex_proposal<-possible_celex[1]
                                celex_multiple<-FALSE
                              }
                            }
                          }
                        } else {if (verbose==TRUE){cat('Error in reading possible page of proposal\n')}}
                      }
                    }
                  } else {
                    if (length(possible_celex)==0){
                      celex_multiple=FALSE
                    } else {
                      celex_proposal=possible_celex
                      celex_multiple=FALSE
                    }
                  } 
                } # End of condition celex of proposal is not NA
                
                ### Move to EUR-Lex page of the proposal with details on the procedure steps
                
                if(!is_empty(celex_proposal) & !is.na(celex_proposal)){
                  
                  if(try(proposal_ALL<-read_html(paste('https://eur-lex.europa.eu/legal-content/EN/ALL/?uri=CELEX:',celex_proposal,sep='')),silent=T)[1]=="Error in open.connection(x, \"rb\") : HTTP error 404.\n"){
                    # Proposal link does not work
                  } else {
                    
                    ### Title of the proposal useful for finding legal instrument if missing
                    
                    proposal_ALL %>%
                      html_node(xpath="//meta[contains(@name,'z_docTitle')]") %>%
                      html_attr('content') %>% str_trim()->proposal_title
                    
                    ### Subject matter
                    
                    proposal_ALL %>%
                      html_nodes(xpath="//dt[contains(.,'Subject matter')]//
                          following-sibling::dd//span") %>% html_text2()->subject_matter
                    paste(as.character(subject_matter),sep='',collapse='; ')->subject_matter
                    subject_matter<-ifelse(subject_matter=='',NA,subject_matter)
                    
                    ### Eurovoc descriptors
                    
                    proposal_ALL %>%
                      html_nodes(xpath="//dt[contains(.,'EUROVOC descriptor')]//
                          following-sibling::dd//span") %>% html_text2()->eurovoc_desc
                    paste(as.character(eurovoc_desc),sep='',collapse='; ')->eurovoc_desc
                    eurovoc_desc<-ifelse(eurovoc_desc=='',NA,eurovoc_desc)
                    
                    ### Directory codes
                    
                    proposal_ALL %>%
                      html_nodes(xpath="//dt[contains(.,'Directory code')]//
                          following-sibling::dd//span") %>% html_text2()->directory_code
                    paste(as.character(directory_code),sep='',collapse='; ')->directory_code
                    directory_code<-ifelse(directory_code=='',NA,directory_code)
                  } # End of condition link of EUR-Lex proposal works
                  
                  if(try(proposal_HIS<-read_html(paste0('https://eur-lex.europa.eu/legal-content/EN/HIS/?uri=CELEX:',celex_proposal)),silent=T)[1]=="Error in open.connection(x, \"rb\") : HTTP error 404.\n"){
                    # Proposal link does not work
                  } else {
                    
                    if(exists("proposal_HIS")==TRUE){
                      
                      ### B item on Council agenda
                      
                      proposal_HIS %>%
                        html_elements(xpath="//dt[contains(.,'Council agenda')]//
                                    following-sibling::dd") %>% as.character()->council_agenda
                      if(!is_empty(council_agenda)){
                        b_item<-max(str_count(council_agenda,'ITEM \"B\"'))
                        if(b_item %in% c(0,1)){} else {
                          b_item<-ifelse(b_item==Inf,0,b_item)
                          b_item<-ifelse(is.na(b_item),0,b_item)
                        }
                      } else {b_item<-0}
                    }
                  } # End of condition link of EUR-Lex proposal works
                  
                  if(extract_texts %in% c('all','proposal') & summary_only==FALSE){
                    if(try(proposal_HTML<-read_html(paste0('https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:',celex_proposal)),silent=T)[1]=="Error in open.connection(x, \"rb\") : HTTP error 404.\n"){
                      # Proposal link does not work
                    } else {
                      ### Text of the proposal (not summary)
                      #xpaths<-c("//body","//div[contains(@id,'TexteOnly')]//p//p","//div[contains(@class,'contentWrapper')]")
                      xpaths<-c("//div[contains(@class,'contentWrapper')]","//div[contains(@id,'TexteOnly')]//p//p","//body")
                      text_proposal<-NULL
                      if(exists("proposal_HTML")==TRUE){
                        if(verbose==T){cat('Searching for proposal text\n')}
                        for(xp in xpaths){
                          attempt<-0
                          while(is.null(text_proposal) & attempt==0){
                            attempt=attempt+1
                            if(!is_empty(nodechar<-max(nchar(as.character(xnode<-proposal_HTML %>% html_nodes(xpath=xp)))))){
                              if(nodechar<(3*(10^6))){
                                if(!is.infinite(abs(nodechar))){
                                  xnode %>% html_text2() %>% as.character() %>% paste(collapse='\n')->text_proposal
                                }
                              } else{
                                cat('Proposal text too long:',nodechar,'characters\n')
                              } # End of condition proposal is too long
                            } # End of condition proposal node exists 
                          } # End of condition / attempts for proposal text
                        } # End of loop over possible paths to proposal node
                      } # End of condition HTML proposal page exists
                    } # End of condition link of HTML of proposal works
                  } 
                } # End of condition there is a valid proposal celex
              } # End of condition if eurlex link of proposal has established connection
            } # End of condition for NA node of proposal
          }
          
          ########################################
          ### Only for non-pending legislation ###
          ########################################
          
          if (procedure_status!='Pending' & !is.na(procedure_status)){
            
            if (verbose==TRUE){cat('Procedure is not still pending\n')}
            
            ### Date of conclusion
            
            key_events_date[which(key_events_event %in% end_procedure_labels)]->date_end
            if(is_empty(date_end)){
              key_events_date[length(key_events_date)]->date_end
            }
            if(length(date_end)>1){date_end<-date_end[1]}
            
            ######################################
            ### Only for completed legislation ###
            ######################################
            
            if (procedure_status=='Completed'){
              
              if (verbose==TRUE){cat('Procedure has been completed\n')}
              
              ### Name of the act
              
              html_search %>%
                html_node(xpath="//div[contains(@id,'final_act-data')]//
                  div[contains(@class,'ep-p_text')]//span") %>%
                html_text2() %>% str_trim()->act_name
              
              ### Official journal name of the final act
              
              ### Summary of final act
              
              if(extract_texts %in% c('all','final')){
                html_search %>%
                  html_node(xpath="//div[contains(@id,'final_act-data')]//
                    span[contains(.,'Summary')]//
                    parent::button") %>%
                  html_attr('onclick')->onclick_attr_final
                onclick_attr_final<-ifelse(is_empty(onclick_attr_final),NA,onclick_attr_final)
                
                if ((is_empty(onclick_attr_final)==FALSE) & is.na(onclick_attr_final)==FALSE){
                  paste('https://oeil.secure.europarl.europa.eu',str_remove(onclick_attr_final,"location.href\\=\\'"),sep='')->url_summary_final
                  try(read_html(url_summary_final) %>%
                        html_node(xpath="//div[contains(@class,'ep-a_text')]") %>% html_text2()->text_summary_final,silent = T)
                }
              }
              ####################
              ### From EUR-Lex ###
              ####################
              
              if(use_eurlex){ # Turn to FALSE to deactivate EUR-Lex if website is down/undergoing maintenance
                if (verbose==TRUE){cat('Moving to EUR-Lex\n')}
                
                ### Celex of the final act
                
                html_search %>%
                  html_node(xpath="//div[contains(@id,'final_act-data')]//
                  div[contains(@class,'ep-p_text')]//a") %>%
                  html_attr('href')->url_eurlex_final
                
                if(!is.na(url_eurlex_final)){
                  if (strsplit(as.character(try(read_html(url_eurlex_final),silent=T)[1]),split=':')[[1]][1] %in% c("Error in open.connection(x, \"rb\") ")){
                    cat('EUR-Lex page not working\n')
                    no_eurlex_final_page<-TRUE
                    # it means that EUR-Lex link for this procedure is not working
                    ### Alternative way to figure out celex of final act then:
                    # 1. if summary info is available:
                    if (!is.na(html_search %>% html_node(xpath="//div[contains(@id,'final_act-data')]//button[contains(@id,'summary')]"))){
                      html_search %>%
                        html_node(xpath="//div[contains(@id,'final_act-data')]//
                      button[contains(@id,'summary')]") %>%
                        html_attr('title') %>% as.character() %>%
                        str_remove('Summary for') %>% str_trim()->celex_final
                    } else {
                      # 2. if summary info is NOT available
                      celex_final<-str_remove(str_remove(url_eurlex_final,'https://www.europarl.europa.eu/doceo/document/'),'_EN.html')
                    }
                    # if EUR-Lex link is working
                  } else {
                    
                    connection_eurlex_final_established<-FALSE
                    eurlex_attempts<-0
                    while((connection_eurlex_final_established==FALSE) & (eurlex_attempts<6)){
                      if (str_count(as.character(error_encountered<-try(eurlex_search_final<-read_html(url_eurlex_final),silent=T)[1]),errors_to_handle)>0){
                        eurlex_attempts<-eurlex_attempts+1
                        Sys.sleep(10)
                        cat('Attempting to establish connection to',url_eurlex_final,'\n')
                      } else {
                        if ('procedure either does not exist or is in the process of being initiated' %in% as.character(eurlex_search_final)){
                          connection_eurlex_final_established<-FALSE
                        } else {
                          connection_eurlex_final_established<-TRUE
                        }
                      }
                    }
                    
                    #if (exists("eurlex_search_final")==TRUE){connection_eurlex_final_established<-TRUE}
                    
                    if(connection_eurlex_final_established==TRUE){
                      try(eurlex_search_final %>%
                            html_node(xpath="//meta[contains(@name,'WT.z_docID')]") %>%
                            html_attr('content') %>% str_trim()->celex_final,silent=T)
                      
                      ### Fill gap on proposal information if managed to find celex_final and the former is missing
                      
                      if ((exists("proposal_number")==FALSE) & (!is.na(celex_final))){
                        if (str_count(as.character(error_encountered<-try(eurlex_final_HIS<-read_html(paste('https://eur-lex.europa.eu/legal-content/EN/HIS/?uri=CELEX:',celex_final,sep='')),silent=T)[1]),errors_to_handle)==0){
                          
                          eurlex_final_HIS %>% 
                            html_nodes(xpath="//div[contains(@id,'PROPCOM')]//
                                     following-sibling::div//div//span//a") %>% 
                            html_text2()->celex_proposal
                          
                          eurlex_final_HIS %>% 
                            html_nodes(xpath="//div[contains(@id,'PROPCOM')]//
                                     following-sibling::div//div//
                                     following-sibling::div[contains(@class,'col-xs-5 col-sm-3 eventDate')]") %>% 
                            html_text2()->date_initiation
                          
                          eurlex_final_HIS %>% 
                            html_node(xpath="//div[contains(@id,'PROPCOM')]//
                                     div//following-sibling::div//
                                     dt[contains(.,'Documents:')]//
                                     following-sibling::dd") %>% 
                            html_text2()->proposal_number
                        } # End of condition HIS page of final act opens without problems
                      } # End of condition proposal information is missing
                      
                      if((extract_texts %in% c('all','final') & summary_only==FALSE)){
                        if(try(final_HTML<-read_html(paste0('https://eur-lex.europa.eu/legal-content/EN/TXT/HTML/?uri=CELEX:',celex_final)),silent=T)[1]=="Error in open.connection(x, \"rb\") : HTTP error 404.\n"){
                          # final act HTML text not found
                        } else {
                          ### Text of the final act (not summary)
                          text_final<-NULL
                          #xpaths<-c("//body","//div[contains(@id,'TexteOnly')]//p//p","//div[contains(@class,'contentWrapper')]")
                          xpaths<-c("//div[contains(@class,'contentWrapper')]","//div[contains(@id,'TexteOnly')]//p//p","//body")
                          if(exists("final_HTML")==TRUE){
                            if(verbose==T){cat('Searching for final text\n')}
                            for(xp in xpaths){
                              attempt<-0
                              while(is.null(text_final) & attempt<length(xpaths)+1){
                                attempt<-attempt+1
                                if(!is_empty(nodechar<-max(nchar(as.character(xnode<-final_HTML %>% html_nodes(xpath=xp)))))){
                                  if(nodechar<(3*(10^6))){
                                    if(!is.infinite(abs(nodechar))){
                                      xnode %>% html_text2() %>% as.character() %>% paste(collapse=' \n ')->text_final
                                    }
                                  } else{
                                    cat('Final text too long:',nodechar,'characters\n')
                                  }
                                }
                              }
                            }
                          }
                        } # End of condition error on page with HTML of final act text
                      }
                    } # End of condition if connectin with EUR-Lex final is established
                  } # End of condition if EUR-Lex page of final act is not available
                } # End of condition if page of final act is avaiable
              } # End of condition do not scrape EUR-Lex
            } # End of condition for completed procedures
          } # End of condition for non-pending legislation
            
          no_eurlex_final_page<-ifelse(exists("no_eurlex_final_page")==TRUE,TRUE,FALSE)
          
          ############################################################################################
          ### Find way around to fill gaps in missing data (e.g. move back and forth OEIL/EUR-Lex) ###
          ############################################################################################
          
          if (is_empty(date_initiation)){date_initiation<-unique(min(key_events_date))}
          
          if (procedure_status %in% c('Completed','Withdrawn')){
            
            if (is_empty(date_end)){date_end<-unique(max(key_events_date))}
            
            if (procedure_status %in% c('Completed')){
              if(no_eurlex_final_page==TRUE){
                
              }
            } 
          } 

          ##################################################################
          ### Store information needed for better post-scraping recoding ###
          ##################################################################
    
          if (verbose==TRUE){cat('Dataframe\n')}
    
          #####################################################
          ### Populate spreadsheet at end of each iteration ###
          #####################################################
        
          ### Legal instrument
          
          act_name<-ifelse(exists("act_name")==TRUE,act_name,'')
          proposal_title<-ifelse(exists("proposal_title")==TRUE,proposal_title,'')
          procedure_title<-ifelse(exists("procedure_title")==TRUE,procedure_title,'')
          procedure_subtype<-ifelse(exists("procedure_subtype")==TRUE,procedure_subtype,'')
          
          leg_instrument<-ifelse(is_empty(leg_instrument),NA,leg_instrument)
          
          if(is.na(leg_instrument)){
            leg_instrument<-case_when(
              str_count(act_name,'Decision')>0 ~ 'Decision',
              str_count(tolower(proposal_title),'decision')>0 ~ 'Decision',
              str_count(tolower(procedure_title),'decision')>0 ~ 'Decision',
              str_count(act_name,'Regulation')>0 ~ 'Regulation',
              str_count(tolower(proposal_title),'regulation')>0 ~ 'Regulation',
              str_count(act_name,'Directive')>0 ~ 'Directive',
              str_count(tolower(proposal_title),'directive')>0 ~ 'Directive',
              procedure_subtype %in% c('Commission strategy paper') ~ 'Communication', # https://www.eumonitor.eu/9353000/1/j9vvik7m1c3gyxp/vh7dptp45uyn
              str_count(proposal_title,'Communication from')>0 ~ 'Communication',
              str_count(act_name,'Budget')>0 ~ 'Budget',
              procedure_type=='BUD' ~ 'Budget',
              str_count(procedure_title,'Discharge|discharge:')>0 ~ 'Budget',
              str_count(procedure_title,'Justice and home affairs')>0 ~ 'JHA act',
              str_count(tolower(act_name),'third pillar|jha|justice and home affairs|justice home affairs')>0 ~ 'JHA act',
              str_count(tolower(procedure_subtype),'resolution')>0 ~ 'EP Resolution',
              procedure_subtype %in% c('Initiative') ~ 'EP Resolution',
              procedure_subtype %in% c('Implementation') ~ 'Implementing act',
              procedure_subtype %in% c('Annual report') ~ 'Report',
              str_count(tolower(procedure_subtype),'report')>0 ~ 'Report',
              str_count(tolower(procedure_subtype),'delegated act')>0 ~ 'Delegated act',
              str_count(tolower(procedure_subtype),'immunity')>0 ~ 'Immunity and rules',
              procedure_subtype %in% c('Rules','Parliament organisation','Interpretation of Rules') ~ 'Immunity and rules',
              str_count(tolower(procedure_subtype),'appointment')>0 ~ 'Appointment',
              str_count(tolower(procedure_title),'appointment')>0 ~ 'Appointment',
              str_count(act_name,'Recommendation')>0 ~ 'Recommendation',
              str_count(tolower(proposal_title),'recommendation')>0 ~ 'Recommendation',
              TRUE ~ NA)
          }
          
          act_name<-ifelse(act_name=='',NA,act_name)
          proposal_title<-ifelse(proposal_title=='',NA,proposal_title)
          procedure_title<-ifelse(procedure_title=='',NA,procedure_title)
          procedure_subtype<-ifelse(procedure_subtype=='',NA,procedure_subtype)

          ### Comparative Agenda Project policy from OEIL subjects
          
          s_t<-c()
          subj_topic<-c()
          if (!is.na(ifelse(exists("subject_oeil")==TRUE,subject_oeil,NA))){
            subjects<-str_trim(strsplit(subject_oeil,'; ')[[1]])
            for (subj in subjects){
              add<-unique(na.omit(cap_schema[cap_schema$subject==subj,]$cap_subtopic))
              s_t<-c(s_t,add)
            }
            cap_subtopics<-paste(s_t,collapse='; ')
            if (cap_subtopics!=''){
              for (c_s in s_t){
                subj_topic<-c(subj_topic,unique(cap_schema[tolower(cap_schema$cap_subtopic)==tolower(c_s),]$cap_topic))
              }
              cap_topics<-paste(modelr::typical(na.omit(subj_topic)),collapse='; ')
            }
          }
          
          ### Store results in new df row ###
          
          if(exists("text_proposal")){if(is.null(text_proposal)){text_proposal<-NA}} else {text_proposal<-NA}
          if(exists("text_final")){if(is.null(text_final)){text_final<-NA}} else {text_final<-NA}
          
          df[i,]<-c(ifelse(exists("procedure_number")==TRUE,procedure_number,NA),
                    ifelse(exists("date_initiation")==TRUE,date_initiation,NA),
                    procedure_title,
                    ifelse(exists("procedure_type")==TRUE,procedure_type,NA),
                    procedure_subtype,
                    ifelse(exists("procedure_status")==TRUE,procedure_status,NA),
                    ifelse(exists("date_end")==TRUE,date_end,NA),
                    act_name,
                    ifelse(exists("proposal_number")==TRUE,proposal_number,NA),
                    proposal_title,
                    ifelse(exists("revised_proposal")==TRUE,revised_proposal,NA),
                    leg_instrument,
                    ifelse(exists("subject_oeil")==TRUE,subject_oeil,NA),
                    ifelse(exists("eurovoc_desc")==TRUE,eurovoc_desc,NA),
                    ifelse(exists("subject_matter")==TRUE,subject_matter,NA),
                    ifelse(exists("directory_code")==TRUE,directory_code,NA),
                    ifelse(exists("cap_topics")==TRUE,cap_topics,NA),
                    ifelse(exists("celex_proposal")==TRUE,celex_proposal,NA),
                    ifelse(exists("celex_multiple")==TRUE,celex_multiple,FALSE),
                    ifelse(exists("celex_final")==TRUE,celex_final,NA),
                    ifelse(exists("ep_cmtee")==TRUE,ep_cmtee,NA),
                    ifelse(exists("ep_cmtee_opinion")==TRUE,ep_cmtee_opinion,NA),
                    ifelse(exists("n_ep_cmtee_opinion")==TRUE,n_ep_cmtee_opinion,NA),
                    ifelse(exists("plenary_texts")==TRUE,plenary_texts,NA),
                    ifelse(exists("rapporteur")==TRUE,rapporteur,NA),
                    ifelse(exists("rapporteur_appnt")==TRUE,rapporteur_appnt,NA),
                    ifelse(exists("rapporteur_party")==TRUE,rapporteur_party,NA),
                    ifelse(exists("rapporteur_url")==TRUE,rapporteur_url,NA),
                    ifelse(exists("commission_dg")==TRUE,commission_dg,NA),
                    ifelse(exists("commissioner")==TRUE,commissioner,NA),
                    ifelse(exists("council_config")==TRUE,council_config,NA),
                    ifelse(exists("council_session_id")==TRUE,council_session_id,NA),
                    ifelse(exists("council_session_date")==TRUE,council_session_date,NA),
                    ifelse(exists("b_item")==TRUE,b_item,0),
                    ifelse(exists("relations")==TRUE,relations,NA),
                    ifelse(exists("legal_basis")==TRUE,legal_basis,NA),
                    ifelse(exists("cmtee_dossier")==TRUE,cmtee_dossier,NA),
                    ifelse(exists("trilogue")==TRUE,trilogue,0),
                    ifelse(exists("early_agreement")==TRUE,early_agreement,0),
                    ifelse(exists("leg_priority")==TRUE,leg_priority,0),
                    ifelse(exists("leg_priority_list")==TRUE,leg_priority_list,NA),
                    ifelse(exists("text_summary_proposal")==TRUE,text_summary_proposal,NA),
                    ifelse(exists("text_summary_final")==TRUE,text_summary_final,NA),
                    text_proposal,
                    text_final,
                    ifelse(exists('key_events_colonsep')==TRUE,key_events_colonsep,NA),
                    ifelse(exists('doc_gateway_colonsep')==TRUE,doc_gateway_colonsep,NA),
                    ifelse(exists("url_oeil")==TRUE,url_oeil,NA),
                    ifelse(exists("error_encountered")==TRUE,as.character(error_encountered),NA)
                    )
          
        } # End of if() condition to deactivate scraping

      } # End of condition if connection established successfully

      ### Remove elements for next iteration only if more than one act is being scraped ###
      rm(list=setdiff(ls(),c('df','variables','year','procedure','i','%!in%','extract_texts','summary_only','no_text_procedures',
                             'all','verbose','dfs','procedures','years','oeil_searches','exact_procedures','cap_schema','use_eurlex',
                             c(ls()[grepl("_labels", ls())])))) # Keep names of metadata
      if (verbose==TRUE){cat(' \n')}
    } # End of loop on possible procedure numbers

    ############################################
    ### (re)code variables from scraped ones ###
    ############################################
    
    df<-subset(df,!is.na(df$procedure_number))
    
    #################################################
    ### Store df of selected year in list of dfs ####
    #################################################
    
    if(is.null(exact_procedures) & length(years)>1){ # If no specific procedures are provided and there is more than 1 year to be collected
      dfs[[paste('df',as.character(year),sep='')]]<-df # Store the df into a list of dfs
    } else {
      dfs<-df # Otherwise the data can fit in a single df
    }
    
    
  } # End of loop on years
  
  return(dfs)

} # End function
cat('\nscRapEU function succesfully downloaded.\n\n')
cat('Documentation: https://mscottodivettimo.github.io/scrapeu/scRapEU_documentation.pdf\n')
cat("Minimal example code: https://mscottodivettimo.github.io/scrapeu/scRapEU_example.R\n\n")
cat('Citation:\nScotto di Vettimo, M. (2022), "scRapEU: An R function to scrape data on EU laws". Version April 2024. DOI: 10.5281/zenodo.10871232.\n\n')
cat('NOTE: The function is still under active development! Comments and suggestions are welcome.\n')
cat('Last update: 29 April 2024.\n\n')