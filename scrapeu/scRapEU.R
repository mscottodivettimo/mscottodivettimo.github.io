#####++++++++++++++++++++++++++++++++++++++++++++++++++#####
##### scRapEU: An R function to scrape data on EU laws #####
##### Author: Michele Scotto di Vettimo                #####
##### https://mscottodivettimo.github.io/              #####
##### Version of 5 December 2024                       #####
#####++++++++++++++++++++++++++++++++++++++++++++++++++#####

scRapEU<-function(exact_procedures=NULL,years,procedures,extract_texts=c('none'),summary_only=FALSE,verbose=FALSE){
  
  options(digits.secs = 0)
  
  use_eurlex=T # This is used to deactivate scraping from EUR-Lex, mostly added for debugging reasons/to allow function to run when EUR-Lex is down
  
  cat('\nStarting:',format(Sys.time(),'%m/%d/%Y at %H:%M:%S'),'\n')
  
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
  variables<-c(
    'procedure_number','date_initiation','title','procedure_type','procedure_subtype',
    'procedure_status','force_status','date_end','act_name','oj_reference','proposal_number','proposal_title','revised_proposal',
    'leg_instrument','subject_oeil','eurovoc_desc','subject_matter','directory_code','cap_topics',
    'celex_proposal','celex_multiple','celex_final',
    'ep_cmtee','ep_cmtee_opinion','n_ep_cmtee_opinion','plenary_texts','texts_adopted',
    'rapporteur','rapporteur_appnt','rapporteur_party','rapporteur_url',
    'commission_dg','commissioner',
    'council_config','council_session_id','council_session_date','b_item',
    'relationship_acquis','legal_basis','cmtee_dossier','trilogue','early_agreement',
    'leg_priority','leg_priority_list',
    'summary_proposal','summary_proposal_id','summary_final','summary_final_id',
    'text_proposal','text_final',
    'key_events','doc_gateway','url_oeil','error_encountered','date_scraped')
  
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
    'Proposal withdrawn by Commission',
    'Delegated act not objected by Parliament',
    'Delegated act not objected by Council')
  
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
        
        xml_link<-paste0("https://oeil.secure.europarl.europa.eu/oeil/en/search/export/XML?fullText.mode=EXACT_WORD&year=",year,"&procedureType=",procedure,"&resultsOnly=true")
        
        xml_file<-readLines(xml_link)
        search_results<-str_extract_all(xml_file,'<reference>[0-9]{4}/[0-9]{4}[A-Z]{0,1}\\([A-Z]{3}\\)</reference>')
        search_results<-str_remove_all(unlist(search_results[lapply(search_results,length)>0]),'<reference>|</reference>')
        
        if(length(search_results)>0){
          if(length(search_results)<500){
            oeil_searches<-c(oeil_searches,search_results)
          } else {
            cat('WARNING: Maybe too many results for',names(which(procedures==procedure)),'\n')
          }
        }
      }
    }
    
    i<-0 # Reset iteration number for each year
    as.data.frame(matrix(NA,1,length(variables))) %>% `colnames<-`(variables)->df # Create new dataframe for each year

    cat('Scraping',ifelse(year==9999,'selected',year),'files.',length(oeil_searches),'search results found.\n\n')
    
    for (procedure_number in sort(oeil_searches)){
      
      url_oeil<-paste('https://oeil.secure.europarl.europa.eu/oeil/popups/ficheprocedure.do?reference=',procedure_number,'&l=en',sep='')
      
      i=i+1 # Track iteration number for filling df with results (inner most loop)
      if (i %% 50==0 | i==1 | i==length(oeil_searches)){
        pct<-(i/length(oeil_searches))
        len_tot<-60
        cat(paste0(format(Sys.time(),'%H:%M:%S'),' |',paste(rep('#',round(len_tot*pct,0)),collapse=''),paste(rep('.',len_tot-round(len_tot*pct,0)),collapse=''),'| ',round(pct*100,0),'% (',i,' of ',length(oeil_searches),')'),'\n')
      }
      
      if(verbose==T){cat(procedure_number,'\n')}

      connection_established<-FALSE
      errors_to_handle<-c("Timeout was reached|Connection refused|HTTP error 500|HTTP error 404|HTTP error 403|HTTP error 504")
      attempts<-0
      while((connection_established==FALSE) & (attempts<6)){
        #cat(attempts,'\n')
        if (str_count(as.character(error_encountered<-try(html_search<-read_html(url_oeil),silent=T)[1]),errors_to_handle)>0){
          attempts<-attempts+1
          Sys.sleep(10)
          if(verbose==T){cat('Attempting to establish connection to',url_oeil,'\n')}
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
            html_elements(xpath="//h2[contains(@class,'erpl_title-h2 mb-3')]") %>%
            html_text2() %>% str_trim()->procedure_title
          
          #####################################
          ### Technical information section ###
          #####################################
          
          if (verbose==TRUE){cat('Technical information\n')}
          
          ### Legal instrument
          html_search %>%
            html_elements(xpath="//h2[contains(.,'Technical information')]//
                      ancestor::div[contains(@class,'erpl-product-content oeil-spy-section mb-2')]//
                      div[contains(@class,'table-responsive')]//
                      th[contains(.,'Legislative instrument')]//
                      following-sibling::td") %>%
            html_text2() %>% str_trim()->leg_instrument
          
          if(is_empty(leg_instrument)){
            if(str_count(procedure_title,'Report')>0){
              leg_instrument<-'Report'
            }
          } 
          
          ### Procedure type
          html_search %>%
            html_elements(xpath="//h2[contains(.,'Technical information')]//
      ancestor::div[contains(@class,'erpl-product-content oeil-spy-section mb-2')]//
      div[contains(@class,'table-responsive')]//
      th[contains(.,'Procedure type')]//
                    following-sibling::td") %>%
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
            html_elements(xpath="//h2[contains(.,'Technical information')]//
                      ancestor::div[contains(@class,'erpl-product-content oeil-spy-section mb-2')]//
                      div[contains(@class,'table-responsive')]//
                      th[contains(.,'Nature of procedure')]//
                      following-sibling::td") %>%
            html_text2() %>% str_trim()->procedure_subtype # now it is called "nature of procedure"
          
          ### Legal basis
          html_search %>%
            html_elements(xpath="//h2[contains(.,'Technical information')]//
                      ancestor::div[contains(@class,'erpl-product-content oeil-spy-section mb-2')]//
                      div[contains(@class,'table-responsive')]//
                      th[contains(.,'Legal basis')]//
                      following-sibling::td//span") %>%
            html_text2() %>% str_trim()->legal_basis
          paste(as.character(legal_basis),sep='',collapse='; ')->legal_basis
          
          ### Committee dossier
          html_search %>%
            html_elements(xpath="//h2[contains(.,'Technical information')]//
                      ancestor::div[contains(@class,'erpl-product-content oeil-spy-section mb-2')]//
                      div[contains(@class,'table-responsive')]//
                      th[contains(.,'Committee dossier')]//
                      following-sibling::td") %>%
            html_text2() %>% str_trim()->cmtee_dossier
          if(!is_empty(cmtee_dossier)){
            cmtee_dossier<-str_split(cmtee_dossier,'\n')[[1]]
            paste(as.character(cmtee_dossier),sep='',collapse='; ')->cmtee_dossier
          } else {
            cmtee_dossier<-NA
          }
          
          #################################
          ### Basic information section ###
          #################################
          
          if (verbose==TRUE){cat('Basic information\n')}
          
          ### Status of the procedure (completed, rejected, withdrawn, pending) from OEIL (basic information section)
          html_search %>%
            html_elements(xpath="//h2[contains(.,'Technical information')]//
                      ancestor::div[contains(@class,'erpl-product-content oeil-spy-section mb-2')]//
                      div[contains(@class,'table-responsive')]//
                      th[contains(.,'Stage reached in procedure')]//
                      following-sibling::td") %>%
            html_text2() %>% str_trim()->stage_reached 
          
          if(!is_empty(stage_reached)){
            procedure_status=case_when(str_count(stage_reached,'Procedure completed')>0 ~ 'Completed',
                                       str_count(tolower(stage_reached),'preparatory phase|awaiting council|awaiting parliament|awaiting committee|awaiting final|awaiting signature|awaiting plenary')>0 ~ 'Pending',
                                      str_count(tolower(stage_reached),'lapsed or withdrawn')>0 ~ 'Withdrawn',
                                      str_count(tolower(stage_reached),'rejected')>0 ~ 'Rejected',
                                      TRUE ~ NA)
          } else {
            stage_reached<-NA
          }
          
          html_search %>%
            html_elements(xpath="//h2[contains(.,'Basic information')]//
                      ancestor::div[contains(@class,'erpl-product-content oeil-spy-section mb-2')]") %>%
            html_text2() %>% str_trim()->basic_info_text 
          
          if(grepl('actually rejected',basic_info_text)){procedure_status<-'Rejected'}
          
          leg_priority<-NA
          
          relations<-NA
          
          html_search %>%
            html_elements(xpath="//h2[contains(.,'Basic information')]//
      ancestor::div[contains(@class,'erpl-product-content oeil-spy-section mb-2')]//
      p[contains(.,'Subject')]//following-sibling::p") %>%
            html_text2() %>% str_trim() %>% str_trim()->subject_oeil
          if(!is_empty(subject_oeil)){
            if(any(subject_oeil=='Geographical area')){
              subject_oeil<-subject_oeil[1:which(subject_oeil=='Geographical area')-1]}
            subject_oeil<-ifelse(subject_oeil=='',NA,subject_oeil)
            subject_oeil<-str_split(subject_oeil,'\n')[[1]]
          }
          paste(as.character(subject_oeil),sep='',collapse='; ')->subject_oeil
          
          ###################
          ### Key players ###
          ###################
          
          ### European Parliament ###
          
          # Committee responsible and rapporteur
          html_search %>%
            html_elements(xpath="//div[contains(@id,'erplAccordionKeyPlayers')]//
                        div[contains(@class,'table-responsive mt-2')]//
                        th[contains(.,'Committee responsible')]//
                        ancestor::table//tbody//tr") %>%
            html_text2()->table_rows
          
          if(!is_empty(table_rows)){
            html_search %>%
              html_elements(xpath="//div[contains(@id,'erplAccordionKeyPlayers')]//
                        div[contains(@class,'table-responsive mt-2')]//
                        th[contains(.,'Committee responsible')]//
                        ancestor::table//thead//tr//th") %>%
              html_text2()->table_colnames
            
            ep_cmtee<-rapporteur<-rapporteur_appnt<-c()
            for(tr in 1:length(table_rows)){
              tb_row<-str_split(table_rows[tr],'\t')[[1]][1:length(table_colnames)]
              ep_cmtee<-setdiff(str_squish(c(ep_cmtee,str_replace_all(tb_row[which(table_colnames=='Committee responsible')],'\n',' '))),'')
              rapporteur<-setdiff(str_squish(c(rapporteur,str_replace_all(tb_row[which(table_colnames=='Rapporteur')],'\n',' '))),'')
              rapporteur_appnt<-setdiff(str_squish(c(rapporteur_appnt,str_replace_all(tb_row[which(table_colnames=='Appointed')],'\n',' '))),'')
            }
            
            if(any(grepl('^Shadow rapporteur',rapporteur))){
              shadow_rapporteur<-str_squish(str_remove_all(rapporteur[which(grepl('^Shadow rapporteur',rapporteur))],'^Shadow rapporteur'))
              rapporteur<-rapporteur[which(!grepl('^Shadow rapporteur',rapporteur))]
              
              paste(as.character(shadow_rapporteur),sep='',collapse='; ')->shadow_rapporteur
              
            } else {
              shadow_rapporteur<-NA
            }
            
            # In case there are two rapporteurs, store appointment dates in same semi-colon separated objects
            str_replace_all(paste(as.character(rapporteur_appnt),sep='',collapse='; '),'\r',';')->rapporteur_appnt
            rapporteur_appnt<-ifelse(rapporteur_appnt=='',NA,rapporteur_appnt)
            paste(as.character(ep_cmtee),sep='',collapse='; ')->ep_cmtee
          } else {
            ep_cmtee<-NA
            rapporteur<-NA
            rapporteur_appnt<-NA
            shadow_rapporteur<-NA
          }
          
          # Committee and rapporteur for opinion
          html_search %>%
            html_elements(xpath="//div[contains(@id,'erplAccordionKeyPlayers')]//
                        div[contains(@class,'table-responsive mt-2')]//
                        th[contains(.,'Committee for opinion')]//
                        ancestor::table//tbody//tr") %>%
            html_text2()->table_rows
          
          if(!is_empty(table_rows)){
            html_search %>%
              html_elements(xpath="//div[contains(@id,'erplAccordionKeyPlayers')]//
                        div[contains(@class,'table-responsive mt-2')]//
                        th[contains(.,'Committee for opinion')]//
                        ancestor::table//thead//tr//th") %>%
              html_text2()->table_colnames
            
            ep_cmtee_opinion<-c()
            for(tr in 1:length(table_rows)){
              tb_row<-str_split(table_rows[tr],'\t')[[1]][1:length(table_colnames)]
              ep_cmtee_opinion<-setdiff(str_squish(c(ep_cmtee_opinion,str_replace_all(tb_row[which(table_colnames=='Committee for opinion')],'\n',' '))),'')
              #rapporteur_opinion<-c(rapporteur_opinion,tb_row[which(table_colnames=='Rapporteur for opinion')])
              #rapporteur_opinion_appnt<-c(rapporteur_opinion_appnt,tb_row[which(table_colnames=='Appointed')])
            }
            length(ep_cmtee_opinion)->n_ep_cmtee_opinion
            paste(as.character(ep_cmtee_opinion),sep='',collapse='; ')->ep_cmtee_opinion
          } else {
            n_ep_cmtee_opinion<-0
            ep_cmtee_opinion<-NA
          }
          
          if(!is_empty(rapporteur)){
            if(!is.na(rapporteur)){
              rapporteur_url<-c()
              for(rap in rapporteur){
                xpath_string=paste0("//div[contains(@id,'erplAccordionKeyPlayers')]//
                        span[contains(@class,'erpl_badge erpl_badge-committee mr-1')]//
                        ancestor::div[contains(@id,'erpl_accordion-committee')]//
                        div[contains(@class,'table-responsive mt-2')]//
                        span[contains(.,'",rap,"')]//
                        ancestor::a")
                
                rapporteur_url<-c(rapporteur_url,html_search %>%
                                    html_elements(xpath=xpath_string) %>% html_attr('href'))
              }
              # In case there are two rapporteurs, store urls in same semi-colon separated objects
              paste(as.character(rapporteur_url),sep='',collapse='; ')->rapporteur_url
              rapporteur_url<-ifelse(rapporteur_url=='',NA,rapporteur_url)
              
              rapporteur_party<-rep(NA,length(rapporteur))
              for(rap in 1:length(rapporteur)){
                rapparty<-str_extract_all(rapporteur[rap],'\\((.*)\\)')[[1]]
                if(!is_empty(rapparty)){
                  rapporteur_party[rap]<-str_extract_all(rapporteur[rap],'\\((.*)\\)')[[1]]
                  rapporteur[rap]<-str_squish(str_remove_all(str_remove_all(rapporteur[rap],rapporteur_party[rap]),'\\(|\\)'))
                  rapporteur_party[rap]<-str_remove_all(rapporteur_party[rap],'\\(|\\)')
                } else {
                  rapporteur_party[rap]<-NA
                }
              }
            } else {
              rapporteur_party<-NA
            }
          } else {
            rapporteur_party<-NA
          }
          
          ### Council of Ministers ###
        
          html_search %>%
            html_elements(xpath="//div[contains(@id,'erplAccordionKeyPlayers')]//
                        div[contains(@class,'table-responsive mt-2')]//
                        th[contains(.,'Council configuration')]//
                        ancestor::table//tbody//tr") %>%
            html_text2()->table_rows
          
          if(!is_empty(table_rows)){
            html_search %>%
              html_elements(xpath="//div[contains(@id,'erplAccordionKeyPlayers')]//
                        div[contains(@class,'table-responsive mt-2')]//
                        th[contains(.,'Council configuration')]//
                        ancestor::table//thead//tr//th") %>%
              html_text2()->table_colnames
            
            council_config<-council_session_date<-council_session_id<-c()
            for(tr in 1:length(table_rows)){
              tb_row<-str_split(table_rows[tr],'\t')[[1]][1:length(table_colnames)]
              council_config<-setdiff(str_squish(c(council_config,str_replace_all(tb_row[which(table_colnames=='Council configuration')],'\n',' '))),'')
              council_session_id<-setdiff(str_squish(c(council_session_id,str_replace_all(tb_row[which(table_colnames=='Meetings')],'\n',' '))),'')
              council_session_date<-setdiff(str_squish(c(council_session_date,str_replace_all(tb_row[which(table_colnames=='Date')],'\n',' '))),'')
            }
            
            # In case there's more than one
            paste(as.character(council_config),sep='',collapse='; ')->council_config
            paste(as.character(council_session_id),sep='',collapse='; ')->council_session_id
            paste(as.character(council_session_date),sep='',collapse='; ')->council_session_date
          } else {
            council_config<-NA
            council_session_id<-NA
            council_session_date<-NA
          }
          
          ### European Commission ###
          
          commission_node<-html_search %>%
            html_elements(xpath="//div[contains(@id,'erplAccordionKeyPlayers')]//
                    li[contains(@class,'erpl_accordion-item')]//
                    span[contains(.,'European Commission')]")
          
          if(!is_empty(commission_node)){
            html_search %>%
              html_elements(xpath="//div[contains(@id,'erplAccordionKeyPlayers')]//
        li[contains(@class,'erpl_accordion-item')]//
                      span[contains(.,'European Commission')]//
                      ancestor::li//
                      div[contains(@class,'table-responsive mt-2')]//thead//tr") %>%
              html_text2() %>% str_split('\t') %>% .[[1]] %>% setdiff('')->comheads
            
            html_search %>%
              html_elements(xpath="//div[contains(@id,'erplAccordionKeyPlayers')]//
        li[contains(@class,'erpl_accordion-item')]//
                      span[contains(.,'European Commission')]//
                      ancestor::li//
                      div[contains(@class,'table-responsive mt-2')]//tbody//tr") %>%
              html_text2() %>% str_split('\t') %>% .[[1]] %>% setdiff('')->combody
            
            if(length(comheads)==length(combody)){
              commissioner<-commission_dg<-c()
              for(com in 1:length(comheads)){
                if(comheads[com]=='Commission DG'){
                  commission_dg<-c(commission_dg,combody[com])
                }
                if(comheads[com]=='Commissioner'){
                  commissioner<-c(commissioner,combody[com])
                }
              }
            }
          }
          
          ##########################
          ### Key events section ###
          ##########################
          
          if (verbose==TRUE){cat('Key events\n')}
          
          html_search %>%
            html_elements(xpath="//h2[contains(.,'Key events')]//
                ancestor::div[contains(@class,'erpl_product-section')]//
                table[contains(@class,'table table-striped table-bordered')]//tbody//tr") %>%
            html_text2()->table_rows 
          
          if(!is_empty(table_rows)){
            html_search %>%
              html_elements(xpath="//h2[contains(.,'Key events')]//
                ancestor::div[contains(@class,'erpl_product-section')]//
                table[contains(@class,'table table-striped table-bordered')]//thead//tr//th") %>%
              html_text2()->table_colnames 
            
            key_events_documents<-key_events_event<-key_events_date<-c()
            for(tr in 1:length(table_rows)){
              tb_row<-str_split(table_rows[tr],'\t')[[1]][1:length(table_colnames)]
              key_events_date<-c(key_events_date,tb_row[which(table_colnames=='Date')])
              key_events_event<-c(key_events_event,tb_row[which(table_colnames=='Event')])
              key_events_documents<-c(key_events_documents,tb_row[which(table_colnames=='Reference')])
            }
          } else {
            key_events_event<-NA
            key_events_date<-NA
            key_events_documents<-NA
          }
          
          key_events_colonsep<-str_remove_all(paste(as.character(key_events_event), sep=" ", collapse="; "),',')
          #setdiff(key_events_documents,"")->key_events_documents
          
          grep("^A[A-Z]?[0-9]{1,2}-|^RC[0-9]{1,2}-|^B[0-9]{1,2}-",key_events_documents,value=T)->plenary_texts
          grep("^T[A-Z]?[0-9]{1,2}-",key_events_documents,value=T)->texts_adopted
          
          # now not working as error 403 from ep page
          # if(is_empty(plenary_texts) & !is_empty(texts_adopted)){
          #   for(ta in texts_adopted){
          #     
          #     if(grepl('^T[0-9]{1,2}',ta)){ta<-str_replace(ta,'T','TA-')}
          #     
          #     paste0(str_split(ta,'-')[[1]][1],'-',
          #            str_split(ta,'-')[[1]][2],'-',
          #            str_split(str_remove_all(ta,'TA-[0-9]{1,2}-'),'/')[[1]][2],'-',
          #            str_split(str_remove_all(ta,'TA-[0-9]{1,2}-'),'/')[[1]][1])->ta_search
          #   
          #     connection_established_votes<-FALSE
          #     vote_attempts<-0
          #     while((connection_established_votes==FALSE) & (vote_attempts<6)){
          #       vote_url<-paste0("https://www.europarl.europa.eu/doceo/document/",ta_search,"_EN.html")
          #       if (str_count(as.character(error_encountered<-try(vote_html<-read_html(vote_url),silent=T)[1]),errors_to_handle)>0){
          #         vote_attempts<-vote_attempts+1
          #         Sys.sleep(10)
          #         cat('Attempting to establish connection to',vote_url,'\n')
          #       } else {
          #         connection_established_votes<-TRUE
          #       }
          #     }
          #     if (connection_established_votes==TRUE){
          #       vote_html %>%
          #         html_elements(xpath="//p[contains(.,'Texts tabled :')]//
          #                     following-sibling::a") %>%
          #         html_text2() %>% str_trim()->pt
          #       plenary_texts<-c(plenary_texts,pt)
          #     }
          #   }
          # }
          
          ### Trilogue dummy
          
          trilogue<-ifelse(any(key_events_event %in% trilogue_labels),1,0)
          
          ### Early agreement dummy
          
          early_agreement<-ifelse(any(key_events_event %in% early_agreement_labels),1,0)
          
          ################################
          ### Document gateway section ###
          ################################
          
          if (verbose==TRUE){cat('Document gateway\n')}
          
          docgate_reference<-docgate_documents<-docgate_date<-c()
          
          html_search %>%
            html_elements(xpath="//h2[contains(.,'Documentation gateway')]//
                ancestor::div[contains(@class,'erpl_product-section')]//
                span[contains(.,'European Parliament')]//ancestor::button//following-sibling::div//
                table[contains(@class,'table table-striped table-bordered')]//tbody//tr") %>%
            html_text2()->ep_table_rows 
          
          if(!is_empty(ep_table_rows)){
            html_search %>%
              html_elements(xpath="//h2[contains(.,'Documentation gateway')]//
                ancestor::div[contains(@class,'erpl_product-section')]//
                span[contains(.,'European Parliament')]//ancestor::button//following-sibling::div//
                table[contains(@class,'table table-striped table-bordered')]//thead//tr//th") %>%
              html_text2()->ep_table_colnames 
            
            for(tr in 1:length(ep_table_rows)){
              tb_row<-str_split(str_remove(ep_table_rows[tr],'\t$'),'\t')[[1]]
              docgate_date<-c(docgate_date,tb_row[which(ep_table_colnames=='Date')])
              docgate_documents<-c(docgate_documents,tb_row[which(ep_table_colnames=='Document type')])
              docgate_reference<-c(docgate_reference,tb_row[which(ep_table_colnames=='Reference')])
            }
          }
          
          html_search %>%
            html_elements(xpath="//h2[contains(.,'Documentation gateway')]//
                ancestor::div[contains(@class,'erpl_product-section')]//
                span[contains(.,'European Commission')]//ancestor::button//following-sibling::div//
                table[contains(@class,'table table-striped table-bordered')]//tbody//tr") %>%
            html_text2()->ec_table_rows 
          
          if(!is_empty(ec_table_rows)){
            html_search %>%
              html_elements(xpath="//h2[contains(.,'Documentation gateway')]//
                ancestor::div[contains(@class,'erpl_product-section')]//
                span[contains(.,'European Commission')]//ancestor::button//following-sibling::div//
                table[contains(@class,'table table-striped table-bordered')]//thead//tr//th") %>%
              html_text2()->ec_table_colnames 
            
            for(tr in 1:length(ec_table_rows)){
              tb_row<-str_split(str_remove(ec_table_rows[tr],'\t$'),'\t')[[1]]
              docgate_date<-c(docgate_date,tb_row[which(ec_table_colnames=='Date')])
              docgate_documents<-c(docgate_documents,tb_row[which(ec_table_colnames=='Document type')])
              docgate_reference<-c(docgate_reference,tb_row[which(ec_table_colnames=='Reference')])
            }
          }
          
          doc_gateway_colonsep<-str_remove_all(paste(as.character(docgate_documents), sep=" ", collapse="; "),',')
          
          unique(c(grep("^A[A-Z]?[0-9]{1,2}-|^RC[0-9]{1,2}-|^B[0-9]{1,2}-",docgate_reference,value=T),plenary_texts))->plenary_texts
          unique(c(grep("^T[A-Z]?[0-9]{1,2}-",docgate_reference,value=T),texts_adopted))->texts_adopted
          
          setdiff(paste(as.character(plenary_texts),sep='',collapse='; '),"")->plenary_texts
          setdiff(paste(as.character(texts_adopted),sep='',collapse='; '),"")->texts_adopted
          
          ### Date of initiation of procedure from OEIL (basic information section)
          
          key_events_date[which(key_events_event %in% start_procedure_labels)][1]->date_initiation
          
          if(is_empty(date_initiation)){
            docgate_date[which(docgate_documents %in% start_procedure_labels)][1]->date_initiation
          } 
          
          if(!is_empty(date_initiation)){
            candidate_proposal_numbers<-unique(setdiff(key_events_documents[which(key_events_date==date_initiation)],''))
            if(!is_empty(candidate_proposal_numbers)){
              if(length(candidate_proposal_numbers)==1){
                proposal_number<-candidate_proposal_numbers
              } else {
                proposal_events<-grep('Proposal|proposal|basic document',key_events_event[which(key_events_date==date_initiation)],value=T)
                if(!is_empty(proposal_events)){
                  proposal_number<-unique(setdiff(key_events_documents[which(key_events_event==proposal_events)],''))
                } else {
                  cat('Could not disambiguate proposal number:',candidate_proposal_numbers,'\n')
                  proposal_number<-candidate_proposal_numbers
                }
              }
            } else {
              candidate_proposal_numbers<-unique(setdiff(docgate_reference[which(docgate_date==date_initiation)],''))
              if(!is_empty(candidate_proposal_numbers)){
                if(length(candidate_proposal_numbers)==1){
                  proposal_number<-candidate_proposal_numbers
                } else {
                  proposal_number<-grep('Proposal|proposal',docgate_documents[which(docgate_date==date_initiation)],value=T)
                  if(is_empty(proposal_number)){
                    cat('Could not disambiguate proposal number:',candidate_proposal_numbers,'\n')
                    proposal_number<-candidate_proposal_numbers
                  }
                }
              } else { 
                proposal_number<-candidate_proposal_numbers # will be empty
              } 
            }
          }
          
          ### URL to PDF and number of the proposal instrument
          
          if(!is_empty(proposal_number)){
            # check if more than one document is identified as proposal?
            #if(any(grepl(paste(proposal_number,collapse='|'),table_rows,fixed=T))){
            if(any(proposal_number %in% unlist(str_split(table_rows,'\t')))){
              #for(prop_nr in grep(paste(proposal_number,collapse='|'),unlist(str_split(table_rows,'\t')),value=T)){
              #}
              xpath_string<-paste0("//h2[contains(.,'Key events')]//
                ancestor::div[contains(@class,'erpl_product-section')]//
                table[contains(@class,'table table-striped table-bordered')]//tbody//tr//
                        a[contains(.,'",proposal_number,"')]//
                        ancestor::td//a")
              
              html_search %>% html_elements(xpath=xpath_string) %>% html_attr('href')->proposal_links 
              
              ### Summary of the proposal instrument
              
              if((extract_texts %in% c('all','proposals')) & procedure_type %!in% c(no_text_procedures)){
                xpath_string<-paste0("//h2[contains(.,'Key events')]//
                ancestor::div[contains(@class,'erpl_product-section')]//
                table[contains(@class,'table table-striped table-bordered')]//tbody//tr//
                a[contains(.,'",proposal_number,"')]//
                ancestor::td//
                following-sibling::td//
                button[contains(.,'Summary')]//
                parent::a")
                
                html_search %>% html_elements(xpath=xpath_string) %>% html_attr('href')->proposal_summary_link
                
                if (!is_empty(proposal_summary_link)){
                  
                  summary_proposal_id<-str_remove_all(proposal_summary_link,"\\/oeil\\/en\\/document-summary\\?")
                  
                  try(read_html(paste('https://oeil.secure.europarl.europa.eu',proposal_summary_link,sep='')) %>%
                        html_nodes(xpath="//div[contains(@id,'website-body')]") %>% 
                        html_text2()->text_summary_proposal,silent=T)
                  
                  if(exists("text_summary_proposal")==FALSE){text_summary_proposal<-NA}
                  if(!is_empty(text_summary_proposal)){text_summary_proposal<-paste(text_summary_proposal,collapse=' \n ')}
                }
              }
            } else {proposal_links<-NA}
          } else {
            proposal_number<-NA
            proposal_links<-NA
          }
          
          ### Revised proposal 0/1
          revised_proposal<-ifelse(any(key_events_event %in% revision_proposal_labels)==TRUE,1,0)
          revised_proposal<-ifelse(length(key_events_date[which(key_events_event %in% start_procedure_labels)])>1,1,revised_proposal)
          
          ### Revised proposal date and number
          if(revised_proposal==1){
            revised_proposal_date<-key_events_date[which(key_events_event %in% revision_proposal_labels)]
            revised_proposal_number<-key_events_documents[which(key_events_event %in% revision_proposal_labels)]
          } else {
            revised_proposal_date<-NA
            revised_proposal_number<-NA
          }
          
          ### Number of EP readings
          
          ### Procedure stage if missing info on page
          
          if(is.na(stage_reached)){
            if(any(end_procedure_labels %in% key_events_event)){
              if(any(grepl('withdrawn|lapsed',key_events_event)==TRUE)){
                procedure_status<-'Withdrawn'
              } else {
                procedure_status<-'Completed'
              }
            } else {
              procedure_status<-'Pending'
            }
          }

          ################################################
          ### Information to be collected from EUR-Lex ###
          ################################################
          
          if(use_eurlex){
            if (verbose==TRUE){cat('EUR-Lex of proposal\n')}
            if(any(grepl('https://eur-lex.europa',proposal_links,fixed=T))){
              
              url_eurlex_proposal<-unique(proposal_links[which(grepl('https://eur-lex.europa',proposal_links,fixed=T))])
              
              if(length(url_eurlex_proposal)>1){
                get_one_link<-grepl(paste0('doc=',as.character(as.integer(str_extract_all(proposal_number,'[0-9]{4}$')[[1]]))),url_eurlex_proposal)
                if(any(get_one_link)==TRUE){
                  url_eurlex_proposal<-url_eurlex_proposal[which(get_one_link==T)]
                } else { # just take the first then
                  url_eurlex_proposal<-url_eurlex_proposal[1]
                }
              }
              
              connection_eurlex_proposal_established<-FALSE
              if (is.na(url_eurlex_proposal)){
                if (verbose==TRUE){
                  cat('EUR-Lex link not found\n')
                  
                  # proposal node is a leftover from previous code, see when it triggers this error as it needs to be seen how to get celex in this case
                  url_eurlex_proposal<-proposal_node %>% html_node(xpath=paste("//a[contains(.,'",proposal_number,"')]",sep='')) %>% html_attr('href')
                }
              } else {
                eurlex_attempts<-0
                
                while((connection_eurlex_proposal_established==FALSE) & (eurlex_attempts<6)){
                  if (str_count(as.character(error_encountered<-try(eurlex_search_proposal<-read_html(url_eurlex_proposal),silent=T)[1]),errors_to_handle)>0){
                    eurlex_attempts<-eurlex_attempts+1
                    Sys.sleep(10)
                    if(verbose==T){cat('Attempting to establish connection to',url_eurlex_proposal,'\n')}
                    if(procedure %in% c('DEA')){eurlex_attempts<-4} else {eurlex_attempts<-eurlex_attempts+1}
                  } else {
                    if (grepl('The HTML format is unavailable|procedure either does not exist or is in the process of being initiated',as.character(try(read_html(url_eurlex_proposal),silent=T)))){
                      connection_eurlex_proposal_established<-FALSE
                      eurlex_attempts<-999
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
                          if(is.na(procedure_in_celex)){
                            possible_proposal_page %>%
                              html_node(xpath="//dt[contains(.,'Additional information:')]//
                                following-sibling::dd//span") %>% html_text2()->procedure_in_celex
                          }
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
                                  if(str_count(xnode %>% html_text2() %>% as.character() %>% paste(collapse='\n'),'requested document does not exist')==0){
                                    xnode %>% html_text2() %>% as.character() %>% paste(collapse='\n')->text_proposal
                                  }
                                }
                              } else{
                                if(verbose==T){cat('Proposal text too long:',nodechar,'characters\n')}
                              } # End of condition proposal is too long
                            } # End of condition proposal node exists 
                          } # End of condition / attempts for proposal text
                        } # End of loop over possible paths to proposal node
                      } # End of condition HTML proposal page exists
                    } # End of condition link of HTML of proposal works
                  } 
                } # End of condition there is a valid proposal celex
              } # End of condition if eurlex link of proposal has established connection
            } 
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
              
              html_search %>%
                html_elements(xpath="//span[contains(.,'Final act')]//
                  ancestor::div[contains(@class,'erpl_product-section')]//a") %>%
                html_attr('href')->final_act_links
              
              html_search %>%
                html_elements(xpath="//span[contains(.,'Final act')]//
                  ancestor::div[contains(@class,'erpl_product-section')]//span") %>%
                html_text2() %>% str_trim()->final_act_elements
              
              ### Name of the act
              act_name<-final_act_elements[which(!grepl('Final act|OJ|Corrigendum',final_act_elements))]
              paste(as.character(act_name),sep='',collapse='; ')->act_name
              
              ### Official journal name of the final act
              oj_refs_pos<-which(grepl('OJ',final_act_elements))
              if(!is_empty(oj_refs_pos)){
                if(length(oj_refs_pos)>1){
                  name_pos<-which(!grepl('Final act|OJ|Corrigendum',final_act_elements))
                  oj_ref<-final_act_elements[oj_refs_pos[which(oj_refs_pos>name_pos)]]
                  paste(as.character(oj_ref),sep='',collapse='; ')->oj_ref
                } else {
                  oj_ref<-final_act_elements[oj_refs_pos]
                }
              }
                
              ### Summary of final act
              
              if(extract_texts %in% c('all','final')){
                
                final_summary_link<-final_act_links[which(grepl('oeil/en/document-summary',final_act_links))]
                
                summary_final_id<-str_remove_all(final_summary_link,"\\/oeil\\/en\\/document-summary\\?")
                paste(as.character(summary_final_id),sep='',collapse='; ')->summary_final_id
                
                if (!is_empty(final_summary_link)){
                  try(read_html(paste('https://oeil.secure.europarl.europa.eu',final_summary_link[1],sep='')) %>%
                        html_nodes(xpath="//div[contains(@id,'website-body')]") %>% 
                        html_text2()->text_summary_final,silent=T)
                  
                  if(exists("text_summary_final")==TRUE){
                    if(!is_empty(text_summary_final)){
                      text_summary_final<-paste(text_summary_final,collapse=' \n ')
                    } else {
                      text_summary_final<-NA
                    }
                  } else {
                    text_summary_final<-NA
                  }
                }
              }
              
              ####################
              ### From EUR-Lex ###
              ####################
              
              if(use_eurlex){ # Turn to FALSE to deactivate EUR-Lex if website is down/undergoing maintenance
                if (verbose==TRUE){cat('Moving to EUR-Lex\n')}
                
                ### Celex of the final act
                url_eurlex_final<-unique(final_act_links[which(grepl('eur-lex(.+)CELEX',final_act_links))])
                
                if(!is_empty(url_eurlex_final)){
                  if(length(url_eurlex_final)>1){
                    if(any(grepl('\\([0-9]{1,2}\\)$',url_eurlex_final))){
                      url_eurlex_final0<-url_eurlex_final[which(!grepl('\\([0-9]{1,2}\\)$',url_eurlex_final))]
                      if(is_empty(url_eurlex_final0)){
                        url_eurlex_final<-url_eurlex_final[length(url_eurlex_final)] # just pick the last
                      } else {
                        if(length(url_eurlex_final0)>1){
                          url_eurlex_final<-url_eurlex_final[length(url_eurlex_final)] # just pick the last
                        } else {
                          url_eurlex_final<-url_eurlex_final0
                        }
                      }
                    } else {
                      url_eurlex_final<-url_eurlex_final[1] # just pick the first
                    }
                  }
                  
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
                      celex_final<-str_extract_all(url_eurlex_final,"[0-9]{5,6}[A-Z]{1,2}[0-9]{4}(.?)$")[[1]]
                    }
                    # if EUR-Lex link is working
                  } else {
                    connection_eurlex_final_established<-FALSE
                    eurlex_attempts<-0
                    while((connection_eurlex_final_established==FALSE) & (eurlex_attempts<6)){
                      if (str_count(as.character(error_encountered<-try(eurlex_search_final<-read_html(url_eurlex_final),silent=T)[1]),errors_to_handle)>0){
                        eurlex_attempts<-eurlex_attempts+1
                        Sys.sleep(10)
                        if(verbose==T){cat('Attempting to establish connection to',url_eurlex_final,'\n')}
                      } else {
                        if (grepl('The HTML format is unavailable|procedure either does not exist or is in the process of being initiated',as.character(eurlex_search_final))){
                          connection_eurlex_final_established<-FALSE
                          eurlex_attempts<-999
                        } else {
                          connection_eurlex_final_established<-TRUE
                        }
                      }
                    }
                    
                    if(connection_eurlex_final_established==TRUE){
                      try(eurlex_search_final %>%
                            html_node(xpath="//meta[contains(@name,'WT.z_docID')]") %>%
                            html_attr('content') %>% str_trim()->celex_final,silent=T)
                      
                      try(eurlex_search_final %>% 
                        html_elements(xpath="//img[contains(@alt,'Legal status of the document')]//
                                      parent::span//parent::p") %>%
                        html_text2()->force_status,silent=T)
                        
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
                            html_text2() %>% unique()->date_initiation 
                          
                          if(length(date_initiation)>1){
                            date_initiation<-date_initiation[which(as.Date(date_initiation,'%d/%m/%Y')==min(as.Date(date_initiation,'%d/%m/%Y')))]
                          }
                          
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
                          if(exists("final_HTML")){
                            if(verbose==T){cat('Searching for final text\n')}
                            for(xp in xpaths){
                              attempt<-0
                              while(is.null(text_final) & attempt<length(xpaths)+1){
                                attempt<-attempt+1
                                if(!is_empty(nodechar<-max(nchar(as.character(xnode<-final_HTML %>% html_nodes(xpath=xp)))))){
                                  if(nodechar<(3*(10^6))){
                                    if(!is.infinite(abs(nodechar))){
                                      if(str_count(xnode %>% html_text2() %>% as.character() %>% paste(collapse=' \n '),'requested document does not exist')==0){
                                        xnode %>% html_text2() %>% as.character() %>% paste(collapse=' \n ')->text_final
                                      }
                                    }
                                  } else{
                                    if(verbose==T){cat('Final text too long:',nodechar,'characters\n')}
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
          
          if ((is_empty(date_initiation) | is.na(date_initiation))){
            all_dates<-unique(c(docgate_date,key_events_date))
            date_initiation<-all_dates[which(as.Date(all_dates,'%d/%m/%Y')==min(as.Date(all_dates,'%d/%m/%Y')))]
          }
          
          if (procedure_status %in% c('Completed','Withdrawn')){
            
            if (is_empty(date_end)){
              date_end<-key_events_date[which(as.Date(key_events_date,'%d/%m/%Y')==max(as.Date(key_events_date,'%d/%m/%Y')))]
            }
            
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
                    ifelse(exists("force_status")==TRUE,force_status,NA),
                    ifelse(exists("date_end")==TRUE,date_end,NA),
                    act_name,
                    ifelse(exists("oj_ref")==TRUE,oj_ref,NA),
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
                    ifelse(exists("texts_adopted")==TRUE,texts_adopted,NA),
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
                    ifelse(exists("summary_proposal_id")==TRUE,summary_proposal_id,NA),
                    ifelse(exists("text_summary_final")==TRUE,text_summary_final,NA),
                    ifelse(exists("summary_final_id")==TRUE,summary_final_id,NA),
                    ifelse(exists("text_proposal")==TRUE,text_proposal,NA),
                    ifelse(exists("text_final")==TRUE,text_final,NA),
                    ifelse(exists('key_events_colonsep')==TRUE,key_events_colonsep,NA),
                    ifelse(exists('doc_gateway_colonsep')==TRUE,doc_gateway_colonsep,NA),
                    ifelse(exists("url_oeil")==TRUE,url_oeil,NA),
                    ifelse(exists("error_encountered")==TRUE,as.character(error_encountered),NA),
                    Sys.Date())
          
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
cat('Citation:\nScotto di Vettimo, M. (2022), "scRapEU: An R function to scrape data on EU laws". Version December 2024. DOI: 10.5281/zenodo.10871232.\n\n')
cat('NOTE: The function is still under active development! Comments and suggestions are welcome.\n')
cat('Last update: 5 December 2024.\n\n')