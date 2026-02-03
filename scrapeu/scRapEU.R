#####++++++++++++++++++++++++++++++++++++++++++++++++++#####
##### scRapEU: An R function to scrape data on EU laws #####
##### Author: Michele Scotto di Vettimo                #####
##### https://michelescottodivettimo.eu/               #####
##### Version of 3 February 2026                       #####
#####++++++++++++++++++++++++++++++++++++++++++++++++++#####

scRapEU <- function(exact_procedures = NULL, years, procedures = NULL, verbose =  FALSE){

  #############################################################
  ### Define subfunctions for scraping of specific sections ###
  #############################################################
  
  ##############################################################################
  OEIL_TECHNICAL_INFORMATION <- function() {
    
    if (verbose){ cat('Technical information\n') }
    
    root_path <- "//h2[contains(.,'Technical information')]//
    ancestor::div[contains(@class,'erpl-product-content oeil-spy-section mb-2')]//
    div[contains(@class,'table-responsive')]//"

    ### Legal instrument
    leg_instrument <- html_search %>%
      html_elements(xpath = paste0(root_path,"th[contains(.,'Legislative instrument')]//following-sibling::td")) %>% 
      html_text2() %>% str_trim()
    
    if(is_empty(leg_instrument)){ if(str_count(procedure_title,'Report') > 0) { leg_instrument <- 'Report' } else { leg_instrument <- NA}} 
    
    ### Procedure type
    procedure_type <- html_search %>%
      html_elements(xpath = paste0(root_path,"th[contains(.,'Procedure type')]//following-sibling::td")) %>%
      html_text2() %>% str_trim()
    
    procedure_type <- case_when(grepl('^[A-Z]{3}\\s-', procedure_type) ~ unlist(str_extract_all(procedure_type,'^[A-Z]{3}')), TRUE ~ procedure_type)
    
    if(is_empty(procedure_type)){
      procedure_type <- str_extract_all(procedure_number, paste(names(procedures_list), collapse = '|'))[[1]]
      if(is_empty(procedure_type)){ procedure_type <- NA }}
    
    ### Procedure subtype
    procedure_subtype <- html_search %>%
      html_elements(xpath = paste0(root_path,"th[contains(.,'Nature of procedure') or contains(.,'Procedure subtype')]//following-sibling::td")) %>%
      html_text2() %>% str_trim()
    procedure_subtype <- ifelse(is_empty(procedure_subtype), NA, procedure_subtype)
    
    ### Legal basis
    legal_basis <- html_search %>%
      html_elements(xpath = paste0(root_path, "th[contains(.,'Legal basis')]//following-sibling::td//span")) %>%
      html_text2() %>% str_trim()
    legal_basis <- paste0(as.character(legal_basis), collapse = '; ')
    
    ### Committee dossier
    cmtee_dossier <- html_search %>%
      html_elements(xpath = paste0(root_path, "th[contains(.,'Committee dossier')]//following-sibling::td")) %>% 
      html_text2() %>% str_trim()
    if(!is_empty(cmtee_dossier)){
      cmtee_dossier <- str_split(cmtee_dossier,'\n')[[1]]
      cmtee_dossier <- paste0(as.character(cmtee_dossier), collapse = '; ')
    } else { cmtee_dossier <- NA }
    
    ### Status of the procedure (completed, rejected, withdrawn, pending) from OEIL (basic information section)
    stage_reached <- html_search %>%
      html_elements(xpath = paste0(root_path,"th[contains(.,'Stage reached in procedure')]//following-sibling::td")) %>% 
      html_text2() %>% str_trim()
    
    if(!is_empty(stage_reached)){
      procedure_status <- case_when(str_count(stage_reached,'Procedure completed') > 0 ~ 'Completed',
                                    str_count(tolower(stage_reached),'preparatory phase|awaiting') > 0 ~ 'Pending',
                                    str_count(tolower(stage_reached),'lapsed or withdrawn') > 0 ~ 'Withdrawn',
                                    str_count(tolower(stage_reached),'rejected') > 0 ~ 'Rejected',
                                    str_count(tolower(stage_reached),'awaiting signature|awaiting publication') > 0 ~ 'Completed',
                                    TRUE ~ NA)
    } else {procedure_status <- NA}
    
    if(grepl('actually rejected',html_search %>%
      html_elements(xpath="//h2[contains(.,'Basic information')]//
                      ancestor::div[contains(@class,'erpl-product-content oeil-spy-section mb-2')]") %>%
      html_text2() %>% str_trim())) { procedure_status <- 'Rejected' }
    
    ### Subject matter from OEIL
    subject_oeil <- html_search %>%
      html_elements(xpath = "//h2[contains(.,'Basic information')]//
                  ancestor::div[contains(@class,'erpl-product-content oeil-spy-section mb-2')]//
                  p[contains(.,'Subject')]//following-sibling::p") %>%
      html_text2() %>% str_trim()
    if(!is_empty(subject_oeil)){
      if(any(subject_oeil == 'Geographical area')){ subject_oeil <- subject_oeil[1:which(subject_oeil == 'Geographical area') - 1]}
      subject_oeil <- ifelse(subject_oeil=='',NA,subject_oeil)
      subject_oeil <- str_split(subject_oeil,'\n')[[1]]
    }
    subject_oeil <- paste0(as.character(subject_oeil), collapse = '; ')
    
    ### Manage outputs in list or global
    return(list(
      leg_instrument = leg_instrument, 
      procedure_type = procedure_type, 
      procedure_subtype = procedure_subtype, 
      legal_basis = legal_basis,
      cmtee_dossier = cmtee_dossier,
      procedure_status = procedure_status,
      subject_oeil = tolower(subject_oeil),
      stage_reached = stage_reached
      ))
  } ### End of OEIL_TECHNICAL_INFORMATION() function ###########################
  
  KEY_PLAYERS <- function() {
    if (verbose){ cat("Key players\n") }
    
    root_node <- html_search %>% html_elements(xpath = "//div[contains(@id,'erplAccordionKeyPlayers')]") 
    
    ### European Parliament ###
    if (verbose){ cat('- European Parliament')}
    
    the_path = "//div[contains(@class,'table-responsive mt-2')]"
      
    # Committee responsible and rapporteur
    table_rows <- root_node %>% html_elements(xpath = paste0(the_path, "//th[contains(.,'Committee responsible')]//ancestor::table//tbody//tr")) 
    
    if(!is_empty(table_rows)){
      table_colnames <- root_node %>%
        html_elements(xpath = paste0(the_path, "//th[contains(.,'Committee responsible')]//
                        ancestor::table//thead//tr//th")) %>% html_text2()
      
      ep_cmtee <- rapporteur <- rapporteur_appnt <- c()
      for(tr in 1:length(table_rows)){
        #table_rows[tr] %>% html_elements(xpath = "//span[contains(@class,'es_badge es_badge-committee mr-1')]")
        tb_row <- str_split(table_rows[tr] %>% html_text2(),'\t')[[1]][1:length(table_colnames)]
        ep_cmtee <- setdiff(str_squish(c(ep_cmtee,str_replace_all(tb_row[which(table_colnames=='Committee responsible')],'\n',' '))),'')
        rapporteur <- setdiff(str_squish(c(rapporteur, str_replace_all(tb_row[which(table_colnames=='Rapporteur')],'\n',' '))),'')
        rapporteur_appnt <- setdiff(str_squish(c(rapporteur_appnt, str_replace_all(tb_row[which(table_colnames=='Appointed')],'\n',' '))),'')
      }
      
      if(any(grepl('^Shadow rapporteur',rapporteur))){
        #root_node %>% html_elements(xpath = paste0(the_path, "//th[contains(.,'Committee responsible')]//ancestor::table//tbody//tr")) %>%
        shadow_rapporteur <- str_squish(str_remove_all(rapporteur[which(grepl('^Shadow rapporteur',rapporteur))],'^Shadow rapporteur'))
        rapporteur <- rapporteur[which(!grepl('^Shadow rapporteur',rapporteur))]
        shadow_rapporteur <- paste0(as.character(shadow_rapporteur),collapse = '; ')
      } else {
        shadow_rapporteur <- NA
      }
      
      # In case there are two rapporteurs, store appointment dates in same semi-colon separated objects
      rapporteur_appnt <- str_replace_all(paste(as.character(rapporteur_appnt), sep = '',collapse = '; '),'\r',';')
      rapporteur_appnt <- ifelse(rapporteur_appnt=='',NA,rapporteur_appnt)
      ep_cmtee <- paste(as.character(ep_cmtee),sep='',collapse='; ')
    } else { ep_cmtee <- NA; rapporteur <- NA; rapporteur_appnt <- NA; shadow_rapporteur <- NA }
    
    # Committee and rapporteur for opinion
    table_rows <- root_node %>%
      html_elements(xpath = paste0(the_path, "//th[contains(.,'Committee for opinion')]//ancestor::table//tbody//tr")) %>%
      html_text2()
    
    if(!is_empty(table_rows)){
      table_colnames <- root_node %>%
        html_elements(xpath =  paste0(the_path,"//th[contains(.,'Committee for opinion')]//ancestor::table//thead//tr//th")) %>%
        html_text2()
      
      ep_cmtee_opinion <- c()
      for(tr in 1:length(table_rows)){
        tb_row<-str_split(table_rows[tr],'\t')[[1]][1:length(table_colnames)]
        ep_cmtee_opinion<-setdiff(str_squish(c(ep_cmtee_opinion,str_replace_all(tb_row[which(table_colnames=='Committee for opinion')],'\n',' '))),'')
        #rapporteur_opinion<-c(rapporteur_opinion,tb_row[which(table_colnames=='Rapporteur for opinion')])
        #rapporteur_opinion_appnt<-c(rapporteur_opinion_appnt,tb_row[which(table_colnames=='Appointed')])
      }
      n_ep_cmtee_opinion <- length(ep_cmtee_opinion)
      ep_cmtee_opinion <- paste(as.character(ep_cmtee_opinion),sep='',collapse='; ')
    } else { n_ep_cmtee_opinion <- 0; ep_cmtee_opinion <- NA }
    
    if(!is_empty(rapporteur)){
      if(!is.na(rapporteur)){
        rapporteur_url <- c()
        for(rap in rapporteur){
          xpath_string <- paste0("//span[contains(@class,'erpl_badge erpl_badge-committee mr-1') or contains(@class,'es_badge es_badge-committee mr-1')]//
                        ancestor::div[contains(@id,'erpl_accordion-committee') or contains(@id,'es_accordion-committee')]//
                        div[contains(@class,'table-responsive mt-2')]//
                        span[contains(.,'",rap,"')]//
                        ancestor::a")
          
          rapporteur_url <- c(rapporteur_url,
                              root_node %>% html_elements(xpath = xpath_string) %>% html_attr('href'))
        }
        # In case there are two rapporteurs, store urls in same semi-colon separated objects
        rapporteur_url <- paste0(as.character(rapporteur_url), collapse = '; ')
        rapporteur_url <- ifelse(rapporteur_url == '', NA, rapporteur_url)
        
        rapporteur_party <- rep(NA,length(rapporteur))
        for(rap in 1:length(rapporteur)){
          rapparty <- str_extract_all(rapporteur[rap],'\\((.*)\\)')[[1]]
          if(!is_empty(rapparty)){
            rapporteur_party[rap] <- str_extract_all(rapporteur[rap],'\\((.*)\\)')[[1]]
            rapporteur[rap] <- str_squish(str_remove_all(str_remove_all(rapporteur[rap],rapporteur_party[rap]),'\\(|\\)'))
            rapporteur_party[rap] <- str_remove_all(rapporteur_party[rap],'\\(|\\)')
          } else { rapporteur_party[rap] <- NA }
        }
      } else { rapporteur <- rapporteur_party <- rapporteur_url <- NA }
    } else { rapporteur <- rapporteur_party <- rapporteur_url <- NA }
    
    ### Council of Ministers ###
    
    if (verbose){ cat(' ; - Council')}
    table_rows <- root_node %>%
      html_elements(xpath = paste0(the_path, "//th[contains(.,'Council configuration')]//ancestor::table//tbody//tr")) %>%
      html_text2()
    
    if(!is_empty(table_rows)){
      table_colnames <- root_node %>%
        html_elements(xpath = paste0(the_path, "//th[contains(.,'Council configuration')]//ancestor::table//thead//tr//th")) %>%
        html_text2()
      
      council_config <- council_session_date <- council_session_id<-c()
      for(tr in 1:length(table_rows)){
        tb_row <- str_split(table_rows[tr],'\t')[[1]][1:length(table_colnames)]
        council_config <- setdiff(str_squish(c(council_config,str_replace_all(tb_row[which(table_colnames=='Council configuration')],'\n',' '))),'')
        council_session_id <- setdiff(str_squish(c(council_session_id,str_replace_all(tb_row[which(table_colnames=='Meetings')],'\n',' '))),'')
        council_session_date <- setdiff(str_squish(c(council_session_date,str_replace_all(tb_row[which(table_colnames=='Date')],'\n',' '))),'')
      }
      # In case there's more than one
      council_config <- paste0(as.character(council_config), collapse='; ')
      council_session_id <- paste0(as.character(council_session_id), collapse='; ')
      council_session_date <- paste0(as.character(council_session_date), collapse='; ')
    } else { council_config <- council_session_id <- council_session_date <- NA }
    
    ### European Commission ###
    
    if (verbose){ cat(' ; - Commission\n')}
    
    ec_path <- "//li[contains(@class,'erpl_accordion-item') or contains(@class,'es_accordion-item')]//
    span[contains(.,'European Commission')]"
  
    if(!is_empty(root_node %>% html_elements(xpath = paste0(ec_path,"//ancestor::li//div[contains(@class,'table-responsive mt-2')]")))){
      comheads <- root_node %>%
        html_elements(xpath = paste0(ec_path, "//ancestor::li//div[contains(@class,'table-responsive mt-2')]//thead//tr")) %>%
        html_text2() %>% str_split('\t') %>% .[[1]] %>% setdiff('')
      
      combody <- root_node %>%
        html_elements(xpath = paste0(ec_path, "//ancestor::li//div[contains(@class,'table-responsive mt-2')]//tbody//tr")) %>%
        html_text2() %>% str_split('\t') %>% .[[1]] %>% setdiff('')
      
      if(length(comheads) >= length(combody)){
        commissioner <- commission_dg <- c()
        for(com in 1:length(comheads)){
          if(comheads[com] == 'Commission DG'){ commission_dg <- c(commission_dg,combody[com]) }
          if(comheads[com] == 'Commissioner'){ commissioner <- c(commissioner,combody[com])}
        }
      } else { commissioner <- commission_dg <- NA}
    } else { commissioner <- commission_dg <- NA}
    
    return(list(
      ep_cmtee = ep_cmtee,
      ep_cmtee_opinion = ep_cmtee_opinion,
      n_ep_cmtee_opinion = n_ep_cmtee_opinion,
      rapporteur = rapporteur,
      rapporteur_appnt = rapporteur_appnt,
      rapporteur_party = rapporteur_party,
      rapporteur_url = rapporteur_url,
      shadow_rapporteur = shadow_rapporteur,
      council_config_oeil = council_config,
      council_session_id_oeil = council_session_id,
      council_session_date_oeil = council_session_date,
      commissioner = commissioner,
      commission_dg = commission_dg
    ))
    
  } ### End of KEY_PLAYERS() function ##########################################
  
  ##############################################################################
  KEY_EVENTS <- function() {
    
    if (verbose){ cat('Key events\n') }
    
    root_path <- "//h2[contains(.,'Key events')]//
          ancestor::div[contains(@class,'erpl_product-section') or contains(@class,'es_product-section')]//
          table[contains(@class,'table table-striped table-bordered')]//tbody//tr"
    
    table_rows <- html_search %>%
      html_elements(xpath = root_path) %>% 
      html_text2()
    
    no_key_events_section <- FALSE
    
    if(!is_empty(table_rows)){
      table_colnames <- html_search %>%
        html_elements(xpath = str_replace_all(root_path,'//tbody//tr',"//thead//tr//th")) %>%
        html_text2() 
      
      key_events_documents <- key_events_event <- key_events_date <- c()
      for(tr in 1:length(table_rows)){
        tb_row <- str_split(table_rows[tr],'\t')[[1]][1:length(table_colnames)]
        key_events_date <- c(key_events_date, tb_row[which(table_colnames=='Date')])
        key_events_event <- c(key_events_event, tb_row[which(table_colnames=='Event')])
        key_events_documents <- c(key_events_documents, tb_row[which(table_colnames=='Reference')])
      }
    } else { key_events_event <- NA; key_events_date <- NA; key_events_documents <- NA; no_key_events_section <- TRUE }
    
    key_events_colonsep <- str_remove_all(paste0(as.character(key_events_event), collapse="; "),',')
    if(length(key_events_event) == length(key_events_date)) {
      key_events_date_colonsep <- paste0(as.character(key_events_date), collapse="; ")
    } 
    
    plenary_texts <- grep("^A[A-Z]?[0-9]{1,2}-|^RC[0-9]{1,2}-|^B[0-9]{1,2}-", key_events_documents, value = TRUE)
    texts_adopted <- grep("^T[A-Z]?[0-9]{1,2}-", key_events_documents, value = TRUE)
    
    ### Trilogue dummy
    
    trilogue <- ifelse(any(key_events_event %in% trilogue_labels), 1, 0)
    
    return(list(
      plenary_texts = plenary_texts,
      texts_adopted = texts_adopted,
      trilogue = trilogue,
      key_events = key_events_colonsep,
      key_dates = key_events_date_colonsep,
      key_events_date = key_events_date,
      key_events_event = key_events_event,
      key_events_documents = key_events_documents,
      no_key_events_section = no_key_events_section
    ))
    
  } ### End of KEY_EVENTS function #############################################
  
  ##############################################################################
  DOCUMENT_GATEWAY <- function() {
    
    if (verbose){ cat('Document gateway\n') }
    
    docgate_reference <- docgate_documents <- docgate_date <- ep_table_rows <- ec_table_rows <- np_table_rows <- c()
    
    root_path <- "//h2[contains(.,'Documentation gateway')]//
          ancestor::div[contains(@class,'erpl_product-section') or contains(@class,'es_product-section')]//
          span[contains(.,'European Parliament')]//ancestor::button//following-sibling::div//
          table[contains(@class,'table table-striped table-bordered')]//tbody//tr"
    
    try(ep_table_rows <- html_search %>% html_elements(xpath = root_path) %>% html_text2(), silent = TRUE)
    
    if(!is_empty(ep_table_rows)){
      ep_table_colnames <- html_search %>%
        html_elements(xpath = str_replace_all(root_path,'//tbody//tr',"//thead//tr//th")) %>%
        html_text2() 
      
      for(tr in 1:length(ep_table_rows)){
        tb_row <- str_split(str_remove(ep_table_rows[tr],'\t$'),'\t')[[1]]
        docgate_date <- c(docgate_date,tb_row[which(ep_table_colnames=='Date')])
        docgate_documents <- c(docgate_documents,tb_row[which(ep_table_colnames=='Document type')])
        docgate_reference <- c(docgate_reference,tb_row[which(ep_table_colnames=='Reference')])
      }
    }
    
    root_path <- "//h2[contains(.,'Documentation gateway')]//
          ancestor::div[contains(@class,'erpl_product-section') or contains(@class,'es_product-section')]//
          span[contains(.,'European Commission')]//ancestor::button//following-sibling::div//
          table[contains(@class,'table table-striped table-bordered')]//tbody//tr"
    
    try(ec_table_rows <- html_search %>%  html_elements(xpath = root_path) %>% html_text2(), silent = TRUE)
    
    if(!is_empty(ec_table_rows)){
      ec_table_colnames <- html_search %>%
        html_elements(xpath = str_replace_all(root_path,'//tbody//tr',"//thead//tr//th")) %>%
        html_text2() 
      
      for(tr in 1:length(ec_table_rows)){
        tb_row <- str_split(str_remove(ec_table_rows[tr],'\t$'),'\t')[[1]]
        docgate_date <- c(docgate_date,tb_row[which(ec_table_colnames=='Date')])
        docgate_documents <- c(docgate_documents,tb_row[which(ec_table_colnames=='Document type')])
        docgate_reference <- c(docgate_reference,tb_row[which(ec_table_colnames=='Reference')])
      }
    }
    
    doc_gateway_colonsep <- str_remove_all(paste(as.character(docgate_documents), sep=" ", collapse="; "),',')
    
    plenary_texts <- unique(c(grep("^A[A-Z]?[0-9]{1,2}-|^RC[0-9]{1,2}-|^B[0-9]{1,2}-",docgate_reference,value=T),plenary_texts))
    texts_adopted <- unique(c(grep("^T[A-Z]?[0-9]{1,2}-",docgate_reference,value=T),texts_adopted))
    
    plenary_texts <- setdiff(paste0(as.character(plenary_texts), collapse = '; '), "")
    texts_adopted <- setdiff(paste0(as.character(texts_adopted), collapse = '; '), "")
    
    # National parliament documents
    
    root_path <- "//h2[contains(.,'Documentation gateway')]//
          ancestor::div[contains(@class,'erpl_product-section') or contains(@class,'es_product-section')]//
          span[contains(.,'National parliaments')]//ancestor::button//following-sibling::div//
          table[contains(@class,'table table-striped table-bordered')]//tbody//tr"
    
    try(np_table_rows <- html_search %>% html_elements(xpath = root_path) %>% html_text2(), silent = TRUE)
    
    if(!is_empty(np_table_rows)){
      np_table_colnames <- html_search %>%
        html_elements(xpath = str_replace_all(root_path,'//tbody//tr',"//thead//tr//th")) %>%
        html_text2() 
      
      natparl_docreference <- natparl_docdate <- natparl_documents <- natparl <- c()
      for(tr in 1:length(np_table_rows)){
        tb_row <- str_split(str_remove(np_table_rows[tr],'\t$'),'\t')[[1]]
        natparl <- c(natparl,tb_row[which(np_table_colnames == 'Parliament/Chamber')])
        natparl_docdate <- c(natparl_docdate,tb_row[which(np_table_colnames=='Date')])
        natparl_documents <- c(natparl_documents,tb_row[which(np_table_colnames=='Document type')])
        natparl_docreference <- c(natparl_docreference,tb_row[which(np_table_colnames=='Reference')])
      }
      natparl <- paste0(natparl, collapse = '; ')
      natparl_docdate <- paste0(natparl_docdate, collapse = '; ')
      natparl_documents <- paste0(natparl_documents, collapse = '; ')
      natparl_docreference <- paste0(natparl_docreference, collapse = '; ')
    } else {
      natparl <- natparl_docdate <- natparl_documents <- natparl_docreference <- NA
    }
    
    ### Date of initiation of procedure from OEIL (basic information section)
    
    date_initiation <- key_events_date[which(key_events_event %in% start_procedure_labels)][1]
    
    if(is_empty(date_initiation) | is.na(date_initiation)){ date_initiation <- docgate_date[which(docgate_documents %in% start_procedure_labels)][1]} 

    if(!is_empty(date_initiation)){
      candidate_proposal_numbers <- unique(setdiff(key_events_documents[which(key_events_date == date_initiation)],''))
      if(!is_empty(candidate_proposal_numbers)){
        if(length(candidate_proposal_numbers) == 1){
          proposal_number <- candidate_proposal_numbers
        } else {
          proposal_events <- grep('Proposal|proposal|basic document',key_events_event[which(key_events_date == date_initiation)][1],value = TRUE)
          if(!is_empty(proposal_events)){
            proposal_number <- unique(setdiff(key_events_documents[which(key_events_event == proposal_events)][1],''))
          } else {
            cat('Could not disambiguate proposal number:', candidate_proposal_numbers,'\n')
            proposal_number <- candidate_proposal_numbers[1]
          }
        }
      } else {
        candidate_proposal_numbers <- unique(setdiff(docgate_reference[which(docgate_date == date_initiation)],''))
        if(!is_empty(candidate_proposal_numbers)){
          if(length(candidate_proposal_numbers) == 1){
            proposal_number <- unlist(str_split(candidate_proposal_numbers,' '))[1]
          } else {
            proposal_number <- grep('Proposal|proposal',docgate_documents[which(docgate_date == date_initiation)][1], value = TRUE)
            if(is_empty(proposal_number)){
              cat('Could not disambiguate proposal number:',candidate_proposal_numbers,'\n')
              proposal_number <- unlist(str_split(candidate_proposal_numbers,' '))[1][1]
            }
          }
        } else { 
          proposal_number <- NA 
        } 
      }
    } else {
      date_initiation <- key_events_date[1]
      proposal_number <- docgate_documents[which(docgate_date == date_initiation)]
      if(is.null(proposal_number)) { proposal_number <- NA}
    }
    
    ### Revised proposal 0/1
    revised_proposal <- ifelse(any(key_events_event %in% revision_proposal_labels) == TRUE, 1, 0)
    revised_proposal <- ifelse(length(key_events_date[which(key_events_event %in% start_procedure_labels)]) > 1, 1, revised_proposal)
    
    ### Revised proposal date and number
    if(revised_proposal == 1){
      revised_proposal_date <- key_events_date[which(key_events_event %in% revision_proposal_labels)]
      revised_proposal_number <- key_events_documents[which(key_events_event %in% revision_proposal_labels)]
    } else { revised_proposal_date <- NA; revised_proposal_number <- NA }
    
    ### Procedure stage if missing info on page
    if(!exists("stage_reached")){ 
      if(any(end_procedure_labels %in% key_events_event)){
        if(any(grepl('withdrawn|lapsed', key_events_event) == TRUE)){
          procedure_status <- 'Withdrawn'
        } else { procedure_status <- 'Completed' }} else { procedure_status <- 'Pending' }}
    
    return(list(
      plenary_texts = plenary_texts,
      texts_adopted = texts_adopted,
      natparl_contribution = natparl,
      natparl_docdate = natparl_docdate,
      natparl_doctype = natparl_documents,
      natparl_docref = natparl_docreference,
      date_initiation = date_initiation,
      proposal_number = proposal_number,
      revised_proposal = revised_proposal,
      revised_proposal_number = revised_proposal_number,
      procedure_status = procedure_status,
      docgate_date = docgate_date
    ))
    
  } ### End of DOCUMENT_GATEWAY function #######################################

  ##############################################################################
  PROPOSAL_INFO_AND_CELEX <- function() {
    
    if (verbose){ cat('EUR-Lex of proposal\n') }
    
    root_path <- "//h2[contains(.,'Key events')]//
          ancestor::div[contains(@class,'erpl_product-section') or contains(@class,'es_product-section')]//
          table[contains(@class,'table table-striped table-bordered')]//tbody//tr"

    table_rows <- html_search %>%
      html_elements(xpath = root_path) %>% 
      html_text2()
    
    if(!is_empty(proposal_number)){
      if(any(proposal_number %in% unlist(str_split(table_rows,'\t')))){

        proposal_links <- html_search %>% 
          html_elements(xpath = paste0(root_path,"//*[self::span or self::a][contains(.,'", proposal_number, "')]//ancestor::td//a")) %>% 
          html_attr('href')
        
        if(!is_empty(proposal_links)) { proposal_links <- unique(proposal_links[which(grepl('https://eur-lex.europa', proposal_links, fixed = TRUE))]) }
        
        # It could be that there are no hrefs even if proposal number is in table (eg 2022/0089(COD))
        if(is_empty(proposal_links)){
          if (inherits(try(remDr$getTitle(), silent = TRUE), "try-error")) {
            message("Restarting remote driver.")
            remDr <- remote_driver$client
            remDr$open(silent = TRUE)
          } 
          remDr$navigate(paste0("https://eur-lex.europa.eu/search.html?text=",proposal_number,"&lang=en&type=quick&qid=1763491671080&scope=EURLEX"))
          attempt_res <- try(read_html(remDr$getPageSource() %>% unlist()), silent = TRUE)
          if (!inherits(attempt_res, "try-error")) {
            celex_in_page <- attempt_res %>%
              html_elements(xpath = "//dt[contains(.,'CELEX number:')]//following-sibling::dd") %>% 
              html_text2()
            remove_celex <- which(grepl("Corrigendum",celex_in_page))
            remove_celex <- c(remove_celex,remove_celex - 1)
            if(!is_empty(remove_celex)) { celex_in_page <- celex_in_page[-remove_celex] }
            celex_in_page <- celex_in_page[which(grepl('^5',celex_in_page))]
            if(!is_empty(celex_in_page)){ proposal_links <- paste0("https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:",celex_in_page[1]) }
          } 
        }
        
        ### Link to summary of the proposal instrument
        proposal_summary_link <- html_search %>% 
          html_elements(xpath = str_replace_all(paste0(root_path,"//*[self::span or self::a][contains(.,'", proposal_number, "')]//ancestor::td//a"),
                                                'ancestor::td//a',
                                                "ancestor::td//following-sibling::td//button[contains(.,'Summary')]//parent::a")) %>% 
          html_attr('href')
        if (!is_empty(proposal_summary_link)){ summary_proposal_id <- str_remove_all(proposal_summary_link,"\\/oeil\\/en\\/document-summary\\?") } 
      } else { # Try to create a proposal link from the proposal number 
        if(grepl('COM',proposal_number)) { # Temptative celex of proposal just for making up a working link
          x <- paste0('5',str_extract(str_extract(proposal_number,'\\([0-9]{4}\\)'),'[0-9]{4}'),
                                   'PC',str_extract(str_extract(proposal_number,'\\)[0-9]{4}$'),'[0-9]{4}'))
          proposal_links <- paste0('https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:', x)
        } else { proposal_links <- NA}
      }
    } else { proposal_number <- NA; proposal_links <- NA }
    
    if(any(grepl('https://eur-lex.europa', proposal_links, fixed = TRUE))){
      
      url_eurlex_proposal <- unique(proposal_links[which(grepl('https://eur-lex.europa',proposal_links, fixed = TRUE))])
      
      if(length(url_eurlex_proposal) > 1){
        get_one_link <- grepl(paste0('doc=[0]{0,3}',as.character(as.integer(str_extract_all(proposal_number,'[0-9]{4}$')[[1]]))),url_eurlex_proposal)
        if(any(get_one_link)){ url_eurlex_proposal <- url_eurlex_proposal[which(get_one_link == TRUE)] } else { url_eurlex_proposal <- url_eurlex_proposal[1] }}
      
      connection_eurlex_proposal_established <- FALSE
      if (is.na(url_eurlex_proposal)){ if (verbose == TRUE){ cat('EUR-Lex link not found\n') }
      } else {
        eurlex_attempts <- 0
        while((connection_eurlex_proposal_established == FALSE) & (eurlex_attempts < 4)){
          if(eurlex_attempts <= 1) {
            attempt_res <- try(read_html(url_eurlex_proposal), silent = TRUE)
          } else {
            if (inherits(try(remDr$getTitle(), silent = TRUE), "try-error")) {
              message("Restarting remote driver.")
              #remDr <- ACTIVATE_DRIVER()$client
              remDr <- remote_driver$client
              remDr$open(silent = TRUE)
            } 
            remDr$navigate(url_eurlex_proposal); Sys.sleep(ifelse(eurlex_attempts > 2, 2, .75))
            attempt_res <- try(read_html(remDr$getPageSource() %>% unlist()), silent = TRUE)
          }
          if (!inherits(attempt_res, "try-error")) {
            eurlex_search_proposal <- attempt_res 
            connection_eurlex_proposal_established <- TRUE
          } else {
            eurlex_attempts <- eurlex_attempts + 1
          }
        }
      } # End of condition for non-missing EUR-Lex url
      
      if (connection_eurlex_proposal_established){
        # Note, it might be the case that a connection is established but there is no real eurlex page for the proposal
        # Example: 1999/0004(AVC): https://eur-lex.europa.eu/search.html?type=expert&qid=1690030654703
        
        ### Celex number of the original proposal instrument
        attempt_celex_proposal <- try(eurlex_search_proposal %>% 
              html_elements(xpath="//meta[contains(@name,'WT.z_docID')]") %>%
              html_attr('content') %>% str_trim(), silent = TRUE)
        
        if(!exists("attempt_celex_proposal")){ attempt_celex_proposal <- NA } else { if(is_empty(attempt_celex_proposal)) { attempt_celex_proposal <- NA }}
        
        if(!is.na(attempt_celex_proposal)) {
          celex_proposal <- attempt_celex_proposal 
        } else {
          Sys.sleep(1.5)
          eurlex_search_proposal <- try(read_html(remDr$getPageSource() %>% unlist()), silent = TRUE)
          try(celex_proposal <- eurlex_search_proposal %>% 
                html_elements(xpath="//meta[contains(@name,'WT.z_docID')]") %>%
                html_attr('content') %>% str_trim(), silent = TRUE)
        }
        
        if(!exists("celex_proposal")){ celex_proposal <- NA } else { if(is_empty(celex_proposal)) { celex_proposal <- NA }}
        
        if(is.na(celex_proposal)){
          possible_celex <- eurlex_search_proposal %>%
            html_nodes(xpath="//dt[contains(.,'CELEX number:')]//following-sibling::dd") %>% html_text2()
          possible_celex <- possible_celex[grepl("[[:digit:]]", possible_celex)] # Remove elements without any number in it
          possible_celex <- possible_celex[!grepl("R\\([[:digit:]]", possible_celex)] # Identify and remove corrigenda
          possible_celex <- unique(possible_celex) # Keep unique remove duplicates if any
          # If both DC and PC in celexes, keep only PC
          if(any(grepl('PC',possible_celex)) == TRUE & any(grepl('DC',possible_celex)) == TRUE){ possible_celex <- grep('PC', possible_celex, value = TRUE)}
          # Sort
          possible_celex <- sort(possible_celex)
          if (length(possible_celex) > 1){
            celex_multiple <- TRUE
            while(celex_multiple){
              for (cel in possible_celex){                         # Try to disambiguate
                if (verbose == TRUE){ cat('Trying to disambiguate CELEX of the proposal\n') }
                if (inherits(try(remDr$getTitle(), silent = TRUE), "try-error")) { remDr <- remote_driver$client; remDr$open(silent = TRUE) }
                remDr$navigate(paste0('https://eur-lex.europa.eu/legal-content/EN/ALL/?uri=CELEX:',cel)); Sys.sleep(.75)
                attempt_res <- try(read_html(remDr$getPageSource() %>% unlist()), silent = TRUE)
                if (!inherits(attempt_res, "try-error")) {
                  possible_proposal_page <- attempt_res
                  procedure_in_celex <- possible_proposal_page %>%
                    html_node(xpath="//dt[contains(.,'Procedure number')]//following-sibling::dd//a") %>% html_text2()
                  if(is.na(procedure_in_celex)){
                    procedure_in_celex <- possible_proposal_page %>%
                      html_node(xpath="//dt[contains(.,'Additional information:')]//following-sibling::dd//span") %>% html_text2()
                  }
                  if(!is.na(procedure_in_celex) & (str_count(procedure_in_celex,substr(procedure_number, 1, 9)) > 0)){
                    celex_proposal <- cel; celex_multiple <- FALSE
                  } else {
                    remDr$navigate(paste0('https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:',cel))
                    possible_proposal_page <- read_html(remDr$getPageSource() %>% unlist())
                    possible_proposal_page %>%
                      html_node(xpath="//div[contains(@id,'TexteOnly')]//txt_te//p") %>% html_text2()->eurlex_proposal_title
                    if(grepl(str_remove_all(substr(procedure_number,6,14),'[^[:alpha:]|^[:digit:]]'),str_remove_all(eurlex_proposal_title,'[^[:alpha:]|^[:digit:]]'))){
                      celex_proposal <- cel; celex_multiple <- FALSE
                    } else{
                      if(cel == possible_celex[length(possible_celex)]){
                        celex_proposal <- possible_celex[1]; celex_multiple <- FALSE
                      }
                    }
                  }
                } else { if(verbose == TRUE){ cat('Error in reading possible page of proposal\n') }}
              }
            }
          } else { if (length(possible_celex) == 0){ celex_multiple <- FALSE } else { celex_proposal <- possible_celex; celex_multiple <- FALSE }} 
        } # End of condition celex of proposal is not NA

        ### Move to EUR-Lex page of the proposal with details on the procedure steps
        
        if(!is.na(celex_proposal)){
          
          if (inherits(try(remDr$getTitle(), silent = TRUE), "try-error")) { remDr <- remote_driver$client; remDr$open(silent = TRUE) }
          try(remDr$navigate(paste0('https://eur-lex.europa.eu/legal-content/EN/ALL/?uri=CELEX:',celex_proposal)), silent = TRUE)#; Sys.sleep(1)
          proposal_ALL <- try(read_html(remDr$getPageSource() %>% unlist()), silent = TRUE)
          
          if (!inherits(proposal_ALL, "try-error")) {
            
            ### Title of the proposal useful for finding legal instrument if missing
            proposal_title <- proposal_ALL %>%
              html_node(xpath="//meta[contains(@name,'z_docTitle')]") %>%
              html_attr('content') %>% str_trim()
            
            ### Subject matter
            subject_matter <- proposal_ALL %>%
              html_elements(xpath="//dt[contains(.,'Subject matter')]//
                          following-sibling::dd[1]//span") %>% html_text2() %>% unique()
            subject_matter <- paste0(as.character(subject_matter), collapse = '; ')
            subject_matter <- ifelse(subject_matter == '', NA, subject_matter)
            
            ### Eurovoc descriptors
            eurovoc_desc <- proposal_ALL %>%
              html_elements(xpath="//dt[contains(.,'EUROVOC descriptor')]//
                          following-sibling::dd[1]//span") %>% 
              html_text2() %>% unique()
            eurovoc_desc <- paste0(as.character(eurovoc_desc), collapse = '; ')
            eurovoc_desc <- ifelse(eurovoc_desc=='',NA,eurovoc_desc)
            
            ### Directory codes
            directory_code <- proposal_ALL %>%
              html_elements(xpath="//dt[contains(.,'Directory code')]//
                          following-sibling::dd[1]//li/text()") %>% html_text2()
            directory_code <- directory_code[c(which(directory_code %!in% c('/','')))]
            directory_code <- paste0(as.character(directory_code), collapse = '; ')
            
            directory_code_names <- proposal_ALL %>%
              html_elements(xpath="//dt[contains(.,'Directory code')]//
                          following-sibling::dd[1]//span") %>% html_text2()
            directory_code_names <- paste0(as.character(directory_code_names), collapse = '; ')
            directory_code_names <- ifelse(directory_code_names=='',NA,directory_code_names)
          } # End of condition link of EUR-Lex proposal works
          
          # compare
          # https://eur-lex.europa.eu/legal-content/EN/HIS/?uri=CELEX:52002PC0008
          # https://eur-lex.europa.eu/legal-content/EN/HIS/?uri=CELEX:52019PC0151
          
          remDr$navigate(paste0('https://eur-lex.europa.eu/legal-content/EN/HIS/?uri=CELEX:',celex_proposal))#; Sys.sleep(1)
          proposal_HIS <- try(read_html(remDr$getPageSource() %>% unlist()), silent = TRUE)
          
          if (!inherits(proposal_HIS, "try-error")) {
            
            ### Council agenda
            
            proposal_page_type <- proposal_HIS %>% html_element(xpath='//h2[contains(.,"Steps of procedure")]') %>% html_text()

            if(is_empty(proposal_page_type) | is.na(proposal_page_type)){
              # This is the "old" page format
              # https://eur-lex.europa.eu/legal-content/EN/HIS/?uri=CELEX:52002PC0008
              eurlex_page_type <- 'OLD'
                
              council_nodes <- proposal_HIS %>%
                html_elements(xpath="//dt[contains(., 'Council session:')]//
                ancestor::div[contains(@class,'panel-body PanelNoPadding')]")
              
              council_session_scope_eurlex <- council_session_date_eurlex <- council_session_id_eurlex <- council_agenda_item_eurlex <- council_config_eurlex <- c()
              
              for(cnode in council_nodes) {
                
                council_session_date_eurlex <- c(council_session_date_eurlex,
                                                 cnode %>% 
                                                   html_element(xpath = "./ancestor::div[contains(@class,'panel')]/
                                                   div[contains(@class,'panel-heading')]//span[@class='ProcedureDate']") %>%
                                                   html_text2() %>% str_remove_all(':$'))
                
                council_session_scope_eurlex <- c(council_session_scope_eurlex,
                                                  cnode %>% 
                                                    html_element(xpath = "./ancestor::div[contains(@class,'panel')]/
                                                   div[contains(@class,'panel-heading')]//span[@class='rightPadding20px']") %>%
                                                    html_text2())
                
                council_session_id_eurlex <- c(council_session_id_eurlex,
                                               cnode %>% 
                                               html_element(xpath = ".//dt[contains(.,'Council session')]//
                                                            following-sibling::dd[1]//span") %>%
                                                 html_text2())
                
                council_agenda_item_eurlex <- c(council_agenda_item_eurlex,
                                                cnode %>% 
                                                  html_element(xpath = ".//span[contains(., 'ITEM ') and contains(., ' ON COUNCIL AGENDA')]") %>%
                                                  html_text2())
                
                council_config_eurlex <- c(council_config_eurlex,
                                           cnode %>% 
                                             html_element(xpath = ".//dt[contains(.,'Subject:')]//following-sibling::dd[1]//span") %>%
                                             html_text2())
              }
              
              council_session_id_eurlex <- paste(council_session_id_eurlex, collapse = '; ')
              council_agenda_item_eurlex <- str_remove_all(council_agenda_item_eurlex, "ITEM | ON COUNCIL AGENDA|\"") 
              council_agenda_item_eurlex <- paste(council_agenda_item_eurlex, collapse = '; ')
              council_config_eurlex <- paste(council_config_eurlex, collapse = '; ')
              council_session_scope_eurlex <- paste(council_session_scope_eurlex, collapse = '; ')
              council_session_date_eurlex <- paste(council_session_date_eurlex, collapse = '; ')
              
            } else {
              eurlex_page_type <- 'NEW'
              # Likely this is the new page format
              # https://eur-lex.europa.eu/legal-content/EN/HIS/?uri=CELEX:52019PC0151
              
              # First search on negotiation section of council
              council_nodes <- proposal_HIS %>%
                html_elements(xpath="//dt[contains(., 'Council session:')]//
                ancestor::div[contains(@class,'collapse')]//
                preceding-sibling::div[contains(@class,'eventRow')]")
              
              council_session_scope_eurlex <- council_session_date_eurlex <- council_session_id_eurlex <- council_agenda_item_eurlex <- council_config_eurlex <- c()
              
              for(cnode in council_nodes) {
                
                council_session_date_eurlex <- c(council_session_date_eurlex,
                                                 cnode %>% 
                                                   html_element(xpath = ".//div[contains(@class,'eventDate')]//span") %>%
                                                   html_text2())
                
                council_session_scope_eurlex <- c(council_session_scope_eurlex,
                                                  cnode %>% 
                                                    html_element(xpath = ".//div[contains(@class,'VMILess')]") %>%
                                                    html_text2())
                
                council_session_id_eurlex <- c(council_session_id_eurlex,
                                               cnode %>% 
                                                 html_element(xpath = "//dt[contains(.,'Council session')]//
                                                            following-sibling::dd[1]//span") %>%
                                                 html_text2())
                
                council_agenda_item_eurlex <- c(council_agenda_item_eurlex,
                                                cnode %>% 
                                                  html_element(xpath = "//span[contains(., 'ITEM ') and contains(., ' ON COUNCIL AGENDA')]") %>%
                                                  html_text2())
                
                council_config_eurlex <- c(council_config_eurlex,
                                           cnode %>% 
                                             html_element(xpath = "//dt[contains(.,'Subject:')]//following-sibling::dd[1]//span") %>%
                                             html_text2())
                
                council_session_id_eurlex <- paste(council_session_id_eurlex, collapse = '; ')
                council_agenda_item_eurlex <- str_remove_all(council_agenda_item_eurlex, "ITEM | ON COUNCIL AGENDA|\"") 
                council_agenda_item_eurlex <- paste(council_agenda_item_eurlex, collapse = '; ')
                council_config_eurlex <- paste(council_config_eurlex, collapse = '; ')
                council_session_scope_eurlex <- paste(council_session_scope_eurlex, collapse = '; ')
                council_session_date_eurlex <- paste(council_session_date_eurlex, collapse = '; ')
                
              }
            }
            council_session_id_eurlex <- ifelse(is.null(council_session_id_eurlex), NA, council_session_id_eurlex)
            council_agenda_item_eurlex <- ifelse(is.null(council_agenda_item_eurlex), NA, council_agenda_item_eurlex) 
            council_config_eurlex <- ifelse(is.null(council_config_eurlex), NA, council_config_eurlex)
            council_session_scope_eurlex <- ifelse(is.null(council_session_scope_eurlex), NA, council_session_scope_eurlex)
            council_session_date_eurlex <- ifelse(is.null(council_session_date_eurlex), NA, council_session_date_eurlex)
            
          }
        } # End of condition there is a valid proposal celex
      } # End of condition if eurlex link of proposal has established connection
    } # End of condition if there's a possible link in proposal_links 
    
    return(list(
      summary_proposal_id = ifelse(exists("summary_proposal_id"), summary_proposal_id, NA),
      celex_proposal = ifelse(exists("celex_proposal"), celex_proposal, NA),
      proposal_title = ifelse(exists("proposal_title"), proposal_title, NA),
      subject_matter = ifelse(exists("subject_matter"), subject_matter, NA),
      eurovoc_desc = ifelse(exists("eurovoc_desc"), eurovoc_desc, NA),
      directory_code = ifelse(exists("directory_code"), directory_code, NA),
      directory_code_names = ifelse(exists("directory_code_names"), directory_code_names, NA),
      proposal_ALL = ifelse(exists("proposal_ALL"), proposal_ALL, NA),
      eurlex_page_type = ifelse(exists("eurlex_page_type"), eurlex_page_type, NA),
      council_session_id_eurlex = ifelse(exists("council_session_id_eurlex"), council_session_id_eurlex, NA),
      council_config_eurlex = ifelse(exists("council_config_eurlex"), council_config_eurlex, NA),
      council_agenda_item_eurlex = ifelse(exists("council_agenda_item_eurlex"), council_agenda_item_eurlex, NA),
      council_session_date_eurlex = ifelse(exists("council_session_date_eurlex"), council_session_date_eurlex, NA),
      council_session_scope_eurlex = ifelse(exists("council_session_scope_eurlex"), council_session_scope_eurlex, NA)
    ))
    
  } ### End of PROPOSAL_INFO_AND_CELEX function ################################
  
  NON_PENDING_AND_COMPLETE <- function() {
    
    if (verbose){cat('Procedure is not still pending\n')}
    
    ### Date of conclusion
    
    date_end <- key_events_date[which(key_events_event %in% end_procedure_labels)]
    if(is_empty(date_end)){ date_end <- key_events_date[length(key_events_date)] }
    if(length(date_end) > 1){ date_end <- date_end[1] }
    
    ######################################
    ### Only for completed legislation ###
    ######################################
    
    if (procedure_status == 'Completed'){
      
      if (verbose){ cat('Procedure has been completed\n') }
      
      final_act_links <- html_search %>%
        html_elements(xpath="//span[contains(.,'Final act')]//
                  ancestor::div[contains(@class,'es_product-section') or contains(@class,'erpl_product-section')]//a") %>%
        html_attr('href')
      
      final_act_elements <- html_search %>%
        html_elements(xpath="//span[contains(.,'Final act')]//
                  ancestor::div[contains(@class,'es_product-section') or contains(@class,'erpl_product-section')]//span") %>%
        html_text2() %>% str_trim()
      
      ### Name of the act
      act_name <- final_act_elements[which(!grepl('Final act|OJ|Corrigendum',final_act_elements))]
      act_name <- paste0(as.character(act_name), collapse = '; ')
      act_name <- ifelse(act_name == '', NA, act_name)
      
      ### Official journal name of the final act
      oj_refs_pos <- which(grepl('OJ',final_act_elements))
      if(!is_empty(oj_refs_pos)){
        if(length(oj_refs_pos) > 1){
          name_pos <- which(!grepl('Final act|OJ|Corrigendum',final_act_elements))
          oj_ref <- final_act_elements[oj_refs_pos[which(oj_refs_pos>name_pos)]]
          oj_ref <- paste0(as.character(oj_ref), collapse = '; ')
        } else { oj_ref <- final_act_elements[oj_refs_pos] }}
      
      ### Summary of final act
      
      summary_final_id <- str_remove_all(final_act_links[which(grepl('oeil/en/document-summary',final_act_links))],"\\/oeil\\/en\\/document-summary\\?")
      summary_final_id <- paste0(as.character(summary_final_id), collapse = '; ')
      
      ####################
      ### From EUR-Lex ###
      ####################
      
      if (verbose){cat('Moving to EUR-Lex\n')}

      ### Celex of the final act
      url_eurlex_final <- unique(final_act_links[which(grepl('eur-lex(.+)CELEX',final_act_links))])
      
      if(is_empty(url_eurlex_final) & exists("proposal_ALL")){
        attempt_res <- try(proposal_ALL %>%
          html_element(xpath="//dt[contains(.,'Adopted act')]//following-sibling::dd//a") %>%
          html_attr('data-celex') %>% str_trim(), silent = TRUE)
        if (inherits(attempt_res, "try-error")) { celex_final <- NA } else { celex_final <- attempt_res}
        if(!is_empty(celex_final) & !is.na(celex_final)) { 
          url_eurlex_final <- paste0("https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:", celex_final[1])
        } else {
          # Last resort
          if (!is.na(html_search %>% html_node(xpath="//div[contains(@id,'final_act-data')]//button[contains(@id,'summary')]"))){
            celex_final <- html_search %>%
              html_node(xpath="//div[contains(@id,'final_act-data')]//button[contains(@id,'summary')]") %>%
              html_attr('title') %>% as.character() %>% str_remove('Summary for') %>% str_trim()
          } else { 
            try(celex_final <- str_extract_all(url_eurlex_final,"[0-9]{5,6}[A-Z]{1,2}[0-9]{4}(.?)$")[[1]], silent = TRUE)
          }
          if(is.na(celex_final) | is_empty(celex_final)) {
            if(!is_empty(act_name)) {
              celex_final <- paste0('3',str_extract(str_extract(act_name,'[a-z]\\s[0-9]{4}\\/'),'[0-9]{4}'),
                                    ifelse(grepl('Regulation', act_name),'R',ifelse(grepl('Directive', act_name),'L',ifelse(grepl('Decision', act_name),'D','MISSING'))),
                                    str_extract(str_extract(act_name,'\\/[0-9]{4}$'),'[0-9]{4}'))
              celex_final <- ifelse(grepl('MISSING',celex_final), NA, celex_final)
              if(!is.na(celex_final)) { url_eurlex_final <- paste0("https://eur-lex.europa.eu/legal-content/EN/TXT/?uri=CELEX:", celex_final) }
            }
          }
        }
      }

      if(!is_empty(url_eurlex_final)){
        if(length(url_eurlex_final) > 1){
          if(any(grepl('\\([0-9]{1,2}\\)$',url_eurlex_final))){
            url_eurlex_final0 <- url_eurlex_final[which(!grepl('\\([0-9]{1,2}\\)$',url_eurlex_final))]
            if(is_empty(url_eurlex_final0)){
              url_eurlex_final<-url_eurlex_final[length(url_eurlex_final)] # just pick the last
            } else {
              if(length(url_eurlex_final0)>1){
                url_eurlex_final <- url_eurlex_final[length(url_eurlex_final)] # just pick the last
              } else {
                url_eurlex_final <- url_eurlex_final0
              }
            }
          } else {
            url_eurlex_final <- url_eurlex_final[1] # just pick the first
          }
        }
        
        connection_eurlex_final_established <- FALSE
        if (is.na(url_eurlex_final)){ if (verbose == TRUE){ cat('EUR-Lex link of final act not found\n') }
        } else {
          eurlex_attempts <- 0
          while((connection_eurlex_final_established == FALSE) & (eurlex_attempts < 4)){
            if(eurlex_attempts <= 1) {
              attempt_res <- try(read_html(url_eurlex_final), silent = TRUE)
            } else {
              if (inherits(try(remDr$getTitle(), silent = TRUE), "try-error")) {
                message("Restarting remote driver.")
                remDr <- remote_driver$client
                remDr$open(silent = TRUE)
              } 
              remDr$navigate(url_eurlex_final); Sys.sleep(ifelse(eurlex_attempts > 2, 2, .75))
              attempt_res <- try(read_html(remDr$getPageSource() %>% unlist()), silent = TRUE)
            }
            if (!inherits(attempt_res, "try-error")) {
              eurlex_search_final <- attempt_res 
              connection_eurlex_final_established <- TRUE
            } else {
              eurlex_attempts <- eurlex_attempts + 1
            }
          }
        } # End of condition for non-missing EUR-Lex url

        if(connection_eurlex_final_established){
          try(celex_final <- eurlex_search_final %>%
                html_node(xpath="//meta[contains(@name,'WT.z_docID')]") %>%
                html_attr('content') %>% str_trim(), silent = TRUE)
            
          try(force_status <- eurlex_search_final %>% 
                html_elements(xpath="//img[contains(@alt,'Legal status of the document')]//parent::span//parent::p") %>%
                html_text2(), silent = TRUE)
            
          ### Fill gap on proposal information if managed to find celex_final and the former is missing
          
          if (!is.na(celex_final)){
            if(exists("proposal_number") == FALSE) {
              if (inherits(try(remDr$getTitle(), silent = TRUE), "try-error")) { remDr <- remote_driver$client; remDr$open(silent = TRUE) } 
              remDr$navigate(paste0('https://eur-lex.europa.eu/legal-content/EN/HIS/?uri=CELEX:',celex_final))
              attempt_res <- try(read_html(remDr$getPageSource() %>% unlist()), silent = TRUE) 
              
              if (!inherits(attempt_res, "try-error")) {
                eurlex_final_HIS <- attempt_res 
                celex_proposal <- eurlex_final_HIS %>%
                  html_nodes(xpath="//div[contains(@id,'PROPCOM')]//
                                 following-sibling::div//div//span//a") %>%
                  html_text2()
                
                date_initiation <- eurlex_final_HIS %>%
                  html_nodes(xpath="//div[contains(@id,'PROPCOM')]//
                               following-sibling::div//div//
                               following-sibling::div[contains(@class,'col-xs-5 col-sm-3 eventDate')]") %>%
                  html_text2() %>% unique()
                
                if(length(date_initiation) > 1){ date_initiation <- date_initiation[which(as.Date(date_initiation,'%d/%m/%Y') == min(as.Date(date_initiation,'%d/%m/%Y')))] }
                
                proposal_number <- eurlex_final_HIS %>%
                  html_node(xpath="//div[contains(@id,'PROPCOM')]//div//following-sibling::div//
                        dt[contains(.,'Documents:')]//following-sibling::dd") %>% html_text2()
              }
            } # End of condition HIS page of final act opens without problems
            if(is.na(date_end)) {
              
              if (inherits(try(remDr$getTitle(), silent = TRUE), "try-error")) { remDr <- remote_driver$client; remDr$open(silent = TRUE) } 
              remDr$navigate(paste0('https://eur-lex.europa.eu/legal-content/EN/ALL/?uri=CELEX:',celex_final))
              attempt_res <- try(read_html(remDr$getPageSource() %>% unlist()), silent = TRUE) 
              
              if (!inherits(attempt_res, "try-error")) {
                eurlex_final_ALL <- attempt_res
                date_end <- eurlex_final_ALL %>%
                  html_element(xpath="//dt[contains(.,'Date of document')]//
                                 following-sibling::dd") %>%
                  html_text2()
              }
            }
          } # End of condition proposal information is missing
        } # End of condition if connectin with EUR-Lex final is established
      } # End of condition if EUR-Lex page of final act is available
    } # End of condition for completed procedures
    
    return(list(
      date_end = date_end,
      summary_final_id = ifelse(exists("summary_final_id"), summary_final_id, NA),
      act_name = ifelse(exists("act_name"), act_name, NA), 
      oj_reference = ifelse(exists("oj_ref"), oj_ref, NA),
      celex_final = ifelse(exists("celex_final"), celex_final, NA),
      force_status = ifelse(exists("force_status"), force_status, NA),
      proposal_number = proposal_number,
      date_initiation = date_initiation,
      celex_proposal = celex_proposal
    ))
  } ### End of NON_PENDING_AND_COMPLETE function ###############################
  
  options(digits.secs = 0, error = traceback)
  
  cat('\nStarting:',format(start_time <<- Sys.time(),'%m/%d/%Y at %H:%M:%S'),'\n')
  
  suppressMessages(library(rvest)) 
  suppressMessages(library(stringr)) 
  suppressMessages(library(rlang)) 
  suppressMessages(library(plyr))
  suppressMessages(library(dplyr))
  suppressMessages(library(RSelenium))
  suppressMessages(library(wdman))
  suppressMessages(library(netstat))
  suppressMessages(library(httpuv))
  
  fprof <- makeFirefoxProfile(list("browser.tabs.remote.autostart" = FALSE))
  
  if(verbose) { cat("Setting up remote driver\n") }
  selenium_object <- selenium(retcommand = TRUE, check = FALSE)
  
  remote_driver <- rsDriver(browser = 'firefox', port = httpuv::randomPort(),
                            verbose = FALSE, check = FALSE, 
                            extraCapabilities = list("moz:firefoxOptions" = list(args = list("--headless"))))

  remDr <- remote_driver$client
  remDr$open(silent = TRUE)
  
  '%!in%' <- Negate('%in%')

  procedures_list <- c(
    ### Ordinary legislative procedure
    'COD'='8', 
    ### Special legislative procedures
    'CNS'='11', 'APP'='41021', 
    ### Historic legislative procedures
    'AVC'='9', 'SYN'='10',
    ### Non legislative procedures
    'NLE'='41020', 
    ### Parliament resolutions and initiatives
    'INI'='14', 'COS'='15', 'RSP'='18', 'DCE'='17', 'INL'='558000',
    ### Budgetary procedures
    'BUD'='19', 'DEC'='20', 'BUI'='597000',
    ### Internal parliament organisation procedures
    'REG'='22', 'IMM'='23', 'RSO'='24',
    ### Quasi-legislative procedures
    'INS'='26', 'ACI'='12', 'DEA'='553000', 'RPS'='41045'
  )
  
  ### Following lines are to figure out user input and check it is correctly provided
  
  if(!is.null(exact_procedures)){
    oeil_searches <- exact_procedures; years <- 9999
  } else {
    if(is.null(procedures)){
      procedures <- procedures_list
    } else {
      procedures <- procedures_list[which(names(procedures_list) %in% procedures)]
    }
    oeil_searches <- NULL
  }
  
  ### Here you should have all info needed for scraping ###
  
  ### Define variables to keep along all the scraping process ###
  
  ### Column names for empty dataframe to be created
  variables <- c(
    'procedure_number','date_initiation','procedure_title','procedure_type','procedure_subtype','procedure_status',
    'date_end','act_name','oj_reference',
    'proposal_number','proposal_title','revised_proposal', 'leg_instrument','subject_oeil',
    'eurovoc_desc','subject_matter',"directory_code",'directory_code_names','cap_topics',
    'celex_proposal','celex_final',
    "council_config_oeil", "council_session_id_oeil", "council_session_date_oeil", # from OEIL
    "council_config_eurlex","council_session_id_eurlex","council_session_date_eurlex",
    "council_session_scope_eurlex","council_agenda_item_eurlex",  # from EUR-Lex
    "eurlex_page_type",
    'ep_cmtee','ep_cmtee_opinion','n_ep_cmtee_opinion','plenary_texts','texts_adopted',
    'rapporteur','rapporteur_appnt','rapporteur_party','rapporteur_url','shadow_rapporteur',
    'commission_dg','commissioner',
    'legal_basis','cmtee_dossier','trilogue',
    "natparl_contribution","natparl_doctype","natparl_docref","natparl_docdate",
    'summary_proposal_id','summary_final_id','key_events',"key_dates",'date_scraped')
  
  ### List of labels to identify key events
  start_procedure_labels <- c(
    'Commission draft budget published',
    'Commission draft budget',
    'Initial legislative proposal',
    'Initial legislative proposal published',
    'Legislative proposal published',
    'Legislative proposal',
    'Non-legislative basic document published',
    'Non-legislative basic document',
    'Preparatory document')
  
  end_procedure_labels <- c(
    'Final act published in Official Journal',
    'Final act signed',
    'Proposal withdrawn by Commission',
    'Delegated act not objected by Parliament',
    'Delegated act not objected by Council')
  
  ep_votes_labels <- c(
    'Committee report tabled for plenary, single reading',
    'Motion for a resolution',
    'Motion for a resolution objecting delegated act',
    'Text adopted by Parliament, 1st reading/single reading')
  
  revision_proposal_labels <- c(
    'Amended legislative proposal',
    'Amended legislative proposal for reconsultation published',
    'Modified legislative proposal published',
    'Modified legislative proposal')
  
  trilogue_labels <- c(
    'Approval in committee of the text agreed at 1st reading interinstitutional negotiations',
    'Committee decision to open interinstitutional negotiations with report adopted in committee',
    'Committee decision to enter into interinstitutional negotiations announced in plenary (Rule 71)',
    'Committee decision to enter into interinstitutional negotiations confirmed by plenary (Rule 71)',
    'Committee decision to enter into interinstitutional negotiations confirmed by plenary (Rule 71 - vote)',
    'Committee decision to enter into interinstitutional negotiations announced in plenary (Rule 72)')
  
  dfs <- list()
  
  #cap_schema <- readRDS(url("https://mscottodivettimo.github.io/scrapeu/oeil_thesaurus.rds"))
  cap_schema <- read.csv(url("https://www.dropbox.com/scl/fi/l9jk4anwo5p7qz00ue9p8/oeil_subjects_thesaurus.csv?rlkey=pw1rkiaq0ozbz1q19smec38bk&st=libcj9cd&dl=1"))
  
  for (year in as.character(c(years))){ # Loop over the years
    gc()
    
    #conns <- data.frame(showConnections(all = TRUE)) %>% filter(grepl("eur-lex.europa.eu", description))
    #if (!is.null(conns)) {for (id in as.numeric(rownames(conns))) {try(close(getConnection(id)), silent = TRUE)}}
    #rm(conns, id)
    
    if(is.null(oeil_searches)){
      oeil_searches <- c()

      ### Adjust procedure list according to year to make search faster
      if((year > 2002) & ('COS' %in% names(procedures))){
        if (verbose == TRUE){cat('COS procedure type has been removed from query as they were no longer in use during the year searched\n')}
        procedures <- procedures[!procedures==procedures[which(names(procedures)=='COS')]]}
      
      if((year > 2005) & ('SYN' %in% names(procedures))){
        if (verbose == TRUE){cat('SYN procedure type has been removed from query as they were no longer in use during the year searched\n')}
        procedures <- procedures[!procedures==procedures[which(names(procedures)=='SYN')]]}
      
      if((year < 2008) & ('APP' %in% names(procedures))){
        if (verbose == TRUE){cat('APP procedure type has been removed from query as it was not yet in use during the year searched\n')}
        procedures <- procedures[!procedures==procedures[which(names(procedures)=='APP')]]}
      
      if((year > 2008) & ('AVC' %in% names(procedures))){
        if (verbose == TRUE){cat('AVC procedure type has been removed from query as they were no longer in use during the year searched\n')}
        procedures <- procedures[!procedures==procedures[which(names(procedures)=='AVC')]]}
      
      if((year > 2012) & ('DCE' %in% names(procedures))){
        if (verbose == TRUE){cat('DCE procedure type has been removed from query as they were no longer in use during the year searched\n')}
        procedures <- procedures[!procedures==procedures[which(names(procedures)=='DCE')]]}
      
      if((year < 2013) & ('DEA' %in% names(procedures))){
        if (verbose == TRUE){cat('DEA procedure type has been removed from query as it was not yet in use during the year searched\n')}
        procedures <- procedures[!procedures==procedures[which(names(procedures)=='DEA')]]}
      
      for (procedure in procedures){
        
        xml_link <- paste0("https://oeil.secure.europarl.europa.eu/oeil/en/search/export/XML?fullText.mode=EXACT_WORD&year=",year,"&procedureType=",procedure,"&resultsOnly=true")
        
        xml_file <- readLines(xml_link)
        search_results <- str_extract_all(xml_file,'<reference>[0-9]{4}/[0-9]{4}[A-Z]{0,1}\\([A-Z]{3}\\)</reference>')
        search_results <- str_remove_all(unlist(search_results[lapply(search_results,length) > 0]),'<reference>|</reference>')
        
        if(length(search_results) > 0){
          if(length(search_results) < 500){
            oeil_searches <- c(oeil_searches, search_results)
            cat('Found',length(search_results),'results for procedure',names(which(procedures == procedure)),'\n')
          } else {
            cat('WARNING: Maybe too many results for',names(which(procedures == procedure)),'\n')
          }
        }
      }
      rm(procedure)
    }
    
    i <- 0; date_scraped <- as.character(Sys.Date())
    df <- as.data.frame(matrix(NA, 1, length(variables))) %>% `colnames<-`(variables) # Create new dataframe for each year

    cat('Scraping',ifelse(year == 9999,'selected',year),'files.',length(oeil_searches),'search results found.\n\n')

    for (procedure_number in sort(oeil_searches)){ procedure_number <<- procedure_number

      url_oeil <- paste0('https://oeil.secure.europarl.europa.eu/oeil/popups/ficheprocedure.do?reference=',procedure_number,'&l=en')
      
      i <- i + 1 # Track iteration number for filling df with results (inner most loop)
      if (i == 1 | i == length(oeil_searches) | i %in% (round(length(oeil_searches)/33, 0) * c(1:33))){
        pct <- (i/length(oeil_searches)); len_tot <- 60
        cat(paste0(format(Sys.time(),'%H:%M:%S'),' |',paste(rep('#',round(len_tot*pct,0)),collapse=''),paste(rep('.',len_tot-round(len_tot*pct,0)),collapse=''),'| ',round(pct*100,0),'% (',i,' of ',length(oeil_searches),')'),'\n')
      }
      
      if(verbose){ cat(procedure_number,'\n') }

      connection_established <- FALSE
      attempts <- 0
      while((connection_established == FALSE) & (attempts < 4)){
        attempt_res <- try(read_html(url_oeil), silent = TRUE)
        if (!inherits(attempt_res, "try-error")) {
          if ('procedure either does not exist or is in the process of being initiated' %in% as.character(attempt_res)){ 
            connection_established <- FALSE 
          } else { 
            html_search <- attempt_res
            connection_established <- TRUE 
          }
        } else {
          attempts <- attempts + 1; Sys.sleep(attempts)
        }
      }
      if (connection_established){

        ###################
        ### Page header ###
        ###################
        
        all_res <- c()
        
        ### Title of procedure in OEIL page (page header)
        procedure_title <- html_search %>%
          html_elements(xpath = "//h2[contains(@class,'erpl_title-h2 mb-3') or contains(@class,'es_title-h2 mb-3')]") %>%
          html_text2() %>% str_trim()
        
        ##########################################
        ### OEIL Technical information section ###
        ##########################################
        
        res <- OEIL_TECHNICAL_INFORMATION(); for(r in names(res)) { assign(r, res[[r]]) }; all_res <- c(all_res, res); rm(res)
        # Obtains: leg_instrument, procedure_type, procedure_subtype, legal_basis, cmtee_dossier

        ###################################
        ### Key Players section on OEIL ###
        ###################################
        
        res <- KEY_PLAYERS(); for(r in names(res)) { assign(r, res[[r]]) }; all_res <- c(all_res, res); rm(res)
        # Obtains: leg_instrument, procedure_type, procedure_subtype, legal_basis, cmtee_dossier
        
        ##################################
        ### Key events section on OEIL ###
        ##################################
        
        res <- KEY_EVENTS(); for(r in names(res)) { assign(r, res[[r]]) }; all_res <- c(all_res, res); rm(res)
        # Obtains: plenary_texts, texts_adopted, trilogue, key_events, key_events_date

        ################################
        ### Document gateway section ###
        ################################
        
        # Requires: plenary_texts, texts_adopted
        res <- DOCUMENT_GATEWAY(); for(r in names(res)) { assign(r, res[[r]]) }; all_res <- c(all_res, res); rm(res)
        # Obtains:  plenary_texts (updates), texts_adopted (updates),
        
        #######################################################################
        ### Scrape data from EUR-Lex on proposal file and Celex identifiers ###
        #######################################################################

        res <- PROPOSAL_INFO_AND_CELEX(); for(r in names(res)) { assign(r, res[[r]]) }; all_res <- c(all_res, res); rm(res)
        # Obtains: proposal_number, celex_proposal
        
        ############################################################################
        ### Scrape information related to conclusion of procedure (also EUR-Lex) ###
        ############################################################################
        
        if (procedure_status!='Pending' & !is.na(procedure_status)){
          res <- NON_PENDING_AND_COMPLETE(); for(r in names(res)) { assign(r, res[[r]]) }; all_res <- c(all_res, res); rm(res)
        } else { 
          if (verbose){ cat('Procedure is still pending\n') }
          act_name <- ifelse(!exists("act_name"), NA, act_name)
        } 
        # Obtains: act_name, celex_final
        
        ############################################################################################
        ### Find way around to fill gaps in missing data (e.g. move back and forth OEIL/EUR-Lex) ###
        ############################################################################################
        
        if ((is_empty(date_initiation) | is.na(date_initiation))){
          all_dates <- unique(c(docgate_date, key_events_date))
          date_initiation <- all_dates[which(as.Date(all_dates,'%d/%m/%Y') == min(as.Date(all_dates,'%d/%m/%Y')))]
        }
        
        if (procedure_status %in% c('Completed','Withdrawn')){
          if (is_empty(date_end)){
            date_end <- key_events_date[which(as.Date(key_events_date,'%d/%m/%Y') == max(as.Date(key_events_date,'%d/%m/%Y')))]
          }
        } 
        
        ##################################################################
        ### Store information needed for better post-scraping recoding ###
        ##################################################################
  
        if (verbose){cat('Dataframe\n')}
  
        #####################################################
        ### Populate spreadsheet at end of each iteration ###
        #####################################################
        
        if(is.na(leg_instrument)){
          leg_instrument <- case_when(
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
        
        ### Comparative Agenda Project policy from OEIL subjects
        
        s_t <- subj_topic <- c()
        if (!is.na(subject_oeil)){
          subjects <- str_trim(strsplit(subject_oeil,'; ')[[1]])
          for (subj in subjects){
            add <- unique(na.omit(cap_schema[cap_schema$subject == subj,]$cap_subtopic))
            s_t <- c(s_t, add)
          }
          cap_subtopics <- paste0(s_t, collapse = '; ')
          if (cap_subtopics!=''){
            for (c_s in s_t){
              subj_topic <- c(subj_topic,unique(cap_schema[tolower(cap_schema$cap_subtopic)==tolower(c_s),]$cap_topic))
            }
            cap_topics <- paste0(modelr::typical(na.omit(subj_topic)), collapse = '; ')
          }
        }

        new_row <- c()
        for(v in variables) { 
          if(!exists(v)) { assign(v, NA) }
          if(is_empty(eval(as.name(v)))) { assign(v, NA) }
          new_row <- c(new_row, eval(as.name(v))) 
        }
        df[i,] <- new_row
        
        closeAllConnections()
        
      } # End of condition if connection established successfully

      ### Remove elements for next iteration only if more than one act is being scraped ###
      rm(list = setdiff(ls(),c('df','variables','year','i','date_scraped','%!in%','remDr','remote_driver',
                               'all','verbose','dfs','procedures','procedures_list','years','oeil_searches','start_time',
                               "OEIL_TECHNICAL_INFORMATION",'KEY_PLAYERS','KEY_EVENTS',"DOCUMENT_GATEWAY","PROPOSAL_INFO_AND_CELEX",'ACTIVATE_DRIVER',
                               "NON_PENDING_AND_COMPLETE",
                               'exact_procedures','cap_schema', c(ls()[grepl("_labels", ls())])))) # Keep names of metadata
      if (verbose == TRUE){cat(' \n')}
    } # End of loop on possible procedure numbers
    
    #################################################
    ### Store df of selected year in list of dfs ####
    #################################################
    
    remDr$close()
    
    if(is.null(exact_procedures) & length(years) > 1){ # If no specific procedures are provided and there is more than 1 year to be collected
      dfs[[paste('df',as.character(year),sep='')]] <- df # Store the df into a list of dfs
    } else { dfs <- df } # Otherwise the data can fit in a single df
    
    cat('\nEnding:',format(end_time <<- Sys.time(),'%m/%d/%Y at %H:%M:%S'),'\n')
    cat('\nTime elapsed:',end_time - start_time,'\n')
    
  } # End of loop on years
  
  return(dfs)

} # End function
cat('\nscRapEU function succesfully downloaded.\n\n')
cat('Documentation: https://michelescottodivettimo.eu/scrapeu/scRapEU_documentation.pdf\n')
cat("Minimal example code: https://michelescottodivettimo.eu/scrapeu/scRapEU_example.R\n\n")
cat('Citation:\nScotto di Vettimo, M. (2022), "scRapEU: An R function to scrape data on EU laws". Version February 2026. https://michelescottodivettimo.eu/scrapeu/scRapEU.R.\n\n')
cat('NOTE: The function is continously updated. Suggestions, and reports of bugs are welcome.\n')
cat('Last update: 3 February 2026.\n\n')