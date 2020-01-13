baixar_paginas_diariorp <- function(diretorio = ".",caderno = "policial", paginas = 10){
  
  url <- paste0("https://diariorp.com.br/category/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}

p <- baixar_paginas_diariorp(diretorio = "pagina")


ler_paginas_diariorp <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//div[@class="td-ss-main-content"]//h3[@class="entry-title td-module-title"]/a') %>% 
        xml2::xml_attr("href") 
  
  headline <- x %>% 
    xml2::xml_find_all('//div[@class="td-ss-main-content"]//h3[@class="entry-title td-module-title"]/a') %>% 
    xml2::xml_text()%>% 
    iconv("utf-8","latin1//TRANSLIT")
  
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//div[@class="td-block-row"]//div[@class="td-module-meta-info"]//time[@class="entry-date updated td-module-date"]') %>% 
    xml2::xml_text()%>% 
    lubridate::dmy()
  
  tibble::tibble(headline,data_publicacao,url)
    
  }),NULL))
  
}

drp <- ler_paginas_diariorp(arquivos = arquivos)  


baixar_noticias_diariorp <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_diariorp_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}

rb<-ler_paginas_oriobranco()

baixar_noticias_diariorp(drp$url,"noticias"[1])



ler_noticias_diariorp <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    headline <- x %>% 
         xml2::xml_find_all('//h1[@class="entry-title"]') %>% 
         xml2::xml_text() %>% 
         iconv("utf-8","latin1//TRANSLIT")
    
   texto <- x %>% 
      xml2::xml_find_all('//p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n") %>% 
     iconv("utf-8","latin1//TRANSLIT")
   
     
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//header[@class="td-post-title"]//time[@class="entry-date updated td-module-date"]') %>% 
     xml2::xml_text() 
   
   autor <- x %>% 
     xml2::xml_find_first('//header[@class="td-post-title"]//div[@class="td-post-author-name"]/a') %>% 
     xml2::xml_text()%>% 
     iconv("utf-8","latin1//TRANSLIT")
   
   
   tibble::tibble(headline,texto,data_publicacao,autor)
   
  }),NULL))
    
}


noticias <- ler_noticias_diariorp(arquivos = arquivos)

