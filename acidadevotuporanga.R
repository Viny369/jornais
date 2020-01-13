baixar_paginas_acidadevotuporanga <- function(diretorio = ".",caderno = "policia", paginas = 10){
  
  url <- paste0("http://www.acidadevotuporanga.com.br/",caderno,"-s25?p=")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}



ler_paginas_acidadevotuporanga <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//ul[@class="lista_paginada"]/li/a') %>% 
        xml2::xml_attr("href") %>% 
    paste0("http://www.acidadevotuporanga.com.br/",.)
  
  headline <- x %>% 
    xml2::xml_find_all('//h3/a') %>% 
    xml2::xml_text()
  
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//li/p[@class="data"]') %>% 
    xml2::xml_text() %>% 
    lubridate::dmy()
  
  
  descricao <- x %>% 
    xml2::xml_find_all('//li/p[@class="data"]/following-sibling::p') %>% 
    xml2::xml_text() 
  
  tibble::tibble(headline,data_publicacao,descricao,url)
    
  }),NULL))
  
}



baixar_noticias_acidadevotuporanga <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
    artigo <- stringr::str_extract(.x,"\\d.+") %>% 
      stringr::str_replace_all("[/-]","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_acidadevotuporanga_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}

acdv <- ler_paginas_acidadevotuporanga()

baixar_noticias_acidadevotuporanga(acdv$url,"noticias")



ler_noticias_acidadevotuporanga <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,full.names = TRUE) 
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    
    
    headline <- x %>% 
         xml2::xml_find_all('//div[@class="colum_left left"]//h2') %>% 
         xml2::xml_text()
         
    descricao <- x %>% 
      xml2::xml_find_all('//div[@class="chamada"]') %>% 
      xml2::xml_text()
    
   corpo <- x %>% 
      xml2::xml_find_all('//div[@class="content_not formatPage"]') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n")
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//div[@class="data_not"]') %>% 
     xml2::xml_text() %>% 
     lubridate::dmy()
   
   
   autor <- corpo %>%
    stringr::str_trim() %>% 
    stringr::str_extract("\r\n\\s+\\w\\X{1,20}\r\n")
   
   
   autor <- stringr::str_trim(autor)
   corpo <- stringr::str_trim(corpo)
   
   tibble::tibble(headline,descricao,corpo,data_publicacao,autor)
   
  }),NULL))
    
}


noticias <- ler_noticias_acidadevotuporanga(arquivos = arquivos)
