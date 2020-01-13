baixar_paginas_ilhasolteiranews <- function(diretorio = ".",caderno = "policia", paginas = 10){
  
  url <- paste0("https://www.ilhasolteira.news/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}


ler_paginas_ilhasolteiranews <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//header[@class="entry-header"]/h2[@class="entry-title"]/a') %>% 
        xml2::xml_attr("href")
  
  headline <- x %>% 
    xml2::xml_find_all('//header[@class="entry-header"]/h2[@class="entry-title"]/a') %>% 
    xml2::xml_text()
  
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//div[@class="row gutter-parent-14 post-wrap"]//div[@class="date"]/a') %>% 
    xml2::xml_text() %>% 
    lubridate::dmy()
  
  autor <- x %>% 
    xml2::xml_find_all('//div[@class="row gutter-parent-14 post-wrap"]//div[@class="by-author vcard author"]/a') %>% 
    xml2::xml_text() 
  
  descricao <- x %>% 
    xml2::xml_find_all('//div[@class="row gutter-parent-14 post-wrap"]//div[@class="entry-content"]/p') %>% 
    xml2::xml_text() 
  
  tibble::tibble(headline,data_publicacao,descricao,autor,url)
    
  }),NULL))
  
}

isn <- ler_paginas_ilhasolteiranews(arquivos = arquivos) 


baixar_noticias_ilhasolteiranews <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_ilhasolteiranews_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}

isn<-ler_paginas_ilhasolteiranews()

baixar_noticias_ilhasolteiranews(isn$url,"noticias")



ler_noticias_ilhasolteiranews <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,full.names = TRUE) 
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    
    
    headline <- x %>% 
         xml2::xml_find_all('//h1[@class="entry-title"]') %>% 
         xml2::xml_text()
         
    
   corpo <- x %>% 
      xml2::xml_find_all('//div[@class="entry-content"]//p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n")
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//header[@class="entry-header"]//div[@class="entry-meta"]/div[@class="date"]/a') %>% 
     xml2::xml_text() %>% 
     lubridate::dmy()
   
   
   autor <- x %>% 
     xml2::xml_find_first('//header[@class="entry-header"]//div[@class="entry-meta"]/div[@class="by-author vcard author"]/a') %>% 
     xml2::xml_text()
   
   tibble::tibble(headline,corpo,data_publicacao,autor)
   
  }),NULL))
    
}


noticias <- ler_noticias_ilhasolteiranews(arquivos = arquivos)
