baixar_paginas_gazetadeiracemapolis <- function(diretorio = ".",caderno = "policia", paginas = 10){
  
  url <- paste0("http://gazetadeiracemapolis.com.br/noticias/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}

p <- baixar_paginas_gazetadeiracemapolis(diretorio = "pagina")


ler_paginas_gazetadeiracemapolis <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//article/a') %>% 
        xml2::xml_attr("href")
  
  headline <- x %>% 
    xml2::xml_find_all('//div[@class="content-text-list"]/h4') %>% 
    xml2::xml_text()
  
  descricao <- x %>% 
    xml2::xml_find_all('//div[@class="content-text-list"]//p') %>% 
    xml2::xml_text()
  
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//p[@class="date-and-catname"]') %>% 
    xml2::xml_text() %>% 
    stringr::str_sub(start = 8, end = 17)
  
  tibble::tibble(headline,descricao,data_publicacao,url)
    
  }),NULL))
  
}

gzt <- ler_paginas_gazetadeiracemapolis(arquivos = arquivos)  


baixar_noticias_gazetadeiracemapolis <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_gazetadeiracemapolis_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}



baixar_noticias_gazetadeiracemapolis(gzt$url,"noticias")



ler_noticias_gazetadeiracemapolis <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    headline <- x %>% 
         xml2::xml_find_all('//h1[@class="title"]') %>% 
         xml2::xml_text()
    
   texto <- x %>% 
      xml2::xml_find_all('//section[@class="row"]//p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n")
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//small') %>% 
     xml2::xml_text() %>% 
    stringr::str_sub(start = 13, end = 35)
   
   
   tibble::tibble(headline,texto,data_publicacao)
   
  }),NULL))
    
}


noticias <- ler_noticias_gazetadeiracemapolis(arquivos = arquivos)
