baixar_paginas_oriobranco <- function(diretorio = ".",caderno = "policial", paginas = 10){
  
  url <- paste0("http://oriobranco.net/",caderno,"/pagina/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}


ler_paginas_oriobranco <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//div[@class="col-sm-8"]//div[@class="post-content"]/h2/a') %>% 
        xml2::xml_attr("href") %>% 
        paste0("http://www.oriobranco.net/",.)
  
  headline <- x %>% 
    xml2::xml_find_all('//div[@class="col-sm-8"]//div[@class="post-content"]/h2/a') %>% 
    xml2::xml_text()
  
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//ul[@class="post-tags"]/li') %>% 
    xml2::xml_text()
  
  tibble::tibble(headline,data_publicacao,url)
    
  }),NULL))
  
}

rb <- ler_paginas_oriobranco(arquivos = arquivos)  


baixar_noticias_oriobranco <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"\\d.+") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_oriobranco_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}

rb<-ler_paginas_oriobranco()

baixar_noticias_oriobranco(rb$url,"noticias")



ler_noticias_oriobranco <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    headline <- x %>% 
         xml2::xml_find_all('//div[@class="title-post"]/h1') %>% 
         xml2::xml_text() %>% 
         iconv("utf-8","latin1//TRANSLIT")
    
   texto <- x %>% 
      xml2::xml_find_all('//div[@class="post-content"]//p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n")
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//ul[@class="post-tags"]/li') %>% 
     xml2::xml_text() %>% 
     iconv("utf-8","latin1//TRANSLIT")
   
   autor <- x %>% 
     xml2::xml_find_first('//p/strong') %>% 
     xml2::xml_text()
   
   tibble::tibble(headline,data_publicacao,autor)
   
  }),NULL))
    
}


noticias <- ler_noticias_oriobranco(arquivos = arquivos)
