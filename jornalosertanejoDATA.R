baixar_paginas_jornalosertanejo <- function(diretorio = ".",caderno = "politica", paginas = 10){
  
  url <- paste0("http://jornalosertanejo.com.br/editoria/",caderno,"/pagina/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}

p <- baixar_paginas_jornalosertanejo(diretorio = "pagina")


ler_paginas_jornalosertanejo <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//div[@class="col-xs-12 col-sm-8 col-md-8 col-lg-8"]//h2/a') %>% 
        xml2::xml_attr("href")
  
  headline <- x %>% 
    xml2::xml_find_all('//div[@class="col-xs-12 col-sm-8 col-md-8 col-lg-8"]//h2/a') %>% 
    xml2::xml_text()

  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//div[@class="col-xs-12 col-sm-8 col-md-8 col-lg-8"]//li/small[@class="text-lowercase"]') %>% 
    xml2::xml_text()
  
  tibble::tibble(headline,data_publicacao,url)
    
  }),NULL))
  
}

jos <- ler_paginas_jornalosertanejo(arquivos = arquivos)  


baixar_noticias_jornalosertanejo <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"\\d.+") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_jornalosertanejo_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}



baixar_noticias_jornalosertanejo(jos$url,"noticias")



ler_noticias_jornalosertanejo <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    headline <- x %>% 
         xml2::xml_find_all('//section[@class="full-news"]//h1') %>% 
         xml2::xml_text()
    
   texto <- x %>% 
      xml2::xml_find_all('//section[@class="full-news"]/p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n") %>% 
     stringr::str_sub(32)
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//section[@class="full-news"]/p[1]') %>% 
     xml2::xml_text() %>% 
     stringr::str_sub(start =1, end=-10)
   

   
   tibble::tibble(headline,texto,data_publicacao)
   
  }),NULL))
    
}


noticias <- ler_noticias_jornalosertanejo(arquivos = arquivos)
