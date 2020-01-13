baixar_paginas_oregionalonline <- function(diretorio = ".",caderno = "nacional", paginas = 10){
  
  url <- paste0("https://www.oregionalonline.com.br/categoria/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}

p <- baixar_paginas_oregionalonline(diretorio = "pagina")


ler_paginas_oregionalonline <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//h2/a') %>% 
        xml2::xml_attr("href")
  
  headline <- x %>% 
    xml2::xml_find_all('//h2/a') %>% 
    xml2::xml_text()

  descricao <- x %>% 
    xml2::xml_find_all('//div[@class="recentpost-image-0"]/p') %>% 
    xml2::xml_text()
  
  descricao2 <-  x %>% 
    xml2::xml_find_all('//div[@class="post-content"]/p') %>% 
    xml2::xml_text()
  
  descricao <- c(descricao2,descricao)
  
  
  tibble::tibble(headline,descricao,url)
    
  }),NULL))
  
}

oro <- ler_paginas_oregionalonline(arquivos = arquivos)  


baixar_noticias_oregionalonline <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_oregionalonline_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}



baixar_noticias_oregionalonline(oro$url,"noticias")



ler_noticias_oregionalonline <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    headline <- x %>% 
         xml2::xml_find_all('//h1[@class="entry-title single-entry-title"]') %>% 
         xml2::xml_text()
    
   texto <- x %>% 
      xml2::xml_find_all('//div[@class="single-right-big"]//div[@class="entry-content"]') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n") %>% 
     stringr::str_sub(1,-41)
   
   

   
   tibble::tibble(headline,texto)
   
  }),NULL))
    
}


noticias <- ler_noticias_oregionalonline(arquivos = arquivos)
