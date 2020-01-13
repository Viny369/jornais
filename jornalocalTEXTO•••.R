baixar_paginas_jornalocal <- function(diretorio = ".",caderno = "brasil", paginas = 10){
  
  url <- paste0("http://jornalocal.com.br/site/category/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}

p <- baixar_paginas_jornalocal(diretorio = "pagina")


ler_paginas_jornalocal <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//h2[@class="post-box-title"]/a') %>% 
        xml2::xml_attr("href")
  
  headline <- x %>% 
    xml2::xml_find_all('//h2[@class="post-box-title"]/a') %>% 
    xml2::xml_text()

  descricao <- x %>% 
    xml2::xml_find_all('//div[@class="entry"]/p') %>% 
    xml2::xml_text()
  
  autor <- x %>% 
    xml2::xml_find_all('//span[@class="post-meta-author"]//a') %>% 
    xml2::xml_text()
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//p[@class="post-meta"]//span[@class="tie-date"]') %>% 
    xml2::xml_text()
  
  tibble::tibble(headline,descricao,autor,data_publicacao,url)
    
  }),NULL))
  
}

jlc <- ler_paginas_jornalocal(arquivos = arquivos)  


baixar_noticias_jornalocal <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_jornalocal_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}



baixar_noticias_jornalocal(jlc$url,"noticias")



ler_noticias_jornalocal <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    headline <- x %>% 
         xml2::xml_find_all('//span[@itemprop="name"]') %>% 
         xml2::xml_text()
    
    descricao <- x %>% 
      xml2::xml_find_all('//figcaption') %>% 
      xml2::xml_text() %>% 
      stringr::str_c(collapse = "\n")
    
   texto <- x %>% 
      xml2::xml_find_all('//div[@class="entry"]//p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n")
   
   autor <- x %>% 
     xml2::xml_find_all('//span[@class="post-meta-author"]//a') %>% 
     xml2::xml_text()
   
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//div[@class="post-inner"]//p[@class="post-meta"]/span[@class="tie-date"]') %>% 
     xml2::xml_text()
   

   
   tibble::tibble(headline,descricao,texto,autor,data_publicacao)
   
  }),NULL))
    
}


noticias <- ler_noticias_jornalocal(arquivos = arquivos)
