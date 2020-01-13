baixar_paginas_abcdoabc <- function(diretorio = ".",caderno = "politica", paginas = 10){
  
  url <- paste0("https://www.abcdoabc.com.br/todas-noticias/",caderno,"/8/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}


#diretorio <- "pagina" e "noticias"

#arquivos <- list-files

#.x <- arquivos[NUMERO]




ler_paginas_abcdoabc <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//div[@class="col-lg-6 col-md-6 col-sm-8 col-xs-8"]/a') %>% 
        xml2::xml_attr("href") %>% 
  paste0("https://www.abcdoabc.com.br",.)
  
  headline <- x %>% 
    xml2::xml_find_all('//div[@class="col-lg-6 col-md-6 col-sm-8 col-xs-8"]//h3') %>% 
    xml2::xml_text()
  
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//div[@class="col-lg-6 col-md-6 col-sm-8 col-xs-8"]//small') %>% 
    xml2::xml_text() 
   
  
  
  tibble::tibble(headline,data_publicacao,url)
    
  }),NULL))
  
}


abc <- ler_paginas_abcdoabc  (arquivos = arquivos) 


baixar_noticias_abcdoabc <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"\\d.+") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_abcdoabc_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}

abc<-ler_paginas_abcdoabc()

baixar_noticias_abcdoabc(abc$url,"noticias")



ler_noticias_abcdoabc <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,full.names = TRUE) 
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    

    
    headline <- x %>% 
         xml2::xml_find_all('//h1[@class="titlenoticia"]') %>% 
         xml2::xml_text()
         
    descricao <- x %>% 
      xml2::xml_find_all('//h2[@class="subtitlenoticia"]') %>% 
      xml2::xml_text()
    
   corpo <- x %>% 
      xml2::xml_find_all('//p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n")
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//div[@class="author"]') %>% 
     xml2::xml_text() %>% 
     stringr::str_extract("\\d.{9}") %>% 
     lubridate::dmy()
   
   autor <- x %>% 
     xml2::xml_find_first('//div[@class="author"]//span[2]') %>% 
     xml2::xml_text() %>% 
     stringr::str_sub(10)
   
   fonte <- x %>% 
     xml2::xml_find_first('//div[@class="author"]/span[3]') %>% 
     xml2::xml_text() %>% 
     stringr::str_sub(10)
   
   
   tibble::tibble(headline,descricao,corpo,data_publicacao,autor,fonte)
   
  }),NULL))
    
}




noticias <- ler_noticias_abcdoabc(arquivos = arquivos)

