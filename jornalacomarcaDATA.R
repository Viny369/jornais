baixar_paginas_jornalacomarca <- function(diretorio = ".",caderno = "noticias", paginas = 10){
  
  url <- paste0("https://www.jornalacomarca.com.br/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}



ler_paginas_jornalacomarca <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//span[@class="bt_bb_headline_content"]//a') %>% 
        xml2::xml_attr("href")
  
  headline <- x %>% 
    xml2::xml_find_all('//span[@class="bt_bb_headline_content"]//a') %>% 
    xml2::xml_text()
  
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//span[@class="btArticleDate"]') %>% 
    xml2::xml_text() 
   
  
  autor <- x %>% 
    xml2::xml_find_all('//a[@class="btArticleAuthorURL"]') %>% 
    xml2::xml_text() 
  
  descricao <- x %>% 
    xml2::xml_find_all('//div[@class="btArticleContent"]/p') %>% 
    xml2::xml_text() 
  
  tibble::tibble(headline,data_publicacao,descricao,autor,url)
    
  }),NULL))
  
}

jac <- ler_paginas_jornalacomarca  (arquivos = arquivos) 


baixar_noticias_jornalacomarca <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_jornalacomarca_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}

jac<-ler_paginas_jornalacomarca()

baixar_noticias_jornalacomarca(jac$url,"noticias")



ler_noticias_jornalacomarca <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,full.names = TRUE) 
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    
    
    headline <- x %>% 
         xml2::xml_find_all('//h1//span[@class="bt_bb_headline_content"]') %>% 
         xml2::xml_text()
         
    
   corpo <- x %>% 
      xml2::xml_find_all('//div[@class="bt_bb_wrapper"]//p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n")
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//span[@class="btArticleDate"]') %>% 
     xml2::xml_text() 
     
   
   
   autor <- x %>% 
     xml2::xml_find_first('//a[@class="btArticleAuthorURL"]') %>% 
     xml2::xml_text()
   
   tibble::tibble(headline,corpo,data_publicacao,autor)
   
  }),NULL))
    
}




noticias <- ler_noticias_jornalacomarca(arquivos = arquivos)
