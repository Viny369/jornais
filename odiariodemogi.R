baixar_paginas_odiariodemogi <- function(diretorio = ".",caderno = "editorial", paginas = 10){
  
  url <- paste0("http://www.odiariodemogi.net.br/categoria/opiniao/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}

b <- baixar_paginas_odiariodemogi(diretorio = "pagina")

ler_paginas_odiariodemogi <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//h2[@class="entry-title h3"]/a') %>% 
        xml2::xml_attr("href")
  
  headline <- x %>% 
    xml2::xml_find_all('//h2[@class="entry-title h3"]/a') %>% 
    xml2::xml_text()
  
  descricao <- x %>% 
    xml2::xml_find_all('//div[@class="entry-content"]/p') %>% 
    xml2::xml_text()
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//div[@class="entry-meta"]/div[@class="meta-item herald-date"]/span[@class="updated"]') %>% 
    xml2::xml_text()
  
  tibble::tibble(headline,descricao,data_publicacao,url)
    
  }),NULL))
  
}

mogi <- ler_paginas_odiariodemogi(arquivos = arquivos)


baixar_noticias_odiariodemogi <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_odiariodemogi_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}

mogi<-ler_paginas_odiariodemogi()

baixar_noticias_odiariodemogi(mogi$url,"noticias")



ler_noticias_odiariodemogi <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    headline <- x %>% 
         xml2::xml_find_all('//h1[@class="entry-title h1"]') %>% 
         xml2::xml_text()
    
   texto <- x %>% 
      xml2::xml_find_all('//div[@class="entry-content herald-entry-content"]//p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n")
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//div[@class="entry-meta entry-meta-single"]/div[@class="meta-item herald-date"]/span[@class="updated"]') %>% 
     xml2::xml_text()
   
   autor <- x %>% 
     xml2::xml_find_first('//a[@class="herald-author-name"]') %>% 
     xml2::xml_text()
   
   tibble::tibble(headline,texto,data_publicacao,autor)
   
  }),NULL))
    
}


noticias <- ler_noticias_odiariodemogi(arquivos = arquivos)
