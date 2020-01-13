baixar_paginas_portalr3 <- function(diretorio = ".",caderno = "portalr3_soberania", paginas = 10){
  
  url <- paste0("https://www.portalr3.com.br/editoria/eventosespeciais/",caderno,"/page/")

  purrr::walk(1:paginas,purrr::possibly(purrrogress::with_progress(~{
    
    arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_pagina_",.x,".html")
    
    httr::GET(url=paste0(url,.x),httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
    
    
  }),NULL))  
}

p <- baixar_paginas_portalr3(diretorio = "pagina")


ler_paginas_portalr3 <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio,pattern= "html",full.names=TRUE)
    
    
  }
  
  purrr::map_dfr(arquivos,purrr::possibly(purrrogress::with_progress(~{
    
  x <- xml2::read_html(.x)  
    
  url <- x %>% 
        xml2::xml_find_all('//section[@class="section-category"]//h1[@class="item-title"]/a') %>% 
        xml2::xml_attr("href")
  
  headline <- x %>% 
    xml2::xml_find_all('//section[@class="section-category"]//h1[@class="item-title"]/a') %>% 
    xml2::xml_text()

  descricao <- x %>% 
    xml2::xml_find_all('//div[@class="item-excerpt"]/p') %>% 
    xml2::xml_text()
  
  data_publicacao <- x %>% 
    xml2::xml_find_all('//section[@class="section-category"]//div[@class="item-content"]/div[@class="item-meta"]/time[@class="item-time"]') %>% 
    xml2::xml_text()
  
  tibble::tibble(headline,descricao,data_publicacao,url)
    
  }),NULL))
  
}

pr3 <- ler_paginas_portalr3(arquivos = arquivos)  


baixar_noticias_portalr3 <- function(url = NULL, diretorio = "."){
  
  if (is.null(url)){
    
    stop("Você tem de informar a url")
    
  }
  
  purrr::walk(url,purrr::possibly(purrrogress::with_progress(~{
    
   artigo <- stringr::str_extract(.x,"[\\w-]+(?=/$)") %>% 
     stringr::str_replace_all("-","_")
    
   arquivo <- paste0(stringr::str_replace_all(Sys.time(),"\\D","_"),"_portalr3_",artigo,".html")
  
   httr::GET(.x,httr::write_disk(paste0(diretorio,"/",arquivo),overwrite = TRUE))
     
     
     
   }),NULL))
   

}



baixar_noticias_portalr3(pr3$url,"noticias")



ler_noticias_portalr3 <- function(arquivos = NULL, diretorio = "."){
  
  if (is.null(arquivos)){
    
    arquivos <- list.files(diretorio, pattern="html",full.names = TRUE)
  }

  purrr::map_dfr(arquivos, purrr::possibly(purrrogress::with_progress(~{
    
    x <- xml2::read_html(.x)
    
    headline <- x %>% 
         xml2::xml_find_all('//h2[@class="entry-title"]') %>% 
         xml2::xml_text()
    
   texto <- x %>% 
      xml2::xml_find_all('//div[@class="entry-content"]//p') %>% 
     xml2::xml_text() %>% 
     stringr::str_c(collapse = "\n")
   
   autor <- x %>% 
     xml2::xml_find_all('//a[@rel="author"]') %>% 
     xml2::xml_text()
   
   data_publicacao <- x %>% 
     xml2::xml_find_all('//div[@class="entry-meta"]/time[@class="entry-time"]') %>% 
     xml2::xml_text()
   

   
   tibble::tibble(headline,texto,autor,data_publicacao)
   
  }),NULL))
    
}


noticias <- ler_noticias_portalr3(arquivos = arquivos)
