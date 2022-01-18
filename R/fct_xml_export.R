#' ID Factories
#'
#' @description A closure for id generation. 
#'
#' @param id the starting value that is to be incremented.
#'
#' @return A function that increments an ID every time it is called.
#'
#' @noRd
id_generator = function(id = 0){
  function(){
    id <<-id+1
    return(id)
  }
}



#' Title
#' @description x
#'
#' @param elem_name x
#'
#' @return x
#' 
#' @export x
#'
#' @examples x
mapping_to_xml = function(elem_name){
  
}

vegx_add_nodes = function(){
  
}

vegx_update_nodes = function(){
  
}
