$(function(){ 
  $(document).on('shiny:connected', function(event) {
    $('.shiny-tree').on('hover_node.jstree', function (e, data) {
      let tabSelected = $(this).closest('.tab-pane').attr('data-value');
      tabSelected = tabSelected.charAt(0).toLowerCase() + tabSelected.slice(1);
      let elemNames = [data.node.text];
      let parentIds = data.node.parents;
      
     for (const id of parentIds){
        if(id === '#'){break};
        parent = $(this).jstree(true).get_node(id);
        if(parent.text === 'choice'){continue}
        elemNames.push(parent.text);
      }
      elemNames.push(tabSelected)
      Shiny.setInputValue('node_hovered_parents', elemNames);
    });
  });
});

$(function(){ 
  $(document).on('shiny:connected', function(event) {
    $('.shiny-tree').on('dehover_node.jstree', function (e, data) {
      Shiny.setInputValue('node_hovered_parents', [""]);
    });
  });
});