$(function(){ 
  $(document).on('shiny:connected', function(event) {
    $('.shiny-tree').on('hover_node.jstree', function (e, data) {
      let treeHovered = $(this).attr('id');
      let nodeId = data.node.a_attr.id;
      let nodeLineage = [data.node.text];
      let parentIds = data.node.parents;

      for (const id of parentIds){
        if(id === '#'){break};
        parent = $(this).jstree(true).get_node(id);
        if(parent.text === 'choice'){continue};
        nodeLineage.push(parent.text);
      }
      
      let nodeHoveredInfo = {
        tree: treeHovered,
        nodeId: nodeId,
        nodeLineage: nodeLineage,
      };
      
      Shiny.setInputValue('nodeHoveredInfo', nodeHoveredInfo);
    });
  });
});


$(function(){ 
  $(document).on('shiny:connected', function(event) {
    $('.shiny-tree').on('dehover_node.jstree', function (e, data) {
      Shiny.setInputValue('nodeHoveredInfo', {});
    });
  });
});
