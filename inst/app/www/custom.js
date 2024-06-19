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


$(document).ready(function() {
  $('.collapsible').on('click', function() {
    $(this).nextUntil('div.collapsible').slideToggle(100);
  });
});


/*
$(document).ready(function() {
  // Observe changes in the Shiny reactive value
  Shiny.addCustomMessageHandler('fileFocusChange', function(message) {
    var fileFocus = message.fileFocus;
    var focusButton = document.getElementById(message.buttonID);
    
    // Remove the 'btn-focus' class from all buttons
    $('.btn-focus').removeClass('btn-focus');
    
    // Add the 'btn-focus' class to the button corresponding to the current value of file_focus
    if (focusButton) {
      focusButton.classList.add('btn-focus');
    }
  });
});

$(document).ready(function() {
  Shiny.addCustomMessageHandler('deleteFileButton', function(message) {
    var button = document.getElementById(message);
    if (button) {
      button.remove();
    }
  });
});
*/
