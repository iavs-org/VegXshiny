$(document).ready(function() {
  Shiny.addCustomMessageHandler('node_tooltip', function(nodeInfo) {
    let titleText = '';
    let attrs = nodeInfo.nodeAttributes;

    delete attrs.name;
    if ('annotation' in attrs) {
      titleText += attrs.annotation + '\n\n';
      delete attrs.annotation;
    };

    for (const [key, value] of Object.entries(attrs)) {
      titleText += key + ': ' + value + '\n';
    };

    $('#' + nodeInfo.tree).find('#' + nodeInfo.nodeId).prop('title', titleText);
  });
});
