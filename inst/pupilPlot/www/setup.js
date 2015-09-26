function hexToR(h) {return parseInt((cutHex(h)).substring(0,2),16)}
function hexToG(h) {return parseInt((cutHex(h)).substring(2,4),16)}
function hexToB(h) {return parseInt((cutHex(h)).substring(4,6),16)}
function cutHex(h) {return (h.charAt(0)=="#") ? h.substring(1,7):h}
                  
$(document).ready(function() {
  $("#errorbars").parent().parent().parent().css("float","left");
  $("#errorbars").parent().parent().parent().css("width","110px")
  $("#exportsubjectmeans").parent().parent().parent().css("float","left");
  
  $("#errorbars").parent().parent().parent().css("margin-bottom","1px")
  $("#exportsubjectmeans").parent().parent().parent().css("margin-bottom","1px");
  
  $("#settings").on("click",function() {
    $('#settingsModal').modal('toggle');
  })
  
  $("#settingsModalCloseButton").on("click",function() {
    $(".colorPickers > input").each(function(a,b) {
      console.log(b.id)
      $( b ).trigger( "change" )
    })
    ;
  })
  
  Shiny.addCustomMessageHandler("settingsUpdate",
        function(settings) {
          console.log(settings)
          if (settings && typeof settings == "object") {
            for (var key in settings) {
              
              if (key == "lineColors" && settings[key] instanceof Array) {
                for (var i=0;i < settings[key].length; i++) {
                  if ($("#lineColor"+(i+1)).length == 0) break;
                  $("#lineColor"+(i+1)).val(settings[key][i]);
                  

                  
                  var r = hexToR(settings[key][i]);
                  var g = hexToG(settings[key][i]);
                  var b = hexToB(settings[key][i]);
                  console.log("SETTING","rgb("+r+","+g+","+b+",)")
                  $("#lineColor"+(i+1)).siblings("span").children().first().css("background-color","rgb("+r+","+g+","+b+")");
                  
                }
                continue;
              }
              
              $("#"+key).val(settings[key]);
              console.log("Populating")
            }
          }
  });
})