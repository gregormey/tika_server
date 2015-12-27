app.service("MapEventRows",function(){
  /**
   * maps counted users to chart OBj row format
   * @param  {[type]} rows [description]
   * @return {[type]}      [description]
   */
   var mapRows=function(rows){
    var result=[];
    for(dateKey in rows){
      var data=rows[dateKey];
      result.push({
        "c":[
          {"v":dateKey},
          {"v":data.created.count,"f":data.created.count+"\n"+data.created.mail},
        ]
      })
    }
    return result;
  };

  //--------------------
  // Interface definition
  //--------------------
  return {
    mapRows:mapRows    
  };
})