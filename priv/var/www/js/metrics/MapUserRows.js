app.service("MapUserRows",function(){
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
          {"v":data.invited.count,"f":data.invited.count+"\n"+data.invited.mail},
          {"v":data.registered.count,"f":data.registered.count+"\n"+data.registered.mail}
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