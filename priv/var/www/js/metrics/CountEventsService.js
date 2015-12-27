app.service('CountEventsService', function(){
  /**
 * addRows for chart Obj with user data
 * @param Date start [description]
 * @param Date end   [description]
 * @param Array user  [description]
 */
  var countEvents=function(start,end,events){
    var rows={};
    var date=new Date(start.getFullYear(), start.getMonth(), start.getDate());
   
    while(date<=end){
      var dateKey=date.getFullYear()+'-'+
            (date.getMonth()<9?("0"+(date.getMonth()+1)):(date.getMonth()+1))+'-'+
            (date.getDate()<10?("0"+date.getDate()):date.getDate());
      rows[dateKey]={
          created:{count:0,mail:""}
      }
      for (i in events) {
        var event=events[i];
        
        var created = new Date(event.created);
        created=new Date(created.getFullYear(), created.getMonth(), created.getDate());

        if(created<=date && created.getTime()){
          rows[dateKey].created.count++;
          if(created.getTime()==date.getTime()){
            rows[dateKey].created.mail+=event.creator.mail+"\n"
          }
        }

      }

      date.setDate(date.getDate()+1);
    };
    return rows;
  };

  //--------------------
  // Interface definition
  //--------------------
  return {
    countEvents:countEvents 
  };
})