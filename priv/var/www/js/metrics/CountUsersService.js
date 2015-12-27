app.service('CountUsersService', function(){
  /**
 * addRows for chart Obj with user data
 * @param Date start [description]
 * @param Date end   [description]
 * @param Array user  [description]
 */
  var countUsers=function(start,end,users){
    var rows={};
    var date=new Date(start.getFullYear(), start.getMonth(), start.getDate());
   
    while(date<=end){
      var dateKey=date.getFullYear()+'-'+
            (date.getMonth()<9?("0"+(date.getMonth()+1)):(date.getMonth()+1))+'-'+
            (date.getDate()<10?("0"+date.getDate()):date.getDate());
      rows[dateKey]={
          created:{count:0,mail:""},
          invited:{count:0,mail:""},
          registered:{count:0,mail:""}
      }
      for (i in users) {
        var user=users[i];
        
        var created = new Date(user.created);
        created=new Date(created.getFullYear(), created.getMonth(), created.getDate());

        var invited = new Date(user.invited);
        invited=new Date(invited.getFullYear(), invited.getMonth(), invited.getDate());

        var registered= new Date(user.registered);
        registered=new Date(registered.getFullYear(), registered.getMonth(), registered.getDate());        
       

        if(registered<=date && registered.getTime()){
          rows[dateKey].registered.count++;
          if(registered.getTime()==date.getTime()){
            rows[dateKey].registered.mail+=user.mail+"\n"
          }
        }else if(invited<=date && invited.getTime()){
          rows[dateKey].invited.count++;
          if(invited.getTime()==date.getTime()){
            rows[dateKey].invited.mail+=user.mail+"\n"
          }
        }else if(created<=date && created.getTime()){
          rows[dateKey].created.count++;
          if(created.getTime()==date.getTime()){
            rows[dateKey].created.mail+=user.mail+"\n"
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
    countUsers:countUsers    
  };
})