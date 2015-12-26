
angular.module('verificationApp', ['ngRoute', 'googlechart'])

.controller('VerificationCtrl', function($scope,$routeParams,$http){
	$scope.codeFound=false;
	var code=$routeParams.code;
	$http.get('/verify/'+code).then(function successCallback(response) {
      if(response.status==200)
    	{
    		$scope.codeFound = true;
    	}
    });
})
.controller('MetricsCtrl', function($scope,$routeParams,$http){
   
  var get

/**
 * addRows for chart Obj with user data
 * @param Date start [description]
 * @param Date end   [description]
 * @param Array user  [description]
 */
  var addRows=function(start,end,users){
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

   $scope.chartObject = {
    "type": "AreaChart",
    "displayed": false,
    "data": {
      "cols": [
        {
          "id": "day",
          "label": "Day",
          "type": "string",
          "p": {}
        },
        {
          "id": "created-id",
          "label": "Installs",
          "type": "number",
          "p": {}
        },
        {
          "id": "invited-id",
          "label": "Invites",
          "type": "number",
          "p": {}
        },
        {
          "id": "registered-id",
          "label": "Registrations",
          "type": "number",
          "p": {}
        }
      ],
      "rows": [],
    "options": {
      "title": "User per day",
      "isStacked": "true",
      "fill": 20,
      "displayExactValues": true,
      "vAxis": {
        "title": "Users",
        "gridlines": {
          "count": 10
        }
      },
      "hAxis": {
        "title": "Date"
      }
    },
    "formatters": {}
  }
};

  $http.get('/user').then(function successCallback(response) {
    if(response.status==200)
    {
      var start=new Date();
      start.setDate(start.getDate()-30);
      var end= new Date();
      var rows=addRows(start,end,response.data);
      $scope.chartObject.data.rows=mapRows(rows);
    }
  });
  $http.get('/event').then(function successCallback(response) {
    if(response.status==200)
    {
     
    }
  });
})
.config(function($routeProvider) {
  $routeProvider
  	.when('/', { templateUrl: 'templates/welcome.html' })
    .when('/metrics', { templateUrl: 'templates/metrics.html', 
                        controller: 'MetricsCtrl' })
    .when('/verify/:code', { templateUrl: 'templates/verification.html',
    						 controller: 'VerificationCtrl'
    					 })
    .otherwise({ redirectTo: '/' });
})
;