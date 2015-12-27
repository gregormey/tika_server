app.controller('MetricsCtrl', function($scope,$routeParams,$http,UserChartObjectFactory,EventChartObjectFactory){
   
  $http.get('/user').then(function successCallback(response) {
    if(response.status==200)
    {
      $scope.userChartObject=UserChartObjectFactory.getChartObjectFromUsers(response.data);
    }
  });

  $http.get('/event').then(function successCallback(response) {
    if(response.status==200)
    {
       $scope.eventChartObject=EventChartObjectFactory.getChartObjectFromEvents(response.data);
    }
  });
})