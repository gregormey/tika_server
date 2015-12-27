app.controller('MetricsCtrl', function($scope,$routeParams,$http,ChartObjectFactory){
   
  $http.get('/user').then(function successCallback(response) {
    if(response.status==200)
    {
      $scope.chartObject=ChartObjectFactory.getChartObjectFromUsers(response.data);
    }
  });

  /*$http.get('/event').then(function successCallback(response) {
    if(response.status==200)
    {
     
    }
  });*/
})