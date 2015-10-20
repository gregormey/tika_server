angular.module('verificationApp', ['ngRoute'])

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
.config(function($routeProvider) {
  $routeProvider
  	.when('/', { templateUrl: 'templates/welcome.html' })
    .when('/verify/:code', { templateUrl: 'templates/verification.html',
    						 controller: 'VerificationCtrl'
    					 })
    .otherwise({ redirectTo: '/' });
})
;