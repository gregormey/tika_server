
var app= angular.module('verificationApp', ['ngRoute', 'googlechart'])
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