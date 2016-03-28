// Ionic Starter App

// angular.module is a global place for creating, registering and retrieving Angular modules
// 'starter' is the name of this angular module example (also set in a <body> attribute in index.html)
// the 2nd parameter is an array of 'requires'
angular.module('notificaoapp', ['ionic'])


.service('$servicoNotificacao', function($http) {

  this.setToken = function(servidor, token){
    var dados = {
                  token : ''
                };

    dados.token = token;

    return $http.post(servidor + '/token', dados);
  };

  this.notificar = function(servidor, mensagem){
    var dados = {
                  mensagem: ''
                };

    dados.mensagem = mensagem;

    return $http.post(servidor + '/notificacao', dados);
  };

  return this;

})

.run(function($ionicPlatform) {
  $ionicPlatform.ready(function() {
    if(window.cordova && window.cordova.plugins.Keyboard) {
      // Hide the accessory bar by default (remove this to show the accessory bar above the keyboard
      // for form inputs)
      cordova.plugins.Keyboard.hideKeyboardAccessoryBar(true);

      // Don't remove this line unless you know what you are doing. It stops the viewport
      // from snapping when text inputs are focused. Ionic handles this internally for
      // a much nicer keyboard experience.
      cordova.plugins.Keyboard.disableScroll(true);
    }
    if(window.StatusBar) {
      StatusBar.styleDefault();
    }

  });
})

.controller('NotificacaoCtrl', function ($scope, $servicoNotificacao) {

  $scope.servidor = 'http://192.168.25.5:9090';

  var push = new Ionic.Push({
    "onNotification": function(notification) {
      console.log('Notificacao', JSON.stringify(notification));
      window.plugins.toast.showShortBottom(notification.text);
    },
    "onRegister": function(data) {
      console.log('Registrado', data.token);
    },
    "pluginConfig": {
      "ios": {
        "badge": true,
        "sound": true
      },
      "android": {
        "iconColor": "#343434",
        "sound": "default",
        "tag": "bar"
      }
    }
  });

  $scope.geraToken = function () {

    push.register(function(pushToken) {

      $scope.token = pushToken.token;
      console.log('Callback register', $scope.token);

      push.saveToken(pushToken.token);

      $scope.$apply();
    });
  };

  $scope.setToken = function(){
    $servicoNotificacao.setToken($scope.servidor, $scope.token).then(function (response) {
      console.log('setToken ok', JSON.stringify(response));
    }).catch(function (erro) {
      console.log('setToken erro', JSON.stringify(erro));
    });
  };

  $scope.enviar = function(){
    //$scope.dados.device = $cordovaDevice.getUUID();

    $servicoNotificacao.notificar($scope.servidor, $scope.mensagem).then(function (response) {
      console.log('notificar ok', JSON.stringify(response));
    }).catch(function (erro) {
      console.log('notificar erro', JSON.stringify(erro));
    });
  }
});
