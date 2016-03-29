// Ionic Starter App

// angular.module is a global place for creating, registering and retrieving Angular modules
// 'starter' is the name of this angular module example (also set in a <body> attribute in index.html)
// the 2nd parameter is an array of 'requires'
angular.module('notificaoapp', ['ionic'])


.service('$servicoNotificacao', function($http) {

  this.setToken = function(servidor, token){
    var dados = {
                  token : '',
                  platform: device.platform,
                  device: device.uuid
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
  $scope.token = undefined;

  var push = undefined;

  $scope.registrar = function () {

    push = PushNotification.init({
      android: {
        senderID: "381509618530"
      },
      ios: {
        alert: "true",
        badge: true,
        sound: 'false'
      },
      windows: {}
    });

    push.on('registration', function(data) {
      console.log("registration event",JSON.stringify(data));
      $scope.token = data.registrationId;
      $scope.$apply();
    });

    push.on('notification', function(data) {

      console.log("notification event",JSON.stringify(data));
      window.plugins.toast.showShortBottom(data.message);

      push.finish(function () {
        console.log('finish successfully called');
      });
    });

    push.on('error', function(e) {
      console.log("push error",JSON.stringify(e));
      window.plugins.toast.showShortBottom(JSON.stringify(e));
    });

  };

  $scope.desRegistrar = function(){
    if($scope.token !== undefined) {
      push.unregister(function () {
        window.plugins.toast.showShortBottom("Ok");
        $scope.token = undefined;
        $scope.$apply();
      }, function (erro) {
        window.plugins.toast.showShortBottom("Erro: "+JSON.stringify(erro));
      });
    }
  };

  $scope.setToken = function(){
    $servicoNotificacao.setToken($scope.servidor, $scope.token).then(function (response) {
      window.plugins.toast.showShortBottom("OK");
    }).catch(function (erro) {
      window.plugins.toast.showShortBottom("Erro: "+JSON.stringify(erro));
    });
  };

  $scope.enviar = function(){
    $servicoNotificacao.notificar($scope.servidor, $scope.mensagem).then(function (response) {
      window.plugins.toast.showShortBottom("Enviado");
    }).catch(function (erro) {
      window.plugins.toast.showShortBottom("Erro: "+JSON.stringify(erro));
    });
  }
});
