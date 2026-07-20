// Service Worker for Web Push notifications (opponent moves).

self.addEventListener('push', function(event) {
  if (!event.data) return;

  var data;
  try {
    data = event.data.json();
  } catch (e) {
    return;
  }

  var title = data.title || "It's your turn!";
  var body = data.body || '';
  var gameId = data.game_id || '';

  event.waitUntil(
    self.clients.matchAll({ type: 'window', includeUncontrolled: true }).then(function(clients) {
      // If the game tab is already focused, skip the notification
      for (var i = 0; i < clients.length; i++) {
        if (clients[i].url.indexOf('/play/' + gameId) !== -1 && clients[i].focused) {
          return;
        }
      }
      return self.registration.showNotification(title, {
        body: body,
        tag: 'game-move-' + gameId,
        renotify: true,
        data: { game_id: gameId }
      });
    })
  );
});

self.addEventListener('notificationclick', function(event) {
  event.notification.close();

  var gameId = event.notification.data && event.notification.data.game_id;
  if (!gameId) return;

  var gameUrl = '/play/' + gameId;

  event.waitUntil(
    self.clients.matchAll({ type: 'window', includeUncontrolled: true }).then(function(clients) {
      // Focus an existing tab with this game
      for (var i = 0; i < clients.length; i++) {
        if (clients[i].url.indexOf(gameUrl) !== -1) {
          return clients[i].focus();
        }
      }
      // Otherwise open a new tab
      return self.clients.openWindow(gameUrl);
    })
  );
});
