importScripts('https://storage.googleapis.com/workbox-cdn/releases/6.4.1/workbox-sw.js');
// Include offline.html in the manifest
workbox.precaching.precacheAndRoute(self.__WB_MANIFEST);

workbox.routing.registerRoute(
  new RegExp('\.js$'),
  new workbox.strategies.CacheFirst({ cacheName: 'scripts-cache',})
);
workbox.routing.registerRoute(
  new RegExp('\.css$'),
  new workbox.strategies.CacheFirst({ cacheName: 'styles', })
);

workbox.recipes.googleFontsCache();
workbox.recipes.pageCache();
workbox.recipes.staticResourceCache();
workbox.recipes.imageCache();
