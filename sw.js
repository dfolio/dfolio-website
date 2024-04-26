// use a cacheName for cache versioning
var cacheName = 'v1:static';
const addResourcesToCache = async (resources) => {
  const cache = await caches.open(cacheName);
  await cache.addAll(resources);
};

const enableNavigationPreload = async () => {
  if (self.registration.navigationPreload) {
    // Enable navigation preloads!
    await self.registration.navigationPreload.enable();
  }
};

self.addEventListener('install', (event) => {
  event.waitUntil(
    addResourcesToCache([
      './',
      './index.html',
      './favicon.ico',
      './favicon.svg',
      './images/logo-df.webp',
      './images/logo-teach.webp',
      './images/logo-research.webp',
      './images/favicons/favicon-16x16.webp',
      './images/favicons/favicon-32x32.webp',
    ])
  );
});
self.addEventListener('install', () => self.skipWaiting());
self.addEventListener('activate', (event) => {
  self.clients.matchAll({ type: 'window' }).then(windowClients => {
    for (let windowClient of windowClients) {
      // Force open pages to refresh, so that they have a chance to load the
      // fresh navigation response from the local dev server.
      windowClient.navigate(windowClient.url);
    }
  });
  event.waitUntil(enableNavigationPreload());
});

self.addEventListener("fetch", (e) => {
  if (!(e.request.url.startsWith('http')) || (e.request.url.startsWith('http://localhost'))) return;
  e.respondWith(
    (async () => {
      const r = await caches.match(e.request);
      //console.log(`[Service Worker] Fetching resource: ${e.request.url}`);
      if (r) { return r;}
      const response = await fetch(e.request);
      const cache = await caches.open(cacheName);
      //console.log(`[Service Worker] Caching new resource: ${e.request.url}`);
      cache.put(e.request, response.clone());
      return response;
    })()
  );
});
