self.addEventListener('install', function(e) {
    e.waitUntil(
        caches.open('EmergenCity').then(function(cache) {
            return cache.addAll([
                '',
                'static/logo-small.svg'
            ]);
        })
    );
});

self.addEventListener('fetch', function(e) {
    console.log(e.request.url);
    e.respondWith(
        caches.match(e.request).then(function(response) {
            console.log("found " + e.request.url);
            return response || fetch(e.request);
        })
    );
});
