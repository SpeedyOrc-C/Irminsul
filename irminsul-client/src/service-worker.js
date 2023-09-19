import {build, files, version} from "$service-worker";


const CACHE_NAME = `cache-${version}123123123`;

const ASSETS = [
    ...build,
    ...files,
];

self.addEventListener("install", e => {
    async function addFilesToCache() {
        console.log("Caching new files...")
        const cache = await caches.open(CACHE_NAME);
        await cache.addAll(build);
        console.log("Build cached.");
        await cache.addAll(files);
        console.log("Files cached.");

        const clusters = await (await fetch("/api/all/cluster")).json();
        for (const cluster of clusters.body) {
            for (const language of ["zh-CN", "en-US"]) {
                for (const whoAmI of ["aether", "lumine"]) {
                    const path = `/api/relation-graph/${cluster}/${language}/${whoAmI}`;
                    ASSETS.push(path);
                    await cache.add(path);
                }
            }
        }
        console.log("Clusters cached.");
    }
    e.waitUntil(addFilesToCache());
});

self.addEventListener("activate", e => {
    async function deleteOldCaches() {
        console.log("Deleting old caches...");
        for (const key of await caches.keys()) {
            if (key !== CACHE_NAME) {
                await caches.delete(key);
            }
        }
    }
    e.waitUntil(deleteOldCaches());
});

self.addEventListener("fetch", e => {
   async function respond() {
       const url = new URL(e.request.url);
       const cache = await caches.open(CACHE_NAME);

       if (ASSETS.includes(url.pathname)) {
           return cache.match(e.request);
       }

       try {
           const response = await fetch(e.request);
           if (response.status === 200) {
               await cache.put(e.request, response.clone());
           }
           return response;
       } catch {
           return cache.match(e.request);
       }
   }

   e.respondWith(respond());
});