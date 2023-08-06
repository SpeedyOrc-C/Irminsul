import type {RelationGraph} from "../../model/RelationGraph";
import type {ApiResponse} from "$lib/util/Api";
import {loop_guard} from "svelte/internal";

export class RelationGraphLoader {

    cache: Map<string, Map<string, RelationGraph>> = new Map();

    load(id: string, lang: string): Promise<RelationGraph> {
        const cachedRelationGraph = this.loadFromCache(id, lang);
        if (cachedRelationGraph == null) {
            console.info('Not found in cache, downloading...');
            return this.downloadFromServer(id, lang);
        }

        console.info('Found in cache.')
        return Promise.resolve(cachedRelationGraph);
    }

    downloadFromServer(id: string, lang: string): Promise<RelationGraph> {
        return new Promise((resolve, reject) =>
            fetch(`/api/relation-graph/${id}/${lang}`)
                .then(response => {
                    if (response.status < 200 || response.status > 299) {
                        reject(response.status);
                        return;
                    }
                    return response.json()
                        .then((json: ApiResponse<RelationGraph>) => {
                            if (json.status === 'OK') {
                                this.saveToCache(id, lang, json.body);
                                resolve(json.body);
                            } else {
                                reject(json.status);
                            }
                        })
                        .catch(() => reject('Failed to decode JSON.'));
                })
                .catch(() => reject('Failed to download relation graph.'))
        );
    }

    loadFromCache(id: string, lang: string): RelationGraph | null {
        const langMap = this.cache.get(id);
        if (langMap === undefined) {
            this.cache.set(id, new Map());
            return null;
        }

        const relationGraph = langMap.get(lang);
        if (relationGraph === undefined) {
            return null;
        }

        return relationGraph;
    }

    saveToCache(id: string, lang: string, relationGraph: RelationGraph) {
        const langMap = this.cache.get(id);
        if (langMap === undefined) {
            this.cache.set(id, new Map());
        } else {
            langMap.set(lang, relationGraph);
        }
    }

}
