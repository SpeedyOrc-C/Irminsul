import type {RelationGraph} from "./RelationGraph";
import type {ApiResponse} from "$lib/util/Api";

export class RelationGraphLoader {

    cache: Map<string, Map<string, RelationGraph>> = new Map();

    load(id: string, lang: string, whoAmI: "aether" | "lumine" = "aether"): Promise<RelationGraph> {
        const cachedRelationGraph = this.loadFromCache(id, lang);
        if (cachedRelationGraph == null) {
            console.info('Not found in cache, downloading...');
            return this.downloadFromServer(id, lang, whoAmI);
        }

        console.info('Found in cache.')
        return Promise.resolve(cachedRelationGraph);
    }

    async downloadFromServer(id: string, lang: string, whoAmI: "aether" | "lumine" = "aether"): Promise<RelationGraph> {
        let response: Response;
        try {
            response = await fetch(`/api/relation-graph/${id}/${lang}/${whoAmI}`);
        } catch (e) {
            throw "Failed to download relation graph.";
        }

        if (response.status < 200 || response.status > 299) {
            throw response.status;
        }

        let json: ApiResponse<RelationGraph>;
        try {
            json = await response.json();
        } catch (e) {
            throw "Failed to decode JSON.";
        }

        if (json.status !== 'OK') {
            throw json.status;
        }

        this.saveToCache(id, lang, json.body);
        return json.body;
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

    clearCache() {
        this.cache.forEach(langMap => langMap.clear());
        this.cache.clear();
    }
}
