<script lang="ts">
    import { page } from "$app/stores";
    import { onMount } from "svelte";
    import type { RelationGraph as TClusterGraph } from "../../model/RelationGraph";
    import type { ApiResponse } from "../../model/Api";
    import ClusterGraph from "./RelationGraph.svelte";

    let clusterGraphResponse: ApiResponse<TClusterGraph> | null;
    let clusterGraph: TClusterGraph | undefined;
    let id: string | null;
    let lang: string | null;

    onMount(() => {
        id = $page.url.searchParams.get("id");
        lang = $page.url.searchParams.get("lang");
        if (id == null || lang == null) return;

        fetch(`/api/relation-graph?id=${id}&lang=${lang}`, {
            method: "GET",
        }).then((r) =>
            r.json().then((json) => {
                clusterGraphResponse = json;
                clusterGraph = clusterGraphResponse?.body;
            })
        );
    });
</script>

{#if id == null || lang == null}
    <p>Missing parameters "id" or "lang"</p>
{:else if clusterGraphResponse === undefined}
    <p>Loading...</p>
{:else if clusterGraphResponse == null}
    <p>Cannot parse fetched result</p>
{:else if clusterGraphResponse.status === "UnsupportedLanguage"}
    <p>Language "{lang}" is not supported</p>
{:else if clusterGraphResponse.status === "NotImplementedCluster"}
    <p>Cluster "{id}" is not yet implemented</p>
{:else if clusterGraphResponse.status === "OK" && clusterGraph !== undefined}
    <ClusterGraph relationGraph={clusterGraph} />
{:else}
    <p>UNKNOWN ERROR</p>
{/if}

<style>
    @font-face {
        font-family: HYWenHei-65W;
        src: url("/font/HYWenHei-65W.ttf");
    }

    @font-face {
        font-family: HYWenHei-85W;
        src: url("/font/HYWenHei-85W.ttf");
    }

    :global(.font-hywh-65w) {
        font-family: HYWenHei-65W, sans-serif;
    }

    :global(.font-hywh-85w) {
        font-family: HYWenHei-85W, sans-serif;
    }
</style>
