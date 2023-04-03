<script lang="ts">
    import {page} from "$app/stores";
    import {onMount} from "svelte";
    import type {Layout} from "../model/Layout";
    import Showcase from "../model/Showcase.svelte";
    import type {ApiResponse} from "../model/Api";

    let layoutResponse: ApiResponse<Layout> | null;
    let layout: Layout | undefined;
    let id: string | null;
    let lang: string | null;

    onMount(() => {
        id = $page.url.searchParams.get("id");
        lang = $page.url.searchParams.get("lang");
        if (id == null || lang == null) return;

        fetch(`/api/cluster?id=${id}&lang=${lang}`).then((r) =>
            r.json().then(json => {
                layoutResponse = json
                layout = layoutResponse?.body
            })
            // r.text().then((t) => {
            //     try {
            //         layoutResponse = JSON.parse(t);
            //         layout = layoutResponse.body;
            //         console.log(layout);
            //     } catch (_) {
            //         layoutResponse = null;
            //     }
            // })
        );
    });
</script>

{#if id === null}
    <p>Missing parameters "id" or "lang"</p>
{:else if layoutResponse === undefined}
    <p>Loading...</p>
{:else if layoutResponse === null}
    <p>Cannot parse fetched result</p>
{:else if layoutResponse.status === "UnsupportedLanguage"}
    <p>Language "{lang}" is not supported</p>
{:else if layoutResponse.status === "NotImplementedCluster"}
    <p>Cluster "{id}" is not yet implemented</p>
{:else if layoutResponse.status === "OK" && layout !== undefined}
    <Showcase {layout} />
{:else}
    <p>UNKNOWN ERROR</p>
{/if}

<style>
    @font-face {
        font-family: HYWenHei-65W;
        src: url("/asset/font/HYWenHei-65W.ttf");
    }

    @font-face {
        font-family: HYWenHei-85W;
        src: url("/asset/font/HYWenHei-85W.ttf");
    }

    :global(.font-hywh-65w) {
        font-family: HYWenHei-65W, sans-serif;
    }

    :global(.font-hywh-85w) {
        font-family: HYWenHei-85W, sans-serif;
    }
</style>
