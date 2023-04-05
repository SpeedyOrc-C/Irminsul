<script lang="ts">
    import { page } from "$app/stores";
    import { onMount, setContext } from "svelte";
    import type { RelationGraph as TRelationGraph } from "../../model/RelationGraph";
    import type { ApiResponse } from "../../lib/util/Api";
    import RelationGraph from "../../lib/relation-graph/RelationGraph.svelte";
    import Prompt from "$lib/ui/Prompt.svelte";

    let clusterGraphResponse: ApiResponse<TRelationGraph> | null;
    let relationGraph: TRelationGraph | undefined;
    let id: string | null;
    let lang: string | null;

    id = $page.url.searchParams.get("id");
    lang = $page.url.searchParams.get("lang");
    if (!(id == null || lang == null)) {
        setContext("lang", lang);
        console.log("Language set to", lang);

        onMount(() => {
            fetch(`/api/relation-graph?id=${id}&lang=${lang}`, {
                method: "GET",
            }).then((r) =>
                r.json().then((json) => {
                    clusterGraphResponse = json;
                    relationGraph = clusterGraphResponse?.body;
                })
            );
        });
    }
</script>

{#if id == null || lang == null}
    <title>Missing Parameter</title>
    <Prompt
        title="Missing Parameter"
        content={"Missing parameters <u>id</u> or <u>lang</u>."}
    />
{:else if clusterGraphResponse === undefined}
    <title>Loading...</title>
    <div class="loading font-hywh-85w">Loading...</div>
{:else if clusterGraphResponse == null}
    <title>Network Error</title>
    <Prompt title="Network Error" content="Failed to fetch data from server." />
{:else if clusterGraphResponse.status === "UnsupportedLanguage"}
    <title>Unsupported Language</title>
    <Prompt
        title="Unsupported Language"
        content={`Language <u>${lang}</u> is not supported.`}
    />
{:else if clusterGraphResponse.status === "NotImplementedCluster"}
    <title>Not Implemented Cluster</title>
    <Prompt
        title="Not Implemented Cluster"
        content={`Cluster <u>${id}</u> is not yet implemented.`}
    />
{:else if clusterGraphResponse.status === "OK" && relationGraph !== undefined}
    <title>Irminsul - {relationGraph.rootTranslation}</title>
    <RelationGraph {relationGraph} />
{/if}

<style>
    :global(body) {
        overflow: hidden;

        background-color: #171f2b;
    }
    @font-face {
        font-family: HYWenHei-65W;
        src: url("/font/HYWenHei-65W.ttf");
    }

    @font-face {
        font-family: HYWenHei-85W;
        src: url("/font/HYWenHei-85W.ttf");
    }

    @font-face {
        font-family: fira-code-regular;
        src: url("/font/FiraCode-Regular.ttf");
    }

    :global(.font-hywh-65w) {
        font-family: HYWenHei-65W, sans-serif;
    }

    :global(.font-hywh-85w) {
        font-family: HYWenHei-85W, sans-serif;
    }

    .loading {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);

        font-size: 5rem;
        color: #eee;

        opacity: 0%;

        animation: loading-animation 1s 1 ease-out;
        animation-fill-mode: forwards;
        animation-delay: 1s;
    }

    @keyframes loading-animation {
        0% {
            opacity: 0%;
        }
        100% {
            opacity: 100%;
        }
    }
</style>
