<script lang="ts">
    import { onMount } from "svelte";
    import RelationGraph from "$lib/relation-graph/RelationGraph.svelte";
    import {page} from "$app/stores";

    let id: string;

    onMount(() => { id = $page.url.searchParams.get("id") ?? "Teyvat"; });
</script>

{#if id !== undefined}
    <RelationGraph {id} />
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

    @keyframes loading-animation {
        0% {
            opacity: 0%;
        }
        100% {
            opacity: 100%;
        }
    }
</style>
