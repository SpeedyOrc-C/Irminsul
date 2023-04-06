<script lang="ts">
    import { page } from "$app/stores";
    import { onMount } from "svelte";
    import RelationGraph from "../../lib/relation-graph/RelationGraph.svelte";

    let id: string | null;
    let lang: string | null;

    onMount(() => {
        id = $page.url.searchParams.has("id")
            ? $page.url.searchParams.get("id")
            : "Mondstadt";
        
        lang = $page.url.searchParams.has("lang")
            ? $page.url.searchParams.get("lang")
            : "zh-cn";
    });
</script>

{#key id}
    {#key lang}
        <RelationGraph {id} {lang} />
    {/key}
{/key}

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
