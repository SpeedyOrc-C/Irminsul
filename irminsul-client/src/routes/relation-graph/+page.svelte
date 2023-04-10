<script lang="ts">
    import { page } from "$app/stores";
    import { onMount } from "svelte";
    import { locale } from "svelte-i18n";
    import { writable, type Writable } from "svelte/store";
    import RelationGraph from "../../lib/relation-graph/RelationGraph.svelte";

    let id: string | null;
    let lang: string;
    let reduceVisualEffect: string;

    let langW: Writable<string> = writable();
    let reduceVisualEffectW: Writable<string> = writable();

    onMount(() => {
        id = $page.url.searchParams.get("id") ?? "Mondstadt";

        lang =
            $page.url.searchParams.get("lang") ??
            localStorage.getItem("lang") ??
            "zh-cn";
        langW.set(lang);
        langW.subscribe((newLang) => locale.set(newLang));
        localStorage.setItem("lang", lang);

        reduceVisualEffect =
            localStorage.getItem("reduce-visual-effect") ?? "on";
        reduceVisualEffectW.set(reduceVisualEffect);
        reduceVisualEffectW.subscribe((newValue) => {
            localStorage.setItem("reduce-visual-effect", newValue);
        });
    });
</script>

{#key id}
    {#key lang}
        <RelationGraph {id} {langW} {reduceVisualEffectW} />
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

    @keyframes loading-animation {
        0% {
            opacity: 0%;
        }
        100% {
            opacity: 100%;
        }
    }
</style>
