<script lang="ts">
    import { page } from "$app/stores";
    import { onMount } from "svelte";
    import { locale } from "svelte-i18n";
    import { writable, type Writable } from "svelte/store";
    import RelationGraph from "../../lib/relation-graph/RelationGraph.svelte";

    let id: string;
    let lang: Writable<string> = writable("zh-cn");
    let reduceVisualEffect: Writable<string> = writable("on");
    let whoAmI: Writable<"aether" | "lumine"> = writable("aether");

    onMount(() => {
        id = $page.url.searchParams.get("id") ?? "Mondstadt";

        lang.set(
            $page.url.searchParams.get("lang") ??
                localStorage.getItem("lang") ??
                "zh-cn"
        );
        lang.subscribe(newLang => {
            console.info("Change language into:", newLang);
            locale.set(newLang);
            localStorage.setItem("lang", newLang);
        });

        reduceVisualEffect.set(
            localStorage.getItem("reduce-visual-effect") ?? "on"
        );
        reduceVisualEffect.subscribe((v) =>
            localStorage.setItem("reduce-visual-effect", v)
        );

        switch (localStorage.getItem("who-am-i")) {
            case "lumine":
                whoAmI.set("lumine");
                break;
            case "aether":
            default:
                whoAmI.set("aether");
                break;
        }
        whoAmI.subscribe(whoAmI => localStorage.setItem("who-am-i", whoAmI));
    });
</script>

{#if id !== undefined}
    <RelationGraph {id} {lang} {reduceVisualEffect} {whoAmI} />
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
