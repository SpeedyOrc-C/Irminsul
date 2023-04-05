<script lang="ts">
    import { page } from "$app/stores";
    import type { PathElement } from "../../model/RelationGraph";

    export let pathElements: Array<PathElement>;

    function jumpToThisCluster(id: string) {
        window.location.href = `/relation-graph?id=${id}&lang=${$page.url.searchParams.get(
            "lang"
        )}`;
    }
</script>

<div class="path">
    {#each pathElements as pathElement}
        <!-- svelte-ignore a11y-click-events-have-key-events -->
        <div
            class="path-element"
            on:click={() => jumpToThisCluster(pathElement.id)}
        >
            {pathElement.translation}
        </div>
    {/each}
</div>

<style>
    .path {
        width: fit-content;
        display: inline-block;
        padding: 0.1rem 0.1rem 0.1rem 0.2rem;
        border-left: 0.5rem solid rgb(255, 189, 34);

        background-color: #bda277;
    }
    .path-element {
        display: inline-block;
        padding: 0.3rem 0.8rem;
        margin: 0.1rem;
        border-radius: 0.2rem;

        background-color: #ece5d8;
        color: #3b4255;

        user-select: none;
        -webkit-user-select: none;
        cursor: pointer;

        transition: transform, box-shadow;
        transition-duration: 0.1s;
    }
    .path-element:hover {
        transform: scale(150%);
        box-shadow: 0 0 0.5rem 0.5rem #0004;
    }
    .path-element:active {
        transform: scale(140%);
        box-shadow: 0 0 0.4rem 0.4rem #0004;
    }
</style>
