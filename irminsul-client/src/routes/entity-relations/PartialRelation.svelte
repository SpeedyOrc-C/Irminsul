<script lang="ts">
    import { page } from "$app/stores";
    import { redirect } from "@sveltejs/kit";

    export let id: string;
    export let translation: string;
    export let action: string;
    export let reversed: boolean;

    function jumpToThisEntity() {
        window.location.href =
            `/entity-relations?` +
            `id=${id}&lang=${$page.url.searchParams.get("lang")}`;
    }
</script>

<div id="partial-relation">
    {#if reversed}
        <span
            id="translation"
            on:click={jumpToThisEntity}
            on:keypress={jumpToThisEntity}
        >
            {translation}
        </span>
        <span id="action">{action}</span>
    {:else}
        <span id="action">{action}</span>
        <span
            id="translation"
            on:click={jumpToThisEntity}
            on:keypress={jumpToThisEntity}
        >
            {translation}
        </span>
    {/if}
</div>

<style>
    #partial-relation {
        margin: 0.5rem;
        padding: 0.5rem 0;

        background-color: #0001;

        width: fit-content;
    }

    #action {
        padding: 0.5rem;
        color: #222;
    }

    #translation {
        padding: 0.5rem 0.8rem;
        background-color: #3b4255;
        color: #eee;
    }
</style>
