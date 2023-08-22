<script lang="ts">
    import { onMount } from "svelte";
    import RelationGraph from "./RelationGraph/RelationGraph.svelte";
    import {page} from "$app/stores";

    import "../../global.scss"

    let id: string;

    onMount(() => { id = $page.url.searchParams.get("id") ?? "Teyvat"; });
</script>

{#if id !== undefined}
    <RelationGraph {id} />
{/if}
