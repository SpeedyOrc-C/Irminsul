<script lang="ts">
    import { onMount } from "svelte";
    import RelationGraph from "./RelationGraph/RelationGraph.svelte";

    import "../../global.scss";

    let mounted = false;
    let id = "Teyvat";

    onMount(() => {
        const shareId = sessionStorage.getItem("share_id");
        if (shareId !== null) {
            id = shareId;
            sessionStorage.removeItem("share_id");
        }
        mounted = true;
    });
</script>

{#if mounted}
    <RelationGraph {id} />
{/if}
