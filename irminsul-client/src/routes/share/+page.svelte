<script lang="ts">
    import {onMount} from "svelte";
    import {page} from "$app/stores";

    onMount(() => {
        const shareId = $page.url.searchParams.get("id") ?? "Teyvat";
        sessionStorage.setItem("share_id", shareId);
        window.location.href = "/app";
    });
</script>
