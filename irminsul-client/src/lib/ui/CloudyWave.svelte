<script lang="ts">
    import { onMount } from "svelte";

    let canvas: HTMLCanvasElement;
    
    onMount(() => {
        const context = canvas.getContext("2d");
    })
</script>

<div class="cloudy-wave">
    <canvas bind:this={canvas} />
</div>
