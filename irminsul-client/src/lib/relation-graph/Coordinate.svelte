<script lang="ts">
    import type { Vector2 } from "../util/Vector2";

    export let coordinate: Vector2;
    export let size: Vector2 | null = null;
</script>

<div class="coordinate">
    <div>x: {coordinate.x}</div>
    <div>y: {coordinate.y}</div>
    <!-- Only cluster has size, atom does not have a size. -->
    {#if size != null}
        <div>w: {size.x}</div>
        <div>h: {size.y}</div>
    {/if}
</div>

<style lang="scss">
    .coordinate {
        position: absolute;
        transform: translate(-50%, 0);
        
        bottom: 2rem;
        width: 4rem;
        padding: 0.2rem 0.4rem;
        border-radius: 0.2rem;
        border: 1px solid #444;

        background-color: #eee;
        color: #222;

        z-index: 100000;

        font-size: 0.7rem;

        & > div {
            font-family: fira-code-regular, monospace;
        }
    }
</style>
