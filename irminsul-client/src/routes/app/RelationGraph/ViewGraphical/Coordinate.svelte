<script lang="ts">
    import type { Vector2 } from "$lib/util/Vector2";

    export let coordinate: Vector2;
    export let size: Vector2 | null = null;

    const numFormat = new Intl.NumberFormat("en-US", { maximumFractionDigits: 2 });
</script>

<div class="coordinate">
    <div>x: {numFormat.format(coordinate.x)}</div>
    <div>y: {numFormat.format(coordinate.y)}</div>
    <!-- Only cluster has size, atom does not have a size. -->
    {#if size != null}
        <div>w: {numFormat.format(size.x)}</div>
        <div>h: {numFormat.format(size.y)}</div>
    {/if}
</div>

<style lang="scss">
    .coordinate {
        position: absolute;
        transform: translate(-50%, 0);
        
        bottom: 1rem;
        width: 4.5rem;
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
