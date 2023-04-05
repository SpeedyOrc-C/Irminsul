<script lang="ts">
    import type { Vector2 } from "../../model/Vector2";

    export let position: Vector2;
    export let size: Vector2 | null = null;
</script>

<div class="coordinate">
    <div class="x">x= {position.x}</div>
    <div class="y">y= {position.y}</div>
    {#if size != null}
        <div class="w">w= {size.x}</div>
        <div class="h">h= {size.y}</div>
    {/if}
</div>

<style>
    .coordinate {
        position: absolute;
        transform: translate(-50%, 0);

        bottom: 2rem;

        border-radius: 0.2rem;
        border: 1px solid #444;

        padding: 0.2rem 0.4rem;
        width: 6rem;
        background-color: #eee;
        color: #222;

        z-index: 100000;
    }

    .coordinate > div {
        font-family: fira-code-regular, monospace;
    }
</style>
