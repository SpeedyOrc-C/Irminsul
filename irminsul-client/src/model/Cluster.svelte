<script lang="ts">
    import type { Vector2 } from "./Vector2";

    export let id: string;
    export let anchor: Vector2;
    export let translation: string;
    export let position: Vector2;
    export let width: number;
    export let height: number;

    let style: string;
    style = [
        `left: ${position.x}rem`,
        `top: ${-position.y}rem`,
        `width: ${width}rem`,
        `height: ${height}rem`,
    ].join("; ");
</script>

<div {id} class="cluster" {style}>
    <div class="translation font-hywh-65w">
        {translation}
    </div>
</div>

<style>
    .cluster {
        position: absolute;
        transform: translate(-50%, -50%);

        background-color: #ffff0003;
        border: 0.2rem solid #bda277;
        border-radius: 0.5rem;

        z-index: 100;
    }

    .translation {
        width: fit-content;
        position: absolute;
        transform: translate(-1rem, -50%);
        padding: 0.5rem 1rem;
        border-radius: 0.3rem;

        background-color: #703b00;
        font-size: 1.2rem;
        color: white;
        text-decoration: none;
    }

    .cluster:hover {
        background-color: #ffff000f;
    }
</style>
