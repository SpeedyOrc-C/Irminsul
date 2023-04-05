<script lang="ts">
    import Coordinate from "./Coordinate.svelte";
    import { deadKeyMultiplier } from "../util/DeadKeyMultiplier";
    import type { Vector2 } from "../../model/Vector2";
    import { getContext, onMount } from "svelte";
    import { page } from "$app/stores";

    export let id: string;
    export let anchor: Vector2;
    export let translation: string;
    export let position: Vector2;
    export let size: Vector2;
    export let showCoordinate: boolean = false;

    let selected = false;

    let style: string;
    $: style = [
        `left: ${position.x}rem`,
        `top: ${-position.y}rem`,
        `width: ${size.x}rem`,
        `height: ${size.y}rem`,
    ].join("; ");

    let coordinateStyle: string;
    $: coordinateStyle = [
        `left: ${anchor.x - position.x + size.x / 2}rem`,
        `top: ${-(anchor.y - position.y - size.y / 2)}rem`,
    ].join("; ");

    function keydown(e: KeyboardEvent) {
        if (e.ctrlKey || e.metaKey) {
            switch (e.code) {
                case "KeyA":
                    if (e.altKey) {
                        selected = false;
                    } else if (e.shiftKey) {
                        selected = selected ? false : true;
                    } else {
                        selected = true;
                    }
                    break;
            }
        } else {
            if (!selected) return;

            let delta = deadKeyMultiplier(e);

            switch (e.code) {
                // Moving
                case "KeyI":
                    position.y += delta;
                    break;
                case "KeyK":
                    position.y -= delta;
                    break;
                case "KeyJ":
                    position.x -= delta;
                    break;
                case "KeyL":
                    position.x += delta;
                    break;
                // Resizing
                case "KeyY":
                    size.x -= delta;
                    break;
                case "KeyU":
                    size.y -= delta;
                    break;
                case "KeyO":
                    size.y += delta;
                    break;
                case "KeyP":
                    size.x += delta;
                    break;
                // Moving anchor
                case "ArrowUp":
                    anchor.y += delta;
                    break;
                case "ArrowDown":
                    anchor.y -= delta;
                    break;
                case "ArrowLeft":
                    anchor.x -= delta;
                    break;
                case "ArrowRight":
                    anchor.x += delta;
                    break;
            }
        }
    }

    function jumpToThisCluster(_: Event) {
        window.location.href = `/relation-graph?id=${id}&lang=${$page.url.searchParams.get(
            "lang"
        )}`;
    }
</script>

<svelte:window on:keydown={keydown} />

<!-- svelte-ignore a11y-click-events-have-key-events -->
<div
    {id}
    class={["cluster", ...(selected ? ["selected"] : [])].join(" ")}
    {style}
    on:click={() => (selected = selected ? false : true)}
>
    <div class="translation font-hywh-65w" on:click={jumpToThisCluster}>
        {translation}
    </div>

    {#if showCoordinate}
        <Coordinate {position} {size} />
        <div class="anchor-emphasis" style={coordinateStyle} />
    {/if}
</div>

<style>
    .cluster {
        position: absolute;
        transform: translate(-50%, -50%);

        background-color: #ffff0003;
        border: 0.2rem solid #bda277;
        border-radius: 0.5rem;

        z-index: 100;

        cursor: pointer;
        user-select: none;
        -webkit-user-select: none;
    }

    .cluster.selected {
        border-color: orange;
        border-width: 0.5rem;
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

    .anchor-emphasis {
        position: absolute;
        transform: translate(-50%, -50%);

        background-color: orange;
        border-radius: 100%;
        height: 3rem;
        width: 3rem;

        z-index: 100000;
    }
</style>
