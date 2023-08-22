<script lang="ts">
    import Coordinate from "./Coordinate.svelte";
    import { deadKeyMultiplier } from "$lib/util/DeadKeyMultiplier";
    import type { Vector2 } from "$lib/util/Vector2";
    import {beforeUpdate, createEventDispatcher} from "svelte";

    export let id: string;
    export let anchor: Vector2;
    export let translation: string;
    export let position: Vector2;
    export let size: Vector2;
    export let showCoordinates = false;

    export let selectedAtoms: Set<string>;
    export let selectedClusters: Set<string>;
    export let selectedEntities: Set<string>;
    export let selectedEntitiesInSelectedClusters: Set<string>;
    export let editMode: boolean;

    let dim: boolean;
    let selected = false;

    const dispatch = createEventDispatcher();

    beforeUpdate(() => {
        selected = selectedClusters.has(id);

        dim = !editMode
            && !selectedClusters.has(id)
            && selectedEntities.size > 0
            && !selectedEntitiesInSelectedClusters.has(id);
    });

    function dispatchUpdateSelectedClusters() {
        dispatch("update-selected-clusters");
    }

    function toggleSelect() {
        selected = !selected;
        if (selected) select();
        else deselect();
    }

    function select() {
        if (!editMode) {
            selectedAtoms.clear();
            selectedClusters.clear();
        }
        selectedClusters.add(id);
        dispatchUpdateSelectedClusters();
    }

    function deselect() {
        selectedClusters.delete(id);
        dispatchUpdateSelectedClusters();
    }

    function keydown(e: KeyboardEvent) {
        if (e.ctrlKey !== e.metaKey) {
            switch (e.code) {
                case "KeyA":
                    e.preventDefault();
                    if (e.altKey) deselect();
                    else if (e.shiftKey) toggleSelect();
                    else select();
                    break;
            }
        } else {
            if (!selected) return;

            let delta = deadKeyMultiplier(e);

            switch (e.code) {
                // Moving (Anchor also moves)
                case "KeyI":
                    position.y += delta;
                    anchor.y += delta;
                    break;
                case "KeyK":
                    position.y -= delta;
                    anchor.y -= delta;
                    break;
                case "KeyJ":
                    position.x -= delta;
                    anchor.x -= delta;
                    break;
                case "KeyL":
                    position.x += delta;
                    anchor.x += delta;
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
                    dispatch("update-anchors");
                    break;
                case "ArrowDown":
                    anchor.y -= delta;
                    dispatch("update-anchors");
                    break;
                case "ArrowLeft":
                    anchor.x -= delta;
                    dispatch("update-anchors");
                    break;
                case "ArrowRight":
                    anchor.x += delta;
                    dispatch("update-anchors");
                    break;
            }
        }
    }
</script>

<svelte:window on:keydown={keydown} />

<!-- svelte-ignore a11y-click-events-have-key-events -->
<div
    class="cluster"
    class:dim
    class:selected
    style:left="{position.x}rem"
    style:top="{-position.y}rem"
    style:width="{size.x}rem"
    style:height="{size.y}rem"
    on:click={toggleSelect}
    on:dblclick={() => { if (!editMode) dispatch("jump-to", {id}); } }
>
    <div class="translation font-hywh-65w">
        {translation}
    </div>

    {#if showCoordinates}
        <Coordinate coordinate={position} {size} />
        <div
            class="anchor-emphasis"
            style:left="{anchor.x - position.x + size.x / 2}rem"
            style:top="{-(anchor.y - position.y - size.y / 2)}rem"></div>
    {/if}
</div>

<style lang="scss">
    .cluster {
        position: absolute;
        transform: translate(-50%, -50%);

        background-color: #e7dfd2;
        border: 0.2rem solid #bda277;
        border-radius: 0.5rem;

        z-index: 10;

        cursor: pointer;
        user-select: none;
        -webkit-user-select: none;
        -moz-user-select: none;

        transition-property: filter;
        transition-duration: 0.2s;

        &.selected {
            border-color: #0ff;
            border-width: 0.3rem;
        }
        &.dim {
            filter: brightness(50%) blur(0.1rem);
            &:hover {
                filter: unset;
            }
        }
    }

    .translation {
        position: absolute;
        top: 50%;

        width: 100%;
        height: fit-content;
        transform: translate(0, -50%);
        border-radius: 0.3rem;
        
        text-align: center;
        font-size: 1.2rem;
        color: #3b4255;
    }

    .anchor-emphasis {
        position: absolute;
        transform: translate(-50%, -50%);

        background-color: #0ff;
        border-radius: 100%;
        height: 1rem;
        width: 1rem;

        z-index: 10000;
    }
</style>
