<script lang="ts">
    import {afterUpdate, createEventDispatcher} from "svelte";
    import Coordinate from "./Coordinate.svelte";
    import type {Vector2} from "$lib/util/Vector2";
    import type Editor from "./Editor";

    const dispatch = createEventDispatcher();

    export let id: string;
    export let label: string;
    export let size: Vector2;

    export let showCoordinates = false;
    export let editor: Editor;

    let dim: boolean;
    let selected = false;
    let position: Vector2 = {x: 0, y: 0};
    let anchor: Vector2 = {x: 0, y: 0};

    afterUpdate(() => {
        selected = editor.isSelected(id);
        dim = !editor.isEditing() && !selected && editor.numSelected() > 0;
        position = editor.positionOf(id);
        anchor = editor.anchorOf(id);
    });

    function click() {
        dispatch("toggle-cluster", id);
    }

    function dblclick() {
        if (!editor.isEditing()) dispatch("jump-to", {id});
    }
</script>

<!-- svelte-ignore a11y-click-events-have-key-events -->
<div class="cluster" class:dim class:selected on:click={click} on:dblclick={dblclick}
     style:left="{position.x}rem" style:top="{-position.y}rem"
     style:width="{size.x}rem" style:height="{size.y}rem"
>
    <div id="label" class="font-hywh-65w">
        {label}
    </div>

    {#if showCoordinates}
        <Coordinate coordinate={position} {size}/>
        <div class="anchor-emphasis"
             style:left="{anchor.x - position.x + size.x / 2}rem"
             style:top="{-(anchor.y - position.y - size.y / 2)}rem"
        />
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

        -webkit-user-select: none;
        -moz-user-select: none;
        user-select: none;
        cursor: pointer;

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

    #label {
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
