<script lang="ts">
    import { createEventDispatcher } from "svelte";
    import type { PathElement } from "./RelationGraph";

    export let pathElements: Array<PathElement>;
    export let enabled = true;

    function pathElementClick(pathElement: PathElement) {
        if (!enabled) return;
        // Prevent jumping if the user clicks on the last cluster
        if (pathElement.id == pathElements[pathElements.length - 1].id) return;
        dispatch("jump-to", pathElement.id);
    }

    const dispatch = createEventDispatcher();
</script>

<div class="path" class:enabled>
    {#each pathElements as pathElement}
        <!-- svelte-ignore a11y-click-events-have-key-events -->
        <div class="path-element" on:click={() => pathElementClick(pathElement)}>
            {pathElement.translation}
        </div>
    {/each}
</div>

<style lang="scss">
    .path {
        width: fit-content;
        display: inline-block;
        padding: 0.1rem 0.1rem 0.1rem 0.2rem;
        border-left: 0.5rem solid #ffbd22;

        background-color: #bda277;


        opacity: 50%;
        filter: saturate(0);
        cursor: not-allowed;
        &.enabled {
            cursor: unset;
            opacity: unset;
            filter: unset;
        }
    }
    .path-element {
        display: inline-block;
        padding: 0.3rem 0.8rem;
        margin: 0.1rem;
        border-radius: 0.2rem;

        background-color: #ece5d8;
        color: #3b4255;

        -webkit-user-select: none;
        -moz-user-select: none;
        user-select: none;

        transition-property: transform, box-shadow;
        transition-duration: 0.1s;

        .path.enabled & {
            cursor: pointer;
            &:hover {
                transform: scale(130%);
                box-shadow: 0 0 0.5rem 0.5rem #0003;
            }
            &:active {
                transform: scale(120%);
                box-shadow: 0 0 0.4rem 0.4rem #0003;
            }
        }
    }
</style>
