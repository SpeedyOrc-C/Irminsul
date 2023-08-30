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
    <div class="path-elements">
        {#each pathElements as pathElement}
            <!-- svelte-ignore a11y-click-events-have-key-events -->
            <div class="path-element" on:click={() => pathElementClick(pathElement)} tabindex="0">
                {pathElement.translation}
            </div>
        {/each}
    </div>
</div>

<style lang="scss">
    .path {
        position: relative;
        margin-right: 1rem;
        border-left: 0.5rem solid #ffbd22;
        padding: 0.1rem 0.1rem 0.1rem 0.2rem;

        background-color: #bda277;

        overflow-y: hidden;
        overflow-x: auto;

        opacity: 50%;
        filter: saturate(0);
        cursor: not-allowed;

        &.enabled {
            cursor: unset;
            opacity: unset;
            filter: unset;
        }
    }

    .path-elements {
        display: flex;
    }

    .path-element {
        flex-shrink: 0;

        padding: 0.3rem 0.8rem;
        margin: 0.1rem;
        border-radius: 0.2rem;

        background-color: #ece5d8;
        color: #3b4255;

        -webkit-user-select: none;
        -moz-user-select: none;
        user-select: none;

        transition-property: color, background-color;
        transition-duration: 0.1s;

        .path.enabled &:not(:last-child) {
            cursor: pointer;
            &:hover, &:focus {
                background-color: white;
            }
            &:active {
                background-color: #3b4255;
                color: #ece5d8;
            }
        }
    }
</style>
