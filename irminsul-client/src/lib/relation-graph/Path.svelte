<script lang="ts">
    import { createEventDispatcher } from "svelte";
    import type { PathElement } from "../../model/RelationGraph";

    export let pathElements: Array<PathElement>;

    const dispatch = createEventDispatcher();
</script>

<div class="path">
    {#each pathElements as pathElement}
        <!-- svelte-ignore a11y-click-events-have-key-events -->
        <div
            class="path-element"
            on:click={() => {
                if (
                    // Prevent jumping if the user clicks on the last cluster
                    pathElement.id != pathElements[pathElements.length - 1].id
                ) {
                    dispatch("rg-action", {
                        action: "jump-to",
                        id: pathElement.id,
                    });
                }
            }}
        >
            {pathElement.translation}
        </div>
    {/each}
</div>

<style lang="scss">
    .path {
        width: fit-content;
        display: inline-block;
        padding: 0.1rem 0.1rem 0.1rem 0.2rem;
        border-left: 0.5rem solid rgb(255, 189, 34);

        background-color: #bda277;
    }
    .path-element {
        display: inline-block;
        padding: 0.3rem 0.8rem;
        margin: 0.1rem;
        border-radius: 0.2rem;

        background-color: #ece5d8;
        color: #3b4255;

        user-select: none;
        -webkit-user-select: none;
        -moz-user-select: none;
        cursor: pointer;

        transition-property: transform, box-shadow;
        transition-duration: 0.1s;

        &:hover {
            transform: scale(130%);
            box-shadow: 0 0 0.5rem 0.5rem #0003;
        }
        &:active {
            transform: scale(120%);
            box-shadow: 0 0 0.4rem 0.4rem #0003;
        }
    }
</style>
