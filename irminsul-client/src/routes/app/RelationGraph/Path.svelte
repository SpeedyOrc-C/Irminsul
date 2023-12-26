<script lang="ts">
    import { createEventDispatcher } from "svelte";
    import type {PathElement, RelationGraph} from "./RelationGraph";

    export let relationGraph: RelationGraph;
    export let enabled = true;

    function jumpTo(pathElement: PathElement) {
        if (enabled) {
            dispatch("jump-to", pathElement.id);
        }
    }

    function keydown(e: KeyboardEvent, pathElement: PathElement) {
        if (e.code == "Enter" || e.code == "Space") {
            jumpTo(pathElement);
        }
    }

    const dispatch = createEventDispatcher();
</script>

<div class="path no-select" class:enabled>
    <div class="path-elements">
        {#each relationGraph.path as element}
            <!-- svelte-ignore a11y-click-events-have-key-events -->
            <!-- svelte-ignore a11y-no-noninteractive-tabindex -->
            <div tabindex="0"
                 on:click={() => jumpTo(element)}
                 on:keydown={e => keydown(e, element)}
            >
                {element.name}
            </div>
        {/each}
        <div class="path-element">{relationGraph.rootName}</div>
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

        & > div {
            flex-shrink: 0;

            padding: 0.3rem 0.8rem;
            margin: 0.1rem;
            border-radius: 0.2rem;

            background-color: #ece5d8;
            color: #3b4255;

            transition-property: color, background-color;
            transition-duration: 0.1s;

            .path.enabled &:not(:last-child) {
                &:hover, &:focus {
                    background-color: white;
                }
                &:active {
                    background-color: #3b4255;
                    color: #ece5d8;
                }
            }
        }
    }
</style>
