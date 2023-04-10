<script lang="ts">
    import { onMount } from "svelte";

    export let forwardRelations: Array<string>;
    export let backwardRelations: Array<string>;
    export let biRelations: Array<string>;
    export let highlight: boolean = false;
    export let dim: boolean = false;
    export let subjectAnchor: Vector2;
    export let objectAnchor: Vector2;

    /*
    Chinese characters (in Chinese and Japanese) and Hangul (in Korean)
    rotate themselves 90 degrees anti-clockwise when they are written vertically.
    As some relations are vertical, we need to rotate these characters.
    */
    export let writingSystemIsCJK: boolean = false;

    import type { Vector2 } from "../util/Vector2";

    let needReverse: boolean;
    $: needReverse =
        objectAnchor.x < subjectAnchor.x ||
        (objectAnchor.x == subjectAnchor.x && objectAnchor.y > subjectAnchor.y);

    let realForwardRelations: Array<string> = [];
    $: realForwardRelations = needReverse
        ? backwardRelations
        : forwardRelations;

    let realBackwardRelations: Array<string> = [];
    $: realBackwardRelations = needReverse
        ? forwardRelations
        : backwardRelations;

    let width: number;
    $: width = Math.sqrt(
        (subjectAnchor.x - objectAnchor.x) ** 2 +
            (subjectAnchor.y - objectAnchor.y) ** 2
    );

    let position: Vector2 = { x: 0, y: 0 };
    $: position.x = (subjectAnchor.x + objectAnchor.x) / 2;
    $: position.y = (subjectAnchor.y + objectAnchor.y) / 2;

    let rotation: number;
    $: rotation =
        -(
            Math.atan(
                (subjectAnchor.y - objectAnchor.y) /
                    (subjectAnchor.x - objectAnchor.x)
            ) + (objectAnchor.x <= subjectAnchor.x ? Math.PI : 0)
        ) + (needReverse ? Math.PI : 0);

    let hasBiRelation: boolean;
    let hasForwardRelation: boolean;
    let hasBackwardRelation: boolean;

    $: hasBiRelation = biRelations.length > 0;
    $: hasForwardRelation = realForwardRelations.length > 0;
    $: hasBackwardRelation = realBackwardRelations.length > 0;

    let forwardRelationY: number;
    let backwardRelationY: number;

    function updateTransform() {
        subjectAnchor = subjectAnchor;
        objectAnchor = objectAnchor;
    }

    onMount(() => {
        // Bi-relations are always centered.
        // But uni-directional relations' offset can change for better readability.
        if (hasBiRelation) {
            // Uni-directional relations are placed
            // above or below the bi-relations.
            forwardRelationY = 1.4;
            backwardRelationY = -1.4;
        } else {
            // If there is only one uni-directional relation
            if (hasForwardRelation !== hasBackwardRelation) {
                // It is placed in the middle.
                forwardRelationY = 0;
                backwardRelationY = 0;
            } else {
                // If there are two uni-directional relations,
                // they are placed above and below the middle.
                forwardRelationY = 0.7;
                backwardRelationY = -0.7;
            }
        }
        updateTransform();
    });
</script>

<svelte:window on:keyup={updateTransform} />

<div
    class="relation-between"
    class:highlight
    class:dim
    style:width="{width}rem"
    style:left="{position.x}rem"
    style:top="{-position.y}rem"
    style:transform="translate(-50%, -50%) rotate({rotation}rad)"
>
    {#if biRelations.length > 0}
        <div
            class="bi-relation font-hywh-65w"
            class:write-vertically={writingSystemIsCJK}
        >
            {#each biRelations as r}
                <div>{r}</div>
            {/each}
        </div>
    {/if}

    {#if realForwardRelations.length > 0}
        <div
            class="forward-relation font-hywh-65w rotate-cjk-clockwise"
            style:top="calc(50% + {-forwardRelationY}rem)"
        >
            {#each realForwardRelations as r}
                <div>{r}</div>
            {/each}
        </div>
    {/if}

    {#if realBackwardRelations.length > 0}
        <div
            class="backward-relation font-hywh-65w"
            class:write-vertically={writingSystemIsCJK}
            style:top="calc(50% + {-backwardRelationY}rem)"
        >
            {#each realBackwardRelations as r}
                <div>{r}</div>
            {/each}
        </div>
    {/if}
</div>

<style lang="scss">
    .relation-between {
        position: absolute;
        transform: translate(-50%, -50%);

        height: 5.4rem;
        z-index: 1000;

        user-select: none;
        -webkit-user-select: none;
        -moz-user-select: none;
        
        transition: filter;
        transition-duration: 0.2s;

        &:hover,
        &:active {
            z-index: 2000;
            background-color: #bda27733;
        }
        &.dim {
            filter: brightness(50%) blur(0.1rem);
        }
        &.highlight {
            z-index: 2000;
        }
    }

    %relation-shared {
        position: absolute;
        transform: translate(-50%, -50%);
        left: 50%;

        width: 100%;
        height: 1.1rem;
        line-height: 1.1rem;
        padding: 0.1rem 0;

        border: 0.1rem solid #bda277;

        font-size: 0.8rem;
    }

    .bi-relation {
        @extend %relation-shared;
        top: 50%;
        background-color: #3b4255;
        color: white;
        text-align: center;
    }

    %uni-relation-shared {
        @extend %relation-shared;
        background-color: #f5ece1;
        color: #3b4255;
    }

    .forward-relation {
        @extend %uni-relation-shared;

        background-image: linear-gradient(
            90deg,
            #ffbd22aa 0%,
            transparent 10rem,
            transparent 100%
        );

        & > div {
            position: absolute;
            left: 3.5rem;
            font-feature-settings: "vert";
        }
    }

    .backward-relation {
        @extend %uni-relation-shared;

        background-image: linear-gradient(
            90deg,
            transparent 0%,
            transparent calc(100% - 10rem),
            #ffbd22aa 100%
        );

        & > div {
            position: absolute;
            right: 3.5rem;
        }
    }
</style>
