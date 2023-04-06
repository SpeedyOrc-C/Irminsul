<script lang="ts">
    import { onMount } from "svelte";

    export let forwardRelations: Array<string>;
    export let backwardRelations: Array<string>;
    export let biRelations: Array<string>;
    export let subjectAnchor: Vector2;
    export let objectAnchor: Vector2;

    import type { Vector2 } from "../../model/Vector2";

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
        if (hasBiRelation) {
            forwardRelationY = 1.4;
            backwardRelationY = -1.4;
        } else if (hasForwardRelation !== hasBackwardRelation) {
            forwardRelationY = 0;
            backwardRelationY = 0;
        } else {
            forwardRelationY = 0.7;
            backwardRelationY = -0.7;
        }
        updateTransform();
    })
</script>

<svelte:window on:keyup={updateTransform} />

<div
    class="relation-between"
    style:width="{width}rem"
    style:left="{position.x}rem"
    style:top="{-position.y}rem"
    style:transform="translate(-50%, -50%) rotate({rotation}rad)"
>
    {#if biRelations.length > 0}
        <div class="bi-relation font-hywh-65w">
            {#each biRelations as r}
                <div>{r}</div>
            {/each}
        </div>
    {/if}

    {#if realForwardRelations.length > 0}
        <div
            class="forward-relation font-hywh-65w"
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

        &:hover,
        &:active {
            z-index: 2000;
            background-color: #bda27733;
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
        text-align: center;
    }

    .bi-relation {
        @extend %relation-shared;
        top: 50%;
        background-color: #3b4255;
        color: white;
    }

    %uni-relation-shared {
        @extend %relation-shared;
        background-color: #f5ece1;
        color: #3b4255;
    }

    .forward-relation {
        @extend %uni-relation-shared;
        // top: calc(50% - 0.7rem);

        background-image: linear-gradient(
            90deg,
            #ffbd22aa 0%,
            transparent 10rem,
            transparent 100%
        );

        & > div {
            position: absolute;
            left: 3.5rem;
        }
    }

    .backward-relation {
        @extend %uni-relation-shared;
        // top: calc(50% + 0.7rem);

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

    .bi-relation ~ .forward-relation {
        // top: calc(50% - 1.4rem);
    }

    .bi-relation ~ .backward-relation {
        // top: calc(50% + 1.4rem);
    }
</style>
