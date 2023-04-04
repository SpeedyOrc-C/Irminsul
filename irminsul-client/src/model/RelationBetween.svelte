<script lang="ts">
    export let forwardRelations: Array<string>;
    export let backwardRelations: Array<string>;
    export let biRelations: Array<string>;
    export let subjectAnchor: Vector2;
    export let objectAnchor: Vector2;

    import type { Vector2 } from "./Vector2";

    let realForwardRelations: Array<string> = [];
    let realBackwardRelations: Array<string> = [];

    let needReverse: boolean;
    let position: Vector2 = { x: 0, y: 0 };
    let width: number;
    let rotation: number;

    $: needReverse =
        objectAnchor.x < subjectAnchor.x ||
        (objectAnchor.x == subjectAnchor.x && objectAnchor.y > subjectAnchor.y);

    $: realForwardRelations = needReverse
        ? backwardRelations
        : forwardRelations;

    $: realBackwardRelations = needReverse
        ? forwardRelations
        : backwardRelations;

    $: width = Math.sqrt(
        (subjectAnchor.x - objectAnchor.x) ** 2 +
            (subjectAnchor.y - objectAnchor.y) ** 2
    );

    $: position.x = (subjectAnchor.x + objectAnchor.x) / 2;
    $: position.y = (subjectAnchor.y + objectAnchor.y) / 2;

    $: rotation =
        Math.atan(
            (subjectAnchor.y - objectAnchor.y) /
                (subjectAnchor.x - objectAnchor.x)
        ) + (objectAnchor.x <= subjectAnchor.x ? Math.PI : 0);

    let style: string;
    $: style = [
        `width: ${width}rem`,
        `left: ${position.x}rem`,
        `top: ${-position.y}rem`,
        "transform: " +
            [
                `translate(-50%, -50%)`,
                `rotate(${-rotation + (needReverse ? 3.141592653589 : 0)}rad)`,
            ].join(" "),
    ].join("; ");

    export function updateTransform() {
        subjectAnchor = subjectAnchor
        objectAnchor = objectAnchor
    }
    updateTransform();
</script>

<svelte:window on:keyup={updateTransform} />

<div class="relation-between" {style}>
    {#if biRelations.length > 0}
        <div class="bi-relation font-hywh-65w">
            {#each biRelations as r}
                <div>{r}</div>
            {/each}
        </div>
    {/if}

    {#if realForwardRelations.length > 0}
        <div class="forward-relation font-hywh-65w">
            {#each realForwardRelations as r}
                <div>{r}</div>
            {/each}
        </div>
    {/if}

    {#if realBackwardRelations.length > 0}
        <div class="backward-relation font-hywh-65w">
            {#each realBackwardRelations as r}
                <div>{r}</div>
            {/each}
        </div>
    {/if}
</div>

<style>
    .relation-between {
        position: absolute;
        transform: translate(-50%, -50%);

        height: 5.4rem;
        z-index: 1000;
    }

    .relation-between:hover,
    .relation-between:active {
        z-index: 2000;
        background-color: #bda27744;
    }

    .relation-between > div {
        border: 0.1rem solid #bda277;
        /*box-shadow: #3b425588 0 0 0.3rem 0.1rem;*/

        position: absolute;
        transform: translate(-50%, -50%);

        font-size: 0.8rem;
        text-align: center;
    }

    .bi-relation {
        left: 50%;
        top: 50%;
        background-color: #3b4255;
        color: white;
    }

    .forward-relation,
    .backward-relation {
        left: 50%;
        /* background-color: #f5ece1; */
        color: #3b4255;
    }

    .bi-relation {
        left: 50%;
        top: 50%;
        background-color: #3b4255;
        color: white;
    }

    .forward-relation,
    .backward-relation {
        left: 50%;
        background-color: #f5ece1;
        color: #3b4255;
    }

    .forward-relation {
        top: calc(50% - 0.7rem);

        background-image: repeating-linear-gradient(
            45deg,
            transparent,
            #00000000 1.1rem,
            #0000000c 1.1rem,
            #0000000c 2.2rem
        );
        background-size: 40rem 2rem;
        animation: 2s linear 0s infinite normal forwards running
            forward-relation-background;
    }

    @keyframes forward-relation-background {
        from {
            background-position-x: -3.1112rem;
        }
        to {
            background-position-x: 0;
        }
    }

    .forward-relation > div {
        position: absolute;
        left: 3.5rem;
    }

    .backward-relation {
        top: calc(50% + 0.7rem);

        background-image: repeating-linear-gradient(
            45deg,
            transparent,
            #00000000 1.1rem,
            #0000000c 1.1rem,
            #0000000c 2.2rem
        );
        background-size: 40rem 2rem;
        animation: 2s linear 0s infinite normal forwards running
            backward-relation-background;
    }

    @keyframes backward-relation-background {
        from {
            background-position-x: 0;
        }
        to {
            background-position-x: -3.1112rem;
        }
    }

    .backward-relation > div {
        position: absolute;
        right: 3.5rem;
    }

    .bi-relation ~ .forward-relation {
        top: calc(50% - 1.4rem);
    }

    .bi-relation ~ .backward-relation {
        top: calc(50% + 1.4rem);
    }

    .bi-relation,
    .forward-relation,
    .backward-relation {
        width: 100%;
        height: 1.1rem;
        line-height: 1.1rem;
        padding: 0.1rem 0;
        /*color: #724302;*/
    }
</style>
