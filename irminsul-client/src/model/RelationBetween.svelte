<script lang="ts">
    import type { Vector2 } from "./Vector2";
    import {onMount} from "svelte";

    export let forwardRelations: Array<string>;
    export let backwardRelations: Array<string>;
    export let biRelations: Array<string>;
    export let width: number;
    export let position: Vector2;
    export let rotation: number;
    export let needReverse: boolean;

    let style = ''
    let realForwardRelations: Array<string> = []
    let realBackwardRelations: Array<string> = []

    onMount(() => {
        style = [
            `width: ${width}rem`,
            `left: ${position.x}rem`,
            `top: ${-position.y}rem`,
            "transform: " +
                [
                    `translate(-50%, -50%)`,
                    `rotate(${-rotation + (needReverse ? 3.141592653589 : 0)}rad)`,
                ].join(" "),
        ].join("; ");

        realForwardRelations = needReverse
            ? backwardRelations
            : forwardRelations;

        realBackwardRelations = needReverse
            ? forwardRelations
            : backwardRelations;
    })

</script>

<div id="relation-between" {style}>
    {#if biRelations.length > 0}
        <div id="bi-relation" class="font-hywh-65w">
            {#each biRelations as r}
                <div>{r}</div>
            {/each}
        </div>
    {/if}

    {#if realForwardRelations.length > 0}
        <div id="forward-relation" class="font-hywh-65w">
            {#each realForwardRelations as r}
                <div>{r}</div>
            {/each}
        </div>
    {/if}

    {#if realBackwardRelations.length > 0}
        <div id="backward-relation" class="font-hywh-65w">
            {#each realBackwardRelations as r}
                <div>{r}</div>
            {/each}
        </div>
    {/if}
</div>

<style>
    #relation-between {
        position: absolute;
        transform: translate(-50%, -50%);

        height: 5.4rem;
        z-index: 1000;
    }

    #relation-between:hover,
    #relation-between:active {
        z-index: 2000;
        background-color: #bda27744;
    }

    #relation-between > div {
        border: 0.1rem solid #bda277;
        /*box-shadow: #3b425588 0 0 0.3rem 0.1rem;*/

        position: absolute;
        transform: translate(-50%, -50%);

        font-size: 0.8rem;
        text-align: center;
    }

    #bi-relation {
        left: 50%;
        top: 50%;
        background-color: #3b4255;
        color: white;
    }

    #forward-relation,
    #backward-relation {
        left: 50%;
        /* background-color: #f5ece1; */
        color: #3b4255;
    }

    #bi-relation {
        left: 50%;
        top: 50%;
        background-color: #3b4255;
        color: white;
    }

    #forward-relation,
    #backward-relation {
        left: 50%;
        background-color: #f5ece1;
        color: #3b4255;
    }

    #forward-relation {
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

    #forward-relation > div {
        position: absolute;
        left: 3.5rem;
    }

    #backward-relation {
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

    #backward-relation > div {
        position: absolute;
        right: 3.5rem;
    }

    #bi-relation ~ #forward-relation {
        top: calc(50% - 1.4rem);
    }

    #bi-relation ~ #backward-relation {
        top: calc(50% + 1.4rem);
    }

    #bi-relation,
    #forward-relation,
    #backward-relation {
        width: 100%;
        height: 1.1rem;
        line-height: 1.1rem;
        padding: 0.1rem 0;
        /*color: #724302;*/
    }
</style>
