<script lang="ts">
    import type {Vector2} from "./Vector2";
    import {onMount} from "svelte";

    export let id: string;
    export let translation: string;
    export let position: Vector2;

    let style: string

    onMount(() => {
        style = `left: ${position.x}rem; top: ${-position.y}rem`
    })

</script>

<div id="atom" {style}>

    <img id="avatar" src="/asset/img/avatar/{id}.png" alt=""/>

    <div id="translation" class="font-hywh-65w">
        {translation}
    </div>

</div>

<style>
    #atom {
        position: absolute;
        transform: translate(-50%, -50%);
        width: fit-content;
        height: fit-content;

        cursor: pointer;

        z-index: 10000;
    }

    #avatar {
        width: 5rem;
        height: 5rem;
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);

        background-color: #fcfcfc;
        border-radius: 100%;
        border: 0.2rem solid #bda277;
        display: block;
        margin: 0 auto;

        transition: transform, border, box-shadow;
        transition-duration: 0.5s;
    }

    #avatar:hover {
        border: 0.2rem solid #bcc6e1;
        box-shadow: 0 0 0.2rem 0.1rem #bcc6e188;

        transform: translate(-50%, -50%) scale(200%);
        transition-duration: 0.2s;
    }

    #avatar:hover + #translation {
        display: block;
    }

    #translation {
        position: absolute;
        transform: translate(-50%, -50%);

        width: 10rem;
        top: 7rem;
        left: 0;
        border-radius: 0.2rem;

        background: #404756;
        padding: 0.2rem 0;
        color: #ebe4d7;

        text-align: center;

        display: none;
    }
</style>
