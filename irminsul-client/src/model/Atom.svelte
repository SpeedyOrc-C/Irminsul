<script lang="ts">
    import { onMount } from "svelte";
    import { getImgAvatar, img_avatar_UnknownAvatar } from "../asset/Asset";
    import type { Vector2 } from "./Vector2";

    export let id: string;
    export let translation: string;
    export let position: Vector2;

    let locked = true;

    let style: string;
    $: style = `left: ${position.x}rem; top: ${-position.y}rem`;

    let avatarSrc: string = img_avatar_UnknownAvatar;

    onMount(() => getImgAvatar(id, (result) => (avatarSrc = result)));


    function click() {
        locked = locked ? false : true;
    }

    function move(e: KeyboardEvent) {
        if (locked) return;

        let delta = e.shiftKey ? 5 : 1;

        switch (e.code) {
            case "ArrowUp":
                position.y += delta;
                break;
            case "ArrowDown":
                position.y -= delta;
                break;
            case "ArrowLeft":
                position.x -= delta;
                break;
            case "ArrowRight":
                position.x += delta;
                break;
        }
    }
</script>

<svelte:window on:keydown|preventDefault={move} />

<!-- svelte-ignore a11y-click-events-have-key-events -->
<div id="{id}" class="atom" {style} on:click={click}>
    <img class="avatar" src={avatarSrc} alt="" />

    <div class="translation font-hywh-65w">
        {translation}
    </div>
</div>

<style>
    .atom {
        position: absolute;
        transform: translate(-50%, -50%);
        width: fit-content;
        height: fit-content;

        cursor: pointer;

        z-index: 10000;
    }

    .avatar {
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

    .avatar:hover {
        border: 0.2rem solid #bcc6e1;
        box-shadow: 0 0 0.2rem 0.1rem #bcc6e188;

        transform: translate(-50%, -50%) scale(200%);
        transition-duration: 0.2s;
    }

    .avatar:hover + .translation {
        display: block;
    }

    .translation {
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
