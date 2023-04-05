<script lang="ts">
    import { onMount } from "svelte";
    import { getImgAvatar, img_avatar_UnknownAvatar } from "../../asset/Asset";
    import Coordinate from "./Coordinate.svelte";
    import { deadKeyMultiplier } from "../util/DeadKeyMultiplier";
    import type { Vector2 } from "../../model/Vector2";

    export let id: string;
    export let translation: string;
    export let position: Vector2;
    export let showCoordinate: boolean = false;

    let selected = false;

    let style: string;
    $: style = `left: ${position.x}rem; top: ${-position.y}rem`;

    let avatarSrc: string = img_avatar_UnknownAvatar;
    onMount(() => getImgAvatar(id, (result) => (avatarSrc = result)));

    function keydown(e: KeyboardEvent) {
        if (e.ctrlKey || e.metaKey) {
            switch (e.code) {
                case "KeyA":
                    if (e.altKey) {
                        selected = false;
                    } else if (e.shiftKey) {
                        selected = selected ? false : true;
                    } else {
                        selected = true;
                    }
                    break;
            }
        } else {
            if (!selected) return;

            let delta = deadKeyMultiplier(e);

            switch (e.code) {
                case "KeyI":
                    position.y += delta;
                    break;
                case "KeyK":
                    position.y -= delta;
                    break;
                case "KeyJ":
                    position.x -= delta;
                    break;
                case "KeyL":
                    position.x += delta;
                    break;
            }
        }
    }
</script>

<svelte:window on:keydown|preventDefault={keydown} />

<!-- svelte-ignore a11y-click-events-have-key-events -->
<div
    {id}
    class={["atom", ...(selected ? ["selected"] : [])].join(" ")}
    {style}
    on:click={() => selected = selected ? false : true}
>
    <img class="avatar" src={avatarSrc} alt="" />
    <div class="translation font-hywh-65w">{translation}</div>
    {#if showCoordinate} <Coordinate {position} /> {/if}
</div>

<style>
    .atom {
        position: absolute;
        transform: translate(-50%, -50%);
        width: fit-content;
        height: fit-content;

        
        z-index: 10000;
        
        cursor: pointer;
        user-select: none;
        -webkit-user-select: none;
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

        transition: transform, border-color, border-width, box-shadow;
        transition-duration: 0.2s;
    }
    .atom.selected > .avatar {
        border-color: orange;
        border-width: 0.5rem;
    }

    .avatar:hover {
        border-color: #bcc6e1;
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