<script lang="ts">
    import { createEventDispatcher, onMount } from "svelte";
    import { getImgAvatar, img_avatar_UnknownAvatar } from "../../asset/Asset";
    import Coordinate from "./Coordinate.svelte";
    import { deadKeyMultiplier } from "$lib/util/DeadKeyMultiplier";
    import type { Vector2 } from "$lib/util/Vector2";

    export let id: string;
    export let translation: string;
    export let position: Vector2;
    export let showCoordinate = false;
    export let selected = false;
    export let dim = false;
    export let selectedAtoms: Set<string>;

    let avatarSrc: string;

    onMount(() => getImgAvatar(id, (result) => (avatarSrc = result)));

    const dispatch = createEventDispatcher();

    function dispatchUpdateSelectedAtoms() {
        dispatch("update-selected-atoms");
    }

    function toggleSelect() {
        selected = !selected;
        if (selected) select();
        else deselect();
    }

    function select() {
        selected = true;
        selectedAtoms.add(id);
        dispatchUpdateSelectedAtoms();
    }

    function deselect() {
        selected = false;
        selectedAtoms.delete(id);
        dispatchUpdateSelectedAtoms();
    }

    function keyDown(e: KeyboardEvent) {
        if (e.ctrlKey !== e.metaKey) {
            switch (e.code) {
                case "KeyA":
                    e.preventDefault();
                    if (e.altKey) deselect();
                    else if (e.shiftKey) toggleSelect();
                    else select();
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

<svelte:window on:keydown={keyDown} />

<!-- svelte-ignore a11y-click-events-have-key-events -->
<div class="atom" class:selected class:dim
    style:left="{position.x}rem" style:top="{-position.y}rem"
    on:click={toggleSelect}
>
    <img class="avatar" src={avatarSrc} alt={translation} />

    <div class="translation font-hywh-65w">{translation}</div>

    {#if showCoordinate}
        <Coordinate coordinate={position} />
    {/if}
</div>

<style lang="scss">
    .atom {
        position: absolute;
        transform: translate(-50%, -50%);
        width: fit-content;
        height: fit-content;

        z-index: 10000;

        cursor: pointer;
        user-select: none;
        -webkit-user-select: none;
        -moz-user-select: none;

        transition-property: filter;
        transition-duration: 0.2s;

        &.dim {
            filter: brightness(50%) blur(0.1rem);
            &:hover {
                filter: unset;
            }
        }
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

        transition-property: transform, border-color, border-width, box-shadow;
        transition-duration: 0.2s;

        &:hover {
            border-color: #bcc6e1;
            box-shadow: 0 0 0.2rem 0.1rem #bcc6e188;

            transform: translate(-50%, -50%) scale(200%);
            transition-duration: 0.2s;
        }

        .atom.selected > & {
            border-color: orange;
            border-width: 0.5rem;
        }
    }

    .translation {
        position: absolute;
        transform: translate(-50%, -50%);

        width: 10rem;
        top: 7rem;
        left: 0;
        border-radius: 0.2rem;

        background: #404756;
        padding: 0.2rem 0.2rem;
        color: #ebe4d7;

        text-align: center;

        display: none;

        .avatar:hover + & {
            display: block;
        }
    }
</style>
