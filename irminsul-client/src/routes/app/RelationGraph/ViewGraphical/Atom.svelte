<script lang="ts">
    import {afterUpdate, createEventDispatcher, onDestroy, onMount} from "svelte";
    import {getImgAvatar} from "../../../../asset/Asset";
    import Coordinate from "./Coordinate.svelte";
    import type {Vector2} from "$lib/util/Vector2";
    import type Editor from "./Editor";

    const dispatch = createEventDispatcher();

    export let id: string;
    export let label: string;

    export let showCoordinates = false;
    export let editor: Editor;

    let avatarSrc: string;
    let selected = false;
    let dim = false;
    let glitched = false;
    let glitchedName: string = id;
    let updateGlitchedTextInterval: number | null = null;
    let position: Vector2 = {x: 0, y: 0};

    afterUpdate(() => {
        selected = editor.isSelected(id);
        dim = !editor.isEditing() && !selected && editor.numSelected() > 0
            && !editor.isEntityInSelectedCluster(id);
        position = editor.anchorOf(id);
    });

    onMount(() => getImgAvatar(id, result => {
        avatarSrc = result;
        if (id === "RukkhadevataGreaterLord") {
            updateGlitchedTextInterval = setInterval(updateGlitchedText, 100);
            glitched = true;
        }
    }));

    onDestroy(() => {
        if (updateGlitchedTextInterval !== null) {
            clearInterval(updateGlitchedTextInterval);
            updateGlitchedTextInterval = null;
        }
    });

    function updateGlitchedText() {
        const characters = "!@#$%^&*()/|\\";
        glitchedName = "";
        for (let i = 0; i < label.length; i++) {
            const char = label.charAt(i);
            if (Math.random() > 0.2 || char === " ") {
                glitchedName += char;
            } else {
                glitchedName += characters.charAt(Math.floor(Math.random() * characters.length));
            }
        }
    }

    function click() {
        dispatch("toggle-atom", id);
    }
</script>

<!-- svelte-ignore a11y-click-events-have-key-events -->
<div class="atom" class:selected class:dim class:glitched on:click={click}
     style:left="{position.x}rem" style:top="{-position.y}rem"
>
    <img class="avatar" src={avatarSrc} alt={label}/>

    <div id="label" class="font-hywh-65w">
        {glitched ? glitchedName : label}
    </div>

    {#if showCoordinates}
        <Coordinate coordinate={position}/>
    {/if}
</div>

<style lang="scss">
    .atom {
        position: absolute;
        transform: translate(-50%, -50%);
        width: fit-content;
        height: fit-content;

        z-index: 100;

        -webkit-user-select: none;
        -moz-user-select: none;
        user-select: none;
        cursor: pointer;

        transition-property: filter;
        transition-duration: 0.2s;

        &.dim {
            filter: brightness(50%) blur(0.1rem);

            &:hover {
                filter: unset;
            }
        }

        &.selected, &:hover {
            z-index: 200;
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

        transition-property: transform, border-color, box-shadow;
        transition-duration: 0.2s;

        &:hover {
            border-color: #bcc6e1;
            box-shadow: 0 0 0.2rem 0.1rem #bcc6e188;

            transform: translate(-50%, -50%) scale(200%);
            transition-duration: 0.2s;
        }

        .atom.selected > & {
            border-color: #0ff;
            border-width: 0.3rem;
        }
    }

    #label {
        position: absolute;
        transform: translate(-50%, -50%);

        width: 10rem;
        top: 7rem;
        left: 0;
        border: 0.1rem solid #fff4;
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