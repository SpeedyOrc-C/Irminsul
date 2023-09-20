<script lang="ts">
    import {afterUpdate, createEventDispatcher, onDestroy} from "svelte";
    import Coordinate from "./Coordinate.svelte";
    import type {Vector2} from "$lib/util/Vector2";
    import type Editor from "./Editor";
    import {getAvatar} from "$lib/AssigningAvatar";

    const dispatch = createEventDispatcher();

    export let id: string;
    export let name: string;

    export let showCoordinates = false;
    export let editor: Editor;

    let selected = false;
    let dim = false;
    let glitched = false;
    let glitchedName: string = id;
    let updateGlitchedTextInterval: number | null = null;
    let position: Vector2 = {x: 0, y: 0};

    const theDisappeared: Array<string> = [
        "RukkhadevataGreaterLord",
        "KunikuzushiRaiden",
    ];

    afterUpdate(() => {
        selected = editor.isSelected(id);
        dim = !editor.isEditing() && !selected && editor.numSelected() > 0
            && !editor.isEntityInSelectedCluster(id);
        position = editor.anchorOf(id);

        if (!editor.isEditing() && theDisappeared.find(name => name === id) !== undefined) {
            glitchEffectStart();
        } else {
            glitchEffectStop();
        }
    });

    onDestroy(() => glitchEffectStop());

    function glitchEffectStart() {
        if (updateGlitchedTextInterval === null) {
            updateGlitchedTextInterval = setInterval(updateGlitchedText, 100);
            glitched = true;
        }
    }

    function glitchEffectStop() {
        if (updateGlitchedTextInterval !== null) {
            clearInterval(updateGlitchedTextInterval);
            updateGlitchedTextInterval = null;
            glitched = false;
        }
    }

    function updateGlitchedText() {
        const characters = "!@#$%^&*()/|\\";
        glitchedName = "";
        for (let i = 0; i < name.length; i++) {
            const char = name.charAt(i);
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
<div class="atom no-select" class:selected class:dim class:glitched on:click={click}
     style:left="{position.x}rem" style:top="{-position.y}rem"
>
    <img class="avatar" src={getAvatar(id)} alt={name}/>

    <div class="label font-hywh-65w">
        {glitched ? glitchedName : name}
    </div>

    {#if showCoordinates}
        <Coordinate coordinate={position}/>
    {/if}
</div>

<style lang="scss">
    .atom {
        position: absolute;
        transform: translate(-50%, -50%);

        z-index: 100;

        transition-property: filter;
        transition-duration: 0.2s;

        &.dim {
            filter: brightness(50%) blur(0.1rem);

            &:hover {
                filter: unset;
            }
        }

        &.selected, &:hover {
            z-index: 2000;
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

    .label {
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
