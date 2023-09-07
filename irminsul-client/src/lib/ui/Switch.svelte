<script lang="ts">
    import IconCross from "../../asset/icon/IconCross.svelte";
    import IconTick from "../../asset/icon/IconTick.svelte";
    import {createEventDispatcher} from "svelte";

    const dispatch = createEventDispatcher();

    export let value = false;

    function click() {
        value = !value;
        dispatch("switch-change", value);
    }

    function keydown(e: KeyboardEvent) {
        if (e.target !== document.activeElement) return;

        if (e.code === "Enter" || e.code === "Space") {
            click();
            return;
        }

        if (e.code === "ArrowLeft") {
            value = false;
            dispatch("switch-change", value);
            return;
        }

        if (e.code === "ArrowRight") {
            value = true;
            dispatch("switch-change", value);
            return;
        }
    }
</script>

<!-- svelte-ignore a11y-no-noninteractive-tabindex -->
<div class="switch" class:checked={value} on:click={click} on:keydown={keydown} tabindex="0">
    <div class="inner">
        <div class="bullet">
            <div class="tick"><IconTick color="#808d9d"/></div>
            <div class="cross"><IconCross color="#d3bc8e"/></div>
        </div>
    </div>
</div>

<style lang="scss">
    $width: 5.5rem;
    $height: 2.5rem;

    .switch {
        position: relative;

        height: $height;
        width: $width;
        border: 2px solid #ece5d8;
        border-radius: 999rem;

        cursor: pointer;
    }

    .inner {
        position: relative;
        height: 100%;
        width: 100%;
        box-sizing: border-box;
        border-radius: 999rem;

        transition-duration: 0.2s;
        transition-property: background-color;

        border: 2px solid #0004;
        background-color: #353d4f;

        .switch.checked > & {
            border: 2px solid #0002;
            background-color: #d3bc8e;
        }
    }

    .bullet {
        position: absolute;
        top: 50%; left: 50%;

        height: 100%; aspect-ratio: 1;
        border-radius: 999rem;

        background-color: #ece5d8;
        box-shadow: 0 0 0.2rem 0 white;

        transition-duration: 0.2s;
        transition-property: transform;

        $displacement: calc(($width - $height) / 2);

        transform: translate(calc(-50% - $displacement), -50%);

        .switch.checked & {
            transform: translate(calc(-50% + $displacement), -50%);
        }
    }

    .bullet > div {
        height: 100%;
        width: 100%;
        position: absolute;

        transition-duration: 0.2s;
        transition-property: opacity, transform;
    }

    .tick {
        opacity: 0%;
        transform: scale(80%);
        .switch.checked & {
            opacity: 100%;
            transform: scale(100%);
        }
    }

    .cross {
        opacity: 100%;
        transform: scale(100%);
        .switch.checked & {
            opacity: 0%;
            transform: scale(80%);
        }
    }
</style>
