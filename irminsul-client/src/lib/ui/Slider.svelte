<script lang="ts">
    import Slider from "$lib/ui/Slider";
    import {createEventDispatcher, onMount} from "svelte";

    export let min: number;
    export let max: number;
    export let value: number;
    export let divisions: number | null;

    let sliderDom: HTMLElement;
    let slider: Slider;

    const dispatch = createEventDispatcher();

    function drag(x: number) {
        slider.drag(x);

        value = slider.value;
        slider = slider;
    }

    function stop() {
        if (slider.dragging) {
            dispatch("slider-change", value);
            slider.stop();

            value = slider.value;
            slider = slider;
        }
    }

    function keydown(e: KeyboardEvent) {
        switch (e.code) {
            case "ArrowLeft": slider.decrease(); break;
            case "ArrowRight": slider.increase(); break;
            case "Home": slider.setPercentage(0); break;
            case "End": slider.setPercentage(1); break;
            default: return;
        }

        value = slider.value;
        slider = slider;
    }

    onMount(() => slider = new Slider(sliderDom, min, max, value, divisions))
</script>

<!-- User may drag the slider beyond its boundary. -->
<svelte:window
    on:mousemove={e => drag(e.clientX)}
    on:touchmove={e => drag(e.targetTouches[0].clientX)}
    on:mouseup={stop}
    on:touchend={stop}
/>

<div class="slider" bind:this={sliderDom}>
    <div class="bar"/>
    {#if slider !== undefined}
        <div class="filled-bar" style:width="{slider.percentage * 100}%"></div>
        <div class="bullet" style:left="{slider.percentage * 100}%"
             on:mousedown={slider.start}
             on:touchstart={slider.start}
        >
            <div class="outer-square"/>
            <div class="inner-square"/>
            <!-- svelte-ignore a11y-no-noninteractive-tabindex -->
            <div class="touch-area" on:keydown={keydown} tabindex="0"/>
        </div>
    {/if}
</div>

<style lang="scss">
    .slider {
        height: 100%;
        width: 100%;
    }

    %bar-common {
        position: absolute;
        top: 50%;
        transform: translate(0, -50%);

        height: 0.5rem;
        border-radius: 0.25rem;
    }
    .bar {
        @extend %bar-common;
        width: 100%;
        background: #7c7872;
    }
    .filled-bar {
        @extend %bar-common;
        background: #fff7e9;
    }

    .bullet {
        position: absolute;
        top: 50%;

        &:hover > .outer-square {
            border: 0.2rem solid white;
        }

        outline: 1px solid red;
    }

    .outer-square {
        position: absolute;
        top: 50%;
        transform: translate(-50%, -50%) rotate(45deg);

        background: #0004;
        box-shadow: 0 0 0.1rem 0.1rem #0004;

        height: 1rem;
        width: 1rem;
        border: 0.2rem solid #ece5d8;
        border-radius: 0.3rem;

        transition-property: border;
        transition-duration: 0.15s;
    }

    .inner-square {
        position: absolute;
        top: 50%; left: 50%;
        transform: translate(-50%, -50%) rotate(45deg);

        height: 0.8rem; width: 0.8rem;
        border-radius: 0.1rem;
        background: #ece5d8;
    }

    .touch-area {
        position: absolute;
        top: 50%; left: 50%;
        transform: translate(-50%, -50%);

        height: 3.2rem; width: 3.2rem;

        cursor: pointer;
    }
</style>
