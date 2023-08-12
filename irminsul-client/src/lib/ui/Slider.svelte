<script lang="ts">
    import Slider from "$lib/ui/Slider";
    import {createEventDispatcher, onMount} from "svelte";

    export let min: number;
    export let max: number;
    export let value: number;
    export let divisions: number | null;

    let sliderDom: HTMLElement;
    let slider: Slider;

    let percentage: number;
    let percentageValue: number;

    const dispatch = createEventDispatcher();

    function callback(p: number) {
        percentage = p;
        percentageValue = percentage * 100;
        value = slider.toValue(p);
    }

    function stop() {
        if (slider.dragging) {
            dispatch("slider-change", value);
            slider.stop();
        }
    }

    onMount(() => {
        slider = new Slider(sliderDom, min, max, divisions, callback);
        percentage = slider.fromValue(value);
        percentageValue = percentage * 100;
    })
</script>

<svelte:window
    on:mousemove={e => slider.drag(e.clientX)}
    on:touchmove={e => slider.drag(e.targetTouches[0].clientX)}
    on:mouseup={stop}
    on:touchend={stop}
/>

<div id="slider" bind:this={sliderDom}>
    <div id="bar"/>
    <div id="filled-bar" style:width="{percentageValue}%"></div>
    <div id="bullet" style:left="{percentageValue}%"
         on:mousedown={slider.start}
         on:touchstart={slider.start}
    >
        <div id="outer-square"/>
        <div id="inner-square"/>
        <div id="touch-area"/>
    </div>
</div>

<style lang="scss">
    #slider {
        height: 100%;
        width: 100%;
    }

    %bar-common {
        position: absolute;
        top: 50%;
        translate: 0 -50%;

        height: 0.5rem;
        border-radius: 0.25rem;
    }
    #bar {
        @extend %bar-common;
        width: 100%;
        background: #7c7872;
    }
    #filled-bar {
        @extend %bar-common;
        background: #fff7e9;
    }

    #bullet {
        position: absolute;
        top: 50%;

        &:hover > #outer-square {
            border: 0.2rem solid white;
        }

        outline: 1px solid red;
    }

    #outer-square {
        position: absolute;
        top: 50%;
        translate: -50% -50%;
        rotate: 45deg;

        background: #0004;
        box-shadow: 0 0 0.1rem 0.1rem #0004;

        height: 1rem;
        width: 1rem;
        border: 0.2rem solid #ece5d8;
        border-radius: 0.3rem;

        transition-property: border;
        transition-duration: 0.15s;
    }

    #inner-square {
        position: absolute;
        top: 50%; left: 50%;
        translate: -50% -50%;
        rotate: 45deg;

        height: 0.8rem; width: 0.8rem;
        border-radius: 0.1rem;
        background: #ece5d8;
    }

    #touch-area {
        position: absolute;
        top: 50%; left: 50%;
        translate: -50% -50%;

        height: 3.2rem; width: 3.2rem;
    }
</style>
