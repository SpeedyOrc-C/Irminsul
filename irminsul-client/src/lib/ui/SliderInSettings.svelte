<script lang="ts">
    import Slider from "$lib/ui/Slider.svelte";

    export let min: number;
    export let max: number;
    export let value: number;
    export let divisions: number | null;
</script>

<div id="slider-in-settings">
    <div id="value">{value}</div>
    <div id="slider">
        <Slider on:slider-change {min} {max} bind:value {divisions} />
    </div>
</div>

<style lang="scss">
    #slider-in-settings {
        height: 3.2rem;
        width: 26.5rem;
    }

    #value {
        position: absolute;
        top: 50%;
        translate: 0 -50%;

        border: 0.15rem solid #0001;
        border-radius: 0.4rem;
        height: 2rem; width: 4rem;

        font-size: 1.3rem;
        color: #485265;
        text-align: center;
        line-height: 2rem;
    }

    #slider {
        position: absolute;
        right: 1.6rem;

        height: 3.2rem;
        width: 19rem;
    }
</style>