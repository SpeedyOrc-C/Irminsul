<script lang="ts">
    import IconCross from "../../asset/icon/IconCross.svelte";
    import IconTick from "../../asset/icon/IconTick.svelte";
    import {createEventDispatcher} from "svelte";

    const dispatch = createEventDispatcher();

    function click() {
        value = !value;
        dispatch("switch-change", value);
    }

    export let value = false;
</script>

<div id="switch" class:checked={value} on:click={click}>
    <div id="inner">
        <div id="bullet">
            <div id="tick"><IconTick color="#808d9d"/></div>
            <div id="cross"><IconCross color="#d3bc8e"/></div>
        </div>
    </div>
</div>

<style lang="scss">
    #switch {
        position: relative;

        height: 2.5rem;
        width: 5.5rem;
        border: 2px solid #ece5d8;
        border-radius: 2.5rem;

        cursor: pointer;
    }

    #inner {
        height: 100%;
        width: 100%;
        border-radius: 2.5rem;

        transition-duration: 0.2s;
        transition-property: background-color;

        box-shadow: inset 0 0 0 2px #0004;
        background-color: #353d4f;

        #switch.checked > & {
            box-shadow: inset 0 0 0 2px #0002;
            background-color: #d3bc8e;
        }
    }

    #bullet {
        position: absolute;
        top: 50%; left: 50%;

        height: calc(2.5rem - 4px);
        width: calc(2.5rem - 4px);
        border-radius: 2.5rem;

        background-color: #ece5d8;
        box-shadow: 0 0 0.2rem 0 white;

        transition-duration: 0.2s;
        transition-property: transform;

        transform: translate(calc(-50% - 1.5rem), -50%);

        #switch.checked & {
            transform: translate(calc(-50% + 1.5rem), -50%);
        }
    }

    #bullet > div {
        height: 100%;
        width: 100%;
        position: absolute;

        transition-duration: 0.2s;
        transition-property: opacity, transform;
    }

    #tick {
        opacity: 0%;
        transform: scale(80%);
        #switch.checked & {
            opacity: 100%;
            transform: scale(100%);
        }
    }

    #cross {
        opacity: 100%;
        transform: scale(100%);
        #switch.checked & {
            opacity: 0%;
            transform: scale(80%);
        }
    }
</style>
