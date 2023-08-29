<script lang="ts">
    import { createEventDispatcher } from "svelte";

    export let width = "";
    export let height = "";
    export let style = "";
    export let hasBorder = false;
    export let inSettings = false;

    const dispatch = createEventDispatcher();

    function click() {
        dispatch("button-clicked");
    }
</script>

<!-- svelte-ignore a11y-click-events-have-key-events -->
<div class="button" class:has-border={hasBorder} class:in-settings={inSettings}
    style:height style:width {style}
    on:click={click}
>
    <slot />
</div>

<style lang="scss">
    .button {
        display: inline-block;
        border-radius: 99rem;

        background-color: #ece5d8;

        box-shadow: 0 0 1rem 0.2rem #0004, 0 0 0 0 transparent,
            inset 0 0 0 0 transparent;

        -webkit-user-select: none;
        -moz-user-select: none;
        user-select: none;
        cursor: pointer;

        transition-property: background-color, box-shadow;
        transition-duration: 0.2s;

        &:hover {
            box-shadow: 0 0 1rem 0.2rem #0004, 0 0 0 0.2rem white,
                inset 0 0 0 0.1rem #0000001c;
        }
        &:active {
            box-shadow: 0 0 0.5rem 0.2rem #fff4, 0 0 0 0.2rem transparent,
                inset 0 0 0 0.15rem #0004;

            animation: click-blink-ani 0.2s;
            animation-fill-mode: forwards;
        }

        &.has-border {
            box-shadow: 0 0 1rem 0.2rem #0004, 0 0 0 0.4rem #ece5daaa,
                inset 0 0 0 0 transparent;

            &:hover {
                box-shadow: 0 0 1rem 0.2rem #fff4, 0 0 0 0.2rem white,
                    inset 0 0 0 0.1rem #0000001c;
            }
            &:active {
                box-shadow: 0 0 0.5rem 0.2rem #fff4, 0 0 0 0.2rem #0004,
                    inset 0 0 0 0.1rem transparent;

                animation: click-blink-ani 0.2s;
                animation-fill-mode: forwards;
            }
        }

        &.in-settings {
            width: 19rem;
            height: 3.2rem;
            border-top-left-radius: 0;
            border-bottom-left-radius: 0;

            box-shadow: none;
            background-color: #d8cdb9;

            &:active {
                background-image: radial-gradient(
                    rgba(255, 242, 205, 0.9),
                    rgba(255, 223, 164, 0.5),
                    #d8cdb9,
                    #d8cdb9
                ) !important;
            }
        }
    }

    @keyframes click-blink-ani {
        from {
            background-color: #fff7cf;
            color: #d1b669;
        }
        to {
            background-color: #ece5d8dd;
            color: white;
        }
    }
</style>
