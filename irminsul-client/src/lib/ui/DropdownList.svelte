<script lang="ts">
    import {afterUpdate, createEventDispatcher} from "svelte";
    import type { Option } from "../util/Option";

    export let options: Array<Option>;
    export let value: string;
    export let show: boolean;
    export let below: boolean;

    let displayed = false;

    const dispatch = createEventDispatcher();

    afterUpdate(() => {
        if (show)
            displayed = true;
        else
            setTimeout(() => displayed = false, 200);
    })

    function click(newValue: string) {
        if (value !== newValue) {
            dispatch("dropdown-list-change", newValue);
            value = newValue;
        }
        show = false;
    }
</script>

<div class="dropdown-list" class:below>
    <div id="options" class:show class:displayed>
        {#each options as option}
            <!-- svelte-ignore a11y-click-events-have-key-events -->
            <div class="option" class:triggered={option.value === value}
                on:click={() => click(option.value)}
            >
                <div class="option-background"></div>
                <div class="option-label">{option.label}</div>
            </div>
        {/each}
    </div>
</div>

<style lang="scss">
    .dropdown-list {
        position: absolute;
        width: 100%;

        top: unset;
        bottom: 3.5rem;

        z-index: 1 !important;

        &.below {
            top: 3.2rem;
            bottom: unset;
        }
    }
    #options {
        width: calc(100% - 2 * 0.3rem);

        padding: 0.1rem 0.3rem;
        border-radius: 1.3rem;
        max-height: calc(12 * (1.2rem + 2 * 0.5rem));
        overflow-y: auto;

        background-color: #495366;

        animation-fill-mode: forwards;

        animation: option-disappear 0.2s;
        &.show {
            animation: option-appear 0.2s;
        }

        display: none;
        &.displayed {
            display: block;
        }
    }

    @keyframes option-appear {
        from {
            opacity: 0%;
        }
        to {
            opacity: 100%;
        }
    }

    @keyframes option-disappear {
        from {
            opacity: 100%;
        }
        to {
            opacity: 0%;
        }
    }

    .option {
        position: relative;

        height: 1.6rem;
        padding: 0.5rem 0;
        margin: 0.2rem 0;

        & > .option-background {
            position: absolute;
            top: -0.2rem;

            height: 1.6rem;
            width: 100%;
            padding: 0.5rem 0;
            margin: 0.2rem 0;
            border-radius: 1.45rem;

            background-color: transparent;
            transform: scale(95%);

            transition-property: background-color, transform;
            transition-duration: 0.2s;
        }

        & > .option-label {
            position: absolute;
            top: -0.2rem;

            height: 1.6rem;
            line-height: 1.6rem;
            width: 100%;
            padding: 0.5rem 0;
            margin: 0.2rem 0;

            text-align: center;
            font-size: 1.3rem;

            -webkit-user-select: none;
            -moz-user-select: none;
            user-select: none;
            cursor: pointer;

            color: #ece4d8;
        }

        &.triggered, &:hover {
            & > .option-background {
                background-color: #606979;
                transform: scale(100%);
            }
        }

        &:active {
            & > .option-background {
                background-color: #ece5d8;
            }

            & > .option-label {
                color: #495366;
            }
        }
    }
</style>
