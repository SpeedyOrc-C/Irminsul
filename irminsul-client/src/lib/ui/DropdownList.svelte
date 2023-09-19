<script lang="ts">
    import {afterUpdate, createEventDispatcher} from "svelte";
    import type { Option } from "../util/Option";

    export let options: Array<Option>;
    export let value: string;
    export let show: boolean;

    let below = true;
    let self: HTMLElement;

    const dispatch = createEventDispatcher();

    enum State { Hidden, Appearing, Shown, Disappearing }
    let state = State.Hidden;

    afterUpdate(() => {
        if (show)
            open();
        else
            close();
    });

    function open() {
        if (state == State.Hidden || state == State.Disappearing) {
            if (state == State.Hidden)
                below = self.getBoundingClientRect().bottom + 300 < document.body.getBoundingClientRect().bottom;
            state = State.Appearing;
            setTimeout(() => {
                if (state == State.Appearing) {
                    state = State.Shown;
                }
            }, 200);
        }
    }

    function close() {
        if (state == State.Shown || state == State.Appearing) {
            state = State.Disappearing;
            setTimeout(() => {
                if (state == State.Disappearing) {
                    state = State.Hidden;
                    below = true;
                }
            }, 200);
        }
    }

    function click(newValue: string) {
        show = false;
        if (value !== newValue) {
            dispatch("dropdown-list-change", newValue);
            value = newValue;
        }
    }

    function keydown(e: KeyboardEvent, newValue: string) {
        if (document.activeElement == e.target && (e.code == "Enter" || e.code == "Space")) {
            click(newValue);
        }
    }
</script>

<div class="dropdown-list" class:below bind:this={self}
     class:hidden={state === State.Hidden}
     class:appearing={state === State.Appearing}
     class:disappearing={state === State.Disappearing}
>
    {#each options as option}
        <!-- svelte-ignore a11y-click-events-have-key-events -->
        <div class="option" class:triggered={option.value === value}
            on:click|stopPropagation={() => click(option.value)}
        >
            <!-- svelte-ignore a11y-no-noninteractive-tabindex -->
            <div class="option-background" tabindex="0" on:keydown={e => keydown(e, option.value)} />
            <div class="option-label">{option.label}</div>
        </div>
    {/each}
</div>

<style lang="scss">
    .dropdown-list {
        position: absolute;
        box-sizing: border-box;
        width: 100%;
        padding: 0.1rem 0.3rem;

        top: unset;
        bottom: 3.5rem;

        border-radius: 1.3rem;
        max-height: calc(12 * (1.2rem + 2 * 0.5rem));
        overflow-y: auto;

        background-color: #495366;

        z-index: 1 !important;

        &.below {
            top: 3.2rem;
            bottom: unset;
        }

        animation-fill-mode: forwards;
        animation-duration: 0.2s;

        &.hidden {
            display: none;
        }
        &.appearing {
            @keyframes option-disappear {
                from { opacity: 100%; }
                to { opacity: 0%; }
            }
            animation-name: option-appear;
        }
        &.disappearing {
            @keyframes option-appear {
                from { opacity: 0%; }
                to { opacity: 100%; }
            }
            animation-name: option-disappear;
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
