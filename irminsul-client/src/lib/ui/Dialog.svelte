<script lang="ts">
    import type { Writable } from "svelte/store";
    import PromptCorner from "./Dialog/DialogCorner.svelte";
    import {onMount} from "svelte";

    export let title: string;
    export let show: Writable<boolean>;

    let displayed = false;

    onMount(() => {
        show.subscribe(() => {
            if ($show) {
                displayed = true;
            } else {
                setTimeout(() => (displayed = false), 200);
            }
        });
    })
</script>

<div
    class="background"
    class:show={$show}
    style:display={displayed ? "block" : "none"}></div>

<div
    class="prompt"
    class:show={$show}
    style:display={displayed ? "block" : "none"}
>
    <div style="position: relative">
        <div class="decoration">
            <div class="inner-line-top"></div>
            <div class="inner-line-bottom"></div>
            <div class="inner-line-left"></div>
            <div class="inner-line-right"></div>

            <div class="corner-upper-left"><PromptCorner /></div>
            <div class="corner-upper-right"><PromptCorner /></div>
            <div class="corner-lower-left"><PromptCorner /></div>
            <div class="corner-lower-right"><PromptCorner /></div>
        </div>

        <div class="content">
            <div class="title font-hywh-85w">{@html title}</div>
            <div class="body font-hywh-65w">
                <slot />
            </div>
        </div>
    </div>
</div>

<style lang="scss">
    .prompt {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);

        width: 35rem;
        height: 25rem;
        border-radius: 1rem;

        background-color: #3e4457;
        border: 0.3rem solid #393e52;

        animation: prompt-disappear 0.2s ease-out;
        animation-fill-mode: forwards;

        &.show {
            animation: prompt-appear 0.2s ease-out;
        }

        @keyframes prompt-appear {
            0% {
                opacity: 0;
                transform: translate(-50%, -50%) scale(95%);
            }
            100% {
                opacity: 1;
                transform: translate(-50%, -50%) scale(100%);
            }
        }

        @keyframes prompt-disappear {
            0% {
                opacity: 1;
                transform: translate(-50%, -50%) scale(100%);
            }
            100% {
                opacity: 0;
                transform: translate(-50%, -50%) scale(95%);
            }
        }
    }


    .title {
        text-align: center;
        font-size: 1.8rem;
        margin: 1rem 0;
        color: #d2bc8d;
    }

    .background {
        position: absolute;
        width: 100vw;
        height: 100vh;

        background-color: #0006;

        animation: background-disappear 0.2s ease-out;
        animation-fill-mode: forwards;

        &.show {
            animation: background-appear 0.2s ease-out;
        }
        @keyframes background-appear {
            0% {
                opacity: 0;
            }
            100% {
                opacity: 1;
            }
        }

        @keyframes background-disappear {
            0% {
                opacity: 1;
            }
            100% {
                opacity: 0;
            }
        }
    }


    .content {
        position: absolute;
        width: calc(35rem - 2 * 0.3rem);
        height: calc(25rem - 2 * 0.3rem);
        margin: 0.3rem;
    }

    .body {
        height: 19rem;
        margin: 0 3rem;
        overflow: auto;

        font-size: 1.2rem;
        color: #f2ebdd;
    }

    :global(em) {
        color: #d2bc8d;
        font-style: normal;
    }

    .decoration {
        position: absolute;
        width: 35rem;
        height: 25rem;

        & > div {
            position: absolute;
        }

        %inner-line {
            background-color: #51545d;
        }

        %inner-line-horizontal {
            @extend %inner-line;
            height: 0.11rem;
            width: calc(100% - 2.7rem);
        }
        .inner-line-top {
            @extend %inner-line-horizontal;
            top: 0.25rem;
            left: 1.35rem;
        }
        .inner-line-bottom {
            @extend %inner-line-horizontal;
            bottom: 0.25rem;
            left: 1.35rem;
        }

        %inner-line-vertical {
            @extend %inner-line;
            height: calc(100% - 2.7rem);
            width: 0.11rem;
        }
        .inner-line-left {
            @extend %inner-line-vertical;
            top: 1.35rem;
            left: 0.25rem;
        }
        .inner-line-right {
            @extend %inner-line-vertical;
            top: 1.35rem;
            right: 0.25rem;
        }

        .corner-upper-left {
            top: -0.3rem;
            left: -0.3rem;
            transform: translate(-0.35rem, -0.35rem);
        }
        .corner-upper-right {
            top: -0.3rem;
            right: -0.3rem;
            transform: translate(0.35rem, -0.35rem) rotateY(180deg);
        }
        .corner-lower-left {
            bottom: -0.3rem;
            left: -0.3rem;
            transform: translate(-0.35rem, 0.35rem) rotateX(180deg);
        }
        .corner-lower-right {
            bottom: -0.3rem;
            right: -0.3rem;
            transform: translate(0.35rem, 0.35rem) rotateX(180deg)
                rotateY(180deg);
        }
    }
</style>
