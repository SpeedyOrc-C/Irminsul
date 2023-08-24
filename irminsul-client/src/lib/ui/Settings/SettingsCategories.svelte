<script lang="ts">
    import SettingsItemCloudyBackground from "$lib/Canvas/SettingsItemCloudyBackground.svelte";
    import { createEventDispatcher } from "svelte";
    import { _ } from "svelte-i18n";

    export let options: Array<string>;
    export let reduceVisualEffect: boolean;
    export let show: boolean;
    export let selectedCategory: string;

    const dispatch = createEventDispatcher();
</script>

<div id="settings-categories" class:show>
    {#each options as option}
        {@const selected = option === selectedCategory}
        <!-- svelte-ignore a11y-click-events-have-key-events -->
        <!-- 小小的一个点竟然暗藏玄机，真有你的！ -->
        <div class="option" class:selected
            on:click={() => {
                selectedCategory = option;
                dispatch("settings-categories-change", {
                    category: selectedCategory
                });
            }}
        >
            {#if selected && !reduceVisualEffect}
                <SettingsItemCloudyBackground />
            {/if}

            <div class="option-background"></div>

            <div class="option-bullet">
                <div class="bullet-outer"></div>
                {#if selected}
                    <div class="bullet-arrow-with-background">
                        <div class="bullet-background"></div>
                        <div class="bullet-arrow"></div>
                    </div>
                {:else}
                    <div class="bullet-inner"></div>
                {/if}
            </div>

            <div class="option-label">{$_(`settings.category.${option}`)}</div>
        </div>
    {/each}
</div>

<style lang="scss">
    #settings-categories {
        position: absolute;
        top: 7rem;
        left: 5vw;

        -webkit-user-select: none;
        -moz-user-select: none;
        user-select: none;

        animation-fill-mode: forwards;

        & {
            animation-name: settings-categories-disappear;
            animation-duration: 0.5s;
            animation-timing-function: ease-out;
        }

        &.show {
            animation-name: settings-categories-appear;
            animation-duration: 0.2s;
            animation-timing-function: ease-in;
        }

        @keyframes settings-categories-appear {
            from { transform: translateX(-3rem); }
            to { transform: translateX(0); }
        }

        @keyframes settings-categories-disappear {
            from { transform: translateX(0); }
            to { transform: translateX(-3rem); }
        }
    }

    .option {
        position: relative;

        height: 3.2rem;
        line-height: 3.2rem;
        width: 20rem;
        margin-bottom: 0.7rem;
        border-radius: 3.2rem;

        cursor: pointer;

        & > .option-background {
            position: absolute;
            height: 3.2rem;
            width: 20rem;
            border-radius: 3.2rem;
        }

        & > .option-bullet {
            position: absolute;
            top: 1.6rem;
            left: 1.6rem;

            & > .bullet-outer {
                position: absolute;
                width: 1.2rem;
                height: 1.2rem;
                border: 0.2rem solid #ece4d800;

                transform: translate(-50%, -50%) rotate(45deg);

                transition-property: border, transform, width, height;
                transition-duration: 0.1s;
            }

            & > .bullet-inner {
                position: absolute;
                width: 0.65rem;
                height: 0.65rem;
                transform: translate(-50%, -50%) rotate(45deg);
                background-color: #ece4d899;
            }

            & > .bullet-arrow-with-background {
                position: absolute;
                transform: translate(-50%, -50%);

                width: 0;
                height: 0;

                & > .bullet-arrow {
                    position: absolute;
                    transform: translate(-50%, -50%);
                }

                & > .bullet-background {
                    position: absolute;
                    transform: translate(-50%, -50%);

                    width: 1rem;
                    height: 1rem;
                }
            }
        }

        & > .option-label {
            transform: translate(2.7rem, 0.03rem) scale(100%);
            font-size: 1.7rem;
            color: #ece4d8cc;

            transition-property: transform;
            transition-duration: 0.1s;
        }

        &:hover {
            & > .option-background {
                background-image: linear-gradient(
                    to right,
                    #ece4d818,
                    #ece4d818,
                    #ece4d804,
                    transparent
                );
            }

            & > .option-bullet {
                & > .bullet-outer {
                    width: 0.75rem;
                    height: 0.75rem;
                    border: 0.2rem solid #ece4d844;
                }

                & > .bullet-inner {
                    background-color: #ece4d8;
                }
            }
        }

        @keyframes option-background-blink {
            0% {
                background-image: linear-gradient(
                    to right,
                    #ece4d866,
                    #ece4d866,
                    #ece4d822,
                    transparent
                );
                opacity: 50%;
            }
            20% {
                background-image: linear-gradient(
                    to right,
                    #ece4d866,
                    #ece4d866,
                    #ece4d822,
                    transparent
                );
                opacity: 100%;
            }
            100% {
                background-image: linear-gradient(
                    to right,
                    #ece4d866,
                    #ece4d866,
                    #ece4d822,
                    transparent
                );
                opacity: 0%;
            }
        }

        &:active {
            & > .option-background {
                animation: option-background-blink 0.3s;
                animation-fill-mode: forwards;
            }
        }

        &.selected {
            & > .option-background {
                animation: none;
                background-image: none;
            }

            & > .option-label {
                transform: translate(5.2rem, 0.03rem) scale(120%);
                color: #ece4d8;
            }

            & > .option-bullet {
                & > .bullet-outer {
                    width: 0.9rem;
                    height: 0.9rem;
                    border: 0.2rem solid #ece4d844;
                }

                & > .bullet-inner {
                    background-color: transparent;
                }

                & > .bullet-arrow-with-background {
                    animation-name: bullet-arrow-horizontal-ani;
                    animation-duration: 1s;
                    animation-timing-function: ease-in-out;
                    animation-iteration-count: infinite;

                    & > .bullet-arrow {
                        // An arrow to the right
                        border-left: 0.45rem solid #ece4d8;
                        border-top: 0.45rem solid transparent;
                        border-bottom: 0.45rem solid transparent;
                    }
                    & > .bullet-background {
                        background-image: radial-gradient(
                            #ece4d844,
                            #ece4d822,
                            #ece4d811,
                            #ece4d800,
                            #ece4d800
                        );
                    }
                }
                @keyframes bullet-arrow-horizontal-ani {
                    0% {
                        transform: translate(calc(-50% + 0.15rem), -50%);
                    }
                    50% {
                        transform: translate(calc(-50% + 0.45rem), -50%);
                    }
                    100% {
                        transform: translate(calc(-50% + 0.15rem), -50%);
                    }
                }
            }
        }
    }
</style>
