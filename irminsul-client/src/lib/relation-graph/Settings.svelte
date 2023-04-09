<script lang="ts">
    import ButtonClose from "$lib/ui/Button/ButtonClose.svelte";
    import Language from "$lib/relation-graph/Settings/Language.svelte";
    import SettingsCategories from "$lib/ui/Settings/SettingsCategories.svelte";
    import type { Writable } from "svelte/store";
    import About from "./Settings/About.svelte";
    import File from "./Settings/File.svelte";

    export let showW: Writable<boolean>;
    export let langW: Writable<string>;

    let show: boolean = false;
    let displayed: boolean = false;
    let selectedCategory: string = "file";

    showW.subscribe((newValue) => {
        show = newValue;
        if (show) {
            displayed = true;
        } else {
            setTimeout(() => {
                displayed = false;
            }, 500);
        }
    });

    function handleKeyup(e: KeyboardEvent) {
        if (!show) return;

        switch (e.code) {
            case "Escape":
                showW.set(false);
                break;
        }
    }

    function close() {
        showW.set(false);
    }
</script>

<svelte:window on:keyup={handleKeyup} />

<!-- svelte-ignore a11y-click-events-have-key-events -->
<div
    class="settings font-hywh-65w"
    class:show
    style:display={displayed ? "block" : "none"}
>
    <div class="top-bar">
        <div class="button-close" on:click={close}>
            <ButtonClose />
        </div>
    </div>

    <div class="settings-categories">
        <SettingsCategories
            options={[
                "file",
                // "view",
                "language",
                // "key-bindings",
                "about"
            ]}
            on:settings-categories-change={(e) =>
                (selectedCategory = e.detail.category)}
        />
    </div>

    <div class="selected-category">
        {#if selectedCategory === "file"}
            <File on:rg-action />
        {/if}
        {#if selectedCategory === "about"}
            <About />
        {/if}
        {#if selectedCategory === "language"}
            <Language {langW} on:rg-action/>
        {/if}
    </div>
</div>

<style lang="scss">
    %ease-out-expo {
        transition-timing-function: cubic-bezier(0.19, 1, 0.22, 1);
    }

    %ease-in-expo {
        transition-timing-function: cubic-bezier(0.95, 0.05, 0.795, 0.035);
    }

    .top-bar {
        position: absolute;
        display: flex;
        width: 100%;
        height: 5.5rem;

        background-color: #0004;

        .settings > & {
            animation-name: top-bar-disappear-ani;
            animation-duration: 0.5s;
            @extend %ease-in-expo;
            @keyframes top-bar-disappear-ani {
                from {
                    transform: translateY(0%);
                }
                to {
                    transform: translateY(-100%);
                }
            }
        }

        .settings.show > & {
            animation-name: top-bar-appear-ani;
            animation-duration: 0.3s;
            @extend %ease-out-expo;
            @keyframes top-bar-appear-ani {
                from {
                    transform: translateY(-100%);
                }
                to {
                    transform: translateY(0%);
                }
            }
        }

        & > .button-close {
            position: absolute;
            top: 1.5rem;
            right: 3rem;
        }
    }

    .settings {
        position: absolute;
        width: 100%;
        height: 100%;

        backdrop-filter: blur(0.5rem);
        -webkit-backdrop-filter: blur(0.5rem);

        animation: settings-disappear-ani 0.5s;
        animation-fill-mode: forwards;

        &.show {
            animation: settings-appear-ani 0.3s;
            animation-fill-mode: forwards;
        }

        @keyframes settings-appear-ani {
            from {
                background-color: #6668;
                opacity: 0%;
            }
            to {
                background-color: #6668;
                opacity: 100%;
            }
        }

        @keyframes settings-disappear-ani {
            from {
                background-color: #6668;
                opacity: 100%;
            }
            to {
                background-color: #6668;
                opacity: 0%;
            }
        }
    }

    .selected-category {
        position: absolute;
        min-width: 30rem;

        left: 26.5rem;
        top: 6.5rem;
        right: 6.5rem;
        bottom: 3rem;

        overflow-y: auto;
        overflow-x: hidden;
    }

    .settings-categories {
        animation-fill-mode: forwards;

        .settings > & {
            animation-name: settings-categories-disappear;
            animation-duration: 0.5s;
            animation-timing-function: ease-out;
            @keyframes settings-categories-disappear {
                from {
                    transform: translateX(0);
                }
                to {
                    transform: translateX(-3rem);
                }
            }
        }
        .settings.show > & {
            animation-name: settings-categories-appear;
            animation-duration: 0.2s;
            animation-timing-function: ease-in;
            @keyframes settings-categories-appear {
                from {
                    transform: translateX(-3rem);
                }
                to {
                    transform: translateX(0);
                }
            }
        }
    }

    :global(.sub-category) {
        margin: 1rem 0rem;

        font-size: 1.5rem;
        color: white;
    }
</style>
