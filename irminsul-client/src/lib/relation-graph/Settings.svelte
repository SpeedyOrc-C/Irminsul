<script lang="ts">
    import Background from "../../asset/img/ui/UI-Settings-Background.png";
    import Icon from "../../asset/img/ui/UI-Settings-Icon.png";
    import ButtonClose from "$lib/ui/Button/ButtonClose.svelte";
    import Language from "$lib/relation-graph/Settings/Language.svelte";
    import SettingsCategories from "$lib/ui/Settings/SettingsCategories.svelte";
    import type { Writable } from "svelte/store";
    import About from "./Settings/About.svelte";
    import File from "./Settings/File.svelte";
    import { _ } from "svelte-i18n";
    import Other from "./Settings/Other.svelte";
    import Graphics from "./Settings/Graphics.svelte";

    export let showW: Writable<boolean>;
    export let langW: Writable<string>;
    export let reduceVisualEffectW: Writable<string>;

    let show: boolean = false;
    let displayed: boolean = false;
    let selectedCategory: string = "file";

    showW.subscribe((newValue) => {
        show = newValue;
        if (show) {
            displayed = true;
        } else {
            setTimeout(() => (displayed = false), 500);
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

{#if displayed}
    <!-- svelte-ignore a11y-click-events-have-key-events -->
    <div
        class="settings font-hywh-85w"
        class:show
        style:display={displayed ? "block" : "none"}
    >
        <!-- This is slightly different to the background in game -->
        <img class="background" src={Background} alt="" />

        <div class="top-bar">
            <img class="icon" src={Icon} alt="" />

            <div class="button-close" on:click={close}>
                <ButtonClose />
            </div>

            <div class="title">
                {$_("settings.self")}&nbsp;&nbsp;/&nbsp;&nbsp;{$_(
                    `settings.category.${selectedCategory}`
                )}
            </div>
        </div>

        <div class="settings-categories">
            <SettingsCategories
                options={["file", "graphics", "language", "other", "about"]}
                on:settings-categories-change={(e) =>
                    (selectedCategory = e.detail.category)}
                {selectedCategory}
                {reduceVisualEffectW}
            />
        </div>

        <div class="selected-category">
            {#if selectedCategory === "file"}
                <File on:rg-action />
            {/if}
            {#if selectedCategory === "graphics"}
                <Graphics {reduceVisualEffectW} />
            {/if}
            {#if selectedCategory === "language"}
                <Language {langW} on:rg-action />
            {/if}
            {#if selectedCategory === "about"}
                <About />
            {/if}
            {#if selectedCategory === "other"}
                <Other />
            {/if}
        </div>
    </div>
{/if}

<style lang="scss">
    %ease-out-expo {
        transition-timing-function: cubic-bezier(0.19, 1, 0.22, 1);
    }

    %ease-in-expo {
        transition-timing-function: cubic-bezier(0.95, 0.05, 0.795, 0.035);
    }

    .background {
        position: absolute;
        transform: translate(-37%, -38%);

        filter: blur(0.1rem);
        opacity: 5%;
    }

    .icon {
        position: absolute;
        display: block;

        top: 1.2rem;
        left: 2.4rem;
        height: 3rem;
    }

    .title {
        position: absolute;

        top: 50%;
        transform: translate(8rem, -50%);

        font-size: 1.3rem;
        color: #d3bc8e;

        user-select: none;
        -webkit-user-select: none;
        -moz-user-select: none;
    }

    .top-bar {
        position: absolute;
        display: flex;
        width: 100%;
        height: 5.35rem;

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
</style>
