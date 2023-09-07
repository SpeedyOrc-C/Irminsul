<script lang="ts">
    import Background from "../../../../asset/img/ui/UI-Settings-Background.png";
    import Icon from "../../../../asset/img/ui/UI-Settings-Icon.png";
    import ButtonClose from "$lib/ui/Button/ButtonClose.svelte";
    import Language from "./Language.svelte";
    import SettingsCategories from "$lib/ui/Settings/SettingsCategories.svelte";
    import About from "./About/About.svelte";
    import File from "./File.svelte";
    import { _ } from "svelte-i18n";
    import Other from "./Other.svelte";
    import RelationGraph from "./RelationGraph.svelte";
    import {afterUpdate, createEventDispatcher} from "svelte";
    import type RelationGraphSettings from "../RelationGraphSettings";
    import type {ShowJoystick} from "../RelationGraphSettings";

    export let settings: RelationGraphSettings;
    export let show: boolean;

    export let showAxis: boolean;
    export let showGrid: boolean;
    export let showJoystick: ShowJoystick;
    export let joystickSensibility: number;

    const dispatch = createEventDispatcher();

    let displayed = false;
    let selectedCategory = "file";
    let reduceVisualEffect: boolean = settings.preference.reduce_visual_effect;

    function close() { show = false; }

    afterUpdate(() => {
        if (show)
            displayed = true;
        else
            setTimeout(() => {displayed = false}, 500);
    });

    function keyup(e: KeyboardEvent) {
        if (!show) return;
        if (e.code === "Escape") close();
    }
</script>

<svelte:window on:keyup={keyup} />

<!-- svelte-ignore a11y-click-events-have-key-events -->
<div id="settings" class="font-hywh-85w" class:show
    style:display={displayed ? "block" : "none"}
>
    <!-- This is slightly different to the background in game -->
    <img class="background" src={Background} alt="" />

    <div class="top-bar">
        <img class="icon" src={Icon} alt="" />

        <div class="button-close">
            <ButtonClose on:button-clicked={close} />
        </div>

        <div class="title">
            {$_("settings.self")}&nbsp;&nbsp;/&nbsp;&nbsp;{$_(`settings.category.${selectedCategory}`)}
        </div>
    </div>

    <SettingsCategories options={["file", "relation-graph", "language", "other", "about"]}
        bind:show
        bind:reduceVisualEffect
        bind:selectedCategory
    />

    <div class="selected-category">
        {#if selectedCategory === "file"}
            <File on:rg-action
                  on:import-json
                  on:export-json
                  on:export-haskell/>
        {/if}
        {#if selectedCategory === "relation-graph"}
            <RelationGraph {settings}
                           on:set-show-axis bind:showAxis
                           on:set-show-grid bind:showGrid
                           on:set-show-joystick bind:showJoystick
                           on:set-joystick-sensibility bind:joystickSensibility
                           on:set-reduce-visual-effect={e => {
                               reduceVisualEffect = e.detail;
                               dispatch("set-reduce-visual-effect", e.detail);
                           }}
            />
        {/if}
        {#if selectedCategory === "language"}
            <Language {settings}
                      on:set-language
                      on:set-who-am-i
            />
        {/if}
        {#if selectedCategory === "about"}
            <About />
        {/if}
        {#if selectedCategory === "other"}
            <Other on:reset-all />
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

    .background {
        position: absolute;
        transform: translate(-37%, -38%);

        -moz-user-select: none;
        -webkit-user-select: none;
        user-select: none;

        filter: blur(0.1rem);
        opacity: 5%;
    }

    .icon {
        position: absolute;
        display: block;

        -moz-user-select: none;
        -webkit-user-select: none;
        user-select: none;

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

        -webkit-user-select: none;
        -moz-user-select: none;
        user-select: none;
    }

    .top-bar {
        position: absolute;
        display: flex;
        width: 100%;
        height: 5.35rem;

        background-color: #0004;

        #settings > & {
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

        #settings.show > & {
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

    #settings {
        position: absolute;
        width: 100%;
        height: 100%;

        -webkit-backdrop-filter: blur(0.5rem);
        backdrop-filter: blur(0.5rem);

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

        @media print {
            display: none;
        }
    }

    .selected-category {
        position: absolute;

        left: 25vw;
        top: 6.5rem;
        right: 6vw;
        bottom: 3rem;

        overflow-y: auto;
        overflow-x: hidden;
    }
</style>
