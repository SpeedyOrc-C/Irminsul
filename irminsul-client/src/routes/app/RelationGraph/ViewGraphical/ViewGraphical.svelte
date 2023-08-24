<script lang="ts">
    import Atom from "./Atom.svelte";
    import Axis from "./Axis.svelte";
    import Cluster from "./Cluster.svelte";
    import Grid from "./Grid.svelte";
    import RelationBetween from "./RelationBetween.svelte";
    import type {RelationGraph} from "../RelationGraph";
    import {createEventDispatcher} from "svelte";
    import type ViewController from "../ViewController";
    import {deadKeyMultiplier} from "$lib/util/DeadKeyMultiplier";
    import type Editor from "./Editor";
    import RootCluster from "./RootCluster.svelte";

    const dispatch = createEventDispatcher();

    export let relationGraph: RelationGraph;
    export let view: ViewController;
    export let showGrid: boolean;
    export let showAxis: boolean;
    export let showCoordinates: boolean;
    export let editor: Editor;

    function keydown(e: KeyboardEvent) {
        if (e.ctrlKey !== e.metaKey) {
            if (e.code === "KeyA") {
                e.preventDefault();
                if (e.shiftKey) {
                    if (!e.altKey) {
                        editor.reverseSelect();
                    }
                } else {
                    if (e.altKey) {
                        editor.deselectAll();
                    } else {
                        editor.selectAll();
                    }
                }
            }

            editor = editor;
            return;
        }

        const distance = deadKeyMultiplier(e);

        switch (e.code) {
            case "KeyW": view.moveUp(); break;
            case "KeyS": view.moveDown(); break;
            case "KeyA": view.moveLeft(); break;
            case "KeyD": view.moveRight(); break;
            case "KeyQ": view.rotateClockwise(); break;
            case "KeyE": view.rotateAnticlockwise(); break;
            case "Minus": view.zoomOut(); break;
            case "Equal": view.zoomIn(); break;
            case "Digit0": view.reset(); break;

            case "KeyX": dispatch("set-show-axis", !showAxis); break;
            case "KeyG": dispatch("set-show-grid", !showGrid); break;
            case "KeyC": showCoordinates = !showCoordinates; break;

            default: if (!editor.isEditing()) return;
                switch (e.code) {
                    case "KeyI": editor.moveSelectedElements({x: 0, y: distance}); break;
                    case "KeyK": editor.moveSelectedElements({x: 0, y: -distance}); break;
                    case "KeyJ": editor.moveSelectedElements({x: -distance, y: 0}); break;
                    case "KeyL": editor.moveSelectedElements({x: distance, y: 0}); break;

                    case "ArrowUp": editor.moveSelectedClustersAnchors({x: 0, y: distance}); break;
                    case "ArrowDown": editor.moveSelectedClustersAnchors({x: 0, y: -distance}); break;
                    case "ArrowLeft": editor.moveSelectedClustersAnchors({x: -distance, y: 0}); break;
                    case "ArrowRight": editor.moveSelectedClustersAnchors({x: distance, y: 0}); break;

                    default: return;
                }
        }

        view = view;
        editor = editor;
    }

    function toggleAtom(id: string) {
        editor.toggleAtom(id);
        editor = editor;
    }

    function toggleCluster(id: string) {
        editor.toggleCluster(id);
        editor = editor;
    }

    function toggleRoot() {
        editor.toggleRoot();
        editor = editor;
    }

    function deselectAll() {
        editor.deselectAll();
        editor = editor;
    }
</script>

<svelte:window on:keydown={keydown}/>

<div id="view-graphical">
    <div id="deselect-all-touch-area" on:click={deselectAll}></div>
    <div id="origin"
         style:transform="rotate({-view.angle}deg) scale({view.scale * 100}%) translate({view.x}rem, {-view.y}rem)"
    >
        {#if showGrid} <Grid/> {/if}
        {#if showAxis} <Axis/> {/if}

        {#each relationGraph.relationsBetween as relationBetween}
            <RelationBetween {editor} {relationBetween}/>
        {/each}

        {#each relationGraph.clusters as cluster}
            <Cluster id={cluster.id} label={cluster.translation} size={cluster.size}
                     on:toggle-cluster={e => toggleCluster(e.detail)}
                     on:jump-to
                     {showCoordinates} {editor}
            />
        {/each}

        {#each relationGraph.atoms as atom}
            <Atom id={atom.id} label={atom.translation}
                  on:toggle-atom={e => toggleAtom(e.detail)}
                  {editor} {showCoordinates}
            />
        {/each}

        <RootCluster on:toggle-root={toggleRoot} {editor} />
    </div>
</div>

<style lang="scss">
    #view-graphical {
        position: absolute;
        top: 0;
        left: 0;
        height: 100vh;
        width: 100vw;

        overflow: hidden;
    }

    #deselect-all-touch-area {
        position: absolute;
        width: 100%; height: 100%;
    }

    #origin {
        position: absolute;
        top: 50%; left: 50%;

        @media (pointer: coarse) {
            transition-duration: 0.1s;
            transition-timing-function: linear;
        }

        transition-property: transform, opacity;
        transition-duration: 0.2s;
    }
</style>