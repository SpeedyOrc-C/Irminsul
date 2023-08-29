<script lang="ts">
    import Atom from "./Atom.svelte";
    import Axis from "./Axis.svelte";
    import Cluster from "./Cluster.svelte";
    import Grid from "./Grid.svelte";
    import RelationBetween from "./RelationBetween/RelationBetween.svelte";
    import type {RelationGraph} from "../RelationGraph";
    import {createEventDispatcher} from "svelte";
    import ViewController from "../ViewController";
    import Editor from "./Editor";
    import RootCluster from "./RootCluster.svelte";
    import type RelationGraphSettings from "../RelationGraphSettings";

    const dispatch = createEventDispatcher();

    export let relationGraph: RelationGraph;
    export let view: ViewController;
    export let showGrid: boolean;
    export let showAxis: boolean;
    export let showCoordinates: boolean;
    export let editor: Editor;
    export let settings: RelationGraphSettings;
    export let hideUi: boolean;

    function keydown(e: KeyboardEvent) {
        function isSelecting(): boolean {
            if (e.ctrlKey || e.metaKey) {
                if (e.code == "KeyA") {
                    if (editor.isEditing()) {
                        if (e.shiftKey) {
                            if (!e.altKey) {
                                editor.reverseSelect(); // CTRL + SHIFT + A
                            } else {
                                return false;
                            }
                        } else {
                            if (e.altKey) {
                                editor.deselectAll(); // CTRL + ALT + A
                            } else {
                                editor.selectAll(); // CTRL + A
                            }
                        }
                        e.preventDefault();
                        editor = editor;
                        return true;
                    }
                    return false;
                }
                return false;
            }
            return false;
        }

        function isEditing(): boolean {
            if (!editor.isEditing()) return false;
            if (e.ctrlKey || e.metaKey) return false;

            const entityMove = Editor.moveDeadKeyMultiplier(e);
            switch (e.code) {
                case "KeyI": editor.moveSelectedElements(0, entityMove); break;
                case "KeyK": editor.moveSelectedElements(0, -entityMove); break;
                case "KeyJ": editor.moveSelectedElements(-entityMove, 0); break;
                case "KeyL": editor.moveSelectedElements(entityMove, 0); break;

                case "ArrowUp": editor.moveSelectedClustersAnchors(0,entityMove); break;
                case "ArrowDown": editor.moveSelectedClustersAnchors(0, -entityMove); break;
                case "ArrowLeft": editor.moveSelectedClustersAnchors(-entityMove, 0); break;
                case "ArrowRight": editor.moveSelectedClustersAnchors(entityMove, 0); break;

                default: return false;
            }
            e.preventDefault();
            editor = editor;
            return true;
        }

        function isMoving(): boolean {
            if (e.ctrlKey || e.metaKey) return false;

            const viewMoveMultiplier = ViewController.moveDeadKeyMultiplier(e);
            const viewRotateMultiplier = ViewController.rotateDeadKeyMultiplier(e);
            switch (e.code) {
                case "KeyW": view.moveDelta(0, 7.5 * viewMoveMultiplier); break;
                case "KeyS": view.moveDelta(0, -7.5 * viewMoveMultiplier); break;
                case "KeyA": view.moveDelta(-7.5 * viewMoveMultiplier, 0); break;
                case "KeyD": view.moveDelta(7.5 * viewMoveMultiplier, 0); break;

                case "KeyQ": view.rotateDelta(-45 * viewRotateMultiplier); break;
                case "KeyE": view.rotateDelta(45 * viewRotateMultiplier); break;

                case "Minus": view.zoomOut(); break;
                case "Equal": view.zoomIn(); break;
                case "Digit0": view.reset(); break;

                case "KeyX": dispatch("set-show-axis", !showAxis); break;
                case "KeyG": dispatch("set-show-grid", !showGrid); break;
                case "KeyC": showCoordinates = !showCoordinates; break;

                default: return false;
            }
            e.preventDefault();
            view = view;
            return true;
        }

        isSelecting() || isEditing() || isMoving();
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

    function clickBackground() {
        hideUi = false;
        deselectAll();
    }
</script>

<svelte:window on:keydown={keydown}/>

<div id="view-graphical">
    <div id="deselect-all-touch-area" on:click={clickBackground}></div>
    <div id="origin"
         style:transform="rotate({-view.angle}deg) scale({view.scale * 100}%) translate({view.x}rem, {-view.y}rem)"
    >
        {#if showGrid} <Grid/> {/if}
        {#if showAxis} <Axis/> {/if}

        {#each relationGraph.relationsBetween as relationBetween}
            <RelationBetween {editor} {relationBetween} {settings}/>
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