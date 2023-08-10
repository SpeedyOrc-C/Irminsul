<script lang="ts">
    import RootCluster_Background from "../../asset/img/ui/RootCluster-Background.png";
    import Atom from "./Atom.svelte";
    import Cluster from "./Cluster.svelte";
    import {
        dumpRelationGraph2Haskell,
        type RelationGraph,
    } from "../../model/RelationGraph";
    import RelationBetween from "./RelationBetween.svelte";
    import { Vector2Zero, type Vector2 } from "../util/Vector2";
    import Axis from "./Axis.svelte";
    import Grid from "./Grid.svelte";
    import { saveStringAsFile } from "$lib/util/String";
    import Panel from "./Panel.svelte";
    import { onMount } from "svelte";
    import { deadKeyMultiplier } from "$lib/util/DeadKeyMultiplier";
    import Settings from "./Settings.svelte";
    import { writable, type Writable } from "svelte/store";
    import DialogOk from "$lib/ui/Dialog/DialogOk.svelte";
    import { _ } from "svelte-i18n";
    import {RelationGraphLoader} from "$lib/relation-graph/RelationGraphLoader";
    import ViewController from "$lib/relation-graph/ViewController";
    import Joystick from "$lib/relation-graph/Joystick.svelte";

    export let id: string;
    export let lang: Writable<string>;
    export let reduceVisualEffect: Writable<string>;
    export let whoAmI: Writable<"aether" | "lumine">;

    let view = new ViewController();

    const loader = new RelationGraphLoader();

    let relationGraph: RelationGraph | null = null;
    let jsonFileInput: HTMLInputElement;
    let jsonFileReader: FileReader;

    let entityAnchor: Map<string, Vector2> = new Map();

    let showAxis: Writable<boolean> = writable(false);
    let showGrid: Writable<boolean> = writable(false);
    let showCoordinate = false;
    let showSettings: Writable<boolean> = writable(false);

    let contentOpacity = 1;

    let selectedAtoms: Set<string> = new Set();
    let selectedClusters: Set<string> = new Set();
    let selectedEntities: Set<string> = new Set();
    let selectedEntitiesInSelectedClusters: Set<string> = new Set();

    let rootClusterSelected = false;

    let showLayoutMissing: Writable<boolean> = writable(false);

    function updateView() { view = view; }

    function updateSelectedAtoms(e: CustomEvent) {
        selectedAtoms = e.detail.atoms;
        selectedEntities = new Set([...selectedAtoms, ...selectedClusters]);
    }

    function updateSelectedClusters(e: CustomEvent) {
        if (relationGraph == null) return;

        selectedClusters = e.detail.clusters;
        selectedEntities = new Set([...selectedAtoms, ...selectedClusters]);
        selectedEntitiesInSelectedClusters = new Set(
            relationGraph.clusters
                .filter(cluster => selectedClusters.has(cluster.id))
                .flatMap(cluster => cluster.elements) ?? []
        );
    }

    function toggleRootClusterSelect() {
        rootClusterSelected = !rootClusterSelected;
    }

    function keydownListener(e: KeyboardEvent) {
        if (relationGraph == null) return;

        if (e.ctrlKey !== e.metaKey) return;

        if (["KeyW", "KeyS", "KeyA", "KeyD", "Minus", "Equal", "KeyQ", "KeyE", "Digit0"].find(v => v === e.code)) {
            view.keyboardEvent(e);
            view = view;
        } else if (e.code === "KeyX") {
            showAxis.set(!$showAxis);
        } else if (e.code === "KeyG") {
            showGrid.set(!$showGrid);
        } else if (e.code === "KeyC") {
            showCoordinate = !showCoordinate;
        } else if (!rootClusterSelected) {
            return;
        } else if (e.code === "KeyI") {
            relationGraph.rootPosition.y += deadKeyMultiplier(e);
        } else if (e.code === "KeyK") {
            relationGraph.rootPosition.y -= deadKeyMultiplier(e);
        } else if (e.code === "KeyJ") {
            relationGraph.rootPosition.x -= deadKeyMultiplier(e);
        } else if (e.code === "KeyL") {
            relationGraph.rootPosition.x += deadKeyMultiplier(e);
        }
    }

    async function loadRelationGraph(loadId: string) {
        console.info("Loading relation graph:", loadId);

        let json: RelationGraph;
        try {
            json = await loader.load(loadId, $lang, $whoAmI);
        } catch (e) {
            console.warn("Failed to load relation graph, error:", e);
            switch (e) {
                case "LayoutMissing":
                    showLayoutMissing.set(true);
                    break;
                default:
                    break;
            }
            return;
        }

        contentOpacity = 0;
        relationGraph = json;
        selectedEntities.clear();
        selectedAtoms.clear();
        selectedClusters.clear();
        updateEntityAnchor();
        view.reset();
        id = loadId;

        window.history.replaceState(undefined, "", `/relation-graph/?id=${id}&lang=${$lang}&who-am-i=${$whoAmI}`);

        console.info("Relation graph loaded: ", json);
        contentOpacity = 1;
    }

    function updateEntityAnchor() {
        // Entities' anchors are updated. It makes relations update their positions,
        // so that they are always connected to the entities.
        if (relationGraph == null) return;

        entityAnchor.set(relationGraph.id, relationGraph.rootPosition);
        relationGraph.atoms.forEach((atom) =>
            entityAnchor.set(atom.id, atom.position)
        );
        relationGraph.clusters.forEach((cluster) =>
            entityAnchor.set(cluster.id, cluster.anchor)
        );
    }

    function exportHaskell() {
        if (relationGraph == null) {
            console.error("No relation graph opened.");
            return;
        }
        console.info("Saving as Haskell...");

        let fileName = `${relationGraph.id}-Haskell.txt`;
        saveStringAsFile(fileName, dumpRelationGraph2Haskell(relationGraph));
    }

    function exportJson() {
        if (relationGraph == null) {
            console.error("No relation graph opened.")
            return;
        }
        console.info("Saving as JSON...");

        let fileName = `${relationGraph.id}-JSON.json`;
        saveStringAsFile(fileName, JSON.stringify(relationGraph, null, 4));
    }

    function importJson() {
        console.info("Trying to import from JSON...");
        jsonFileInput.click();
    }

    function openSettings() {
        showSettings.set(true);
    }

    function handleRgAction(e: CustomEvent) {
        switch (e.detail.action) {
            case "import-json":
                importJson();
                break;
            case "export-json":
                exportJson();
                break;
            case "export-haskell":
                exportHaskell();
                break;
            case "jump-to":
                loadRelationGraph(e.detail.id);
                break;
            case "update-selected-atoms":
                updateSelectedAtoms(e);
                break;
            case "update-selected-clusters":
                updateSelectedClusters(e);
                break;
            case "change-lang":
                lang.set(e.detail.lang);
                break;
            case "open-settings":
                openSettings();
                break;
            default:
                console.error("Unknown action:", e.detail);
        }
    }

    function changeLanguage() {
        loadRelationGraph(id);
    }

    function changeWhoAmI() {
        loader.clearCache();
        loadRelationGraph(id);
    }

    onMount(() => {
        jsonFileReader = new FileReader();

        jsonFileInput.addEventListener("change", () => {
            if (jsonFileInput.files?.length == 0) {
                console.error("No import file selected.");
                return;
            }

            const files = jsonFileInput.files;

            if (files?.length > 0) {
                const file = files[0];
                jsonFileReader.readAsText(file);
            }
        });

        jsonFileReader.addEventListener(
            "load",
            (event: ProgressEvent<FileReader>) => {
                const text = event.target?.result as string;
                try {
                    const layout = JSON.parse(text);
                    relationGraph = layout;
                    console.info("Relation graph updated with", layout);
                    updateEntityAnchor();
                } catch (e) {
                    console.error(
                        "Failed to parse JSON file.",
                        event.target?.result
                    );
                }
            }
        );
        loadRelationGraph(id);
    });
</script>

<!-- svelte-ignore missing-declaration -->
<svelte:window on:keydown={keydownListener} />

<title>
    Irminsul
    {#if relationGraph != null}
        - {relationGraph.rootTranslation}
    {/if}
</title>

<div class="relation-graph">
    <div
        class="content"
        style:transform="rotate({-view.angle}deg) scale({view.scale * 100}%)
        translate({view.x}rem, {-view.y}rem)"
        style:opacity={contentOpacity}
    >
        {#if $showGrid} <Grid /> {/if}
        {#if $showAxis} <Axis /> {/if}

        {#if relationGraph != null}
            {#key relationGraph}
                {#each relationGraph.relationsBetween as relationBetween}
                    {@const highlight =
                        selectedEntities.has(relationBetween.subjectId) ||
                        selectedEntities.has(relationBetween.objectId)}
                    {@const dim = !highlight && selectedEntities.size > 0}
                    <RelationBetween
                        forwardRelations={relationBetween.forwardRelations}
                        backwardRelations={relationBetween.backwardRelations}
                        biRelations={relationBetween.biRelations}
                        subjectAnchor={entityAnchor.get(relationBetween.subjectId) ?? Vector2Zero}
                        objectAnchor={entityAnchor.get(relationBetween.objectId) ?? Vector2Zero}
                        {highlight}
                        {dim}
                    />
                {/each}

                {#each relationGraph.clusters as cluster}
                    <Cluster {...cluster} {showCoordinate} on:rg-action={handleRgAction} />
                {/each}

                {#each relationGraph.atoms as atom}
                    {@const dim =
                        !selectedAtoms.has(atom.id) &&
                        selectedEntities.size > 0 &&
                        !selectedEntitiesInSelectedClusters.has(atom.id)}
                    <Atom {...atom} {showCoordinate} on:rg-action={handleRgAction} {dim}/>
                {/each}

                <!-- svelte-ignore a11y-click-events-have-key-events -->
                <div
                    class="root-cluster"
                    class:selected={rootClusterSelected}
                    style:left="{relationGraph.rootPosition.x}rem"
                    style:top="{-relationGraph.rootPosition.y}rem"
                    on:click={toggleRootClusterSelect}
                >
                    <img class="root-cluster-background" src={RootCluster_Background} alt=""/>
                    <div class="translation font-hywh-85w">
                        {relationGraph.rootTranslation}
                    </div>
                </div>
            {/key}
        {/if}
    </div>

    <Joystick callback={view.joystickEvent} on:update-view={updateView}/>

    <Panel on:rg-action={handleRgAction} {relationGraph} {id}/>

    <DialogOk title={$_("error.layout-missing.self")} show={showLayoutMissing}>
        {$_("error.layout-missing.detail")}
    </DialogOk>

    <Settings show={showSettings} on:rg-action={handleRgAction}
        {lang} {changeLanguage}
        {whoAmI} {changeWhoAmI}
        {reduceVisualEffect}
        {showAxis} {showGrid}
    />

    <input type="file" bind:this={jsonFileInput} style:display="none" />
</div>

<style lang="scss">
    .content {
        position: absolute;
        top: 50%;
        left: 50%;

        @media (pointer: coarse) {
            transition-duration: 0.1s;
            transition-timing-function: linear;
        }

        transition-property: transform, opacity;
        transition-duration: 0.2s;
    }

    .root-cluster {
        position: absolute;
        transform: translate(-50%, -50%);

        z-index: 10000;

        user-select: none;
        -webkit-user-select: none;
        -moz-user-select: none;

        &.selected {
            filter: drop-shadow(0 0 1rem orange);
        }
    }

    .translation {
        position: absolute;
        transform: translate(-50%, -50%);

        width: 16rem;
        text-align: center;
        font-size: 1.3rem;
        color: #703b00;
        text-shadow: #e6dfd2 1px 0 0, #e6dfd2 -1px 0 0, #e6dfd2 0 1px 0,
            #e6dfd2 0 -1px 0, #e6dfd2 1px 1px 0, #e6dfd2 -1px 1px 0,
            #e6dfd2 1px -1px 0, #e6dfd2 -1px -1px 0;
    }

    .root-cluster-background {
        position: absolute;
        transform: translate(-50%, -50%);

        width: 20rem;
        height: calc(20rem * 101 / 327);
    }
</style>
