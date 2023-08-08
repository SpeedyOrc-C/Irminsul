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

    export let id: string;
    export let lang: Writable<string>;
    export let reduceVisualEffect: Writable<string>;
    export let whoAmI: Writable<"aether" | "lumine">;

    const loader = new RelationGraphLoader();

    let relationGraph: RelationGraph | null = null;
    let jsonFileInput: HTMLInputElement;
    let jsonFileReader: FileReader;

    let entityAnchor: Map<string, Vector2> = new Map();

    let showAxis: Writable<boolean> = writable(false);
    let showGrid: Writable<boolean> = writable(false);
    let showCoordinate = false;
    let showSettings: Writable<boolean> = writable(false);

    let viewX = 0;
    let viewY = 0;
    let viewAngle = 0;
    let viewScaleExponent = 0;
    let viewScale: number;
    let viewAngleRad: number;
    let viewAngleSin: number;
    let viewAngleCos: number;
    $: viewScale = Math.pow(2, 0.5 * viewScaleExponent);
    $: viewAngleRad = (viewAngle * Math.PI) / 180;
    $: viewAngleSin = Math.sin(viewAngleRad);
    $: viewAngleCos = Math.cos(viewAngleRad);

    let contentOpacity = 1;

    let selectedAtoms: Set<string> = new Set();
    let selectedClusters: Set<string> = new Set();
    let selectedEntities: Set<string> = new Set();
    let selectedEntitiesInSelectedClusters: Set<string> = new Set();

    let rootClusterSelected = false;

    let showLayoutMissing: Writable<boolean> = writable(false);

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

    function moveUp() {
        viewX -= (10 / viewScale) * viewAngleSin;
        viewY -= (10 / viewScale) * viewAngleCos;
    }

    function moveDown() {
        viewX += (10 / viewScale) * viewAngleSin;
        viewY += (10 / viewScale) * viewAngleCos;
    }

    function moveLeft() {
        viewX += (10 / viewScale) * viewAngleCos;
        viewY -= (10 / viewScale) * viewAngleSin;
    }

    function moveRight() {
        viewX -= (10 / viewScale) * viewAngleCos;
        viewY += (10 / viewScale) * viewAngleSin;
    }

    function zoomIn() { viewScaleExponent += 1; }

    function zoomOut() { viewScaleExponent -= 1; }

    function rotateAnticlockwise() { viewAngle = viewAngle + 22.5; }

    function rotateClockwise() { viewAngle = viewAngle - 22.5; }

    function resetView() {
        viewX = 0;
        viewY = 0;
        viewScaleExponent = 0;
        viewScale = 1;
        viewAngle = 0;
    }

    function toggleRootClusterSelect() {
        rootClusterSelected = !rootClusterSelected;
    }

    function keydownListener(e: KeyboardEvent) {
        if (relationGraph == null) return;

        if (e.ctrlKey !== e.metaKey) return;

        if (e.code === "KeyW") {
            moveUp();
        } else if (e.code === "KeyS") {
            moveDown();
        } else if (e.code === "KeyA") {
            moveLeft();
        } else if (e.code === "KeyD") {
            moveRight();
        } else if (e.code === "Minus") {
            zoomOut();
        } else if (e.code === "Equal") {
            zoomIn();
        } else if (e.code === "KeyQ") {
            rotateClockwise();
        } else if (e.code === "KeyE") {
            rotateAnticlockwise();
        } else if (e.code === "Digit0") {
            resetView();
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
            json = await loader.load(loadId, $lang);
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
        resetView();
        id = loadId;

        window.history.replaceState(undefined, "", `/relation-graph/?id=${id}&lang=${$lang}`);

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
        lang.subscribe(() => loadRelationGraph(id));
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
    <div class="background-dark-blue"></div>
    <div class="background-cloud"></div>

    <div
        class="content"
        style:transform="rotate({-viewAngle}deg) scale({viewScale * 100}%)
        translate({viewX}rem, {-viewY}rem)"
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
                        subjectAnchor={entityAnchor.get(
                            relationBetween.subjectId
                        ) ?? Vector2Zero}
                        objectAnchor={entityAnchor.get(
                            relationBetween.objectId
                        ) ?? Vector2Zero}
                        {highlight}
                        {dim}
                    />
                {/each}

                {#each relationGraph.clusters as cluster}
                    <Cluster
                        {...cluster}
                        {showCoordinate}
                        on:rg-action={handleRgAction}
                    />
                {/each}

                {#each relationGraph.atoms as atom}
                    {@const dim =
                        !selectedAtoms.has(atom.id) &&
                        selectedEntities.size > 0 &&
                        !selectedEntitiesInSelectedClusters.has(atom.id)}
                    <Atom
                        {...atom}
                        {showCoordinate}
                        on:rg-action={handleRgAction}
                        {dim}
                    />
                {/each}

                <!-- svelte-ignore a11y-click-events-have-key-events -->
                <div
                    class="root-cluster"
                    class:selected={rootClusterSelected}
                    style:left="{relationGraph.rootPosition.x}rem"
                    style:top="{-relationGraph.rootPosition.y}rem"
                    on:click={toggleRootClusterSelect}
                >
                    <img
                        class="root-cluster-background"
                        src={RootCluster_Background}
                        alt=""
                    />
                    <div class="translation font-hywh-85w">
                        {relationGraph.rootTranslation}
                    </div>
                </div>
            {/key}
        {/if}
    </div>

    <Panel on:rg-action={handleRgAction} {relationGraph} {id}/>

    <DialogOk title={$_("error.layout-missing.self")} show={showLayoutMissing}>
        {$_("error.layout-missing.detail")}
    </DialogOk>

    <Settings
        show={showSettings}
        on:rg-action={handleRgAction}
        {lang}
        {reduceVisualEffect}
        {showAxis}
        {showGrid}
        {whoAmI}
    />

    <input type="file" bind:this={jsonFileInput} style:display="none" />
</div>

<style lang="scss">
    .background-dark-blue {
        overflow: hidden;
        position: absolute;
        width: 100vw;
        height: 100vh;

        background: #171f2b;
    }

    .content {
        position: absolute;
        top: 50%;
        left: 50%;

        transition-property: transform, opacity;
        transition-duration: 0.3s;
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
