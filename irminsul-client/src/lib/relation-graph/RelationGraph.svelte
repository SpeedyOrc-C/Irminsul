<script lang="ts">
    import RootCluster_Background from "../../asset/img/ui/RootCluster-Background.png";
    import Atom from "./Atom.svelte";
    import Cluster from "./Cluster.svelte";
    import {dumpRelationGraph2Haskell, type RelationGraph,} from "../../model/RelationGraph";
    import RelationBetween from "./RelationBetween.svelte";
    import {type Vector2, Vector2Zero} from "../util/Vector2";
    import Axis from "./Axis.svelte";
    import Grid from "./Grid.svelte";
    import {saveStringAsFile} from "$lib/util/String";
    import Panel from "./Panel.svelte";
    import {onMount} from "svelte";
    import {deadKeyMultiplier} from "$lib/util/DeadKeyMultiplier";
    import Settings from "./Settings.svelte";
    import {writable, type Writable} from "svelte/store";
    import DialogOk from "$lib/ui/Dialog/DialogOk.svelte";
    import {_, locale} from "svelte-i18n";
    import {RelationGraphLoader} from "$lib/relation-graph/RelationGraphLoader";
    import ViewController from "$lib/relation-graph/ViewController";
    import Joystick from "$lib/relation-graph/Joystick.svelte";
    import RelationGraphSettings, {ShowJoystick, WhoAmI} from "$lib/relation-graph/RelationGraphSettings";

    export let id: string;

    let view = new ViewController();

    const loader = new RelationGraphLoader();

    const settings = new RelationGraphSettings("relation_graph_settings", "2023.08.12.2230");

    let relationGraph: RelationGraph | null = null;
    let jsonFileInput: HTMLInputElement;
    let jsonFileReader: FileReader;

    let entityAnchor: Map<string, Vector2> = new Map();

    let showAxis = false;
    let showGrid = false;
    let showCoordinate = false;
    let showJoystick: ShowJoystick = ShowJoystick.Never;
    let showSettings = false;
    let joystickSensibility = 4;

    let selectedAtoms: Set<string> = new Set();
    let selectedClusters: Set<string> = new Set();
    let selectedEntities: Set<string> = new Set();
    let selectedEntitiesInSelectedClusters: Set<string> = new Set();

    let rootClusterSelected = false;

    let showLayoutMissing: Writable<boolean> = writable(false);

    function updateView() { view = view; }

    function updateSelectedAtoms() {
        selectedEntities = new Set([...selectedAtoms, ...selectedClusters]);
    }

    function updateSelectedClusters() {
        if (relationGraph == null) return;

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
            setShowAxis(!showAxis);
        } else if (e.code === "KeyG") {
            setShowGrid(!showGrid);
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

    async function loadRelationGraph(
        loadId: string,
        language=settings.preference.language,
        whoAmI_=settings.preference.who_am_i
    ) {
        console.info("Loading relation graph:", loadId);

        let json: RelationGraph;
        try {
            json = await loader.load(loadId, language, whoAmI_);
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

        relationGraph = json;
        selectedEntities.clear();
        selectedAtoms.clear();
        selectedClusters.clear();
        updateEntityAnchor();
        view.reset();
        updateView();
        id = loadId;

        window.history.replaceState(undefined, "", `/app/?id=${id}`);

        console.info("Relation graph loaded: ", json);
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
        showSettings = true;
    }

    function setLanguage(language: string) {
        settings.preference.language = language;
        settings.save();
        locale.set(language);
        loadRelationGraph(id);
    }

    function setWhoAmI(who_am_i: WhoAmI) {
        settings.preference.who_am_i = who_am_i;
        settings.save();
        loader.clearCache();
        loadRelationGraph(id);
    }

    function setShowAxis(show_axis: boolean) {
        showAxis = show_axis;
        settings.preference.show_axis = show_axis;
        settings.save();
    }

    function setShowGrid(show_grid: boolean) {
        showGrid = show_grid;
        settings.preference.show_grid = show_grid;
        settings.save();
    }

    function setReduceVisualEffect(reduce_visual_effect: boolean) {
        settings.preference.reduce_visual_effect = reduce_visual_effect;
        settings.save();
    }

    function setShowJoystick(show_joystick: ShowJoystick) {
        settings.preference.show_joystick = show_joystick;
        settings.save();
    }

    function setJoystickSensibility(joystick_sensibility: number) {
        settings.preference.joystick_sensitivity = joystick_sensibility;
        settings.save();
    }

    function resetAll() {
        localStorage.clear();
        window.location.href = "/app";
    }

    onMount(() => {
        showAxis = settings.preference.show_axis;
        showGrid = settings.preference.show_grid;
        showJoystick = settings.preference.show_joystick;
        joystickSensibility = settings.preference.joystick_sensitivity;
        locale.set(settings.preference.language);

        jsonFileReader = new FileReader();

        jsonFileInput.addEventListener("change", () => {
            if (jsonFileInput.files?.length == 0) {
                console.error("No import file selected.");
                return;
            }

            const files = jsonFileInput.files!;

            if (files.length > 0) {
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
    >
        {#if showGrid} <Grid /> {/if}
        {#if showAxis} <Axis /> {/if}

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
                    <Cluster on:update-selected-clusters={updateSelectedClusters}
                             on:jump-to={e => loadRelationGraph(e.detail.id)}
                             {...cluster} {showCoordinate} {selectedClusters}/>
                {/each}

                {#each relationGraph.atoms as atom}
                    {@const dim =
                        !selectedAtoms.has(atom.id) &&
                        selectedEntities.size > 0 &&
                        !selectedEntitiesInSelectedClusters.has(atom.id)}
                    <Atom on:update-selected-atoms={updateSelectedAtoms}
                          {...atom} {showCoordinate} {dim} {selectedAtoms}/>
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

    <Joystick callback={(dx, dy) => view.joystickEvent(dx, dy, 0.1 * (0.25 + joystickSensibility / 4))}
              bind:showJoystick={settings.preference.show_joystick}
              on:update-view={updateView}/>

    <Panel on:jump-to={e => loadRelationGraph(e.detail)}
           on:open-settings={openSettings}
           {relationGraph} {id}/>

    <DialogOk title={$_("error.layout-missing.self")} show={showLayoutMissing}>
        {$_("error.layout-missing.detail")}
    </DialogOk>

    <Settings {settings}
              bind:show={showSettings}
              on:import-json={importJson}
              on:export-json={exportJson}
              on:export-haskell={exportHaskell}
              on:set-show-axis={e => setShowAxis(e.detail)} bind:showAxis
              on:set-show-grid={e => setShowGrid(e.detail)} bind:showGrid
              on:set-show-joystick={e => setShowJoystick(e.detail)} bind:showJoystick
              on:set-joystick-sensibility={e => setJoystickSensibility(e.detail)} bind:joystickSensibility
              on:set-reduce-visual-effect={e => setReduceVisualEffect(e.detail)}
              on:set-language={e => setLanguage(e.detail)}
              on:set-who-am-i={e => setWhoAmI(e.detail)}
              on:reset-all={resetAll}
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
