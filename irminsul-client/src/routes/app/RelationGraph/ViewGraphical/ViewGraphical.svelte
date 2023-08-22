<script lang="ts">
    import RootCluster_Background from "../../../../asset/img/ui/RootCluster-Background.png";
    import Atom from "./Atom.svelte";
    import Axis from "./Axis.svelte";
    import Cluster from "./Cluster.svelte";
    import Grid from "./Grid.svelte";
    import RelationBetween from "./RelationBetween.svelte";
    import type {RelationGraph} from "../../../../model/RelationGraph";
    import type {Vector2} from "$lib/util/Vector2";
    import {createEventDispatcher} from "svelte";
    import type ViewController from "../ViewController";
    import {deadKeyMultiplier} from "$lib/util/DeadKeyMultiplier";
    const dispatch = createEventDispatcher();

    export let relationGraph: RelationGraph;
    export let view: ViewController;
    export let showGrid: boolean;
    export let showAxis: boolean;
    export let showCoordinates: boolean;
    export let editMode: boolean;

    let rootClusterSelected: boolean;
    let selectedAtoms: Set<string> = new Set();
    let selectedClusters: Set<string> = new Set();
    let selectedEntities: Set<string> = new Set();
    let selectedEntitiesInSelectedClusters: Set<string> = new Set();
    let entityAnchor: Map<string, Vector2> = new Map();

    updateAnchors();

    function updateAnchors() {
        entityAnchor.set(relationGraph.id, relationGraph.rootPosition);
        relationGraph.atoms.forEach(a => entityAnchor.set(a.id, a.position));
        relationGraph.clusters.forEach(c => entityAnchor.set(c.id, c.anchor));
        entityAnchor = entityAnchor;
    }

    function updateSelectedAtoms() {
        selectedAtoms = selectedAtoms;
        selectedEntities = new Set([...selectedAtoms, ...selectedClusters]);
    }

    function updateSelectedClusters() {
        selectedClusters = selectedClusters;
        selectedEntities = new Set([...selectedAtoms, ...selectedClusters]);
        selectedEntitiesInSelectedClusters = new Set(
            relationGraph.clusters
                .filter(cluster => selectedClusters.has(cluster.id))
                .flatMap(cluster => cluster.elements) ?? []
        );
    }

    function keydown(e: KeyboardEvent) {
        if (e.ctrlKey !== e.metaKey) return;

        if (["KeyW", "KeyS", "KeyA", "KeyD", "Minus", "Equal", "KeyQ", "KeyE", "Digit0"].find(v => v === e.code)) {
            view.keyboardEvent(e);
            view = view;
        } else if (e.code === "KeyX") {
            dispatch("set-show-axis", !showAxis);
        } else if (e.code === "KeyG") {
            dispatch("set-show-grid", !showGrid);
        } else if (e.code === "KeyC") {
            showCoordinates = !showCoordinates;
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
</script>

<svelte:window on:keydown={keydown} />

<div id="view-graphical"
    style:transform="rotate({-view.angle}deg) scale({view.scale * 100}%) translate({view.x}rem, {-view.y}rem)"
>
    {#if showGrid} <Grid /> {/if}
    {#if showAxis} <Axis /> {/if}

    {#each relationGraph.relationsBetween as relationBetween}
        {#key entityAnchor}
            <RelationBetween bind:editMode
                             {entityAnchor} {selectedEntities} {relationBetween} />
        {/key}
    {/each}

    {#each relationGraph.clusters as cluster}
        <Cluster on:update-selected-clusters={updateSelectedClusters}
                 on:update-anchors={updateAnchors}
                 on:jump-to
                 bind:editMode
                 {selectedAtoms}
                 {selectedClusters}
                 {selectedEntities}
                 {selectedEntitiesInSelectedClusters}
                 {...cluster} {showCoordinates} />
    {/each}

    {#each relationGraph.atoms as atom}
        <Atom on:update-selected-atoms={updateSelectedAtoms}
              on:update-anchors={updateAnchors}
              bind:editMode
              {selectedAtoms}
              {selectedClusters}
              {selectedEntities}
              {selectedEntitiesInSelectedClusters}
              {...atom} {showCoordinates}/>
    {/each}

    <!-- svelte-ignore a11y-click-events-have-key-events -->
    <div id="root-cluster"
         style:z-index="1000"
         class:selected={rootClusterSelected}
         on:click={() => rootClusterSelected = !rootClusterSelected}
         style:left="{relationGraph.rootPosition.x}rem"
         style:top="{-relationGraph.rootPosition.y}rem"
    >
        <img class="root-cluster-background" src={RootCluster_Background} alt=""/>
        <div class="translation font-hywh-85w">
            {relationGraph.rootTranslation}
        </div>
    </div>
</div>

<style lang="scss">
    #view-graphical {
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

    #root-cluster {
        position: absolute;
        transform: translate(-50%, -50%);

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