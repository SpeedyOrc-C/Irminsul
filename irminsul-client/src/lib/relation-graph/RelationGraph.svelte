<script lang="ts">
    import { img_ui_background_root_cluster } from "../../asset/Asset";
    import Atom from "./Atom.svelte";
    import Cluster from "./Cluster.svelte";
    import {
        dumpRelationGraphRelation2Haskell,
        type RelationGraph,
    } from "../../model/RelationGraph";
    import RelationBetween from "./RelationBetween.svelte";
    import { Vector2Zero, type Vector2 } from "../../model/Vector2";
    import Axis from "./Axis.svelte";
    import Grid from "./Grid.svelte";
    import { saveStringAsFile } from "$lib/util/String";
    import Panel from "./Panel.svelte";
    import { onMount } from "svelte";
    import type { ApiResponse, ApiStatusCode } from "$lib/util/Api";
    import Prompt from "$lib/ui/Prompt.svelte";

    export let id: string | null = null;
    export let lang: string | null = null;

    let relationGraph: RelationGraph | null = null;
    let responseStatus: ApiStatusCode | null = null;

    let jsonFileInput: HTMLInputElement;
    let jsonFileReader: FileReader;

    let entityAnchor: Map<string, Vector2> = new Map();

    let showAxis: boolean = false;
    let showGrid: boolean = false;
    let showCoordinate: boolean = false;

    let viewX = 0;
    let viewY = 0;
    let viewAngle = 0;
    let viewScaleExponent = 0;
    let viewScale: number = Math.pow(2, 0.5 * viewScaleExponent);
    $: viewScale = Math.pow(2, 0.5 * viewScaleExponent);

    function moveUp() {
        let deltaViewX = 0;
        let deltaViewY = 0;
        let viewAngleRad = (viewAngle * Math.PI) / 180;

        deltaViewX -= (10 / viewScale) * Math.sin(viewAngleRad);
        deltaViewY -= (10 / viewScale) * Math.cos(viewAngleRad);

        viewAngle %= 360;
        viewX += deltaViewX;
        viewY += deltaViewY;
    }

    function moveDown() {
        let viewAngleRad = (viewAngle * Math.PI) / 180;
        viewX += (10 / viewScale) * Math.sin(viewAngleRad);
        viewY += (10 / viewScale) * Math.cos(viewAngleRad);
    }

    function moveLeft() {
        let viewAngleRad = (viewAngle * Math.PI) / 180;
        viewX += (10 / viewScale) * Math.cos(viewAngleRad);
        viewY -= (10 / viewScale) * Math.sin(viewAngleRad);
    }

    function moveRight() {
        let viewAngleRad = (viewAngle * Math.PI) / 180;
        viewX -= (10 / viewScale) * Math.cos(viewAngleRad);
        viewY += (10 / viewScale) * Math.sin(viewAngleRad);
    }

    function zoomIn() {
        viewScaleExponent += 1;
    }

    function zoomOut() {
        viewScaleExponent -= 1;
    }

    function rotateAnticlockwise() {
        viewAngle += 22.5;
    }

    function rotateClockwise() {
        viewAngle -= 22.5;
    }

    function resetView() {
        viewX = 0;
        viewY = 0;
        viewScaleExponent = 0;
        viewScale = 1;
        viewAngle = 0;
    }

    function keydownListener(e: KeyboardEvent) {
        if (e.ctrlKey || e.metaKey) {
        } else {
            switch (e.code) {
                case "KeyW":
                    moveUp();
                    break;
                case "KeyS":
                    moveDown();
                    break;
                case "KeyA":
                    moveLeft();
                    break;
                case "KeyD":
                    moveRight();
                    break;
                case "Minus":
                    zoomOut();
                    break;
                case "Equal":
                    zoomIn();
                    break;
                case "KeyQ":
                    rotateClockwise();
                    break;
                case "KeyE":
                    rotateAnticlockwise();
                    break;
                case "Digit0":
                    resetView();
                    break;
                case "KeyX":
                    showAxis = showAxis ? false : true;
                    break;
                case "KeyG":
                    showGrid = showGrid ? false : true;
                    break;
                case "KeyC":
                    showCoordinate = showCoordinate ? false : true;
                    break;
            }
        }
    }

    function loadRelationGraph() {
        if (id == null || lang == null) return;

        console.info("Loading relation graph:", id);

        fetch(`/api/relation-graph?id=${id}&lang=${lang}`)
            .then((response) => response.json())
            .then((json: ApiResponse<RelationGraph>) => {
                if (json.status != "OK") {
                    console.error(
                        "Failed to load relation graph, error:",
                        json.status
                    );
                }

                relationGraph = json.body;
                responseStatus = json.status;
                updateEntityAnchor();
                resetView();

                if (json.body != null) {
                    console.info("Relation graph loaded: ", relationGraph);
                }
            });
    }

    function updateEntityAnchor() {
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
        if (relationGraph == null) return;

        let fileName = `${relationGraph.id}-Haskell.txt`;
        console.log("Saving as Haskell...", fileName);
        saveStringAsFile(
            dumpRelationGraphRelation2Haskell(relationGraph),
            fileName
        );
    }

    function exportJson() {
        if (relationGraph == null) return;

        let fileName = `${relationGraph.id}-JSON.json`;
        console.log("Saving as JSON...", fileName);
        saveStringAsFile(JSON.stringify(relationGraph, null, 4), fileName);
    }

    function importJson() {
        console.info("Trying to import from JSON...");
        jsonFileInput.click();
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
                console.info("Jump to: " + e.detail.id);
                id = e.detail.id;
                loadRelationGraph();
                break;
        }
    }

    onMount(() => {
        loadRelationGraph();

        jsonFileReader = new FileReader();
        jsonFileInput = document.createElement("input");
        jsonFileInput.style.display = "none";
        jsonFileInput.type = "file";

        jsonFileInput.addEventListener("change", (_: Event) => {
            if (
                jsonFileInput.files == null ||
                jsonFileInput.files?.length == 0
            ) {
                console.error("No import file selected.");
                return;
            }

            const file = jsonFileInput.files[0];
            jsonFileReader.readAsText(file);
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
    });
</script>

<!-- svelte-ignore missing-declaration -->
<svelte:window on:keydown={keydownListener} />

<div class="relation-graph">
    <div class="background-dark-blue" />
    <div class="background-cloud" />

    <div
        class="content"
        style:transform="rotate({-viewAngle}deg) scale({viewScale * 100}%)
        translate({viewX}rem, {-viewY}rem)"
    >
        {#if showGrid} <Grid /> {/if}
        {#if showAxis} <Axis /> {/if}

        {#if relationGraph == null}
            {#key responseStatus}
                {#if responseStatus != null && responseStatus !== "OK"}
                    {#if responseStatus === "UnsupportedLanguage"}
                        <Prompt
                            title="Unsupported Language"
                            content="Language [{lang}] is not supported."
                        />
                    {/if}
                    {#if responseStatus === "NotImplementedCluster"}
                        <Prompt
                            title="Not Implemented Cluster"
                            content="Entity [{id}] is not implemented."
                        />
                    {/if}
                    {#if responseStatus === "LayoutMissing"}
                        <Prompt
                            title="Layout Missing"
                            content="Layout for [{id}] is missing."
                        />
                    {/if}
                {/if}
            {/key}
        {:else}
            {#key relationGraph}
                {#each relationGraph.relationsBetween as relationBetween}
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
                    <Atom {...atom} {showCoordinate} />
                {/each}

                <div
                    class="root-cluster"
                    style:left="{relationGraph.rootPosition.x}rem"
                    style:top="{-relationGraph.rootPosition.y}rem"
                >
                    <img
                        class="root-cluster-background"
                        src={img_ui_background_root_cluster}
                        alt=""
                    />
                    <div class="translation font-hywh-85w">
                        {relationGraph.rootTranslation}
                    </div>
                </div>
            {/key}
        {/if}
    </div>

    {#key lang}
        {#if relationGraph != null && lang != null}
            <Panel
                on:rg-action={handleRgAction}
                pathElements={relationGraph.path}
                {lang}
            />
        {/if}
    {/key}
</div>

<style>
    .background-dark-blue {
        overflow: hidden;
        position: absolute;
        width: 100vw;
        height: 100vh;

        background: #171f2b;
    }

    .background-cloud {
        overflow: hidden;
        position: absolute;
        transform: translate(-50%, -50%);
        top: 50%;
        left: 50%;
        width: 100vw;
        height: 100vh;

        /*background: url("/asset/img/ui/background-cloud-repeating.png") repeat;*/
        background-position-x: 0;
        opacity: 20%;

        animation: cloud-ani;
        animation-iteration-count: infinite;
        animation-timing-function: linear;
        animation-duration: 1000s;
    }

    /* @keyframes cloud-ani {
        0% {
            background-position-x: 0;
            background-position-y: 0;
        }
        100% {
            background-position-x: -8192px;
            background-position-y: 4096px;
        }
    } */

    .content {
        position: absolute;
        top: 50%;
        left: 50%;

        transition: transform;
        transition-duration: 0.3s;
    }

    .root-cluster {
        position: absolute;
        transform: translate(-50%, -50%);

        z-index: 10000;

        user-select: none;
        -webkit-user-select: none;
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
