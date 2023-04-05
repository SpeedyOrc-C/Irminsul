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

    export let relationGraph: RelationGraph;

    console.info("Relation graph loaded: ", relationGraph);

    let entityAnchor: Map<string, Vector2> = new Map();
    function updateEntityAnchor() {
        entityAnchor.set(relationGraph.id, relationGraph.rootPosition);
        relationGraph.atoms.forEach((atom) =>
            entityAnchor.set(atom.id, atom.position)
        );
        relationGraph.clusters.forEach((cluster) =>
            entityAnchor.set(cluster.id, cluster.anchor)
        );
    }
    updateEntityAnchor();

    let showAxis: boolean = false;
    let showGrid: boolean = false;
    let showCoordinate: boolean = false;

    let viewX = 0;
    let viewY = 0;
    let viewAngle = 0;
    let viewScaleExponent = 0;
    let viewScale: number = Math.pow(2, 0.5 * viewScaleExponent);
    $: viewScale = Math.pow(2, 0.5 * viewScaleExponent);

    let transform: string;
    $: transform = [
        `transform:`,
        `rotate(${-viewAngle}deg)`,
        `scale(${viewScale * 100}%)`,
        `translate(${viewX}rem, ${-viewY}rem)`,
    ].join(" ");

    let rootClusterTransform: string;
    $: rootClusterTransform = `left: ${
        relationGraph.rootPosition.x
    }rem; top: ${-relationGraph.rootPosition.y}rem`;

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

    function saveAsHaskell() {
        let fileName = `${relationGraph.id}-Haskell.txt`;
        console.log("Saving as Haskell...", fileName);
        saveStringAsFile(
            dumpRelationGraphRelation2Haskell(relationGraph),
            fileName
        );
    }

    function saveAsJson() {
        let fileName = `${relationGraph.id}-JSON.json`;
        console.log("Saving as JSON...", fileName);
        saveStringAsFile(JSON.stringify(relationGraph, null, 4), fileName);
    }

    let jsonFileInput = document.createElement("input");
    jsonFileInput.style.display = "none";
    jsonFileInput.type = "file";
    const jsonFileReader = new FileReader();

    jsonFileInput.addEventListener("change", (_: Event) => {
        if (jsonFileInput.files == null || jsonFileInput.files?.length == 0) {
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

    function importFromJson() {
        console.info("Trying to import from JSON...");
        jsonFileInput.click();
    }

    function panelClicked(e: CustomEvent) {
        switch (e.detail.action) {
            case "save-as-JSON":
                saveAsJson();
                break;
            case "export-haskell":
                saveAsHaskell();
                break;
            case "import-json":
                importFromJson();
                break;
            case "go-to-parent":
                history.back();
                break;
        }
    }
</script>

<!-- svelte-ignore missing-declaration -->
<svelte:window on:keydown={keydownListener} />

<div id="background-dark-blue" />

<div id="background-cloud" />

<div id="relation-graph" style={transform}>
    {#if showGrid}
        <Grid width="2px" color="gray" />
    {/if}

    {#if showAxis}
        <Axis width="4px" color="white" />
    {/if}

    {#each relationGraph.relationsBetween as relationBetween}
        {#key relationGraph}
            <RelationBetween
                forwardRelations={relationBetween.forwardRelations}
                backwardRelations={relationBetween.backwardRelations}
                biRelations={relationBetween.biRelations}
                subjectAnchor={entityAnchor.get(relationBetween.subjectId) ??
                    Vector2Zero}
                objectAnchor={entityAnchor.get(relationBetween.objectId) ??
                    Vector2Zero}
            />
        {/key}
    {/each}

    {#each relationGraph.clusters as cluster}
        <Cluster {...cluster} {showCoordinate} />
    {/each}

    {#each relationGraph.atoms as atom}
        <Atom {...atom} {showCoordinate} />
    {/each}

    <div id="root-cluster" style={rootClusterTransform}>
        <img
            id="root-cluster-background"
            src={img_ui_background_root_cluster}
            alt=""
        />
        <div id="translation" class="font-hywh-85w">
            {relationGraph.rootTranslation}
        </div>
    </div>
</div>

<Panel on:panel-clicked={panelClicked} />

<style>
    #background-dark-blue {
        overflow: hidden;
        position: absolute;
        width: 100vw;
        height: 100vh;

        background: #171f2b;
    }

    #background-cloud {
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

    #relation-graph {
        position: absolute;
        top: 50%;
        left: 50%;

        transition: transform;
        transition-duration: 0.3s;
    }

    #root-cluster {
        position: absolute;
        transform: translate(-50%, -50%);

        z-index: 10000;

        user-select: none;
        -webkit-user-select: none;
    }

    #translation {
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

    #root-cluster-background {
        position: absolute;
        transform: translate(-50%, -50%);

        width: 20rem;
        height: calc(20rem * 101 / 327);
    }
</style>
