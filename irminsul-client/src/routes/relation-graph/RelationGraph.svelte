<script lang="ts">
    import { img_ui_background_root_cluster } from "../../asset/Asset";
    import Atom from "../../model/Atom.svelte";
    import Cluster from "../../model/Cluster.svelte";
    import type { RelationGraph } from "../../model/RelationGraph";
    import RelationBetween from "../../model/RelationBetween.svelte";
    import type { Vector2 } from "../../model/Vector2";

    export let relationGraph: RelationGraph;

    let entityAnchor: Map<string, Vector2> = new Map();

    entityAnchor.set(relationGraph.id, relationGraph.rootPosition);
    relationGraph.atoms.forEach((atom) =>
        entityAnchor.set(atom.id, atom.position)
    );
    relationGraph.clusters.forEach((cluster) =>
        entityAnchor.set(cluster.id, cluster.anchor)
    );

    let viewX = 0;
    let viewY = 0;
    let viewAngle = 0;
    let viewScaleExponent = 0;
    let viewScale: number = Math.pow(2, 0.5 * viewScaleExponent);

    let transform: string;
    let rootClusterTransform: string;

    $: rootClusterTransform = `left: ${
        relationGraph.rootPosition.x
    }rem; top: ${-relationGraph.rootPosition.y}rem`;

    $: viewScale = Math.pow(2, 0.5 * viewScaleExponent);
    $: transform = [
        `transform:`,
        `rotate(${-viewAngle}deg)`,
        `scale(${viewScale * 100}%)`,
        `translate(${viewX}rem, ${-viewY}rem)`,
    ].join(" ");

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
            case "BracketLeft":
                rotateClockwise();
                break;
            case "BracketRight":
                rotateAnticlockwise();
                break;
            case "Digit0":
                resetView();
                break;
        }
    }
</script>

<svelte:window on:keydown={keydownListener} />

<div id="background-dark-blue" />

<div id="background-cloud" />

<div id="showcase" style={transform}>
    {#each relationGraph.relationsBetween as relationBetween}
        <RelationBetween
            forwardRelations={relationBetween.forwardRelations}
            backwardRelations={relationBetween.backwardRelations}
            biRelations={relationBetween.biRelations}
            subjectAnchor={entityAnchor.get(relationBetween.subjectId) ?? {
                x: 0,
                y: 0,
            }}
            objectAnchor={entityAnchor.get(relationBetween.objectId) ?? {
                x: 0,
                y: 0,
            }}
        />
    {/each}

    {#each relationGraph.clusters as cluster}
        <Cluster {...cluster} />
    {/each}

    {#each relationGraph.atoms as atom}
        <Atom {...atom} />
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

    @keyframes cloud-ani {
        0% {
            background-position-x: 0;
            background-position-y: 0;
        }
        100% {
            background-position-x: -8192px;
            background-position-y: 4096px;
        }
    }

    #showcase {
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
