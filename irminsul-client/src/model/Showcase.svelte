<script lang="ts">
    import Atom from "./Atom.svelte";
    import Cluster from "./Cluster.svelte";
    import type {Layout} from "./Layout";
    import RelationBetween from "./RelationBetween.svelte";
    import {onMount} from "svelte";

    export let layout: Layout;

    // View Controller

    let viewX = 0;
    let viewY = 0;
    let viewAngle = 0;
    let viewScaleExponent = 0;
    let viewScale: number = Math.pow(2, 0.5 * viewScaleExponent);

    let transform: string;
    let rootClusterTransform: string;
    let cloudTransform: string;

    onMount(() => {
        rootClusterTransform = `left: ${layout.rootPosition.x}rem; top: ${-layout.rootPosition.y}rem`;
    })

    $: viewScale = Math.pow(2, 0.5 * viewScaleExponent);
    $: transform = [
        `transform:`,
        `rotate(${-viewAngle}deg)`,
        `scale(${viewScale * 100}%)`,
        `translate(${viewX}rem, ${-viewY}rem)`,
    ].join(" ");
    $: cloudTransform = [`transform:`, ``, ``, ``].join("; ");

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

    // For keyboard users

    document.addEventListener("keydown", (e) => {
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
    });
</script>

<div id="background-dark-blue"></div>

<div id="background-cloud"></div>

<div id="showcase" style={transform}>
    {#each layout.relationsBetween as relationBetween}
        <RelationBetween {...relationBetween}/>
    {/each}

    {#each layout.clusters as cluster}
        <Cluster {...cluster}/>
    {/each}

    {#each layout.atoms as atom}
        <Atom {...atom}/>
    {/each}

    <div id="root-cluster" style={rootClusterTransform}>
        <img
            id="root-cluster-background"
            src="/asset/img/ui/background-root-cluster.png"
            alt=""
        />
        <div id="translation" class="font-hywh-85w">
            {layout.rootTranslation}
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
