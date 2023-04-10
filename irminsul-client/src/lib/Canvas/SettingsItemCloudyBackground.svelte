<script lang="ts">
    import { onDestroy, onMount } from "svelte";
    import { Canvas } from "./Canvas";
    import { CloudyWave } from "./Renderables/CloudyWave";
    import { Snowflake } from "./Renderables/Particle/Snowflake";
    import { Star } from "./Renderables/Particle/Star";

    import { ParticleSystem } from "./Renderables/ParticleSystem";

    let canvasEffect: HTMLCanvasElement;
    let effect: Canvas;

    const height = 50;
    const width = 300;
    const fps = 60;

    onMount(() => {
        effect = new Canvas(canvasEffect, height, width, 0, 1, -1, 1, fps, [
            new CloudyWave(),
            new ParticleSystem((self) => {
                if (Math.random() < 0.3) {
                    self.particles.push(new Snowflake());
                }
                if (Math.random() < 0.1) {
                    self.particles.push(new Star());
                }
            }),
        ]);
        effect.startRendering();
    });

    onDestroy(() => effect.stopRendering());
</script>

<canvas bind:this={canvasEffect} />

<style>
    canvas {
        position: absolute;
        /* outline: 5px solid #fff4; */
        width: 19.2rem;
        height: 3.2rem;
    }
</style>
