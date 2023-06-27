import { TimedParticle } from "../Particle";
import type { Vector2 } from "$lib/util/Vector2";
import type { Canvas } from "../../Canvas";
import { linearMap } from "$lib/util/LinearMap";
import type {Renderable} from "$lib/Canvas/Renderable";

export class Snowflake extends TimedParticle implements Renderable {
    fromX: number;
    toX: number
    maxRadius: number;

    constructor() {
        const fromX = 0.8 * Math.random();
        super({ x: fromX, y: Math.random() * 1.2 - 0.6 }, 0.5 + Math.random())

        this.fromX = fromX;
        this.toX = this.fromX + 0.2 * Math.random();
        this.maxRadius = 0.015 * Math.random();
        this.position = { x: this.fromX, y: Math.random() * 1.2 - 0.6 };
    }

    renderOn(canvas: Canvas): void {
        const drawPosition: Vector2 = {
            x: linearMap(0, this.lasts, this.duration, this.fromX, this.toX),
            y: this.position.y,
        }
        const t = this.t
        const radius = 4 * t * (1 - t) * this.maxRadius

        const canvasDrawPosition = canvas.mapViewport(drawPosition);

        const ctx = canvas.getContext();
        ctx.beginPath();
        ctx.fillStyle = "white";

        ctx.arc(
            canvasDrawPosition.x,
            canvasDrawPosition.y,
            (canvas.viewportMaxY - canvas.viewportMinY) * radius * canvas.height,
            0, 2 * Math.PI);
        ctx.fill();
    }
}
