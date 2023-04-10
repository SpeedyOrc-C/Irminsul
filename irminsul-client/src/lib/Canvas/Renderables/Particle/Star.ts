import { TimedParticle } from "../Particle";
import type { Canvas } from "../../Canvas";
import type { Vector2 } from "../../../util/Vector2";

export class Star extends TimedParticle {
    maxSize: number;
    opacity: number;

    constructor() {
        super({ x: Math.random(), y: Math.random() * 1.2 - 0.6 }, Math.random() * 2);
        this.maxSize = 0.4 * Math.random();
        this.opacity = Math.random();
    }

    renderOn(canvas: Canvas): void {
        const canvasDrawPosition: Vector2 = canvas.mapViewport(this.position);
        const t = this.t;
        const t2 = t * t
        const t3 = t2 * t

        // Magic happens here...
        const size = t * (-47.22 * t3 + 94.44 * t2 - 61.03 * t + 13.81) * this.maxSize;
        
        const aspectRatio = 2 / 3;
        const stretchX = 2;
        const stretchY = 4;
        
        const horizontalSize = size * aspectRatio / canvas.aspectRatio / 2;

        // Vertices
        const topVertex: Vector2 = ({
            x: this.position.x,
            y: this.position.y + size})
        const bottomVertex: Vector2 = ({
            x: this.position.x,
            y: this.position.y - size})
        const leftVertex: Vector2 = ({
            x: this.position.x - horizontalSize,
            y: this.position.y})
        const rightVertex: Vector2 = ({
            x: this.position.x + horizontalSize,
            y: this.position.y})

        // Handles
        const topHandle: Vector2 = ({
            x: this.position.x,
            y: this.position.y + size / stretchY})
        const bottomHandle: Vector2 = ({
            x: this.position.x,
            y: this.position.y - size / stretchY})
        const leftHandle: Vector2 = ({
            x: this.position.x - horizontalSize / stretchX,
            y: this.position.y})
        const rightHandle: Vector2 = ({
            x: this.position.x + horizontalSize / stretchX,
            y: this.position.y})

        // Calculate positions on canvas
        const canvasTopVertex = canvas.mapViewport(topVertex);
        const canvasBottomVertex = canvas.mapViewport(bottomVertex);
        const canvasLeftVertex = canvas.mapViewport(leftVertex);
        const canvasRightVertex = canvas.mapViewport(rightVertex);
        const canvasTopHandle = canvas.mapViewport(topHandle);
        const canvasBottomHandle = canvas.mapViewport(bottomHandle);
        const canvasLeftHandle = canvas.mapViewport(leftHandle);
        const canvasRightHandle = canvas.mapViewport(rightHandle);
        
        const ctx = canvas.getContext();
        ctx.beginPath();
        ctx.fillStyle = `rgba(255,255,255,${this.opacity})`;

        ctx.moveTo(canvasTopVertex.x, canvasTopVertex.y);
        ctx.bezierCurveTo(
            canvasTopHandle.x, canvasTopHandle.y,
            canvasLeftHandle.x, canvasLeftHandle.y,
            canvasLeftVertex.x, canvasLeftVertex.y);
        ctx.bezierCurveTo(
            canvasLeftHandle.x, canvasLeftHandle.y,
            canvasBottomHandle.x, canvasBottomHandle.y,
            canvasBottomVertex.x, canvasBottomVertex.y);
        ctx.bezierCurveTo(
            canvasBottomHandle.x, canvasBottomHandle.y,
            canvasRightHandle.x, canvasRightHandle.y,
            canvasRightVertex.x, canvasRightVertex.y);
        ctx.bezierCurveTo(
            canvasRightHandle.x, canvasRightHandle.y,
            canvasTopHandle.x, canvasTopHandle.y,
            canvasTopVertex.x, canvasTopVertex.y);
        ctx.fill();
    }
}
