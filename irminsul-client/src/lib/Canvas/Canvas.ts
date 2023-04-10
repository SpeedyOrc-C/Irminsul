import type { Renderable } from "./Renderable";
import { linearMap } from "../util/LinearMap";
import type { Vector2 } from "../util/Vector2";

export class Canvas {
    readonly canvas: HTMLCanvasElement;
    readonly context: CanvasRenderingContext2D;
    readonly height: number;
    readonly width: number;
    readonly fps: number;

    elements: Array<Renderable<any>>;

    private rendering = false;
    // private interval: number | null = null;
    private time: number;
    private deltaTime = 0;
    private addedElements: Array<Renderable<any>> = [];

    readonly viewportMinX: number;
    readonly viewportMaxX: number;
    readonly viewportMinY: number;
    readonly viewportMaxY: number;
    readonly aspectRatio: number;

    constructor(
        canvas: HTMLCanvasElement,
        height: number, width: number,
        viewportMinX: number, viewportMaxX: number,
        viewportMinY: number, viewportMaxY: number,
        fps: number,
        elements: Array<Renderable<any>>
    ) {
        canvas.height = height
        canvas.width = width;
        this.aspectRatio = width / height
        this.canvas = canvas;
        // eslint-disable-next-line @typescript-eslint/no-non-null-assertion
        this.context = canvas.getContext("2d")!;
        this.height = height;
        this.width = width;
        this.time = 0;
        this.elements = elements;
        this.fps = fps;

        this.viewportMinX = viewportMinX;
        this.viewportMaxX = viewportMaxX;
        this.viewportMinY = viewportMinY;
        this.viewportMaxY = viewportMaxY;

        this.update = this.update.bind(this);
    }

    public getCanvas(): HTMLCanvasElement {
        return this.canvas;
    }

    public getContext(): CanvasRenderingContext2D {
        return this.context;
    }

    private update() {
        this.deltaTime = Date.now() / 1000 - this.time;
        this.time = Date.now() / 1000;

        this.elements = this.addedElements.concat(
            this.elements.filter((element) => !element.willBeDestroyed))

        this.context.clearRect(0, 0, this.width, this.height)
        this.elements.forEach(element => element.renderOn(this))
        this.elements.forEach(element => element.update(element, this))
        this.addedElements = [];

        if (this.rendering) {
            requestAnimationFrame((_time) => this.update())
        }
    }

    public getTime(): number {
        return this.time;
    }

    public getDeltaTime(): number {
        return this.deltaTime;
    }

    public startRendering() {
        this.rendering = true;
        this.update();
    }

    public stopRendering() {
        this.rendering = false;
    }

    public addElement(element: Renderable<any>) {
        this.addedElements.push(element);
    }

    public mapViewport(v: Vector2): Vector2 {
        return { x: this.mapViewportX(v.x), y: this.mapViewportY(v.y)}
    }

    public mapViewportX(x: number): number {
        return linearMap(this.viewportMinX, x, this.viewportMaxX, 0, this.width)
    }

    public mapViewportY(y: number): number {
        return linearMap(this.viewportMinY, y, this.viewportMaxY, this.height, 0)
    }

    public mapCanvas(v: Vector2): Vector2 {
        return { x: this.mapCanvasW(v.x), y: this.mapCanvasH(v.y)}
    }

    public mapCanvasW(w: number): number {
        return linearMap(0, w, this.width, this.viewportMinX, this.viewportMaxX)
    }

    public mapCanvasH(h: number): number {
        return linearMap(0, h, this.height, this.viewportMaxY, this.viewportMinY)
    }
}
