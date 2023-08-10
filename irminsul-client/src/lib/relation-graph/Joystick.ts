export default class Joystick {
    callback: (dx: number, dy: number) => void;

    moving = false;

    #firstX = 0;
    #firstY = 0;

    #lastX = 0;
    #lastY = 0;

    angle = 0;

    constructor(callback: (dx: number, dy: number) => void) {
        this.callback = callback;
    }

    move(x: number, y: number) {
        if (this.moving) {
            this.angle = Math.atan(-(y - this.#firstY) / (x - this.#firstX)) + ((x - this.#firstX) < 0 ? Math.PI : 0);
            this.callback(x - this.#lastX, y - this.#lastY);
        } else {
            this.moving = true;
            this.#firstX = x;
            this.#firstY = y;
        }
        this.#lastX = x;
        this.#lastY = y;

    }
    
    stop() {
        this.moving = false;
        this.#firstX = this.#lastX;
        this.#firstY = this.#lastY;
    }
}