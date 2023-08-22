export default class Joystick {
    callback: (dx: number, dy: number) => void;

    innerCircle: HTMLElement;
    angle = 0;

    #posX = 0;
    #posY = 0;
    #lastX = 0;
    #lastY = 0;

    constructor(innerCircle: HTMLElement, callback: (dx: number, dy: number) => void) {
        this.innerCircle = innerCircle;
        this.callback = callback;
    }

    start = () => {
        const rect = this.innerCircle.getBoundingClientRect();
        this.#posX = (rect.left + rect.right) / 2;
        this.#posY = (rect.top + rect.bottom) / 2;
        this.#lastX = this.#posX;
        this.#lastY = this.#posY;
    };

    move = (x: number, y: number) => {
        this.angle = Math.atan(-(y - this.#posY) / (x - this.#posX)) + ((x - this.#posX) < 0 ? Math.PI : 0);
        this.callback(x - this.#lastX, y - this.#lastY);

        this.#lastX = x;
        this.#lastY = y;
    };
}