export default class ViewController {
    x = 0;
    y = 0;

    angle = 0;
    #angleRad = 0;
    #angleSin = 0;
    #angleCos = 1;

    scaleExponent = 0;
    scale = 1;

    #update() {
        this.#angleRad = (this.angle * Math.PI) / 180;
        this.#angleSin = Math.sin(this.#angleRad);
        this.#angleCos = Math.cos(this.#angleRad);
        this.scale = Math.pow(2, 0.5 * this.scaleExponent);
    }

    moveDelta(dx: number, dy: number) {
        this.x += - (dx / this.scale) * this.#angleCos - (dy / this.scale) * this.#angleSin;
        this.y += - (dy / this.scale) * this.#angleCos + (dx / this.scale) * this.#angleSin;
    }

    reset() {
        this.x = 0;
        this.y = 0;

        this.angle = 0;
        this.#angleRad = 0;
        this.#angleSin = 0;
        this.#angleCos = 1;

        this.scaleExponent = 0;
        this.scale = 1;
    }

    moveUp = () => this.moveDelta(0, 7.5);
    moveDown = () => this.moveDelta(0, -7.5);
    moveLeft = () => this.moveDelta(-7.5, 0);
    moveRight = () => this.moveDelta(7.5, 0);

    zoomIn() { this.scaleExponent += 1; this.#update(); }
    zoomOut() { this.scaleExponent -= 1; this.#update(); }

    rotateAnticlockwise() { this.angle += 22.5; this.#update(); }
    rotateClockwise() { this.angle -= 22.5; this.#update(); }

    joystickEvent = (dx: number, dy: number, multiplier: number) => {
        this.moveDelta(dx * multiplier, -dy * multiplier);
    }
}
