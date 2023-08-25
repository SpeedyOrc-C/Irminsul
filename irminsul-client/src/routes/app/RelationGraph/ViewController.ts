export default class ViewController {
    x = 0;
    y = 0;

    angle = 0;
    private angleRad = 0;
    private angleSin = 0;
    private angleCos = 1;

    scaleExponent = 0;
    scale = 1;

    static moveDeadKeyMultiplier(e: KeyboardEvent): number {
        if (e.shiftKey)
            return 4;
        if (e.altKey)
            return 0.25;
        return 1
    }

    static rotateDeadKeyMultiplier(e: KeyboardEvent): number {
        if (e.shiftKey)
            return 2;
        if (e.altKey)
            return 0.25;
        return 1
    }

    private update() {
        this.angleRad = (this.angle * Math.PI) / 180;
        this.angleSin = Math.sin(this.angleRad);
        this.angleCos = Math.cos(this.angleRad);
        this.scale = Math.pow(2, 0.5 * this.scaleExponent);
    }

    moveDelta(dx: number, dy: number) {
        this.x += - (dx / this.scale) * this.angleCos - (dy / this.scale) * this.angleSin;
        this.y += - (dy / this.scale) * this.angleCos + (dx / this.scale) * this.angleSin;
    }

    rotateDelta(angle: number) {
        this.angle += angle;
        this.update();
    }

    reset() {
        this.x = 0;
        this.y = 0;

        this.angle = 0;
        this.angleRad = 0;
        this.angleSin = 0;
        this.angleCos = 1;

        this.scaleExponent = 0;
        this.scale = 1;
    }

    zoomIn() { this.scaleExponent += 1; this.update(); }
    zoomOut() { this.scaleExponent -= 1; this.update(); }

    joystickEvent = (dx: number, dy: number, multiplier: number) => {
        this.moveDelta(dx * multiplier, -dy * multiplier);
    }
}
