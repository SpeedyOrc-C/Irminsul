export default class Joystick {
    callback: (x: number, y: number, dx: number, dy: number) => void;

    innerCircle: HTMLElement;
    angle = 0;

    private posX = 0;
    private posY = 0;
    private lastX = 0;
    private lastY = 0;
    private moving = false;

    constructor(innerCircle: HTMLElement, callback: (x: number, y: number, dx: number, dy: number) => void) {
        this.innerCircle = innerCircle;
        this.callback = callback;
    }

    start = () => {
        this.moving = true;

        const rect = this.innerCircle.getBoundingClientRect();
        this.posX = (rect.left + rect.right) / 2;
        this.posY = (rect.top + rect.bottom) / 2;
        this.lastX = this.posX;
        this.lastY = this.posY;
    };

    move = (x: number, y: number) => {
        const dx = x - this.posX;
        const dy = y - this.posY;
        this.angle = Math.atan(-dy / dx) + (dx < 0 ? Math.PI : 0);
        this.callback(dx, dy, x - this.lastX, y - this.lastY);
        this.lastX = x;
        this.lastY = y;
    };

    end = () => {
        this.moving = false;
    }

    isMoving = () => this.moving;
}
