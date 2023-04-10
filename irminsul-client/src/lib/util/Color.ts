export class Color {
    readonly r: number;
    readonly g: number;
    readonly b: number;

    constructor(r: number, g: number, b: number) {
        this.r = r;
        this.g = g;
        this.b = b;
    }
}

export const BLACK = new Color(0, 0,  0);
export const WHITE = new Color(255, 255, 255);
export const RED = new Color(255, 0, 0);
export const GREEN = new Color(0, 255, 0);
export const BLUE = new Color(0, 0, 255);
