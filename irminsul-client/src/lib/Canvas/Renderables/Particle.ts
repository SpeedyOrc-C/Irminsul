import type {Vector2} from "$lib/util/Vector2";
import type {Canvas} from "../Canvas";

export interface ITimedParticle {
    position: Vector2,
    lasts: number,
    t: number;
    duration: number;
    willBeDestroyed: boolean;
}

export class TimedParticle implements ITimedParticle {
    position: Vector2;
    lasts = 0;
    t = 0;
    duration: number;
    willBeDestroyed = false;

    constructor(position: Vector2, duration: number) {
        this.position = position;
        this.duration = duration;
    }

    update(canvas: Canvas): void {
        this.lasts += canvas.getDeltaTime();
        if (this.lasts > this.duration) {
            this.willBeDestroyed = true;
        }
        this.t = this.lasts / this.duration;
    }
}
