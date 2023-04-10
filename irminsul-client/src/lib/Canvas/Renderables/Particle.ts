import type {Renderable} from "../Renderable";
import type {Vector2} from "../../util/Vector2";
import type {Canvas} from "../Canvas";

export class TimedParticle implements Renderable<TimedParticle> {
    position: Vector2;
    lasts = 0;
    t = 0;
    duration: number;
    willBeDestroyed = false;

    constructor(position: Vector2, duration: number) {
        this.position = position;
        this.duration = duration;
    }

    renderOn(canvas: Canvas) {

    }

    update(_self: TimedParticle, canvas: Canvas) {
        this.lasts += canvas.getDeltaTime();
        if (this.lasts > this.duration) {
            this.willBeDestroyed = true;
        }
        this.t = this.lasts / this.duration;
    }
}
