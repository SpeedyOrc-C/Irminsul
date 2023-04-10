import type { Canvas } from "../Canvas";
import type { Renderable } from "../Renderable";

export class CloudyWave implements Renderable<CloudyWave> {
    willBeDestroyed = false;

    readonly f = (t: number, k: number, x: number) =>
        k * Math.sin(2 * Math.PI * (x - t)) * 4 * x * (1 - x) * Math.exp(-2 * x)

    readonly waveEffect = (x: number) => {
        if (x >= 1)
            return 0;
        if (x >= 0)
            return 0.5 * (1 - x) ** 3;
        return 0.5 * Math.exp(16 * x);
    }

    renderOn(canvas: Canvas) {
        const t = canvas.getTime();
        const ctx = canvas.getContext();
        let x: number, y: number, yDownward: number, yUpward: number, brightness: number;

        for (let w = 0; w < canvas.width; w++) {
            x = canvas.mapCanvasW(w);
            yDownward = this.f(0.4 * t, 0.7, x);
            yUpward = this.f(0.4 * t + 0.4, 0.7, x);
            for (let h = 0; h < canvas.height; h++) {
                y = canvas.mapCanvasH(h);
                brightness =
                    0.8
                    * (this.waveEffect(yDownward - y)
                        + this.waveEffect(y - yUpward))
                    * (1 - Math.pow(2 * x - 1, 2));

                ctx.fillStyle = `rgba(255,255,255,${brightness})`;
                ctx.fillRect(w, h, 1, 1);
            }
        }
    }

    update(self: CloudyWave, canvas: Canvas) {

    };
}