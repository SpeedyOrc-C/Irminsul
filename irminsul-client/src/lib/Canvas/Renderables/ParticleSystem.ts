import type { Renderable } from "../Renderable";
import type { Canvas } from "../Canvas";
import type { ITimedParticle } from "./Particle";

type RenderableTimeParticle = Renderable & ITimedParticle;

export class ParticleSystem implements Renderable {
    willBeDestroyed = false;
    particles: Array<RenderableTimeParticle> = [];
    additionalUpdate: (self: ParticleSystem) => void;

    constructor(additionalUpdate: (self: ParticleSystem) => void) {
        this.additionalUpdate = additionalUpdate
    }

    renderOn(canvas: Canvas): void {
        this.particles = this.particles.filter((particle) => !particle.willBeDestroyed)
        this.particles.forEach((particle) => particle.renderOn(canvas));
    }

    update(canvas: Canvas): void {
        this.particles.forEach(particle => particle.update(canvas))
        this.additionalUpdate(this)
    }
}