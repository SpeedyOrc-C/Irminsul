import type { Renderable } from "../Renderable";
import type { Canvas } from "../Canvas";
import type { TimedParticle } from "./Particle";

export class ParticleSystem implements Renderable<ParticleSystem> {
    willBeDestroyed = false;
    particles: Array<TimedParticle> = [];
    additionalUpdate: (self: ParticleSystem) => void;

    constructor(additionalUpdate: (self: ParticleSystem) => void) {
        this.additionalUpdate = additionalUpdate
    }

    renderOn(canvas: Canvas): void {
        this.particles = this.particles.filter((particle) => !particle.willBeDestroyed)
        this.particles.forEach((particle) => particle.renderOn(canvas));
    }

    update(self: ParticleSystem, canvas: Canvas) {
        self.particles.forEach(particle => particle.update(particle, canvas))
        self.additionalUpdate(self)
    }
}