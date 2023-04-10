import type {Canvas} from "./Canvas";

export interface Renderable<T> {
    willBeDestroyed: boolean;
    renderOn: (canvas: Canvas) => void;
    update: (self: T, canvas: Canvas) => void;
}
