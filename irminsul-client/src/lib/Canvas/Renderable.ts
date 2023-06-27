import type {Canvas} from "./Canvas";

export interface Renderable {
    willBeDestroyed: boolean;
    renderOn: (canvas: Canvas) => void;
    update: (canvas: Canvas) => void;
}
