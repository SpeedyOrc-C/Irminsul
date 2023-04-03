import type {Atom, Cluster} from "./Entity";
import type {RelationBetween} from "./RelationBetween";
import type { Vector2 } from "./Vector2";

export interface Layout {
    rootPosition: Vector2
    rootTranslation: string

    atoms: Array<Atom>
    clusters: Array<Cluster>
    relationsBetween: Array<RelationBetween>
}
