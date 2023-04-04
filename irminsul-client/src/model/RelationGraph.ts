import type { Vector2 } from "./Vector2";

interface Atom {
    id: string
    translation: string

    position: Vector2
}

interface Cluster {
    id: string
    translation: string

    position: Vector2
    anchor: Vector2
    width: number
    height: number
}

interface RelationBetween {
    subjectId: string
    objectId: string

    forwardRelations: Array<string>
    backwardRelations: Array<string>
    biRelations: Array<string>

    width: number
    position: Vector2
    rotation: number

    needReverse: boolean
}

export interface RelationGraph {
    id: string
    rootPosition: Vector2
    rootTranslation: string

    atoms: Array<Atom>
    clusters: Array<Cluster>
    relationsBetween: Array<RelationBetween>
}
