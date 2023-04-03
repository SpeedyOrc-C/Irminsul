import type { Vector2 } from "./Vector2"

export interface RelationBetween {
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
