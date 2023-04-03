import type { Vector2 } from "./Vector2"

export interface Atom {
    id: string
    translation: string

    position: Vector2
}

export interface Cluster {
    id: string
    translation: string

    position: Vector2
    width: number
    height: number
}
