import { lowercaseFirstLetter } from "$lib/util/String";
import type { Vector2 } from "$lib/util/Vector2";

interface Atom {
    id: string
    translation: string

    position: Vector2
}

interface Cluster {
    id: string
    translation: string
    elements: Array<string>

    position: Vector2
    anchor: Vector2
    size: Vector2
}

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

export interface PathElement {
    id: string
    translation: string
}

export interface RelationGraph {
    id: string
    path: Array<PathElement>
    rootPosition: Vector2
    rootTranslation: string

    atoms: Array<Atom>
    clusters: Array<Cluster>
    relationsBetween: Array<RelationBetween>
}

export function dumpRelationGraph2Haskell(rg: RelationGraph): string {
    return [
        `(layout (${rg.rootPosition.x}, ${rg.rootPosition.y}) [`,
        [
            ...(rg.atoms.length > 0 ?
                [rg.atoms.map(atom =>
                    `    al ` +
                    `${lowercaseFirstLetter(atom.id)} ` +
                    `(${atom.position.x}, ${atom.position.y})`
                ).join(',\n')] : []),

            ...(rg.clusters.length > 0 ?
                [rg.clusters.map(cluster =>
                    `    cl ` +
                    `${lowercaseFirstLetter(cluster.id)} ` +
                    `(${cluster.position.x}, ${cluster.position.y}) ` +
                    `(${cluster.anchor.x}, ${cluster.anchor.y}) ` +
                    `(${cluster.size.x}, ${cluster.size.y})`
                ).join(',\n')] : []),
        ].join(',\n\n'),
        '])',
    ].join('\n')
}
