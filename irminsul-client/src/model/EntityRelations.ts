
interface PartialRelation {
    id: string
    translation: string
    action: string
}

export interface EntityRelations {
    id: string
    translation: string
    asSubject: Array<PartialRelation>
    asObject: Array<PartialRelation>
    asBoth: Array<PartialRelation>
}
