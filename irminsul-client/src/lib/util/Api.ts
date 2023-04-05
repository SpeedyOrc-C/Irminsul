export type ApiStatusCode
    = "OK"
    | "UnsupportedLanguage"
    | "NotImplementedCluster"
    | "NotImplementedEntity"
    | "LayoutMissing"

export interface ApiResponse<T> {
    status: ApiStatusCode
    body: T
}
