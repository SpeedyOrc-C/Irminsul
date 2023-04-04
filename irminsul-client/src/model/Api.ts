export type ApiStatusCode
    = "OK"
    | "UnsupportedLanguage"
    | "NotImplementedCluster"
    | "NotImplementedEntity"

export interface ApiResponse<T> {
    status: ApiStatusCode
    body: T
}
