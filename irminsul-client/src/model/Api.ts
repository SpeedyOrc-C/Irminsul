export type ApiStatusCode
    = "OK"
    | "UnsupportedLanguage"
    | "NotImplementedCluster"

export interface ApiResponse<T> {
    status: ApiStatusCode
    body: T
}
