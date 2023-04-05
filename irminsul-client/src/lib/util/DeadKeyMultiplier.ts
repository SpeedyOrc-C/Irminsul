export function deadKeyMultiplier(e:KeyboardEvent): number {
    if (e.shiftKey)
        return 5
    if (e.altKey)
        return 0.25
    return 1
}
