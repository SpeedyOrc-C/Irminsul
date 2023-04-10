export function linearMap(lower: number, value: number, upper: number, newLower: number, newUpper: number ): number {
    const proportion = (value - lower) / (upper - lower);
    return newLower + proportion * (newUpper - newLower);
}
