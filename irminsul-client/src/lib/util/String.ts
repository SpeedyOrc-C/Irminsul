export function lowercaseFirstLetter(s: string): string {
    return s.charAt(0).toLowerCase() + s.slice(1)
}

export function saveStringAsFile(str: string, filename: string) {
    const blob = new Blob([str], { type: 'text/plain' });
    const url = URL.createObjectURL(blob);
    const link = document.createElement('a');
    link.download = filename;
    link.href = url;
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    URL.revokeObjectURL(url);
}
