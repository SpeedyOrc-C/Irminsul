export function lowercaseFirstLetter(s: string): string {
    return s.charAt(0).toLowerCase() + s.slice(1)
}

export function saveStringAsFile(str: string, filename: string) {
    const blob = new Blob([str], { type: 'plain/text' });
    const url = URL.createObjectURL(blob);
    
    saveDataUrlAsFile(url, filename);
}

export function saveDataUrlAsFile(dataUrl: string, filename: string) {
    const link = document.createElement('a');
    link.download = filename;
    link.href = dataUrl;
    document.body.appendChild(link);
    link.click();
    document.body.removeChild(link);
    URL.revokeObjectURL(dataUrl);
}
