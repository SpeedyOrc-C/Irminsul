const acceptedOtherLanguages = new Set<string>(['en', 'en-GB', 'zh', 'zh-TW']);
const availableLanguages = new Set<string>(['en-US', 'zh-CN', 'ja']);

const standardizedLocales = new Map<string, string>([
    ['en', 'en-US'],
    ['en-GB', 'en-US'],
    ['zh', 'zh-CN'],
    ['zh-TW', 'zh-CN'],
]);

export function detectLanguage(): string {
    const preferredLanguages = navigator.languages;
    for (const language of preferredLanguages) {
        if (availableLanguages.has(language)) {
            return language;
        }
        if (acceptedOtherLanguages.has(language)) {
            return standardizedLocales.get(language) ?? "en-US";
        }
    }
    return "en-US";
}
