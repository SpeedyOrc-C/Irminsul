import { init, register } from 'svelte-i18n'

const defaultLocale = 'en-US'

init({
	fallbackLocale: defaultLocale,
	initialLocale: defaultLocale,
});

register('en-US', () => import('./locales/en-US.json'));
register('zh-CN', () => import('./locales/zh-CN.json'));
register('ja', () => import('./locales/ja.json'));
