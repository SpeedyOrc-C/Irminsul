import { init, register } from 'svelte-i18n'

const defaultLocale = 'en-us'

register('en-us', () => import('./locales/en-us.json'));
register('zh-cn', () => import('./locales/zh-cn.json'));
register('ja', () => import('./locales/ja.json'));

init({
	fallbackLocale: defaultLocale,
	initialLocale: defaultLocale,
});
