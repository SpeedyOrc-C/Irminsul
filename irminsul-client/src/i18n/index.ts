import { init, register } from 'svelte-i18n'

const defaultLocale = 'zh-cn'

register('en-us', () => import('./locales/en-us.json'))
register('zh-cn', () => import('./locales/zh-cn.json'))

init({
	fallbackLocale: defaultLocale,
	initialLocale: defaultLocale,
})
