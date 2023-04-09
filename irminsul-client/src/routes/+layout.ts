import type { LayoutLoad } from "./$types";
import { locale } from "svelte-i18n";
import "../i18n/index";

export const prerender = true;
export const trailingSlash = 'always';

export const load: LayoutLoad = async () => {
    locale.set('zh-cn')
}
