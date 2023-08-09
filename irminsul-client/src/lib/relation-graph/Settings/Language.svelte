<script lang="ts">
    import ItemBar from "../../ui/Settings/ItemBar.svelte";
    import { _ } from "svelte-i18n";
    import DropdownInSettings from "../../ui/Dropdown/DropdownInSettings.svelte";
    import { beforeUpdate } from "svelte";
    import type { Writable } from "svelte/store";
    import type { Option } from "$lib/util/Option";

    export let lang: Writable<string>;
    export let whoAmI: Writable<"aether" | "lumine">;

    export let changeLanguage: () => void;
    export let changeWhoAmI: () => void;

    let interfaceLanguageOptions: Array<Option>;
    let whoAmIOptions: Array<Option>;
    beforeUpdate(() => {
        interfaceLanguageOptions = [
            { label: $_("language.zh-cn"), value: "zh-cn" },
            { label: $_("language.en-us"), value: "en-us" },
        ];
        whoAmIOptions = [
            { label: $_("who-am-i.aether"), value: "aether" },
            { label: $_("who-am-i.lumine"), value: "lumine" },
        ];
    });
</script>

<ItemBar text={$_("settings.language.interface-language")}>
    {#key interfaceLanguageOptions}
        <DropdownInSettings options={interfaceLanguageOptions} value={lang} on:dropdown-change={changeLanguage} />
    {/key}
</ItemBar>

<ItemBar text={$_("settings.language.who-am-i")}>
    {#key whoAmIOptions}
        <DropdownInSettings options={whoAmIOptions} value={whoAmI} on:dropdown-change={changeWhoAmI} />
    {/key}
</ItemBar>
