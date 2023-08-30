<script lang="ts">
    import ItemBar from "$lib/ui/Settings/ItemBar.svelte";
    import { _ } from "svelte-i18n";
    import DropdownInSettings from "$lib/ui/Dropdown/DropdownInSettings.svelte";
    import {beforeUpdate, createEventDispatcher} from "svelte";
    import type { Option } from "$lib/util/Option";
    import type RelationGraphSettings from "../RelationGraphSettings";

    const dispatch = createEventDispatcher();

    export let settings: RelationGraphSettings;

    let whoAmIOptions: Array<Option>;
    beforeUpdate(() => {
        whoAmIOptions = [
            { label: $_("who-am-i.aether"), value: "aether" },
            { label: $_("who-am-i.lumine"), value: "lumine" },
        ];
    });
</script>

<ItemBar text={$_("settings.language.interface-language")}>
    <DropdownInSettings
        options={[
            { label: "简体中文", value: "zh-CN" },
            { label: "English (US)", value: "en-US" },
            { label: "日本語", value: "ja"},
        ]}
        value={settings.preference.language}
        on:dropdown-change={e => dispatch("set-language", e.detail)}
    />
</ItemBar>

<ItemBar text={$_("settings.language.who-am-i")}>
    <DropdownInSettings options={whoAmIOptions}
        value={settings.preference.who_am_i}
        on:dropdown-change={e => dispatch("set-who-am-i", e.detail)}
    />
</ItemBar>
