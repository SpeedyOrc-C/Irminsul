<script lang="ts">
    import ItemBar from "../../ui/Settings/ItemBar.svelte";
    import { _ } from "svelte-i18n";
    import DropdownInSettings from "../../ui/Dropdown/DropdownInSettings.svelte";
    import {beforeUpdate, createEventDispatcher} from "svelte";
    import type { Option } from "$lib/util/Option";
    import type RelationGraphSettings from "$lib/relation-graph/RelationGraphSettings";

    const dispatch = createEventDispatcher();

    export let settings: RelationGraphSettings;

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
    <DropdownInSettings options={interfaceLanguageOptions}
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
