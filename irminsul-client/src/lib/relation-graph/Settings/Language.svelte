<script lang="ts">
    import ItemBar from "../../ui/Settings/ItemBar.svelte";
    import { _ } from "svelte-i18n";
    import DropdownInSettings from "../../ui/Dropdown/DropdownInSettings.svelte";
    import { beforeUpdate } from "svelte";
    import type { Writable } from "svelte/store";
    import type { Option } from "$lib/util/Option";

    export let lang: Writable<string>;

    let options: Array<Option>;
    beforeUpdate(() => {
        options = [
            { label: $_("language.zh-cn"), value: "zh-cn" },
            { label: $_("language.en-us"), value: "en-us" },
        ];
    });
</script>

<ItemBar caption={$_("settings.language.interface-language")}>
    {#key options}
        <DropdownInSettings {options} value={lang} />
    {/key}
</ItemBar>
