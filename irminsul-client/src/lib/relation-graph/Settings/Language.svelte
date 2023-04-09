<script lang="ts">
    import ItemBar from "../../ui/Settings/ItemBar.svelte";
    import { _ } from "svelte-i18n";
    import DropdownInSettings from "../../ui/Dropdown/DropdownInSettings.svelte";
    import { afterUpdate, createEventDispatcher } from "svelte";
    import type { Writable } from "svelte/store";
    import type { Option } from "$lib/util/Option";
    const dispatch = createEventDispatcher();

    export let langW: Writable<string>;

    let options: Array<Option> = [];
    afterUpdate(() => {
        options = [
            { label: $_("language.zh-cn"), value: "zh-cn" },
            { label: $_("language.en-us"), value: "en-us" },
        ];
    });
</script>

<ItemBar caption={$_("settings.language.interface-language")}>
    {#key options}
        <DropdownInSettings
            {options}
            valueW={langW}
            on:dropdown-change={(e) => {
                dispatch("rg-action", {
                    action: "change-lang",
                    lang: e.detail.value,
                });
            }}
        />
    {/key}
</ItemBar>
