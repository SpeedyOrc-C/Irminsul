<script lang="ts">
    import DropdownInSettings from "$lib/ui/Dropdown/DropdownInSettings.svelte";
    import ItemBar from "$lib/ui/Settings/ItemBar.svelte";
    import type { Option } from "$lib/util/Option";
    import { beforeUpdate, createEventDispatcher } from "svelte";
    import { _ } from "svelte-i18n";
    import type { Writable } from "svelte/store";

    const dispatch = createEventDispatcher();

    export let reduceVisualEffect: Writable<string>;

    let options: Array<Option>
    beforeUpdate(() => {
        options = [
            { label: $_("settings.common.off"), value: "off"},
            { label: $_("settings.common.on"), value: "on"},
        ]
    })
</script>

<ItemBar caption={$_("settings.graphics.reduce-visual-effect")}>
    {#key options}
        <DropdownInSettings
            {options}
            value={reduceVisualEffect}
            on:dropdown-change={(e) => {
                dispatch("rg-action", {
                    action: "change-reduce-visual-effect",
                    value: e.detail.value,
                });
            }}
        />
    {/key}
</ItemBar>
