<script lang="ts">
    import DropdownInSettings from "$lib/ui/Dropdown/DropdownInSettings.svelte";
    import ItemBar from "$lib/ui/Settings/ItemBar.svelte";
    import SubCategory from "$lib/ui/Settings/SubCategory.svelte";
    import type { Option } from "$lib/util/Option";
    import {beforeUpdate, createEventDispatcher} from "svelte";
    import { _ } from "svelte-i18n";
    import { writable, type Writable } from "svelte/store";

    const dispatch = createEventDispatcher();

    export let showAxis: Writable<boolean>;
    export let showGrid: Writable<boolean>;
    export let reduceVisualEffect: Writable<string>;

    let options: Array<Option>;
    beforeUpdate(() => {
        options = [
            new Option($_("settings.common.off"), "off"),
            new Option($_("settings.common.on"), "on"),
        ];
    });
</script>

<SubCategory>{$_("settings.graphics.relation-graph")}</SubCategory>

<ItemBar text={$_("settings.graphics.show-axis")}>
    {#key options}
        <DropdownInSettings
            {options}
            value={writable($showAxis ? "on" : "off")}
            on:dropdown-change={(e) => showAxis.set(e.detail.value === "on")}
        />
    {/key}
</ItemBar>

<ItemBar text={$_("settings.graphics.show-grid")}>
    {#key options}
        <DropdownInSettings
            {options}
            value={writable($showGrid ? "on" : "off")}
            on:dropdown-change={(e) => showGrid.set(e.detail.value === "on")}
        />
    {/key}
</ItemBar>

<SubCategory>{$_("settings.graphics.performance")}</SubCategory>

<ItemBar text={$_("settings.graphics.reduce-visual-effect")}>
    {#key options}
        <DropdownInSettings
            {options}
            value={reduceVisualEffect}
            on:dropdown-change={(e) => {
                dispatch("rg-action", {
                    action: "change-reduce-visual-effect",
                    value: e.detail.value
                });
            }}
        />
    {/key}
</ItemBar>
