<script lang="ts">
    import DropdownInSettings from "$lib/ui/Dropdown/DropdownInSettings.svelte";
    import ItemBar from "$lib/ui/Settings/ItemBar.svelte";
    import SubCategory from "$lib/ui/Settings/SubCategory.svelte";
    import type { Option } from "$lib/util/Option";
    import {beforeUpdate, createEventDispatcher} from "svelte";
    import { _ } from "svelte-i18n";
    import type RelationGraphSettings from "$lib/relation-graph/RelationGraphSettings";
    import type {ShowJoystick} from "$lib/relation-graph/RelationGraphSettings";

    const dispatch = createEventDispatcher();

    export let settings: RelationGraphSettings;

    export let showAxis: boolean;
    export let showGrid: boolean;
    export let showJoystick: ShowJoystick;

    let options: Array<Option>;
    beforeUpdate(() => {
        options = [
            {value: "off", label: $_("settings.common.off")},
            {value: "on", label: $_("settings.common.on")}
        ];
    });
</script>

<SubCategory>{$_("settings.graphics.relation-graph")}</SubCategory>

<ItemBar text={$_("settings.graphics.show-axis")}>
    <DropdownInSettings {options}
        value={showAxis ? "on" : "off"}
        on:dropdown-change={e => dispatch("set-show-axis", e.detail === "on")}
    />
</ItemBar>

<ItemBar text={$_("settings.graphics.show-grid")}>
    <DropdownInSettings {options}
        value={showGrid ? "on" : "off"}
        on:dropdown-change={e => dispatch("set-show-grid", e.detail === "on")}
    />
</ItemBar>

<ItemBar text={$_("settings.graphics.show-joystick.self")}>
    <DropdownInSettings
        options={[{value: "never", label: $_("settings.common.never")},
                  {value: "has-coarse-pointer", label: $_("settings.graphics.show-joystick.has-coarse-pointer")},
                  {value: "always", label: $_("settings.common.always")}]}
        bind:value={showJoystick}
        on:dropdown-change={e => dispatch("set-show-joystick", e.detail)}
    />
</ItemBar>

<SubCategory>{$_("settings.graphics.performance")}</SubCategory>

<ItemBar text={$_("settings.graphics.reduce-visual-effect")}>
    <DropdownInSettings {options}
        value={settings.preference.reduce_visual_effect ? "on" : "off"}
        on:dropdown-change={e => dispatch("set-reduce-visual-effect", e.detail === "on")}
    />
</ItemBar>
