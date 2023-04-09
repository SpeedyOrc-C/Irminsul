<script lang="ts">
    import ButtonInSettings from "$lib/ui/Button/ButtonInSettings.svelte";
    import ItemBar from "$lib/ui/Settings/ItemBar.svelte";
    import { createEventDispatcher } from "svelte";
    import { _ } from "svelte-i18n";
    import SubCategory from "../../ui/Settings/SubCategory.svelte";

    const dispatch = createEventDispatcher();
</script>

<SubCategory>{$_("settings.file.import")}</SubCategory>

<ItemBar caption={$_("settings.file.import-json")} on:rg-action>
    <ButtonInSettings
        on:button-clicked={() => {
            dispatch("rg-action", { action: "import-json" });
        }}
    >
        {$_("settings.msg.click-to-import")}
    </ButtonInSettings>
</ItemBar>

<SubCategory>{$_("settings.file.export")}</SubCategory>

<ItemBar caption={$_("settings.file.export-json")}>
    <ButtonInSettings
        on:button-clicked={() => {
            dispatch("rg-action", { action: "export-json" });
        }}
    >
        {$_("settings.msg.click-to-export")}
    </ButtonInSettings>
</ItemBar>

<ItemBar caption={$_("settings.file.export-haskell")}>
    <ButtonInSettings
        on:button-clicked={() => {
            dispatch("rg-action", { action: "export-haskell" });
        }}
    >
        {$_("settings.msg.click-to-export")}
    </ButtonInSettings>
</ItemBar>
