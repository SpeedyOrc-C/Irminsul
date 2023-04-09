<script lang="ts">
    import ButtonInSettings from "$lib/ui/Button/ButtonInSettings.svelte";
    import ItemBar from "$lib/ui/Settings/ItemBar.svelte";
    import { createEventDispatcher } from "svelte";
    import { _ } from "svelte-i18n";

    const dispatch = createEventDispatcher();
</script>

<div class="sub-category">{$_("settings.file.import")}</div>

<ItemBar caption={$_("settings.file.import-json")} on:rg-action>
    <ButtonInSettings
        on:button-clicked={() => {
            dispatch("rg-action", { action: "import-json" });
        }}
    >
        {$_("settings.msg.click-to-import")}
    </ButtonInSettings>
</ItemBar>

<div class="sub-category">{$_("settings.file.export")}</div>

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
