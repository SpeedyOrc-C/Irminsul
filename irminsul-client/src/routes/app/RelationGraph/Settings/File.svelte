<script lang="ts">
    import ButtonInSettings from "$lib/ui/Button/ButtonInSettings.svelte";
    import ItemBar from "$lib/ui/Settings/ItemBar.svelte";
    import { createEventDispatcher } from "svelte";
    import { _ } from "svelte-i18n";
    import SubCategory from "$lib/ui/Settings/SubCategory.svelte";

    const dispatch = createEventDispatcher();
</script>

<SubCategory>{$_("settings.file.import")}</SubCategory>

<ItemBar text={$_("settings.file.import-json")}>
    <ButtonInSettings on:button-clicked={() => dispatch("import-json")}>
        {$_("settings.msg.click-to-import")}
    </ButtonInSettings>
</ItemBar>

<SubCategory>{$_("settings.file.export")}</SubCategory>

<ItemBar text={$_("settings.file.export-json")}>
    <ButtonInSettings on:button-clicked={() => dispatch("export-json")}>
        {$_("settings.msg.click-to-export")}
    </ButtonInSettings>
</ItemBar>

<ItemBar text={$_("settings.file.export-haskell")}>
    <ButtonInSettings on:button-clicked={() => dispatch("export-haskell")}>
        {$_("settings.msg.click-to-export")}
    </ButtonInSettings>
</ItemBar>
