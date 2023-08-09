<script lang="ts">
    import ButtonInSettings from "$lib/ui/Button/ButtonInSettings.svelte";
    import {_} from "svelte-i18n";
    import ItemBar from "$lib/ui/Settings/ItemBar.svelte";

    export let text: string;
    export let link: string;
</script>

<ItemBar {text}>
    <ButtonInSettings on:on:button-clicked={() => window.open(link, "_blank")}>
        {$_("settings.msg.click-to-proceed")}
    </ButtonInSettings>
</ItemBar>
