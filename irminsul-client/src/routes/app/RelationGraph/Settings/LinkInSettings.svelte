<script lang="ts">
    import ButtonInSettings from "$lib/ui/Button/ButtonInSettings.svelte";
    import {_} from "svelte-i18n";
    import ItemBar from "$lib/ui/Settings/ItemBar.svelte";

    export let text: string;
    export let link: string;

    function click() {
        window.open(link, "_blank");
    }
</script>

<ItemBar {text}>
    <ButtonInSettings on:button-clicked={click}>
        {$_("settings.msg.click-to-proceed")}
    </ButtonInSettings>
</ItemBar>
