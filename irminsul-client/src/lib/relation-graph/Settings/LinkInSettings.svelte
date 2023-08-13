<script lang="ts">
    import ButtonInSettings from "$lib/ui/Button/ButtonInSettings.svelte";
    import {_} from "svelte-i18n";
    import ItemBar from "$lib/ui/Settings/ItemBar.svelte";

    export let text: string;
    export let link: string;
</script>

<ItemBar {text}>
    <a href={link}>
        <ButtonInSettings>{$_("settings.msg.click-to-proceed")}</ButtonInSettings>
    </a>
</ItemBar>

<style lang="scss">
    a {
        color: unset;
        text-decoration: unset;
    }
</style>
