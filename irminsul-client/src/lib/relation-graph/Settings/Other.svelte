<script lang="ts">
    import ButtonInSettings from "$lib/ui/Button/ButtonInSettings.svelte";
    import ItemBar from "$lib/ui/Settings/ItemBar.svelte";
    import { _ } from "svelte-i18n";
    import SubCategory from "../../ui/Settings/SubCategory.svelte";

    function openLink(link: string) {
        window.open(link, "_blank");
    }
</script>

<SubCategory>{$_("settings.other.tracing-back")}</SubCategory>

<ItemBar caption={$_("settings.other.github-repository")}>
    <ButtonInSettings
        on:button-clicked={() =>
            openLink("https://github.com/speedyorc-c/irminsul")}
    >
        {$_("settings.msg.click-to-proceed")}
    </ButtonInSettings>
</ItemBar>

<ItemBar caption={$_("settings.other.yuanshen-official-website")}>
    <ButtonInSettings
        on:button-clicked={() => openLink("https://ys.mihoyo.com/main/")}
    >
        {$_("settings.msg.click-to-proceed")}
    </ButtonInSettings>
</ItemBar>

<ItemBar caption={$_("settings.other.genshin-impact-official-website")}>
    <ButtonInSettings
        on:button-clicked={() =>
            openLink("https://genshin.hoyoverse.com/en/home")}
    >
        {$_("settings.msg.click-to-proceed")}
    </ButtonInSettings>
</ItemBar>

<SubCategory>{$_("settings.other.wiki")}</SubCategory>

<ItemBar caption={$_("settings.other.miyoushe-obs")}>
    <ButtonInSettings
        on:button-clicked={() => openLink("https://bbs.mihoyo.com/ys/obc")}
    >
        {$_("settings.msg.click-to-proceed")}
    </ButtonInSettings>
</ItemBar>

<ItemBar caption={$_("settings.other.hoyolab-hoyowiki")}>
    <ButtonInSettings
        on:button-clicked={() => openLink("https://wiki.hoyolab.com")}
    >
        {$_("settings.msg.click-to-proceed")}
    </ButtonInSettings>
</ItemBar>

<ItemBar caption={$_("settings.other.fandom")}>
    <ButtonInSettings
        on:button-clicked={() => openLink("https://genshin-impact.fandom.com")}
    >
        {$_("settings.msg.click-to-proceed")}
    </ButtonInSettings>
</ItemBar>

<ItemBar caption={$_("settings.other.bwiki")}>
    <ButtonInSettings
        on:button-clicked={() => openLink("https://wiki.biligame.com/ys")}
    >
        {$_("settings.msg.click-to-proceed")}
    </ButtonInSettings>
</ItemBar>
