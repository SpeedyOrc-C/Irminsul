<script lang="ts">
    import { _ } from "svelte-i18n";
    import SubCategory from "../../ui/Settings/SubCategory.svelte";
    import LinkInSettings from "$lib/relation-graph/Settings/LinkInSettings.svelte";
    import ItemBar from "$lib/ui/Settings/ItemBar.svelte";
    import ButtonInSettings from "$lib/ui/Button/ButtonInSettings.svelte";
    import {createEventDispatcher} from "svelte";

    const dispatch = createEventDispatcher();
</script>

<SubCategory>{$_("settings.other.tracing-back")}</SubCategory>

<LinkInSettings text={$_("settings.other.github-repository")} link="https://github.com/speedyorc-c/irminsul"/>
<LinkInSettings text={$_("settings.other.give-feedback-on-github")} link="https://github.com/SpeedyOrc-C/Irminsul/issues/new"/>
<LinkInSettings text={$_("settings.other.give-feedback-on-gitee")} link="https://gitee.com/Chen_Zhanming/Irminsul/issues/new"/>
<LinkInSettings text={$_("settings.other.yuanshen-official-website")} link="https://ys.mihoyo.com/main/"/>
<LinkInSettings text={$_("settings.other.genshin-impact-official-website")} link="https://genshin.hoyoverse.com/en/home"/>

<SubCategory>{$_("settings.other.wiki")}</SubCategory>

<LinkInSettings text={$_("settings.other.miyoushe-obs")} link="https://bbs.mihoyo.com/ys/obc"/>
<LinkInSettings text={$_("settings.other.hoyolab-hoyowiki")} link="https://wiki.hoyolab.com"/>
<LinkInSettings text={$_("settings.other.fandom")} link="https://genshin-impact.fandom.com"/>
<LinkInSettings text={$_("settings.other.bwiki")} link="https://wiki.biligame.com/ys"/>

<SubCategory>{$_("settings.other.reset")}</SubCategory>

<ItemBar text={$_("settings.other.reset-all")}>
    <ButtonInSettings on:button-clicked={() => dispatch("reset-all")}>
        {$_("settings.msg.click-to-reset")}
    </ButtonInSettings>
</ItemBar>
