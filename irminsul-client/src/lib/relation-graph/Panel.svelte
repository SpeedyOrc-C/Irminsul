<script lang="ts">
    import Button from "$lib/ui/Button.svelte";
    import Separator from "$lib/ui/Seperator.svelte";
    import { createEventDispatcher } from "svelte";

    import { dictionary, locale, _ } from "svelte-i18n";
    import type { PathElement } from "../../model/RelationGraph";
    import Path from "./Path.svelte";

    export let lang: string;
    export let pathElements: Array<PathElement>;

    locale.set(lang);
    dictionary.set({
        "zh-cn": {
            panel: {
                "import-json": "导入 JSON",
                "export-json": "导出 JSON",
                "export-haskell": "导出 Haskell",
            },
        },
        "en-us": {
            panel: {
                "import-json": "Import from JSON",
                "export-json": "Export as JSON",
                "export-haskell": "Export as Haskell",
            },
        },
    });

    const dispatch = createEventDispatcher();

    function dispatchRgAction(e: CustomEvent) {
        dispatch("rg-action", e.detail);
    }
</script>

<div class="panel font-hywh-65w">
    <Button
        label={$_("panel.import-json")}
        action="import-json"
        on:button-clicked={dispatchRgAction}
    />
    <Separator />
    <Button
        label={$_("panel.export-json")}
        action="export-json"
        on:button-clicked={dispatchRgAction}
    />
    <Separator />
    <Button
        label={$_("panel.export-haskell")}
        action="export-haskell"
        on:button-clicked={dispatchRgAction}
    />
    <Separator width="4rem"/>
    <Path on:rg-action {pathElements} />
</div>

<style>
    .panel {
        position: absolute;
        top: 0;
        left: 0;

        display: flex;
        align-items: center;
        width: 100vw;
        padding: 1rem;

        background-color: #3e4457ee;
    }
</style>
