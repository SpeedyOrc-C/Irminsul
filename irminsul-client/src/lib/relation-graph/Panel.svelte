<script lang="ts">
    import Button from "$lib/ui/Button.svelte";
    import Separator from "$lib/ui/Seperator.svelte";
    import { createEventDispatcher, getContext } from "svelte";

    import { dictionary, locale, _ } from "svelte-i18n";
    locale.set(getContext("lang"));

    dictionary.set({
        "zh-cn": {
            panel: {
                "import-json": "导入 JSON",
                "export-json": "导出 JSON",
                "export-haskell": "导出 Haskell",
                "go-to-parent": "返回上一层",
            },
        },
        "en-us": {
            panel: {
                "import-json": "Import from JSON",
                "export-json": "Export as JSON",
                "export-haskell": "Export as Haskell",
                "go-to-parent": "Go to parent",
            },
        },
    });

    const dispatch = createEventDispatcher();

    function buttonClicked(e: CustomEvent) {
        dispatch("panel-clicked", e.detail);
    }
</script>

<div class="panel font-hywh-65w">
    <Button
        label={$_("panel.go-to-parent")}
        action="go-to-parent"
        on:button-clicked={buttonClicked}
    />
    <Separator width="3rem"/>
    <Button
        label={$_("panel.import-json")}
        action="import-json"
        on:button-clicked={buttonClicked}
    />
    <Separator />
    <Button
        label={$_("panel.export-json")}
        action="save-as-JSON"
        on:button-clicked={buttonClicked}
    />
    <Separator />
    <Button
        label={$_("panel.export-haskell")}
        action="export-haskell"
        on:button-clicked={buttonClicked}
    />
</div>

<style>
    .panel {
        position: absolute;
        top: 0;
        left: 0;
        width: 100vw;
        padding: 1rem;

        display: flex;

        background-color: #3e4457ee;
    }
</style>
