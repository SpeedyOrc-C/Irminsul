<script lang="ts">
    import ButtonInferior from "$lib/ui/Button/ButtonInferior.svelte";
    import Separator from "$lib/ui/Seperator.svelte";
    import { createEventDispatcher } from "svelte";

    import { _ } from "svelte-i18n";
    import type { PathElement } from "../../model/RelationGraph";
    import Path from "./Path.svelte";

    export let pathElements: Array<PathElement>;

    const dispatch = createEventDispatcher();

    function dispatchRgAction(e: CustomEvent) {
        dispatch("rg-action", e.detail);
    }
</script>

<div class="panel font-hywh-65w">
    <Separator />
    <ButtonInferior action="open-settings" on:button-clicked={dispatchRgAction}
        >{$_("panel.settings")}</ButtonInferior
    >
    <Separator width="4rem" />
    <Path on:rg-action {pathElements} />
</div>

<style lang="scss">
    .panel {
        position: absolute;
        top: 0;
        left: 0;

        display: flex;
        align-items: center;
        width: 100vw;
        height: 4rem;

        background-color: #3e4457cc;
        backdrop-filter: blur(0.2rem);
        -webkit-backdrop-filter: blur(0.2rem);
    }
</style>
