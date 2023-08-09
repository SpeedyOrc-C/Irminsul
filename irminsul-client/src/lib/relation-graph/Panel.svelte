<script lang="ts">
    import ButtonInferior from "$lib/ui/Button/ButtonInferior.svelte";
    import Separator from "$lib/ui/Seperator.svelte";
    import { createEventDispatcher } from "svelte";

    import { _ } from "svelte-i18n";
    import type {RelationGraph} from "../../model/RelationGraph";
    import Path from "./Path.svelte";

    export let relationGraph: RelationGraph | null;

    export let id: string;

    const dispatch = createEventDispatcher();

    function dispatchRgAction(e: CustomEvent) {
        dispatch("rg-action", e.detail);
    }
</script>

<div class="panel font-hywh-65w">
    <Separator />

    <ButtonInferior action="open-settings" on:button-clicked={dispatchRgAction}>
        {$_("panel.settings")}
    </ButtonInferior>

    <Separator width="4rem" />

    {#if relationGraph != null}
        {@const pathElements = relationGraph.path.concat([{ id: id, translation: relationGraph.rootTranslation }])}
        <Path on:rg-action {pathElements}/>
    {/if}
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
