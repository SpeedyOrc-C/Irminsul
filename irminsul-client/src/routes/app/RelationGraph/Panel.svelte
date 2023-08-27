<script lang="ts">
    import ButtonInferior from "$lib/ui/Button/ButtonInferior.svelte";
    import Separator from "$lib/ui/Seperator.svelte";
    import { createEventDispatcher } from "svelte";

    import { _ } from "svelte-i18n";
    import type {RelationGraph} from "./RelationGraph";
    import Path from "./Path.svelte";

    export let relationGraph: RelationGraph | null;
    export let id: string;
    export let editing: boolean;

    const dispatch = createEventDispatcher();
</script>

<div class="panel font-hywh-65w">
    <Separator />

    <ButtonInferior action="open-settings" on:button-clicked={() => dispatch("open-settings")}>
        {$_("panel.settings")}
    </ButtonInferior>

    <Separator width="4rem" />

    {#if relationGraph != null}
        {@const pathElements = relationGraph.path.concat([{ id: id, translation: relationGraph.rootTranslation }])}
        <Path enabled={!editing} on:jump-to {pathElements}/>
    {/if}
</div>

<style lang="scss">
    .panel {
        position: absolute;
        top: 0;
        left: 0;

        display: flex;
        align-items: center;
        width: 100%;
        height: 4rem;

        background-color: #3e4457cc;
        -webkit-backdrop-filter: blur(0.2rem);
        backdrop-filter: blur(0.2rem);
    }
</style>
