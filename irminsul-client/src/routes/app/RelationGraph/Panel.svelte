<script lang="ts">
    import ButtonInferior from "$lib/ui/Button/ButtonInferior.svelte";
    import Separator from "$lib/ui/Seperator.svelte";
    import {createEventDispatcher} from "svelte";

    import { _ } from "svelte-i18n";
    import type {RelationGraph} from "./RelationGraph";
    import Path from "./Path.svelte";

    export let relationGraph: RelationGraph | null;
    export let editing: boolean;
    export let hideUi: boolean;

    const dispatch = createEventDispatcher();
</script>

<div class="panel font-hywh-65w" class:hidden={hideUi}>
    <ButtonInferior on:button-clicked={() => dispatch("open-settings")}>
        {$_("panel.settings")}
    </ButtonInferior>

    {#if relationGraph != null}
        <Separator width="2rem" />

        <ButtonInferior on:button-clicked={() => hideUi = true}>
            {$_("panel.hide-ui")}
        </ButtonInferior>

        <Separator width="2rem" />

        <Path enabled={!editing} on:jump-to {relationGraph}/>
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
        padding: 0 1rem;

        background-color: #3e4457cc;
        -webkit-backdrop-filter: blur(0.2rem);
        backdrop-filter: blur(0.2rem);

        transition-property: transform;
        transition-duration: 0.5s;
        transform: translate(0, 0);

        &.hidden {
            transform: translate(0, -100%);
        }

        @media print {
            display: none;
        }
    }
</style>
