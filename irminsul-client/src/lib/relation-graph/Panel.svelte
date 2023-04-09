<script lang="ts">
    import ButtonInferior from "$lib/ui/Button/ButtonInferior.svelte";
    import Separator from "$lib/ui/Seperator.svelte";
    import { createEventDispatcher } from "svelte";

    import { _ } from "svelte-i18n";
    import type { PathElement } from "../../model/RelationGraph";
    import Path from "./Path.svelte";

    export let lang: string;
    export let pathElements: Array<PathElement>;

    const dispatch = createEventDispatcher();

    function dispatchRgAction(e: CustomEvent) {
        dispatch("rg-action", e.detail);
    }
</script>

{#key lang}
    <div class="panel font-hywh-65w">
        <Separator />
        <ButtonInferior
            action="open-settings"
            on:button-clicked={dispatchRgAction}
            >{$_("panel.settings")}</ButtonInferior
        >
        <Separator width="4rem" />
        <Path on:rg-action {pathElements} />
    </div>
{/key}

<style lang="scss">
    .panel {
        position: absolute;
        top: 0;
        left: 0;

        display: flex;
        align-items: center;
        width: 100vw;
        padding: 2rem;

        background-color: #3e4457cc;
        backdrop-filter: blur(0.2rem);
        -webkit-backdrop-filter: blur(0.2rem);

        transform: translate(0, calc(-100% + 2rem));
        transition-property: transform, background-color;
        transition-duration: 1s;
        transition-delay: 1s;

        &:hover,
        &:active {
            transform: translate(0, 0);
            transition-duration: 0.2s;
            transition-delay: 0s;
        }
    }
</style>
