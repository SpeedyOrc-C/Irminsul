<script lang="ts">
    import {_} from "svelte-i18n";
    import Switch from "$lib/ui/Switch.svelte";

    export let showCoordinates: boolean;
    export let editing: boolean;
    export let hideUi: boolean;
</script>

<div id="view-options" class:hidden={hideUi}>
    <div class="option font-hywh-65w">
        <div class="label">{$_("view-options.show-coordinates")}</div>
        <Switch bind:value={showCoordinates} />
    </div>
    <div class="option font-hywh-65w">
        <div class="label">{$_("view-options.edit-mode")}</div>
        <Switch bind:value={editing} />
    </div>
</div>

<style lang="scss">
    #view-options {
        position: absolute;
        left: 2rem;
        bottom: 2rem;

        transition-property: transform, box-shadow, background-color;
        transition-duration: 0.5s;

        background-color: #0003;
        box-shadow: 0 0 5rem 5rem #0003;
        transform: translate(0, 0);

        &.hidden {
            box-shadow: 0 0 5rem 5rem transparent;
            background-color: transparent;
            transform: translate(calc(-100% - 2rem), 0);
        }

        @media print {
            display: none;
        }
    }

    .option {
        margin: 1rem 0;
    }

    .label {
        margin: 0.5rem 0 0.5rem 0.5rem;
        color: #fffc;

        -moz-user-select: none;
        -webkit-user-select: none;
        user-select: none;
    }
</style>