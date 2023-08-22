<script lang="ts">
    import {_} from "svelte-i18n";
    import Switch from "$lib/ui/Switch.svelte";

    export let showCoordinates: boolean;
    export let editMode: boolean;
</script>

<div id="options">
    <div class="option font-hywh-65w">
        <div class="label">{$_("view-options.show-coordinates")}</div>
        <Switch bind:value={showCoordinates} />
    </div>
    <div class="option font-hywh-65w">
        <div class="label">{$_("view-options.edit-mode")}</div>
        <Switch bind:value={editMode}/>
    </div>
</div>

<style lang="scss">
    #options {
        position: absolute;
        left: 2rem;
        bottom: 2rem;
    }

    .option {
        margin: 1rem 0;
    }

    .label {
        margin: 0.5rem 0 0.5rem 0.5rem;
        color: #fffc;

        user-select: none;
        -moz-user-select: none;
        -webkit-user-select: none;
    }
</style>