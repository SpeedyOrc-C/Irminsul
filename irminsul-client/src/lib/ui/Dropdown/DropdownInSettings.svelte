<script lang="ts">
    import type { Option } from "$lib/util/Option";
    import {afterUpdate, beforeUpdate, createEventDispatcher} from "svelte";
    import Button from "../Button.svelte";
    import DropdownList from "../DropdownList.svelte";
    import ButtonInSettings from "$lib/ui/Button/ButtonInSettings.svelte";

    export let options: Array<Option>;
    export let value: string;

    const dispatch = createEventDispatcher();

    let show = false;
    let label = "option";

    function dropdownListChange(e: CustomEvent) {
        label = options.find(op => op.value === e.detail)?.label ?? "option";
        dispatch("dropdown-change", e.detail);
    }

    function click() {
        console.log("click");
        show = !show;
    }

    afterUpdate(() => {label = options.find(op => op.value === value)?.label ?? "option";});
</script>

<ButtonInSettings on:button-clicked={click}>
        <div class="down-arrow"/>
        <div class="label">{label}</div>
        <DropdownList {options} bind:show bind:value on:dropdown-list-change={dropdownListChange}/>
</ButtonInSettings>

<style lang="scss">
    .label {
        position: absolute;
        height: 3.2rem;
        line-height: 3.2rem;
        width: 100%;

        text-align: center;
        font-size: 1.3rem;
        color: #3b4255;
    }

    .down-arrow {
        position: absolute;
        top: calc(50% - 0.4rem);
        right: 1rem;

        width: 0;
        height: 0;
        border-style: solid;
        border-color: #3b4255 transparent transparent transparent;
        border-width: 0.7rem 0.55rem 0 0.55rem;
    }
</style>
