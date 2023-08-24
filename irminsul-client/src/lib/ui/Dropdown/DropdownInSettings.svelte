<script lang="ts">
    import type { Option } from "$lib/util/Option";
    import {beforeUpdate, createEventDispatcher} from "svelte";
    import Button from "../Button.svelte";
    import DropdownList from "../DropdownList.svelte";

    export let options: Array<Option>;
    export let value: string;
    export let below = true;

    const dispatch = createEventDispatcher();

    let show = false;
    let label = "option";

    function dropdownListChange(e: CustomEvent) {
        label = options.find(op => op.value === e.detail)?.label ?? "option";
        dispatch("dropdown-change", e.detail);
    }

    function click() {
        show = !show;
    }

    beforeUpdate(() => {label = options.find(op => op.value === value)?.label ?? "option";});
</script>

<Button inSettings={true}>
    <div class="down-arrow"/>
    <!-- svelte-ignore a11y-click-events-have-key-events -->
    <div class="label" on:click={click}>{label}</div>
    <DropdownList {options} bind:below bind:show bind:value on:dropdown-list-change={dropdownListChange}/>
</Button>

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
