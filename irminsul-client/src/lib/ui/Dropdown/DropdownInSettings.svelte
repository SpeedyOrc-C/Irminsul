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

    beforeUpdate(() => {label = options.find(op => op.value === value)?.label ?? "option";});
</script>

<Button inSettings={true}>
    <!-- svelte-ignore a11y-click-events-have-key-events -->
    <div class="label" on:click={() => {show = !show}}>{label}</div>
    <div class="down-arrow"/>
    <div class="dropdown-list" class:below>
        <DropdownList {options} bind:show bind:value on:dropdown-list-change={dropdownListChange}/>
    </div>
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
    .dropdown-list {
        position: absolute;
        width: 100%;

        top: unset;
        bottom: 3.5rem;
        
        z-index: 1;

        &.below {
            top: 3.2rem;
            bottom: unset;
        }
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
