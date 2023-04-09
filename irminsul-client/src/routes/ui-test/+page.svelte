<script lang="ts">
    import DropdownList from "$lib/ui/DropdownList.svelte";
    import { writable } from "svelte/store";

    let showW = writable(false);
    let valueW = writable("");

    showW.set(true)
</script>

<div class="font-hywh-65w">
    <DropdownList options={[
        {label: "A", value: "1"},
        {label: "B", value: "2"},
        {label: "C", value: "3"},
    ]} {showW} {valueW}></DropdownList>
</div>

<style>
    div {
        padding: 5rem;
        background-color: #b7e2ec;
        height: 100vh;
    }
    @font-face {
        font-family: HYWenHei-65W;
        src: url("/font/HYWenHei-65W.ttf");
    }

    @font-face {
        font-family: HYWenHei-85W;
        src: url("/font/HYWenHei-85W.ttf");
    }

    @font-face {
        font-family: fira-code-regular;
        src: url("/font/FiraCode-Regular.ttf");
    }

    :global(.font-hywh-65w) {
        font-family: HYWenHei-65W, sans-serif;
    }

    :global(.font-hywh-85w) {
        font-family: HYWenHei-85W, sans-serif;
    }
</style>
