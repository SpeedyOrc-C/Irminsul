<script lang="ts">
    export let width: string;
    export let color: string;
</script>

{#each Array.from({ length: 34 }, (_, i) => 15 * i) as i}
    <div
        class="horizontal"
        style="height: {width}; background-color: {color}; top: {i}rem"
    />
    <div
        class="vertical"
        style="width: {width}; background-color: {color}; left: {i}rem"
    />
    <div
        class="horizontal"
        style="height: {width}; background-color: {color}; top: {-i}rem"
    />
    <div
        class="vertical"
        style="width: {width}; background-color: {color}; left: {-i}rem"
    />
{/each}

<style>
    div {
        position: absolute;
        transform: translate(-50%, -50%);
        background-color: white;
    }

    .horizontal {
        width: 1000rem;
    }
    .vertical {
        height: 1000rem;
    }
</style>
