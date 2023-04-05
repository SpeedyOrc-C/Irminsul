<script lang="ts">
    export let width: string;
    export let color: string;
</script>

<div class="horizontal" style="height: {width}; background-color: {color}" />
<div class="vertical" style="width: {width}; background-color: {color}" />

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
