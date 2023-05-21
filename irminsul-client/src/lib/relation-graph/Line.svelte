<script lang="ts">
    export let offset: number = 0;
    export let length: number = 500;
    export let thickness: string;
    export let color: string;
    export let type: "horizontal" | "vertical" = "horizontal";
</script>

{#if type === "horizontal"}
    <div
        class="line"
        style:width="{length}rem"
        style:height={thickness}
        style:background-color={color}
        style:top="{-offset}rem"
    />
{:else if type === "vertical"}
    <div
        class="line"
        style:height="{length}rem"
        style:width={thickness}
        style:background-color={color}
        style:left="{offset}rem"
    />
{/if}

<style lang="scss">
    .line {
        position: absolute;
        transform: translate(-50%, -50%);
    }
</style>
