<script lang="ts">
    export let relation: string;
    export let flip = false;
    export let cjkRotateLeft = false;
</script>

<span class="relation-text" class:flip>
    {#if cjkRotateLeft}
        {#each relation.split("") as cjkCharacter}
            <span class="cjk" class:rotate-left={cjkRotateLeft}>
                {cjkCharacter}
            </span>
        {/each}
    {:else}
        {relation}
    {/if}
</span>

<style>
    .relation-text {
        display: inline-block;
        &.flip {
            transform: rotate(180deg);
        }
    }

    .cjk {
        display: inline-block;
        &.rotate-left  {
            transform: rotate(-90deg);
        }
    }
</style>