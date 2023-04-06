<script lang="ts">
    import Line from "./Line.svelte";
</script>

{#each Array.from({ length: 33 }, (_, i) => 15 * (i - 16)) as i}
    <Line
        offset={i}
        thickness="2px"
        color="gray"
        type="horizontal"
    />
    <Line
        offset={i}
        thickness="2px"
        color="gray"
        type="vertical"
    />
{/each}
