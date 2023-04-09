<script lang="ts">
    import Button from "../Button.svelte";

    export let action: string = "";
</script>

<Button height="2.2rem" style="min-width: 10rem" {action} on:button-clicked>
    <div class="content"><slot /></div>
</Button>

<style>
    .content {
        line-height: 2.2rem;
        padding: 0 1.1rem;
        text-align: center;
        color: #3b4255;
    }
</style>
