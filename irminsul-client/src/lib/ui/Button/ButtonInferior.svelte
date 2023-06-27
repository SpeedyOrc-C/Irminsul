<script lang="ts">
    import Button from "../Button.svelte";

    export let action = "";
</script>

<Button height="2.2rem" style="min-width: 10rem" {action} on:button-clicked>
    <div class="content font-hywh-85w"><slot /></div>
</Button>

<style>
    .content {
        line-height: 2.2rem;
        padding: 0 1.1rem;
        text-align: center;
        
        color: #3b4255;
        font-size: 1.1rem;
    }
</style>
