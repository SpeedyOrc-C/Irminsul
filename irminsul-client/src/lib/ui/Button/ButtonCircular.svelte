<script>
    import Button from "../Button.svelte";

</script>

<Button height="2.5rem" width="2.5rem" hasBorder={true} on:button-clicked>
    <div class="content"><slot /></div>
</Button>

<style>
    .content {
        line-height: 2.2rem;
    }
</style>