<script>
    import Button from "../Button.svelte";

</script>

<Button height="2.2rem" width="2.2rem" on:button-clicked>
    <div class="content"><slot /></div>
</Button>

<style>
    .content {
        line-height: 2.2rem;
    }
</style>