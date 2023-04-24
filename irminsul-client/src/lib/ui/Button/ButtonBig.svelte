<script>
    import Button from "../Button.svelte";

</script>

<Button height="3rem" style="min-width: 15rem" on:button-clicked>
    <div class="content font-hywh-85w"><slot /></div>
</Button>

<style>
    .content {
        line-height: 3rem;
        padding: 0 1.1rem;
        text-align: center;
        
        color: #3b4255;
        font-size: 1.3rem;
    }
</style>
