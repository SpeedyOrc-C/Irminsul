<script lang="ts">
    import Button from "../Button.svelte";
</script>

<Button inSettings={true} on:button-clicked>
    <div class="label">
        <slot />
    </div>

    <div class="right-arrow">
        <div class="big-arrow"></div>
        <div class="small-arrow"></div>
    </div>
</Button>

<style lang="scss">
    .label {
        position: absolute;
        height: 3.2rem;
        line-height: 3.2rem;
        width: 100%;

        text-align: center;
        font-size: 1.3rem;
        color: #3b4255;
    }

    .right-arrow {
        position: absolute;
        width: 1.4rem;
        height: 1.4rem;
        border-radius: 0.7rem;

        top: 1.6rem;
        right: 1.6rem;
        transform: translate(50%, -50%);
        background-color: #3b4255;

        & > .big-arrow {
            position: absolute;
            top: 50%;
            left: 50%;
            transform: translate(-0.3rem, -50%);

            height: 0;
            width: 0;
            border-style: solid;
            border-width: 0.4rem 0 0.4rem 0.693rem;
            border-color: transparent transparent transparent #d4bc8e;
        }
        
        & > .small-arrow {
            position: absolute;
            top: 50%;
            left: 50%;
            transform: translate(calc(-0.05rem - 0.3rem), -50%);

            height: 0;
            width: 0;
            border-style: solid;
            border-width: 0.5rem 0 0.5rem 0.25rem;
            border-color: transparent transparent transparent #3b4255;
    
        }
    }
</style>