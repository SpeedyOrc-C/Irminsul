<script lang="ts">
    import { _ } from "svelte-i18n";
    import Dialog from "../Dialog.svelte";
    import ButtonBig from "../Button/ButtonBig.svelte";

    export let title: string;
    export let show: boolean;

    function keydown(e: KeyboardEvent) {
        if (e.code === "Escape") {
            show = false;
        }
    }
</script>

<svelte:window on:keydown={keydown} />

<Dialog {title} bind:show>
    <div class="content"><slot /></div>

    <div class="button-ok">
        <ButtonBig on:button-clicked={() => show = false}>
            {$_("prompt.button.confirm")}
        </ButtonBig>
    </div>
</Dialog>

<style lang="scss">
    .content {
        position: absolute;
        width: 85%;
        top: 45%;
        left: 50%;
        transform: translate(-50%, -50%);

        text-align: center;
    }

    .button-ok {
        position: absolute;
        bottom: 1.5rem;
        left: 50%;
        transform: translateX(-50%);
    }
</style>
