<script lang="ts">
    import { _ } from "svelte-i18n";
    import Dialog from "../Dialog.svelte";
    import ButtonBig from "../Button/ButtonBig.svelte";
    import type { Writable } from "svelte/store";

    export let title: string;
    export let show: Writable<boolean>;
</script>

<svelte:body on:keydown={event => {
    if (event.key === "Escape") {
        show.set(false);
    }
}} />

<Dialog {title} {show}>
    <div class="content">
        <slot />
    </div>

    <div class="button-ok">
        <ButtonBig on:button-clicked={() => show.set(false)}>
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
