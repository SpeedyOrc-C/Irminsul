<script lang="ts">
    import {createEventDispatcher} from "svelte";

    const dispatch = createEventDispatcher();

    let self: HTMLElement;

    function click() {
        dispatch("button-clicked");
        self.blur();
    }

    function keydown(e: KeyboardEvent) {
        if (document.activeElement == self && (e.code === "Enter" || e.code === "Space")) {
            click();
        }
    }
</script>

<!-- svelte-ignore a11y-no-noninteractive-tabindex -->
<div class="button-in-settings" bind:this={self} tabindex="0" on:click={click} on:keydown={keydown}>
    <div class="label">
        <slot />
    </div>

    <div class="right-arrow">
        <div class="big-arrow"></div>
        <div class="small-arrow"></div>
    </div>
</div>

<style lang="scss">
    .button-in-settings {
        width: 19rem;
        height: 3.2rem;
        border-radius: 0 99rem 99rem 0;

        box-shadow: none;
        background-color: #d8cdb9;

        -webkit-user-select: none;
        -moz-user-select: none;
        user-select: none;
        cursor: pointer;

        &:active {
            background-image: radial-gradient(
                rgba(255, 242, 205, 0.9),
                rgba(255, 223, 164, 0.5),
                #d8cdb9,
                #d8cdb9
            ) !important;
        }
    }

    .label {
        position: absolute;
        height: 3.2rem;
        line-height: 3.2rem;
        width: 100%;

        text-align: center;
        font-size: 1.3rem;
        color: #485265;
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