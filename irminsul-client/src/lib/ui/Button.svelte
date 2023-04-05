<script lang="ts">
    import { createEventDispatcher } from "svelte";

    export let label: string;
    export let action: string = '';

    const dispatch = createEventDispatcher();
</script>

<div
    class="button"
    on:click={() => {
        dispatch("button-clicked", { action: action });
    }}
>
    {label}
</div>

<style>
    .button {
        height: 1.2rem;
        padding: 0.5rem 2rem;

        background-color: #ece5d8;
        color: #3b4255;

        border-radius: 1.1rem;

        min-width: 3rem;
        text-align: center;

        user-select: none;
        -webkit-user-select: none;
        cursor: pointer;

        transition: background-color, color, box-shadow;
        transition-duration: 0.2s;
    }

    .button:hover {
        padding: 0.3rem 1.8rem;
        border: 0.2rem solid white;
    }
    
    .button:active {
        padding: 0.4rem 1.9rem;
        margin: 0.1rem;
        border: 0 solid white;

        background-color: #b4a38e;
        color: white;

        box-shadow: 0 0 0.2rem 0.1rem #b4a38edd;
    }
</style>
