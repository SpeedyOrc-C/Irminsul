<script lang="ts">
    import type Name from "./Name";

    export let name: Name;
</script>

<span class="name">
    {#if name.link != null}
        <a href={name.link} class="text" target="_blank">{name.native}</a>
    {:else}
        {name.native}
    {/if}
    {#if name.roman != null}
        <span class="roman-name">{name.roman}</span>
    {/if}
</span>

<style lang="scss">
    @import "./global.scss";

    .roman-name {
        @extend %common;

        padding-left: 0.5rem;
        font-family: Menlo, Consolas, monospace;

        color: #ece4d844;

        &:hover {
            color: #ffbd22;
        }
    }
</style>