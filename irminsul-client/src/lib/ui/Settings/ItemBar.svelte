<script lang="ts">
    export let caption: string;
</script>

<div class="item-bar">
    <div class="background"></div>
    <div class="left-figure"></div>
    <div class="right-stars"></div>
    <div class="caption">{caption}</div>
    <div class="widget">
        <slot />
    </div>
</div>

<style lang="scss">
    .item-bar {
        margin: 0.6rem 0.2rem;

        width: calc(100% - 0.4rem);
        height: 3.2rem;
        border-radius: 1.6rem;

        user-select: none;
        -webkit-user-select: none;
        -moz-user-select: none;

        background-color: #ece5d8;
        opacity: 80%;

        transition-property: box-shadow background-color;
        transition-duration: 0.1s;
        
        &:hover {
            box-shadow: 0 0 0 0.18rem white;
            opacity: 100%;
        }
        
        &:active {
            box-shadow: 0 0 0 0.18rem #ddd;
            opacity: 100%;
        }
    }

    .caption {
        position: absolute;
        left: 1.6rem;

        height: 3.2rem;
        line-height: 3.2rem;

        font-size: 1.3rem;
        color: #3b4255;
    }

    .widget {
        position: absolute;
        right: 0.18rem;
    }
</style>
