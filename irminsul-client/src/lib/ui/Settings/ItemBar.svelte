<script lang="ts">
    import LeftArrow from '../../../asset/img/ui/UI-Settings-Item-LeftFigure-Colored.png'
    import ThreeStars from "../../../asset/img/ui/UI-Settings-Item-ThreeStars-Recolored.png";

    export let text: string;
    export let showStars = true;
</script>

<div class="item-bar">
    <img class="left-arrow" src={LeftArrow} alt="">
    <div class="widget">
        <slot />
    </div>
    <img src={ThreeStars} alt="" class="three-stars" style:display={showStars ? "block" : "none"}>
    <div class="caption">
        <div class="caption-text">
            {text}
        </div>
    </div>
</div>

<style lang="scss">
    .item-bar {
        position: relative;
        //overflow-x: hidden;

        margin: 0.6rem 0.2rem;
        width: calc(100% - 0.4rem);
        height: 3.2rem;
        border-radius: 1.6rem;

        -webkit-user-select: none;
        -moz-user-select: none;
        user-select: none;

        background: rgba(236, 229, 216, 0.9);

        transition-property: box-shadow, background-color;
        transition-duration: 0.1s;

        &:hover {
            box-shadow: 0 0 0 0.15rem white;
            background: rgba(236, 229, 216, 1);
        }

        &:active {
            box-shadow: 0 0 0 0.15rem #ddd;
            background: rgba(236, 229, 216, 1);
        }
    }

    .left-arrow {
        position: absolute;
        display: block;

        height: 3.2rem;
    }

    .three-stars {
        position: absolute;
        display: block;
        right: 17.55rem;

        height: 3.2rem;
    }

    .caption {
        position: absolute;
        width: calc(100% - 29rem);
        overflow-x: auto; overflow-y: hidden;

        left: 1.6rem;
        height: 3.2rem;
    }

    .caption-text {
        font-size: 1.3rem;
        color: #485265;
        line-height: 3.2rem;
        text-align: left;
        white-space: nowrap;
    }

    .widget {
        position: absolute;
        right: 0;
    }
</style>
