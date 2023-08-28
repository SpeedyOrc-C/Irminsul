<script lang="ts">
    import {createEventDispatcher, onMount} from "svelte";
    import Joystick from "./Joystick";
    import {ShowJoystick} from "./RelationGraphSettings";

    const dispatch = createEventDispatcher();

    export let callback: (dx: number, dy: number) => void;
    export let showJoystick: ShowJoystick;

    let joystick: Joystick;
    let innerCircle: HTMLElement;
    let moving = false;
    let angle = 0;

    function touchStart() {
        moving = true;
        joystick.start();
    }

    function touchMove(e: TouchEvent) {
        const touch = e.targetTouches[0];
        joystick.move(touch.clientX, touch.clientY);
        angle = joystick.angle;

        dispatch("update-view");
    }

    onMount(() => joystick = new Joystick(innerCircle, callback))
</script>

{#if showJoystick !== ShowJoystick.Never}
<div id="joystick" class:has-coarse-pointer={showJoystick === ShowJoystick.HasCoarsePointer}
     on:touchstart|preventDefault={touchStart}
     on:touchmove|preventDefault={touchMove}
     on:touchend|preventDefault={() => moving = false}
>
    <div id="outer-circle" style:transform="translate(-50%, -50%) rotate({-angle + Math.PI / 4}rad)" class:moving>
        <div id="pivot">
            <svg id="direction-indicator" viewBox="0 0 210 210" xmlns="http://www.w3.org/2000/svg">
                <path d="M 105 2.5 A 102.5 102.5 0 0 1 207.5 105"
                      stroke-width="5" stroke="#0ff" fill-opacity="0"/>
            </svg>
        </div>
    </div>
    <div id="inner-circle" bind:this={innerCircle}/>
</div>
{/if}

<style lang="scss">
    #joystick {
        position: absolute;
        bottom: 6vw;
        right: 8vw;

        height: 12vw;
        width: 12vw;

        -webkit-user-select: none;
        -moz-user-select: none;
        user-select: none;

        &.has-coarse-pointer {
            display: none;
            @media (pointer: coarse) {
                display: block;
            }
        }
    }

    #outer-circle {
        position: absolute;
        top: 50%;
        left: 50%;

        height: 14vw;
        width: 14vw;
        border-radius: 100%;
        border: 0.3rem solid #0003;

        opacity: 0;

        &.moving {
          opacity: 100%;
        }
    }

    #pivot {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);

        height: 14vw;
        width: 14vw;

        & > #direction-indicator {
            position: absolute;
            top: 50%;
            left: 50%;
            transform: translate(-50%, -50%);

            height: calc(14vw + 0.6rem);
            width: calc(14vw + 0.6rem);
        }
    }

    #inner-circle {
        position: absolute;
        top: 50%;
        left: 50%;
        transform: translate(-50%, -50%);

        height: 2.5vw;
        width: 2.5vw;
        border-radius: 100%;
        border: 0.3rem solid #fff3;
    }
</style>