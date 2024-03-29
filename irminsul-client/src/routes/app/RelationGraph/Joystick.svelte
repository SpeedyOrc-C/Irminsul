<script lang="ts">
    import {afterUpdate, createEventDispatcher} from "svelte";
    import Joystick from "./Joystick";
    import {ShowJoystick} from "./RelationGraphSettings";

    const dispatch = createEventDispatcher();

    export let callback: (x: number, y: number, dx: number, dy: number) => void;
    export let showJoystick: ShowJoystick;
    export let hideUi: boolean;
    export let smoothMovement: boolean;

    let joystick: Joystick;
    let innerCircle: HTMLElement;
    let angle = 0;

    let lastX = 0;
    let lastY = 0;

    $: show = showJoystick !== ShowJoystick.Never && !hideUi;

    function touchStart(e: TouchEvent) {
        const touch = e.targetTouches[0];
        lastX = touch.clientX;
        lastY = touch.clientY;

        joystick.start();
        joystick = joystick;

        smoothMovement = false;
        loop();
    }

    function touchMove(e: TouchEvent) {
        const touch = e.targetTouches[0];
        lastX = touch.clientX;
        lastY = touch.clientY;
    }

    function loop() {
        requestAnimationFrame(() => {
            if (joystick.isMoving()) {
                joystick.move(lastX, lastY);
                angle = joystick.angle;
                dispatch("update-view");

                loop();
            }
        });
    }

    function touchEnd() {
        joystick.end();
        joystick = joystick;

        smoothMovement = true;
    }

    afterUpdate(() => {
        if (!joystick?.isMoving() && show) {
            joystick = new Joystick(innerCircle, callback);
        }
    })
</script>

{#if show}
<div id="joystick" class:has-coarse-pointer={showJoystick === ShowJoystick.HasCoarsePointer}
     on:touchstart|preventDefault={touchStart}
     on:touchmove|preventDefault={touchMove}
     on:touchend|preventDefault={touchEnd}
>
    <div id="outer-circle" style:transform="translate(-50%, -50%) rotate({-angle + Math.PI / 4}rad)" class:moving={joystick?.isMoving()}>
        <div class="direction-indicator"/>
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

    .direction-indicator {
        $width: 0.3rem;

        position: absolute;
        height: calc(50% + $width); width: calc(50% + $width);
        top: -$width; right: -$width;

        overflow: hidden;

        &:after {
            content: "";
            position: absolute;
            height: calc(2 * (100% - $width)); width: calc(2 * (100% - $width));
            right: $width; top: $width;

            border-radius: 100%;
            box-shadow: 0 0 0 $width #0ff;
        }
    }
</style>