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