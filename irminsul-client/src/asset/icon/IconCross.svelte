<script lang="ts">
    export let color = "black";
</script>

<div class="cross">
    <div class="bar1 bar" style:background-color={color}/>
    <div class="bar2 bar" style:background-color={color}/>
</div>

<style lang="scss">
    .cross {
        position: relative;
        height: 100%;
        width: 100%;
    }

    .bar {
        position: absolute;
        top: 50%; left: 50%;

        border-radius: 999rem;
        height: 10%; width: 55%;
    }

    .bar1 {
        transform: translate(-50%, -50%) rotate(45deg);
    }

    .bar2 {
        transform: translate(-50%, -50%) rotate(-45deg);
    }
</style>
