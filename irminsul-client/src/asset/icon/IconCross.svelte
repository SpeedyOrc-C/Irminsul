<script lang="ts">
    export let color = "black";
</script>

<div id="cross">
    <div id="bar1" class="bar" style:background-color={color}/>
    <div id="bar2" class="bar" style:background-color={color}/>
</div>

<style lang="scss">
    #cross {
        position: relative;
        height: 100%;
        width: 100%;
    }

    .bar {
        position: absolute;
        top: 50%; left: 50%;
        translate: -50% -50%;

        border-radius: 999rem;
        height: 10%; width: 55%;
    }

    #bar1 {
        rotate: 45deg;
    }

    #bar2 {
        rotate: -45deg;
    }
</style>