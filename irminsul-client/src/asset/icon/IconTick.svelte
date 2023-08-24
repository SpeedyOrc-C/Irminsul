<script lang="ts">
    export let color = "black";
</script>

<div id="tick">
    <div id="bar1" class="bar" style:background-color={color}/>
    <div id="bar2" class="bar" style:background-color={color}/>
</div>

<style lang="scss">
    #tick {
        position: relative;
        height: 100%;
        width: 100%;
    }

    .bar {
        position: absolute;
        top: 50%; left: 50%;

        border-radius: 999rem;
        height: 10%;
    }

    #bar1 {
        width: 35%;
        transform: translate(-100%, 70%) rotate(45deg);
    }

    #bar2 {
        width: 60%;
        transform: translate(-30%, -30%) rotate(-45deg);
    }
</style>
