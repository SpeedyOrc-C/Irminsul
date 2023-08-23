<script lang="ts">
    import RootCluster_Background from "../../../../asset/img/ui/RootCluster-Background.png";
    import type Editor from "./Editor";
    import {afterUpdate, createEventDispatcher} from "svelte";

    const dispatch = createEventDispatcher();

    export let editor: Editor;

    let selected = false;
    let dim = false;

    afterUpdate(() => {
        selected = editor.isSelected(editor.relationGraph.id);
        dim = !editor.isEditing() && !editor.isSelected(editor.relationGraph.id) && editor.numSelected() > 0;
    })

    function click() {
        dispatch("toggle-root");
    }
</script>

<!-- svelte-ignore a11y-click-events-have-key-events -->
<div id="root-cluster" class:selected class:dim on:click={click}
     style:left="{editor.relationGraph.rootPosition.x}rem"
     style:top="{-editor.relationGraph.rootPosition.y}rem"
>
    <img id="root-cluster-background" src={RootCluster_Background} alt=""/>
    <div id="label" class="font-hywh-85w">
        {editor.relationGraph.rootTranslation}
    </div>
</div>

<style lang="scss">
    #root-cluster {
        position: absolute;
        transform: translate(-50%, -50%);

        z-index: 1000;

        user-select: none;
        -webkit-user-select: none;
        -moz-user-select: none;

        transition-property: filter;
        transition-duration: 0.2s;

        &.dim {
            filter: brightness(50%) blur(0.1rem);

            &:hover {
                filter: unset;
            }
        }
    }

    #root-cluster-background {
        position: absolute;
        transform: translate(-50%, -50%);

        width: 20rem;
        height: calc(20rem * 101 / 327);

        border-radius: 0.5rem;

        #root-cluster.selected & {
            border: 0.3rem solid #0ff;
        }
    }

    #label {
        position: absolute;
        transform: translate(-50%, -50%);

        width: 16rem;
        text-align: center;
        font-size: 1.3rem;
        color: #703b00;
        text-shadow: #e6dfd2 1px 0 0, #e6dfd2 -1px 0 0, #e6dfd2 0 1px 0,
        #e6dfd2 0 -1px 0, #e6dfd2 1px 1px 0, #e6dfd2 -1px 1px 0,
        #e6dfd2 1px -1px 0, #e6dfd2 -1px -1px 0;
    }
</style>
