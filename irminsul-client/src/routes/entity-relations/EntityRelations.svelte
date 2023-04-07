<script lang="ts">
    import type { EntityRelations } from "../../model/EntityRelations";
    import PartialRelation from "./PartialRelation.svelte";

    export let entityRelations: EntityRelations;
</script>

<div id="entity-relations" class="font-hywh-65w">
    <div id="avatar">
        <img src="/asset/img/avatar/{entityRelations.id}.png" alt="" />
    </div>

    <div id="translation">{entityRelations.translation}</div>

    <div id="relations">
        <div id="as-both">
            {#each entityRelations.asBoth as r}
                <PartialRelation {...r} reversed={false} />
            {/each}
        </div>

        <div id="as-object">
            {#each entityRelations.asObject as r}
                <PartialRelation {...r} reversed={true}/>
            {/each}
        </div>

        <div id="as-subject">
            {#each entityRelations.asSubject as r}
                <PartialRelation {...r} reversed={false} />
            {/each}
        </div>
    </div>
</div>

<style lang="scss">
    #avatar {
        height: fit-content; width: fit-content;
        margin: 2rem auto;

        border: 0.5rem solid #bda277;
        border-radius: 100%;

        overflow: hidden;

        & > img {
            height: 20rem;
            display: block;
        }
    }
    
    #translation {
        color: #eee;
        font-size: 3rem;
        margin: 2rem 0;
        text-align: center;
    }

    #relations {
        width: 30rem;
        max-width: 100%;

        margin: 0 auto;
        padding: 1rem;
        border-radius: 0.5rem;

        background-color: #F5F1E5;
    }
</style>
